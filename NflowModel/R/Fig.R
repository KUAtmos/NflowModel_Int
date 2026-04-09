library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(tidyverse)

NH3_df <- read_csv("../data/NH3_CEDS_emissions_by_country_sector_v_2025_03_18.csv")
NH3_df2 <- NH3_df %>%
      pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "Value") %>%
      mutate(year = as.integer(str_remove(year, "X"))) %>%
      filter(year == 2010 | year == 2020) %>%
      filter(sector == "3D_Soil-emissions") %>%
      group_by(sector, year) %>%
      summarise(Value = sum(Value))


NOx_df <- read_csv("../NOx_CEDS_emissions_by_country_sector_v_2025_03_11.csv")
NOx_df2 <- NOx_df %>%
      pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "Value") %>%
      mutate(year = as.integer(str_remove(year, "X"))) %>%
      filter(year >= 2000 & year <= 2023) %>%
      group_by(sector, year) %>%
      summarise(Value = sum(Value))

N2O_df <- read_csv("../N2O_CEDS_emissions_by_country_sector_v_2025_03_11.csv")
N2O_df2 <- N2O_df %>%
      pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "Value") %>%
      mutate(year = as.integer(str_remove(year, "X"))) %>%
      filter(year >= 2000 & year <= 2023) %>%
      group_by(sector, year) %>%
      summarise(Value = sum(Value))

df_raw <- read_csv("../data/Result_ver6.csv")

df_plot <- df_raw %>%
  mutate(
    Year = case_when(
      SCENARIO == "Base2010" ~ "2010",
      grepl("2030", SCENARIO) ~ "2030",
      grepl("2050", SCENARIO) ~ "2050",
      grepl("2100", SCENARIO) ~ "2100"
    ),
    ScenarioType = case_when(
      SCENARIO == "Base2010" ~ "Baseline",
      grepl("Base", SCENARIO) ~ "Baseline",
      grepl("Mit", SCENARIO) ~ "500C"
    ),
    group = paste(Year, ScenarioType),
    
    # ベース位置
    x_base = case_when(
      group == "2010 Baseline" ~ 1,
      group == "2030 Baseline" ~ 2,
      group == "2030 500C"     ~ 3,
      group == "2050 Baseline" ~ 4,
      group == "2050 500C"     ~ 5,
      group == "2100 Baseline" ~ 6,
      group == "2100 500C"     ~ 7
    ),
    # INかOUTかでずらす（INは-0.15, OUTは+0.15）
    x_pos = ifelse(grepl("^IN", Nf), x_base - 0.15, x_base + 0.15),
    x_label = case_when(
      SCENARIO == "Base2010" ~ "2010",
      TRUE ~ paste(Year, ScenarioType, sep = "_")
    )
  )

df_plot <- df_plot %>%
  mutate(Nf = factor(Nf, levels = c(
    "INfer",
    "INman",
    "INdep",
    "INfix",
    "OUTlea",
    "OUTcrop",
    "OUTgas",
    "OUTres"   
  )))

  plot1 <- ggplot(df_plot, aes(x = x_pos, y = Total, fill = Nf)) +
  geom_bar(stat = "identity", width = 0.25) +
  scale_x_continuous(
    breaks = unique(df_plot$x_base),
    labels = unique(df_plot$x_label),
    expand = expansion(add = 0.2)
  ) +
  geom_bar(stat = "identity", width = 0.25, color = "black", linewidth = 0.5) +
  scale_fill_manual(
 values = c(
  "INfer"   = "#a6cee3",   
  "INman"   = "#33a02c",
  "INdep"   = "#ffff99",
  "INfix"   = "#b2df8a",
  "OUTcrop" = "#fdbf6f",
  "OUTgas"  = "#fb9a99",
  "OUTlea"  = "#e31a1c",
  "OUTres"  = "#8b4513"    
)) +
  labs(x = "", y = "Tg N yr⁻¹", fill = "N flow") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  
  panel.grid.minor = element_blank(),  
  axis.line = element_line(color = "black"), 
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
    text = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )



ggsave(filename = "../output/plot1_finalrep.png", plot = plot1, width = 7, height = 6, dpi = 300)




df_raw2 <- df_raw %>%
  mutate(Total = Cropland + Pasture) %>%
  mutate(
    INOUT = ifelse(grepl("^IN", Nf), "IN", "OUT"),
    Year = case_when(
      SCENARIO == "Base2010" ~ 2010,
      grepl("2030", SCENARIO) ~ 2030,
      grepl("2050", SCENARIO) ~ 2050,
      grepl("2100", SCENARIO) ~ 2100
    )
  ) %>%
  group_by(INOUT, SCENARIO, Year) %>%
  summarise(
    TotalSum = sum(Total, na.rm = TRUE),
    CroplandSum = sum(Cropland, na.rm = TRUE),
    PastureSum = sum(Pasture, na.rm = TRUE),
    .groups = "drop"
  )

df_summary <- df_raw2 %>%
  mutate(
    INOUT = ifelse(grepl("^IN", Nf), "IN", "OUT"),
    Year = case_when(
      SCENARIO == "Base2010" ~ 2010,
      grepl("2030", SCENARIO) ~ 2030,
      grepl("2050", SCENARIO) ~ 2050,
      grepl("2100", SCENARIO) ~ 2100
    )
  ) %>%
  group_by(INOUT, SCENARIO, Year) %>%
  summarise(
    TotalSum = sum(Total, na.rm = TRUE),
    CroplandSum = sum(Cropland, na.rm = TRUE),
    PastureSum = sum(Pasture, na.rm = TRUE),
    .groups = "drop"
  )

df_in <- df_summary %>%
  filter(INOUT == "IN") %>%
  pivot_longer(cols = c(CroplandSum, PastureSum),
               names_to = "LandUse",
               values_to = "Value") %>%
  mutate(
    LandUse = recode(LandUse,
                     "CroplandSum" = "Cropland",
                     "PastureSum" = "Pasture"),
    group = paste(Year, SCENARIO),
    x_base = case_when(
      group == "2010 Base2010" ~ 1,
      group == "2030 Base2030" ~ 2,
      group == "2030 Mit2030"  ~ 3,
      group == "2050 Base2050" ~ 4,
      group == "2050 Mit2050"  ~ 5,
      group == "2100 Base2100" ~ 6,
      group == "2100 Mit2100"  ~ 7
    ),
    x_label = case_when(
      SCENARIO == "Base2010" ~ "2010",
      TRUE ~ paste(Year, sub("Base", "Baseline", SCENARIO), sep = "_")
    )
  )

# プロット作成
plot_in_only <- ggplot(df_in, aes(x = x_base, y = Value, fill = LandUse)) +
  geom_bar(stat = "identity", width = 0.4, color = "black", linewidth = 0.5) +
  scale_x_continuous(
    breaks = unique(df_in$x_base),
    labels = unique(df_in$x_label),
    expand = expansion(add = 0.2)
  ) +
  scale_fill_manual(values = c(
      "Cropland" = "#2ca02c",  # 緑系（落ち着いたミントグリーン）
      "Pasture"  = "#8c510a"   # 茶系（温かみのあるブラウン）
    )) +
  labs(x = "", y = "Tg N yr⁻¹", fill = "Land use") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
    text = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  )

# 保存
ggsave("../output/plot2.png", plot = plot_in_only, width = 7, height = 6, dpi = 300)


df_2010 <- df_raw %>%
  filter(SCENARIO == "Base2010", Nf != "OUTres") %>%  # OUTres を除外
  select(Nf, Cropland, Pasture) %>%
  pivot_longer(cols = c(Cropland, Pasture), names_to = "LandType", values_to = "Value")

plot3 <- ggplot(df_2010, aes(x = Nf, y = Value, fill = LandType)) +
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.3, width = 0.6) +  
  scale_x_discrete(expand = expansion(mult = c(0, 0.02))) +  
  labs(
    x = "Nitrogen Flow",
    y = "Tg N yr⁻¹",
    fill = "Land Type",
  ) +
  scale_fill_manual(
    values = c("Cropland" = "#3cb371", "Pasture" = "#8b4513")
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
    text = element_text(size = 20)
  )
ggsave("../output/plot3.png", plot = plot3, width = 7, height = 6, dpi = 300)


plot3 <- ggplot(df_2010, aes(x = Nf, y = Value, fill = LandType)) +
  geom_bar(stat = "identity", position = "fill", color = "black", linewidth = 0.3, width = 0.6) +
  geom_hline(yintercept = 1, color = "black", linewidth = 0.6) +
  scale_x_discrete(expand = expansion(mult = c(0, 0.02))) +
  labs(
    x = "Nitrogen Flow",
    y = "Share (%)", 
    fill = "Land Type"
  ) +
  scale_fill_manual(
    values = c("Cropland" = "#3cb371", "Pasture" = "#8b4513")
  ) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
    text = element_text(size = 20)
  )

ggsave("../output/plot3_relative2.png", plot = plot3, width = 7, height = 6, dpi = 300)


#Kyoeiken 2025
library(ggpattern)


library(ggplot2)
library(dplyr)
library(ggpattern)
library(purrr)


pattern_list <- c(
  "INfer"   = "stripe45",
  "INman"   = "circle",
  "INdep"   = "crosshatch",
  "INfix"   = "pch",
  "OUTcrop" = "polygon_tiling",
  "OUTgas"  = "none1",
  "OUTlea"  = "none2",
  "OUTres"  = "none3"
)


pattern_mapping <- c(
  "stripe45" = "stripe",
  "circle" = "circle",
  "crosshatch" = "crosshatch",
  "pch" = "pch",
  "polygon_tiling" = "polygon_tiling",
  "none1" = "none",
  "none2" = "none",
  "none3" = "none"
)


pattern_angle_list <- c(
  "INfer"   = 45,
  "INman"   = 0,
  "INdep"   = 0,
  "INfix"   = 0,
  "OUTcrop" = 0,
  "OUTgas"  = 0,
  "OUTlea"  = 0,
  "OUTres"  = 0
)


fill_colors <- c(
  "INfer"   = "#e0e0e0",
  "INman"   = "#e0e0e0",
  "INdep"   = "#e0e0e0",
  "INfix"   = "#e0e0e0",
  "OUTcrop" = "#e0e0e0",
  "OUTgas"  = "#777777",  
  "OUTlea"  = "#aaaaaa",  
  "OUTres"  = "#f0f0f0"   
)


df_plot <- df_raw %>%
  mutate(
    Year = case_when(
      SCENARIO == "Base2010" ~ "2010",
      grepl("2030", SCENARIO) ~ "2030",
      grepl("2050", SCENARIO) ~ "2050",
      grepl("2100", SCENARIO) ~ "2100"
    ),
    ScenarioType = case_when(
      SCENARIO == "Base2010" ~ "Baseline",
      grepl("Base", SCENARIO) ~ "Baseline",
      grepl("Mit", SCENARIO) ~ "500C"
    ),
    group = paste(Year, ScenarioType),
    x_base = case_when(
      group == "2010 Baseline" ~ 1,
      group == "2030 Baseline" ~ 2,
      group == "2030 500C"     ~ 3,
      group == "2050 Baseline" ~ 4,
      group == "2050 500C"     ~ 5,
      group == "2100 Baseline" ~ 6,
      group == "2100 500C"     ~ 7
    ),
    x_pos = ifelse(grepl("^IN", Nf), x_base - 0.15, x_base + 0.15),
    x_label = case_when(
      SCENARIO == "Base2010" ~ "2010",
      TRUE ~ paste(Year, ScenarioType, sep = "_")
    ),
    pattern_id = pattern_list[Nf],
    pattern = pattern_mapping[pattern_id],
    pattern_angle = pattern_angle_list[Nf]
  )

# pattern_id をそのまま aes() に使う
pattern_values <- c(
  "stripe45" = "stripe",
  "circle" = "circle",
  "crosshatch" = "crosshatch",
  "pch" = "pch",
  "polygon_tiling" = "polygon_tiling",
  "none1" = "none",
  "none2" = "none",
  "none3" = "none"
)

# 凡例ラベル（pattern_id → Nf）
pattern_labels <- c(
  "stripe45" = "INfer",
  "circle" = "INman",
  "crosshatch" = "INdep",
  "pch" = "INfix",
  "polygon_tiling" = "OUTcrop",
  "none1" = "OUTgas",
  "none2" = "OUTlea",
  "none3" = "OUTres"
)


legend_order <- names(pattern_list)

# 描画
plot1 <- ggplot(df_plot, aes(
  x = x_pos,
  y = Total,
  fill = Nf,
  pattern = pattern_id,
  pattern_angle = pattern_angle
)) +
  geom_bar_pattern(
    aes(pattern = pattern_id, pattern_angle = pattern_angle),
    stat = "identity",
    width = 0.25,
    color = "black",
    fill = fill_colors[df_plot$Nf],
    pattern_fill = "black",
    pattern_density = 0.4,
    pattern_spacing = 0.02,
    pattern_key_scale_factor = 0.8
  ) +
  scale_fill_manual(
    values = fill_colors,
    labels = names(fill_colors)
  ) +
  scale_pattern_manual(
    values = pattern_values,
    labels = pattern_labels,
    breaks = names(pattern_labels)[match(legend_order, pattern_labels)]
  ) +
  scale_x_continuous(
    breaks = unique(df_plot$x_base),
    labels = unique(df_plot$x_label),
    expand = expansion(add = 0.2)
  ) +
  labs(
    x = "", y = "Tg N yr⁻¹",
    fill = "N flow", pattern = "N flow"
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(pattern_angle = "none") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
    text = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 16)
  )

# 保存
ggsave(filename = "../output/plot1_kyoeiken.png", plot = plot1, width = 7, height = 6, dpi = 300)

#this R script is to estimate the parameter of N budget model 
#Fertilizer input
#The proportion of synthetic fertiliser applied to arable land and to pasture

fert_crop_pasture_ratio <- read_csv("../data/FUBC_1_to_9_data.csv") %>% #input data
 dplyr::select("ISO3_code","Year","Crop", "Crop_area_k_ha","N_k_t")

region_iso_17 <- readLines("../data/region_iso_17.map")
df_region_iso_17 <- do.call(rbind, strsplit(region_iso_17, "\\s+")) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    setNames(c("ISO3_code", "RegionCode"))

fert_crop_pasture_ratio2 <-fert_crop_pasture_ratio %>%
left_join(df_region_iso_17, by = c("ISO3_code" = "ISO3_code"))

fert_crop_pasture_ratio3 <- fert_crop_pasture_ratio2 %>%
  group_by(RegionCode, Crop, Year) %>%
  summarise(N_k_t = sum(N_k_t), Crop_area = sum(Crop_area_k_ha)
  )

fert_crop_pasture_ratio4_grass <- fert_crop_pasture_ratio3 %>%
  mutate(CropType = ifelse(Crop == "Grassland", "Grassland", "Other")) %>%
  group_by(RegionCode, CropType, Year) %>%
  summarise(Total_N_k_t = sum(N_k_t), Total_area = sum(Crop_area)) %>%
  filter(CropType == "Grassland") %>%
  pivot_wider(names_from = CropType, values_from = c(Total_N_k_t)) %>%
  rename(Total_area_grass = Total_area)

fert_crop_pasture_ratio4_other <- fert_crop_pasture_ratio3 %>%
  mutate(CropType = ifelse(Crop == "Grassland", "Grassland", "Other")) %>%
  group_by(RegionCode, CropType, Year) %>%
  summarise(Total_N_k_t = sum(N_k_t), Total_area = sum(Crop_area)) %>%
  filter(CropType == "Other") %>%
  pivot_wider(names_from = CropType, values_from = c(Total_N_k_t)) %>%
  rename(Total_area_other = Total_area)

fert_crop_pasture_ratio4 <- fert_crop_pasture_ratio4_grass %>%
  left_join(fert_crop_pasture_ratio4_other, by = c("RegionCode", "Year"))

fert_crop_pasture_ratio4$Year <- as.numeric(sub("/.*", "",fert_crop_pasture_ratio4$Year))
fert_crop_pasture_ratio4 <- as.data.frame(fert_crop_pasture_ratio4) #N_k_t

max_value <- max(fert_crop_pasture_ratio4$Total_area_grass, 
                 fert_crop_pasture_ratio4$Total_area_other, 
                 na.rm = TRUE)

fert_crop_pasture_ratio4.5 <- fert_crop_pasture_ratio4 %>%
  mutate(
    Total_area_grass = Total_area_grass / max_value,
    Grassland = Grassland / max_value,
    Total_area_other = Total_area_other / max_value,
    Other = Other / max_value
  )

fert_crop_pasture_ratio5 <- amelia(x = fert_crop_pasture_ratio4.5, m = 1000, cs = "RegionCode", ts = "Year") #Grassland and Other mean the amount of fertilizer use (N_k_t)

imputed_dfs <- lapply(1:1000, function(i) {
  imputed_data <- fert_crop_pasture_ratio5$imputations[[i]]
  imputed_data[imputed_data < 0] <- 0
  return(imputed_data)
})

numeric_columns <- sapply(imputed_dfs[[1]], is.numeric)
sum_imputed_df_numeric <- Reduce(`+`, lapply(imputed_dfs, function(df) df[, numeric_columns]))
mean_imputed_df_numeric <- sum_imputed_df_numeric / 1000
non_numeric_columns <- imputed_dfs[[1]][, !numeric_columns]
mean_imputed_df <- cbind(non_numeric_columns, mean_imputed_df_numeric)

fert_crop_pasture_ratio6 <- mean_imputed_df %>%
  group_by(non_numeric_columns) %>%
  slice_max(Year, with_ties = FALSE) %>%
  ungroup()

fert_crop_pasture_ratio6.5 <- fert_crop_pasture_ratio6 %>%
  mutate(
    Total_area_grass = Total_area_grass * max_value,
    Grassland = Grassland * max_value,
    Total_area_other = Total_area_other * max_value,
    Other = Other * max_value
  )

fert_crop_pasture_ratio7 <- fert_crop_pasture_ratio6.5[,c(1, 2, 4, 6)] %>%
  mutate(Grassland_rate = Grassland/(Grassland + Other)) %>%
  mutate(Other_rate = Other/(Grassland + Other)) %>%
  rename(RegionCode = non_numeric_columns)
fert_crop_pasture_ratio7 <- fert_crop_pasture_ratio7[,c(1,5,6)]

#Fertilizer amount data used as the result of AIM/Hub
Global_17 <- "../data/global_17_IAMC.gdx" #input data
Global_17_data <- gdxrrw::rgdx(Global_17)
IAMC_Template <- rgdx.param(Global_17, 'IAMC_Template')

IAMC_Template_CRP <- filter(IAMC_Template, VEMF == "Agr_Pro_NonEneCro") #million t/yr

base_values <- IAMC_Template_CRP %>%
  filter(YEMF == "2015") %>%
  select(SCENARIO, REMF, VEMF, IAMC_Template) %>%
  rename(BaseValue = IAMC_Template)

Rate_df <- IAMC_Template_CRP %>%
  left_join(base_values, by = c("SCENARIO", "REMF", "VEMF")) %>%
  mutate(Ratio = IAMC_Template/BaseValue) %>%
  select(SCENARIO, REMF, VEMF, YEMF, Ratio)

IAMC_Template_FERT_Past <- filter(IAMC_Template, VEMF == "Fer_Use_Nit", YEMF == "2005" | YEMF == "2010" | YEMF == "2015") 

IAMC_Template_FERT_Past2 <- IAMC_Template_FERT_Past[,-3] %>%
  rename(Fert_Use = IAMC_Template) #TgN/yr

IAMC_Template_FERT_Past2$YEMF <- as.numeric(as.character(IAMC_Template_FERT_Past2$YEMF))

IAMC_Template_FERT_2015 <- filter(IAMC_Template, VEMF == "Fer_Use_Nit", YEMF == "2015") #TgN/yr

df_sorted <- Rate_df[order(Rate_df$SCENARIO, Rate_df$REMF), ]

df_sorted2 <- df_sorted[,-3]

IAMC_Template_FERT_2015_2 <- IAMC_Template_FERT_2015[rep(1:nrow(IAMC_Template_FERT_2015), each = 20), ]

IAMC_Template_FERT_2015_3 <- IAMC_Template_FERT_2015_2[,-4]

tmp1 <- df_sorted2$Ratio
tmp2 <- IAMC_Template_FERT_2015_3$IAMC_Template
tmp3 <- tmp1*tmp2

df_sorted2$Ratio <- tmp3 

df_sorted2 <- df_sorted2 %>%
  rename(Fert_Use = Ratio)

df_sorted2$YEMF <- as.numeric(as.character(df_sorted2$YEMF))

Fert_Use_Region <- df_sorted2 %>%
  filter(YEMF >2015) %>%
  bind_rows(IAMC_Template_FERT_Past2) %>%
  arrange(YEMF) %>%
  arrange(SCENARIO, REMF) %>%
  rename(RegionCode = REMF)

#Land cover ratio(LC/ΣlonlatLC) from AIM/PLUM
AIMPLUM_BaU <- "../data/results_SSP2_BaU_NoCC.gdx" #input data
AIMPLUM_BaU_data <- gdxrrw::rgdx(AIMPLUM_BaU)
VY_load <- rgdx.param(AIMPLUM_BaU, 'VY_load') %>%
  rename(G = i4)

data_prep <- "../data/data_prep.gdx"
data_prep_data <- gdxrrw::rgdx(data_prep)
GA <- rgdx.param(data_prep, 'GA') #kha
MAP_GIJ <- rgdx.set(data_prep, 'MAP_GIJ') %>%
  rename(lat = I) %>%
  rename(lon = J)

VY_load2 <- VY_load %>%
  left_join(GA, by = "G") %>%
  mutate(LUAREA = value*GA*1000) #ha

LUAREA_Region <- VY_load2 %>%
  group_by(i1, i2, i3) %>%
  summarise(sum_LUAREA = sum(LUAREA))

VY_load2 <- VY_load2 %>%
  left_join(LUAREA_Region, by = c("i1", "i2", "i3")) %>%
  mutate(LURATIO = LUAREA/sum_LUAREA) %>%
  left_join(MAP_GIJ, by = c("G"))

VY_load2$lat <- as.numeric(as.character(VY_load2$lat))
VY_load2$lon <- as.numeric(as.character(VY_load2$lon))
VY_load2$i2 <- as.numeric(as.character(VY_load2$i2))

#yieldgrid
FAO_yield <- read_csv("../data/FAOSTAT_data_en_10-10-2024.csv") %>% #t
  select("Area Code (M49)","Element","Item", "Value") %>%
  mutate(`Area Code (M49)` = str_pad(`Area Code (M49)`, width = 3, pad = "0"))

Areacode_3cha <- read.csv("../data/UNSD_Methodology.csv", stringsAsFactors = FALSE)
a_column <- Areacode_3cha$Country_data
split_data <- str_split(a_column, ";")
extracted_data <- lapply(split_data, function(x) {
  three_letters <- x[length(x) - 3]
  number <- x[length(x) - 5]
  return(c(number, three_letters))
})

Areacode_ISO <- do.call(rbind, extracted_data) %>%
  as.data.frame()

colnames(Areacode_ISO) <- c("Area Code (M49)", "ISO3_code")

Areacode_17 <- Areacode_ISO %>%
  left_join(df_region_iso_17, by = c("ISO3_code"))

Areacode_17 <- Areacode_17[,c(1,3)] %>%
  add_row(`Area Code (M49)` = as.character(c(158, 159, 344, 570, 729, 876, 833, 832, 831, 772, 744, 732, 729, 666, 663, 660, 654, 652, 638, 612
  , 581, 574, 570, 534, 531, 500, 474, 336, 334, 312, 260, 254, 248, 239, 238, 184, 175, 166, 162, 086, 074, 010)), 
  RegionCode = c("CHN", "CHN", "CHN", "XSE", "XNF", "XSE", "XR25", "XR25", "XR25", "XSE", "XR25", "XNF", "XNF", "USA", "XLM", "XLM", "XAF", "XLM", "XAF", "XSE"
  , "XSE", "XSE", "XSE", "XLM", "XLM", "XLM", "XLM", "XR25", "XOC", "XLM", "XAF", "XLM", "XR25", "XLM", "XLM", "XSE", "XAF", "XOC", "XOC", "XAF", "XLM", "XLM")) %>%
  filter(!is.na(RegionCode))

FAO_yield <- FAO_yield %>%
  mutate(`Area Code (M49)` = as.character(`Area Code (M49)`))

FAO_yield <- FAO_yield %>%
  left_join(Areacode_17, by = c("Area Code (M49)"))

FAO_yield <- filter(FAO_yield, Item %in% c("Barley", "Maize (corn)", "Millet", "Potatoes", "Rice", "Sugar beet",
                                          "Sugar cane", "Wheat", "Beans, dry", "Groundnuts, excluding shelled",
                                          "Sorghum", "Cassava, fresh", "Sweet potatoes", "Green corn (maize)" )) %>%
mutate(value = ifelse(Item == "Beans, dry", Value * 1, Value)) #Moisture Content #元は1.13

FAO_yield2 <- FAO_yield %>%
  mutate(Item = recode(Item, "Barley"="GRO", 
                       "Maize (corn)"="GRO", "Millet"="GRO", "Potatoes"="OTH_A", "Rice"="PDR", "Sugar beet"="C_B",
                       "Sugar cane"="C_B", "Wheat"="WHT", "Beans, dry"="OSD", "Groundnuts, excluding shelled"="OSD",
                       "Sorghum"="GRO", "Cassava, fresh"="OTH_A", "Sweet potatoes"="OTH_A", "Green corn (maize)"="GRO",)) %>%
  filter(Item %in% c("PDR", "WHT", "GRO", "OSD", "C_B", "OTH_A")) %>%
  filter(!is.na(RegionCode)) %>%
  filter(Element == "Production") %>%
  group_by(RegionCode,Item) %>%
  summarise(Value = sum(Value)) %>%
  ungroup()

items <- c("PDR", "C_B", "GRO", "OSD", "OTH_A", "WHT")
years <- c(2005, 2010, 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)

yield_data <- lapply(items, function(item) {
  FAO_yield2 %>% filter(Item == item)
})

names(yield_data) <- paste0(items, "_Yield")

yieldgrid_list <- list()
yieldgrid_save_list <- list()

for (year in years) {
process_item <- function(item) {
  VY_IR <- VY_load2[, c(1, 2, 3, 4, 7, 8)] %>%
    filter(i3 == paste0(item, "IR")) %>%
    rename(!!paste0("sum_", item, "IR") := sum_LUAREA,
           !!paste0("LUAREA_", item, "IR") := LUAREA)

  VY_RF <- VY_load2[, c(1, 2, 3, 4, 7, 8)] %>%
    filter(i3 == paste0(item, "RF")) %>%
    rename(!!paste0("sum_", item, "RF") := sum_LUAREA,
           !!paste0("LUAREA_", item, "RF") := LUAREA)

  VY <- VY_IR %>%
    full_join(VY_RF, by = c("i1", "i2", "G")) %>%
    mutate(!!paste0("sum_", item, "RF") := replace_na(!!sym(paste0("sum_", item, "RF")), 0),
           !!paste0("LUAREA_", item, "RF") := replace_na(!!sym(paste0("LUAREA_", item, "RF")), 0)) %>%
    mutate(!!paste0("IR_Ratio_", item) := !!sym(paste0("LUAREA_", item, "IR")) / (!!sym(paste0("sum_", item, "IR")) + !!sym(paste0("sum_", item, "RF"))),
           !!paste0("ALL_Ratio_", item) := (!!sym(paste0("LUAREA_", item, "IR")) + !!sym(paste0("LUAREA_", item, "RF"))) / (!!sym(paste0("sum_", item, "IR")) + !!sym(paste0("sum_", item, "RF")))) %>%
    filter(i2 == as.character(year)) %>%
    select(i1, G, !!sym(paste0("IR_Ratio_", item)), !!sym(paste0("ALL_Ratio_", item))) %>%
    rename(RegionCode = i1)

  item_yield <- yield_data[[paste0(item, "_Yield")]]
  YG <- VY %>%
    full_join(item_yield, by = "RegionCode") %>%
    mutate(!!paste0("IR_Yield_", item) := !!sym(paste0("IR_Ratio_", item)) * Value,
           !!paste0("ALL_Yield_", item) := !!sym(paste0("ALL_Ratio_", item)) * Value)
  
  return(YG)
}

yield_data2 <- lapply(items, process_item)

names(yield_data2) <- paste0("YG_", items)
invisible(lapply(items, function(item) {
  assign(paste0("YG_", item), yield_data2[[paste0("YG_", item)]], envir = .GlobalEnv)
}))

yield_data_list <- list(YG_PDR, YG_C_B, YG_GRO, YG_OSD, YG_OTH_A, YG_WHT)
yieldgrid <- reduce(yield_data_list, full_join, by = c("RegionCode", "G"))
yieldgrid_save <- yieldgrid[,c(1,2,7,8,13,14,19,20,25,26,31,32,37,38)] 
yieldgrid <- yieldgrid_save %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(IR_Yield = IR_Yield_C_B) %>% #Change crop name
  mutate(ALL_Yield = ALL_Yield_C_B) #Change crop name
yieldgrid <- yieldgrid[,c(1,2,15,16)]

yieldgrid <- yieldgrid %>%
  group_by(RegionCode) %>%
  mutate(
    IR_Yield_Ratio = IR_Yield / sum(IR_Yield, na.rm = TRUE),
    ALL_Yield_Ratio = ALL_Yield / sum(ALL_Yield, na.rm = TRUE)
  ) %>%
  ungroup()

 yieldgrid_list[[as.character(year)]] <- yieldgrid
 yieldgrid_save_list[[as.character(year)]] <- yieldgrid_save
}

yieldgrid_combined <- bind_rows(lapply(names(yieldgrid_list), function(year) {
  yieldgrid_list[[year]] %>%
    mutate(Year = as.numeric(year)) 
}))

yieldgrid_combined2 <- yieldgrid_combined[,c(1,2,5,6,7)] %>%
  rename(YEMF = Year)

yieldgrid_save_combined <- bind_rows(lapply(names(yieldgrid_save_list), function(year) {
  yieldgrid_save_list[[year]] %>%
    mutate(Year = as.numeric(year)) 
})) %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))

#agrsys
IFA_fertuse <- read_csv("../data/IFADATA_Plant_Nutrition_query.csv") #All data are in 000 metric tonnes of nutrients

split_data2 <- str_split(a_column, ";")
extracted_data2 <- lapply(split_data, function(x) {
  three_letters <- x[length(x) - 5]
  number <- x[length(x) - 6]
  return(c(number, three_letters))
})
Countryname_Areacode <- do.call(rbind, extracted_data2) %>%
  as.data.frame()
colnames(Countryname_Areacode) <- c("Country", "Area Code (M49)")

Countryname_17 <- Countryname_Areacode %>%
  left_join(Areacode_17, by = "Area Code (M49)")
Countryname_17 <- Countryname_17 %>%
  add_row(Country = "USA", `Area Code (M49)` = "840", RegionCode = "USA") %>%
  add_row(Country = "Turkey", `Area Code (M49)` = "792", RegionCode = "TUR") %>%
  add_row(Country = "UK", `Area Code (M49)` = "826", RegionCode = "XE25")

IFA_fertuse2 <- IFA_fertuse %>%
  left_join(Countryname_17, by = "Country")

IFA_fertuse3 <- IFA_fertuse2 %>%
  mutate(Product = recode(Product,"Ammonium nitrate (N)"="chemical",
                       "Ammonia (N)"="chemical",
                       "Ammonium Phosphate (N)"="chemical",
                       "Ammonium Sulphate (N)"="chemical",
                       "Calcium Ammonium Nitrate (N)"="mineral",
                       "Potassium Nitrate"="mineral",
                       "Urea (N)"="chemical",
                       "Urea Ammonium Nitrate (N)"="chemical",
                       "NK (N)"="mineral",
                       "NP (N)"="chemical",
                       "NPK (N)"="chemical",
                       "Other (N)"="chemical",
                    ))
IFA_fertuse4 <- IFA_fertuse3 %>%
  filter(!is.na(RegionCode)) %>%
  filter(Product %in% c("chemical","mineral")) %>%
  group_by(RegionCode,Product) %>%
  summarise(Total = sum(Consumption)) %>%
  ungroup()

IFA_fertuse5 <- IFA_fertuse4 %>%
  pivot_wider(names_from = Product, values_from = Total, values_fill = list(Total = 0))

IFA_fertuse5 <- IFA_fertuse5 %>%
  mutate(
    chemical_ratio = chemical / (chemical + mineral),
    mineral_ratio = mineral / (chemical + mineral)
  )
IFA_fertuse5 <- IFA_fertuse5[,c(1,4,5)]

#INfer to cropland
Fert_Use_Region_Crop_Pas <- Fert_Use_Region %>%
  left_join(fert_crop_pasture_ratio7, by = "RegionCode") %>%
  mutate(Fert_Use_Crop = Fert_Use*Other_rate, Fert_Use_Pas = Fert_Use*Grassland_rate)

Fert_Use_Region2 <- Fert_Use_Region_Crop_Pas %>%
  left_join(IFA_fertuse5, by = "RegionCode") %>%
  mutate(Chemical_Fert_Use = Fert_Use_Crop*chemical_ratio, Mineral_Fert_Use = Fert_Use_Crop*mineral_ratio)
Fert_Use_Region2 <- Fert_Use_Region2[,c(1,2,3,11,12)]

yieldgrid3 <- yieldgrid_combined2 %>%
  left_join(Fert_Use_Region2, by = c("RegionCode", "YEMF")) %>%
  mutate(Chemical_Grid = ALL_Yield_Ratio*Chemical_Fert_Use, Mineral_Grid = IR_Yield_Ratio*Mineral_Fert_Use) %>%
  mutate(Fert_Use_Grid = Chemical_Grid+Mineral_Grid)
yieldgrid3 <- yieldgrid3[,c(1,2,5,6,11)]

INfercrop <- yieldgrid3 %>%
  group_by(SCENARIO, YEMF) %>%
  summarise(
    INfercrop = sum(ifelse(is.na(Fert_Use_Grid), 0, Fert_Use_Grid))
  ) %>%
  ungroup()

#INfer to pasture
Fert_Use_Region_Pas <- Fert_Use_Region_Crop_Pas[,c(1,2,3,8)]

VY_load_pas <- VY_load2 %>%
  filter(i3 == "PAS") %>%
  rename(RegionCode = i1, YEMF = i2)
VY_load_pas <- VY_load_pas[,c(1,2,3,4,9)] %>%
  left_join(Fert_Use_Region_Pas, by = c("RegionCode", "YEMF")) %>%
  mutate(Fert_Use_Grid = Fert_Use_Pas*LURATIO)

VY_load_pas2 <- VY_load_pas[,c(1,2,3,4,6,8)]

INferpas <- VY_load_pas2 %>%
  group_by(SCENARIO, YEMF) %>%
  summarise(INferpas = sum(Fert_Use_Grid, na.rm = TRUE)) %>%
  ungroup()

INfer <- INfercrop %>%
  left_join(INferpas, by = c("SCENARIO", "YEMF")) %>%
  group_by(SCENARIO, YEMF) %>%
  summarise(INfer = INfercrop+INferpas)

INfercroptotal <- INfercrop
INferpastotal <- INferpas

INferdis <- yieldgrid3[, c(2,3,4,5)] %>%
  full_join(VY_load_pas2[, c(2,4,5,6)], by = c("G","YEMF", "SCENARIO")) %>%
  mutate(Fert_Use_Grid.y = ifelse(is.na(Fert_Use_Grid.y), 0, Fert_Use_Grid.y)) %>%
  mutate(Fert_Use_Grid.x = ifelse(is.na(Fert_Use_Grid.x), 0, Fert_Use_Grid.x)) %>%
  mutate(Fert_Use_Grid = Fert_Use_Grid.x + Fert_Use_Grid.y)
INferdis <- INferdis[, c(1,2,3,6)]

INfercropdis <- yieldgrid3[,c(2,3,4,5)]
INferpasdis <- VY_load_pas2[, c(2,4,5,6)]


INfercropdis <- INfercropdis %>%
   filter(SCENARIO %in% "SSP2_BaU_NoCC") #scenario
INfercropdis <- INfercropdis[, c(1,2,4)]

MAP_GIJ$lat <- as.numeric(as.character(MAP_GIJ$lat))
MAP_GIJ$lon <- as.numeric(as.character(MAP_GIJ$lon))

#Cropland distribution
Cropdistribution <- VY_load2[, c(2,3,7,10,11)] %>%
  filter(i2 == 2005) %>%
  filter(i3 %in% c("C_BIR", "C_BRF")) %>%
  group_by (i2, lat, lon) %>%
  summarise(LUAREA = sum(LUAREA, na.rm = TRUE))

Cropdistribution2 <- Cropdistribution %>%
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25) %>% #ha
  mutate(LUAREA = LUAREA/1000) #kha
Cropdistribution2 <- Cropdistribution2[, c(1,4,5,6)]

custom_colors <- c("#d4e6f1", "#7fb3d5", "#2e86c1", "#f5b041", "#d35400", "#c0392b")

plot1 <- ggplot() +
  geom_tile(data = Cropdistribution2, aes(x = x, y = y, fill = LUAREA)) + 
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.075, 0.15, 0.25, 0.625, 1),
    limits = c(0, 60), 
    breaks = c(0, 15, 30, 45, 60),  
    labels = c("0", "15", "30", "45", "60+"), 
    oob = scales::squish,
    name = "cropland area\n(kha grid^-1 yr^-1)",  
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Cropland Area"
  ) +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/Cropland_2005_C_B.png",  
  plot = plot1,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)

#Region Crop Type
VY_load_type <- VY_load2 %>%
  select(i1, i2, i3, G, LUAREA) %>%
  group_by(i1, i2, i3) %>%
  summarise(LUAREA = sum(LUAREA))

library(dplyr)
library(tidyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(patchwork)
library(cowplot)
world <- ne_countries(scale = "medium", returnclass = "sf")

#Cropland distribution
#Cropdistribution <- VY_load2[, c(2,3,7,10,11)] %>%
# filter(i2 == 2010) %>%
#  filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A")) %>%
#  group_by (i2, lat, lon) %>%
#  summarise(LUAREA = sum(LUAREA, na.rm = TRUE))

Totaldistribution <- VY_load2[, c(2,3,7,10,11)] %>%
  filter(i2 == 2010) %>%
  filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A", "PAS")) %>%
  group_by (i2, lat, lon) %>%
  summarise(LUAREA = sum(LUAREA, na.rm = TRUE))


Cropdistribution2 <- Totaldistribution %>%
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25) %>% #ha
  mutate(LUAREA = LUAREA/1000) #kha
Cropdistribution2 <- Cropdistribution2[, c(1,4,5,6)]

custom_colors <- c("#d4e6f1", "#7fb3d5", "#2e86c1", "#f5b041", "#d35400", "#c0392b")

plot1 <- ggplot() +
  geom_tile(data = Cropdistribution2, aes(x = x, y = y, fill = LUAREA)) + 
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.075, 0.15, 0.25, 0.625, 1),
    limits = c(0, 60), 
    breaks = c(0, 15, 30, 45, 60),  
    labels = c("0", "15", "30", "45", "60+"), 
    oob = scales::squish,
    name = "cropland area\n(kha grid^-1 yr^-1)",  
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Cropland Area"
  ) +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/agrland_2010.png",  
  plot = plot1,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)

#INfer Map, OUTgasMap
Totaldistribution <- VY_load2[, c(2,3,7,10,11)] %>%
  filter(i2 == 2005) %>%
  filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A", "PAS")) %>%
  group_by (i2, lat, lon) %>%
  summarise(LUAREA = sum(LUAREA, na.rm = TRUE))

INferdismap <- INferdis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2005)  #Substitute variable
INferdismap <- INferdismap[, c(2,3,4,5)] %>%
  left_join(Totaldistribution, by = c("lat", "lon")) %>% #ha
  mutate(Fert_Use_Flux = Fert_Use_Grid*100000000/LUAREA) %>% #gN/m2/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
INferdismap <- INferdismap[, c(1,7,8,9)] 

OUTgasdismap <- OUTgasdis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2005)  #Substitute variable
OUTgasdismap <- OUTgasdismap[, c(2,3,4,5)] %>%
  left_join(gridarea2, by = c("lat", "lon")) %>% #kha
  mutate(OUTgasgrid = OUTgasgrid*1000000/area) %>% #kg/ha/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
OUTgasdismap <- OUTgasdismap[, c(1,2,6,7)]

world <- ne_countries(scale = "medium", returnclass = "sf")

custom_colors <- c("#d4e6f1", "#7fb3d5", "#2e86c1", "#f5b041", "#d35400", "#c0392b")

plot1 <- ggplot() +
  geom_tile(data = INferdismap, aes(x = x, y = y, fill = Fert_Use_Flux)) + 
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.075, 0.15, 0.25, 0.625, 1),
    limits = c(0, 20), 
    breaks = c(0, 5, 10, 15, 20),  
    labels = c("0", "5", "10", "15", "20+"), 
    oob = scales::squish,
    name = "Fertilizer application\n(g N m^-2 agricultural land yr^-1)",  
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Fertilizer Application"
  ) +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/fertilizer_application_2005.png",  
  plot = plot1,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)

custom_colors <- c("#f7fbff", "#fdae61", "#d7191c")

plot2 <- ggplot() +
  geom_tile(data = OUTgasdismap, aes(x = x, y = y, fill = OUTgasgrid)) +  
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.025, 0.05, 0.1, 0.15, 0.25, 0.5, 0.75, 1), 
    limits = c(0, 20), 
    breaks = c(0, 5, 10, 15, 20),  
    labels = c("0", "5", "10", "15", "20+"),  
    oob = scales::squish,
    name = "Gaseous Losses\n(kg ha^-1 yr^-1)", 
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Gaseous Losses"
  ) +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(size = 15), 
    legend.text = element_text(size = 12),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/outgas_2005.png",  
  plot = plot2,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white")

#INfercropMap
INfercropdismap <- INfercropdis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2005)  #Substitute variable
INfercropdismap <- INfercropdismap[, c(2,3,4,5)] %>%
  left_join(Cropdistribution, by = c("lat", "lon")) %>% #ha
  mutate(Fert_Use_Flux = Fert_Use_Grid*100000000/LUAREA) %>% #gN/m2/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
INfercropdismap <- INfercropdismap[, c(1,7,8,9)] 

custom_colors <- c("#d4e6f1", "#7fb3d5", "#2e86c1", "#f5b041", "#d35400", "#c0392b")

plot1 <- ggplot() +
  geom_tile(data = INfercropdismap, aes(x = x, y = y, fill = Fert_Use_Flux)) + 
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.04, 0.07, 0.2, 0.75, 0.9, 1),
    limits = c(0, 25), 
    breaks = c(0, 5, 10, 15, 20, 25),  
    labels = c("0", "5", "10", "15", "20", "25+"), 
    oob = scales::squish,
    name = "Fertilizer application\n(g N m^-2 cropland yr^-1)",  
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Fertilizer Application"
  ) +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 15),  
    legend.text = element_text(size = 12),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/fertilizer_application_cropland_2005_C_B.png",  
  plot = plot1,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)

#INfercroptotalMap
INfercroptotaldismap <- INfercropdis %>% #Tg/yr
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2005) %>%  #Substitute variable
  mutate(Fert_Use_Grid = Fert_Use_Grid*1000) #Gg/yr
INfercroptotaldismap <- INfercroptotaldismap[, c(2,3,4,5)] %>%
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
INfercroptotaldismap <- INfercroptotaldismap[, c(1,2,5,6)] 

custom_colors <- c("#d4e6f1", "#7fb3d5", "#2e86c1", "#f5b041", "#d35400", "#c0392b")

plot1 <- ggplot() +
  geom_tile(data = INfercroptotaldismap, aes(x = x, y = y, fill = Fert_Use_Grid)) + 
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.075, 0.15, 0.25, 0.625, 1),
    limits = c(0, 4), 
    breaks = c(0, 1, 2, 3, 4),  
    labels = c("0", "1.0", "2.0", "3.0", "4.0+"), 
    oob = scales::squish,
    name = "Fertilizer application\n(GgN grid^-1 yr^-1)",  
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Fertilizer Application"
  ) +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/fertilizer_totalapplication_cropland_2005.png",  
  plot = plot1,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)

#INmancropMap
INmandismap <- INmandis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2005)  #Substitute variable
INmandismap <- INmandismap[, c(1,3,4,5)] %>%
  left_join(Cropdistribution, by = c("lat", "lon")) %>% #ha
  mutate(ManureGrid = ManureGrid*100000000/LUAREA) %>% #gN/m2/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
INmandismap <- INmandismap[, c(1,2,7,8)] 

custom_colors <- c("#d4e6f1", "#7fb3d5", "#2e86c1", "#f5b041", "#d35400", "#c0392b")

plot1 <- ggplot() +
  geom_tile(data = INmandismap, aes(x = x, y = y, fill = ManureGrid)) + 
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.075, 0.15, 0.25, 0.625, 1),
    limits = c(0, 2), 
    breaks = c(0, 0.5, 1, 1.5, 2),  
    labels = c("0", "0.5", "1.0", "1.5", "2.0+"), 
    oob = scales::squish,
    name = "Manure input\n(g N m^-2 agricultural land yr^-1)",  
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Manure Application"
  ) +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/manure_application_2005.png",  
  plot = plot1,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)

#OUTgascropMap
OUTgascropdismap <- OUTgascropdis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2005)  #Substitute variable
OUTgascropdismap <- OUTgascropdismap[, c(2,3,4,5)] %>%
  left_join(Cropdistribution, by = c("lat", "lon")) %>% #ha
  mutate(OUTgascropgrid = OUTgascropgrid*1000000000/LUAREA) %>% #kg/ha/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
OUTgascropdismap <- OUTgascropdismap[, c(1,2,7,8)]

custom_colors <- c("#f7fbff", "#fdae61", "#d7191c")

plot2 <- ggplot() +
  geom_tile(data = OUTgascropdismap, aes(x = x, y = y, fill = OUTgascropgrid)) +  
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.15, 0.30, 0.4, 0.45, 0.65, 0.85, 0.9, 1), 
    limits = c(0, 80), 
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80),  
    labels = c("0", "10", "20", "30", "40", "50", "60", "70", "80+"),  
    oob = scales::squish,
    name = "Gaseous Losses\n(kg ha^-1 cropland yr^-1)", 
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Gaseous Losses"
  ) +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 8),  
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/outgas_cropland_2005.png",  
  plot = plot2,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)


#Input
#Input Map 2100_500C
Totaldistribution <- VY_load2[, c(2,3,7,10,11)] %>%
  filter(i2 == 2100) %>% #year
  filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A", "PAS")) %>%
  group_by (i2, lat, lon) %>%
  summarise(LUAREA = sum(LUAREA, na.rm = TRUE))

Inputdismap <- Inputdis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2100)  #Substitute variable
Inputdismap <- Inputdismap[, c(2,3,4,5)] %>%
  left_join(Totaldistribution, by = c("lat", "lon")) %>% #ha
  mutate(Input = Input*100000000/LUAREA) %>% #gN/m2/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
Inputdismap <- Inputdismap[, c(2,7,8)] 

world <- ne_countries(scale = "medium", returnclass = "sf")

breaks_vals <- seq(0, 25, by = 2.5)  # 0, 2.5, 5, ..., 25
labels_vals <- c("0", "2.5", "5", "7.5", "10", "12.5", "15", "17.5", "20", "22.5", "25+")

custom_colors <- c(
  "#ffffff",  # white
  "#deebf7",  # light blue
  "#c7e9c0",  # light green
  "#74c476",  # medium green
  "#ffffb2",  # pale yellow
  "#fecc5c",  # soft orange
  "#fd8d3c",  # orange
  "#f16913",  # moderate orange-red (↓調整)
  "#d94801",  # soft reddish-brown (↓代替)
  "#a63603"   # medium brown (←旧最濃色より薄い)
)

Inputdismap_2100500C <- Inputdismap

plot_input <- ggplot() +
  geom_tile(data = Inputdismap_2100500C, aes(x = x, y = y, fill = Input)) +
  scale_fill_stepsn(
    colors = custom_colors,
    breaks = breaks_vals,
    limits = c(0, 25),
    labels = labels_vals,
    na.value = "white",
    name = "Total N input in 2100\n(g N m⁻² yr⁻¹)",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(12, "cm"),
      barheight = unit(0.2, "cm")  # ← 横棒を細く！
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) +
    labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/Input_500C_2100.png",
  plot = plot_input,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

#Input Map 2100_Base
Totaldistribution <- VY_load2[, c(2,3,7,10,11)] %>%
  filter(i2 == 2100) %>%
  filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A", "PAS")) %>%
  group_by (i2, lat, lon) %>%
  summarise(LUAREA = sum(LUAREA, na.rm = TRUE))

Inputdismap <- Inputdis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2100)  #Substitute variable
Inputdismap <- Inputdismap[, c(2,3,4,5)] %>%
  left_join(Totaldistribution, by = c("lat", "lon")) %>% #ha
  mutate(Input = Input*100000000/LUAREA) %>% #gN/m2/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
Inputdismap <- Inputdismap[, c(2,7,8)] 

world <- ne_countries(scale = "medium", returnclass = "sf")

breaks_vals <- seq(0, 25, by = 2.5)  # 0, 2.5, 5, ..., 25
labels_vals <- c("0", "2.5", "5", "7.5", "10", "12.5", "15", "17.5", "20", "22.5", "25+")

custom_colors <- c(
  "#ffffff",  # white
  "#deebf7",  # light blue
  "#c7e9c0",  # light green
  "#74c476",  # medium green
  "#ffffb2",  # pale yellow
  "#fecc5c",  # soft orange
  "#fd8d3c",  # orange
  "#f16913",  # moderate orange-red
  "#d94801",  # soft reddish-brown 
  "#a63603"   # medium brown 
)

Inputdismap_2100Base <- Inputdismap

plot_input2 <- ggplot() +
  geom_tile(data = Inputdismap_2100Base, aes(x = x, y = y, fill = Input)) +
  scale_fill_stepsn(
    colors = custom_colors,
    breaks = breaks_vals,
    limits = c(0, 25),
    labels = labels_vals,
    na.value = "white",
    name = "Total N input in 2100\n(g N m⁻² yr⁻¹)",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(12, "cm"),
      barheight = unit(0.2, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/Input_Base_2100_ver2.png",
  plot = plot_input2,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

#Input Map 2010
Totaldistribution <- VY_load2[, c(2,3,7,10,11)] %>%
  filter(i2 == 2010) %>%
  filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A", "PAS")) %>%
  group_by (i2, lat, lon) %>%
  summarise(LUAREA = sum(LUAREA, na.rm = TRUE))

Inputdismap <- Inputdis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2010)  #Substitute variable
Inputdismap <- Inputdismap[, c(2,3,4,5)] %>%
  left_join(Totaldistribution, by = c("lat", "lon")) %>% #ha
  mutate(Input = Input*100000000/LUAREA) %>% #gN/m2/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
Inputdismap <- Inputdismap[, c(2,7,8)] 

world <- ne_countries(scale = "medium", returnclass = "sf")

breaks_vals <- seq(0, 25, by = 2.5)  # 0, 2.5, 5, ..., 25
labels_vals <- c("0", "2.5", "5", "7.5", "10", "12.5", "15", "17.5", "20", "22.5", "25+")

custom_colors <- c(
  "#ffffff",  # white
  "#deebf7",  # light blue
  "#c7e9c0",  # light green
  "#74c476",  # medium green
  "#ffffb2",  # pale yellow
  "#fecc5c",  # soft orange
  "#fd8d3c",  # orange
  "#f16913",  # moderate orange-red
  "#d94801",  # soft reddish-brown
  "#a63603"   # medium brown 
)

Inputdismap_2010 <- Inputdismap

plot_input3 <- ggplot() +
  geom_tile(data = Inputdismap_2010, aes(x = x, y = y, fill = Input)) +
  scale_fill_stepsn(
    colors = custom_colors,
    breaks = breaks_vals,
    limits = c(0, 25),
    labels = labels_vals,
    na.value = "white",
    name = "Total N input in 2100\n(g N m⁻² yr⁻¹)",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(12, "cm"),
      barheight = unit(0.2, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/Input_2010.png",
  plot = plot_input3,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)


#Aggregate Input Map
plot1_clean <- plot_input + theme(legend.position = "none")
plot2_clean <- plot_input2 + theme(legend.position = "none")
plot3_clean <- plot_input3 + theme(legend.position = "none")
plot1_clean <- plot1_clean +
  annotate("text", x = 0, y = -55, label = "500C 2100", size = 5, fontface = "bold") +
  labs(x = "Longitude", y = "Latitude")

plot2_clean <- plot2_clean +
  annotate("text", x = 0, y = -55, label = "Baseline 2100", size = 5, fontface = "bold") +
  labs(x = "Longitude", y = "Latitude")

plot3_clean <- plot3_clean +
  annotate("text", x = 0, y = -55, label = "2010", size = 5, fontface = "bold") +
  labs(x = "Longitude", y = "Latitude")


legend_shared <- get_legend(plot_input +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))
)

add_black_border_with_axes <- function(p) {
  p +
    theme_bw() +  
    theme(
      panel.border = element_rect(color = "black", fill = NA, size = 0.5),
      panel.grid = element_blank(),  
      axis.title = element_text(size = 10),  
      axis.text = element_text(size = 8),    
      axis.ticks = element_line(size = 0.3), 
      plot.title = element_blank(),          
      plot.margin = margin(1, 1, 1, 1, "mm")
    )
}

plot1_bordered <- add_black_border(plot1_clean)
plot2_bordered <- add_black_border(plot2_clean)
plot3_bordered <- add_black_border(plot3_clean)

breaks_vals <- seq(0, 25, by = 2.5)
labels_vals <- c("0", "2.5", "5", "7.5", "10", "12.5", "15", "17.5", "20", "22.5", "25+")

custom_colors <- c(
  "#ffffff",  # white
  "#deebf7",  # light blue
  "#c7e9c0",  # light green
  "#74c476",  # medium green
  "#ffffb2",  # pale yellow
  "#fecc5c",  # soft orange
  "#fd8d3c",  # orange
  "#f16913",  # moderate orange-red
  "#d94801",  # soft reddish-brown
  "#a63603"   # medium brown
)

legend_shared <- get_legend(
  ggplot() +
    geom_tile(data = Inputdismap_2010, aes(x = x, y = y, fill = Input)) +
    scale_fill_stepsn(
      colors = custom_colors,
      breaks = breaks_vals,
      limits = c(0, 25),
      labels = labels_vals,
      na.value = "white",
      name = "Total N input in 2100\n(g N m⁻² yr⁻¹)",
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(12, "cm"),
        barheight = unit(0.2, "cm")
      )
    ) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8)
    )
)


top_row <- plot3_bordered
bottom_row <- plot_grid(plot2_bordered, plot1_bordered, ncol = 2, rel_widths = c(1, 1))

map_grid <- plot_grid(
  top_row,
  bottom_row,
  ncol = 1,
  rel_heights = c(1, 1),
  align = "v"
)

final_plot <- plot_grid(
  map_grid,
  legend_shared,
  ncol = 1,
  rel_heights = c(1, 0.11)
)


ggsave("../output/Input_combined_rev.png", plot = final_plot, width = 12, height = 7.5, dpi = 300)

#Gas
# Gasmap 2010
OUTgasdismap <- OUTgasdis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2010)  #Substitute variable
OUTgasdismap <- OUTgasdismap[, c(2,3,4,5)] %>%
  left_join(gridarea2, by = c("lat", "lon")) %>% #kha
  mutate(OUTgasgrid = OUTgasgrid*1000000/area) %>% #kg/ha/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
OUTgasdismap <- OUTgasdismap[, c(1,2,6,7)]
custom_colors <- c("#f7fbff", "#fdae61", "#d7191c")

OUTgasdismap_2010 <- OUTgasdismap

plot1 <- ggplot() +
  geom_tile(data = OUTgasdismap_2010, aes(x = x, y = y, fill = OUTgasgrid)) +  
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.025, 0.05, 0.1, 0.15, 0.25, 0.5, 0.75, 1), 
    limits = c(0, 20), 
    breaks = c(0, 5, 10, 15, 20),  
    labels = c("0", "5", "10", "15", "20+"),  
    oob = scales::squish,
    name = "Gaseous Losses\n(kg ha^-1 yr^-1)", 
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Gaseous Losses"
  ) +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(size = 15), 
    legend.text = element_text(size = 12),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/outgas_2010.png",  
  plot = plot1,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)

#outgas 2100 base
OUTgasdismap <- OUTgasdis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2100)  #Substitute variable
OUTgasdismap <- OUTgasdismap[, c(2,3,4,5)] %>%
  left_join(gridarea2, by = c("lat", "lon")) %>% #kha
  mutate(OUTgasgrid = OUTgasgrid*1000000/area) %>% #kg/ha/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
OUTgasdismap <- OUTgasdismap[, c(1,2,6,7)]
custom_colors <- c("#f7fbff", "#fdae61", "#d7191c")

OUTgasdismap_2100base <- OUTgasdismap

plot2 <- ggplot() +
  geom_tile(data = OUTgasdismap_2100base, aes(x = x, y = y, fill = OUTgasgrid)) +  
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.025, 0.05, 0.1, 0.15, 0.25, 0.5, 0.75, 1), 
    limits = c(0, 20), 
    breaks = c(0, 5, 10, 15, 20),  
    labels = c("0", "5", "10", "15", "20+"),  
    oob = scales::squish,
    name = "Gaseous Losses\n(kg ha^-1 yr^-1)", 
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Gaseous Losses"
  ) +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(size = 15), 
    legend.text = element_text(size = 12),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/outgas_Base_2100_ver2.png",  
  plot = plot2,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)

#Outgas 2100 500C
OUTgasdismap <- OUTgasdis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2100)  #Substitute variable
OUTgasdismap <- OUTgasdismap[, c(2,3,4,5)] %>%
  left_join(gridarea2, by = c("lat", "lon")) %>% #kha
  mutate(OUTgasgrid = OUTgasgrid*1000000/area) %>% #kg/ha/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
OUTgasdismap <- OUTgasdismap[, c(1,2,6,7)]
custom_colors <- c("#f7fbff", "#fdae61", "#d7191c")

OUTgasdismap_2100500C <- OUTgasdismap

plot3 <- ggplot() +
  geom_tile(data = OUTgasdismap_2100500C, aes(x = x, y = y, fill = OUTgasgrid)) +  
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.025, 0.05, 0.1, 0.15, 0.25, 0.5, 0.75, 1), 
    limits = c(0, 20), 
    breaks = c(0, 5, 10, 15, 20),  
    labels = c("0", "5", "10", "15", "20+"),  
    oob = scales::squish,
    name = "Gaseous Losses\n(kg ha^-1 yr^-1)", 
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Gaseous Losses"
  ) +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(size = 15), 
    legend.text = element_text(size = 12),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/outgas_500C_2100.png",  
  plot = plot3,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)


#Aggregate Outgas Map
plot1_clean <- plot1 + theme(legend.position = "none")
plot2_clean <- plot2 + theme(legend.position = "none")
plot3_clean <- plot3 + theme(legend.position = "none")
plot1_clean <- plot1_clean +  
  annotate("text",
           x = 0, y = -55,  
           label = "2010",
           size = 5, fontface = "bold")+
  labs(title = NULL)
plot2_clean <- plot2_clean +  
  annotate("text",
           x = 0, y = -55,  
           label = "Baseline 2100",
           size = 5, fontface = "bold")+
  labs(title = NULL)
plot3_clean <- plot3_clean +  
  annotate("text",
           x = 0, y = -55,  
           label = "500C 2100",
           size = 5, fontface = "bold")+
  labs(title = NULL)



add_black_border <- function(p) {
  p + theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.margin = margin(1, 1, 1, 1, "mm") 
  )
}


plot1_bordered <- add_black_border(plot1_clean)
plot2_bordered <- add_black_border(plot2_clean)
plot3_bordered <- add_black_border(plot3_clean)

legend_shared <- get_legend(
  plot1 +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 12)
    )
)

top_row <- plot1_bordered
bottom_row <- plot_grid(plot2_bordered, plot3_bordered, ncol = 2, rel_widths = c(1, 1))


map_grid <- plot_grid(
  top_row,
  bottom_row,
  ncol = 1,
  rel_heights = c(1, 1),
  align = "v"
)


final_plot <- plot_grid(
  map_grid,
  legend_shared,
  ncol = 1,
  rel_heights = c(1, 0.131)
)


ggsave("../output/Outgas_combined.png", plot = final_plot, width = 12, height = 7.5, dpi = 300)





#livnum
df_plot <- IAMC_Template_liv %>%
  filter(
    RegionCode == "World",
    SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_CACN_NoCC")
  ) %>%
  mutate(
    YEMF = as.numeric(as.character(YEMF)),
    Change_Rate = as.numeric(as.character(Change_Rate)),
     ScenarioLabel = case_when(
      SCENARIO == "SSP2_BaU_NoCC" ~ "Baseline",
      SCENARIO == "SSP2_500C_CACN_NoCC" ~ "500C"
    )) %>%
  group_by(SCENARIO) %>%
  mutate(Change_Rate_norm = Change_Rate / Change_Rate[YEMF == 2010]) %>%
  ungroup()


livnum <- ggplot(df_plot, aes(x = YEMF, y = Change_Rate_norm, color = ScenarioLabel)) +
  geom_line(size = 1.2) +
  labs(
    x = "Year",
    y = "Livestock Population Change Rate",
    color = "Scenario"
  ) +
  scale_x_continuous(
    breaks = c(2010, 2030, 2050, 2100),
    limits = c(2010, 2100)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 25)
  )

ggsave("../output/livnum.png", plot = livnum, width = 12, height = 7, dpi = 300)


#NH3 emission Region Aggregate
    #CEDS
    NH3_df <- read_csv("../data/NH3_CEDS_emissions_by_country_sector_v_2025_03_18.csv")
    NH3_df2 <- NH3_df %>%
          pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "Value") %>%
          mutate(year = as.integer(str_remove(year, "X"))) %>%
          filter(year == 2010 | year == 2020) %>%
          filter(sector == "3D_Soil-emissions") %>%
          select(country, year, Value)
    ISO17<-read.csv("../data/ISO17.csv")
    NH3_df2$country <- toupper(NH3_df2$country)
    NH3_df2 <- NH3_df2 %>%
      left_join(ISO17, by = "country") %>%
        filter(!is.na(region)) %>%
      group_by(region, year) %>%
      summarise(Value = sum(Value*(14/17)/1000)) #TgN

    #Nitrogen flow model
    OUTgasNH3_R <- OUTgasdis_NH3_2020 %>%
      filter(SCENARIO == "SSP2_BaU_NoCC") %>%
      left_join(MAP_RG, by = "G") %>%
      group_by(YEMF, R) %>%
      summarise(OUTgasNH3 = sum(OUTgasgrid)) %>%
      filter(YEMF == 2020)

    OUTgasNH3_R2 <- OUTgasdis_NH3_2010 %>%
      filter(SCENARIO == "SSP2_BaU_NoCC") %>%
      left_join(MAP_RG, by = "G") %>%
      group_by(YEMF, R) %>%
      summarise(OUTgasNH3 = sum(OUTgasgrid)) %>%
      filter(YEMF == 2010)

    OUTgasNH3_all <- bind_rows(OUTgasNH3_R, OUTgasNH3_R2) %>%
      rename(year = YEMF, region = R, Value = OUTgasNH3)

    #AIM-Hub
    OUTgasNH3_AIMHub <- IAMC_Template %>%
      filter(SCENARIO == "SSP2_BaU_NoCC") %>%
      filter(VEMF == "Emi_NH3_AFO_Agr_Man_Soi") %>%
      filter(YEMF == 2010 | YEMF == 2020) %>%
      select(REMF, YEMF, IAMC_Template)%>%
      rename(Value = IAMC_Template)  %>%
      rename(region = REMF) %>%
      rename(year = YEMF) %>%
      mutate(Value = Value*14/17) #TgN/yr

    #EDGAR
    EDGAR_df <- read_csv("../../../Others/Data/EDGAR/EDGAR_NH3_1970_2022.csv") #Gg
    EDGAR_df2 <- EDGAR_df %>%
      select(Country_code_A3, ipcc_code_2006_for_standard_report_name, Y_2010, Y_2020)
    EDGAR_df2 <- EDGAR_df2 %>%
      mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
      rename(country = Country_code_A3) %>%
      rename(sector = ipcc_code_2006_for_standard_report_name)
    ISO17<-read.csv("../data/ISO17.csv")
    EDGAR_df2 <- EDGAR_df2 %>% mutate(country = toupper(country))
    OUTgasNH3_EDGAR <- EDGAR_df2 %>%
      left_join(ISO17, by = "country") %>%
      filter(sector == "Direct N2O Emissions from managed soils") %>%
      select(country, region, Y_2010, Y_2020) 

    OUTgasNH3_EDGAR_long <- OUTgasNH3_EDGAR %>%
    pivot_longer(
    cols = c(Y_2010, Y_2020),  
    names_to  = "year",   
    values_to = "Value"  
    ) %>%
    mutate(
    year = as.integer(sub("Y_", "", year))
    ) %>%
    filter(!is.na(region)) %>%
    group_by(region, year) %>%
    summarise(Value = sum(Value*(14/17)/1000))
      
    #plot
    valid_regions <- c(
      NH3_df2$region,
      OUTgasNH3_all$region,
      OUTgasNH3_EDGAR_long$region
    ) %>%
      unique() %>%
      na.omit()
    NH3_comp <- bind_rows(
        NH3_df2 %>%
          mutate(
            year   = as.integer(as.character(year)),
            Source = "CEDS"
          ),
        OUTgasNH3_all %>%
          mutate(
            year   = as.integer(as.character(year)),
            Source = "This_study"
          ),
        OUTgasNH3_AIMHub %>%
          filter(region %in% valid_regions) %>% 
          mutate(
            year   = as.integer(as.character(year)),
            Source = "AIM-Hub"
          ),
        OUTgasNH3_EDGAR_long %>%
          mutate(
            year   = as.integer(as.character(year)),
            Source = "EDGAR"
          )
      )

    NH3_2010_wide <- NH3_comp %>%
      filter(year == 2010) %>%
      select(region, Source, Value) %>%
      pivot_wider(
        names_from  = Source,
        values_from = Value
      )

    NH3_2020_wide <- NH3_comp %>%
      filter(year == 2020) %>%
      select(region, Source, Value) %>%
      pivot_wider(
        names_from  = Source,
        values_from = Value
      )

    NH3_comp <- NH3_comp %>%
     mutate(
      Source = factor(
      Source,
      levels = c("This_study", "CEDS", "AIM-Hub", "EDGAR")
    )
    )

   region_lab <- c(
  BRA  = "ブラジル",
  CAN  = "カナダ",
  CHN  = "中国・モンゴル",
  CIS  = "旧ソビエト連邦",
  IND  = "インド",
  JPN  = "日本",
  TUR  = "トルコ",
  USA  = "アメリカ合衆国",
  XAF  = "サブサハラ",
  XER  = "EUを除くヨーロッパ",
  XE25 = "EU",
  XLM  = "ラテンアメリカ",
  XME  = "中東",
  XNF  = "北アフリカ",
  XOC  = "ニュージーランド\nおよびオーストラリア",
  XSA  = "その他アジア",
  XSE  = "その他の東・\n東南アジア"
)

# facetの並び順を固定したい場合（任意）
NH3_comp <- NH3_comp %>%
  mutate(region = factor(region, levels = names(region_lab)))

NH3emi_region <- ggplot(
  NH3_comp %>% filter(year == 2010),
  aes(x = Source, y = Value, fill = Source)
) +
  geom_col(position = "dodge") +
  facet_wrap(
    ~ region, scales = "free_y",
    labeller = as_labeller(region_lab)   # ←ここがポイント
  ) +
  labs(
    title = "NH3 emissions by region (2010)",
    x     = "",
    y     = "NH3 emissions (Tg N/yr)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )

NH3emi_region2 <- ggplot(
  NH3_comp %>% filter(year == 2020),
  aes(x = Source, y = Value, fill = Source)
) +
  geom_col(position = "dodge") +
  facet_wrap(
    ~ region, scales = "free_y",
    labeller = as_labeller(region_lab)
  ) +
  labs(
    title = "NH3 emissions by region (2020)",
    x     = "",
    y     = "NH3 emissions (Tg N/yr)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )

ggsave(
  filename = "NH3_emiRegion_2010.png",  
  plot     = NH3emi_region,              
  width    = 10,                      
  height   = 7,                     
  dpi      = 300                    
)

ggsave(
  filename = "NH3_emiRegion_2020.png",  
  plot     = NH3emi_region2,              
  width    = 10,                      
  height   = 7,                     
  dpi      = 300                    
)


#OUTlea Region Aggregate
OUTlea_R <- OUTleadis %>%
  left_join(MAP_RG, by = "G") %>%
  group_by(YEMF, R) %>%
  summarise(OUTleagrid = sum(OUTleagrid)) %>%
  filter(YEMF == 2010) %>%
  mutate(OUTleagrid = OUTleagrid*(77/71))

region_map <- c(
  "BRA" = "South America",
  "CAN" = "North America",
  "CHN" = "Asia",
  "CIS" = "Europe",
  "IND" = "Asia",
  "JPN" = "Asia",
  "TUR" = "Europe",
  "USA" = "North America",
  "XAF" = "Africa",
  "XER" = "Europe",
  "XE25" = "Europe",
  "XLM" = "South America",
  "XME" = "Asia",
  "XNF" = "Africa",
  "XOC" = "Oceania",
  "XSA" = "Asia",
  "XSE" = "Asia"
)

OUTlea_summary <- OUTlea_R %>%
  mutate(R = as.character(R),                    
         RegionGroup = region_map[R])  %>%  
  group_by(RegionGroup) %>%
  summarise(
    OUTleagrid = sum(OUTleagrid, na.rm = TRUE)
  )


#INput Region Aggregate
MAP_RG <- rgdx.set(data_prep, 'MAP_RG')
Input_R_2010 <- Inputdis %>%
  left_join(MAP_RG, by = "G") %>%
  group_by(YEMF, R) %>%
  summarise(Input = sum(Input)*(238/243)) %>%
  filter(YEMF == 2010)
Input_R_2030 <- Inputdis %>%
  left_join(MAP_RG, by = "G") %>%
  group_by(YEMF, R) %>%
  summarise(Input = sum(Input)) %>%
  filter(YEMF == 2030)
Input_R_2050 <- Inputdis %>%
  left_join(MAP_RG, by = "G") %>%
  group_by(YEMF, R) %>%
  summarise(Input = sum(Input)) %>%
  filter(YEMF == 2050)
Input_R_2100 <- Inputdis %>%
  left_join(MAP_RG, by = "G") %>%
  group_by(YEMF, R) %>%
  summarise(Input = sum(Input)) %>%
  filter(YEMF == 2100)
Input_R_2010 <- Input_R_2010[, c(2,3)] %>%
  rename(Input2010 = Input)
Input_R_2030 <- Input_R_2030[, c(2,3)] %>%
  rename(Input2030 = Input)
Input_R_2050 <- Input_R_2050[, c(2,3)] %>%
  rename(Input2050 = Input)
Input_R_2100 <- Input_R_2100[, c(2,3)] %>%
  rename(Input2100 = Input)

Input_R <- Input_R_2010 %>%
  left_join(Input_R_2030, by = "R") %>%
  left_join(Input_R_2050, by = "R") %>%
  left_join(Input_R_2100, by = "R")

df_input_long <- Input_R %>%  
  pivot_longer(
    cols = starts_with("Input"), 
    names_to = "Year", 
    values_to = "Value"
  ) %>%
  mutate(
    Year = str_remove(Year, "Input") %>% as.numeric()
  ) %>%
  mutate(R = recode(R,
    "BRA" = "Brazil",
    "CAN" = "Canada",
    "CHN" = "China",
    "CIS" = "Former USSR",
    "IND" = "India",
    "JPN" = "Japan",
    "TUR" = "Turkey",
    "XAF" = "Sub-saharan",
    "XER" = "Other Europe",
    "XR25" = "EU25",
    "XLM" = "Other South America",
    "XME" = "Middle East",
    "XNF" = "North Africa",
    "XOC" = "Oceania",
    "XSA" = "Other Asia",
    "XSE" = "South-East Asia"
  ))

df_input_long$R <- str_wrap(df_input_long$R, width = 12)

Input_main <- ggplot(df_input_long, aes(x = factor(Year), y = Value, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.7) +
  facet_wrap(~ R, scales = "free_y", ncol = 5) +
  labs(
    x = "Year",
    y = "Nitrogen Input (Tg N/yr)"
  ) +
  scale_fill_brewer(palette = "Set2", guide = "none") +  
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "gray85", color = NA),
    strip.text = element_text(size = 23),
     axis.text.x = element_text(size = 27, angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 28),
    axis.title = element_text(size = 36),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("../output/Input_17_revised2.png", plot = Input_main, width = 14, height = 14, dpi = 500)


MAP_RG <- rgdx.set(data_prep, "MAP_RG")  # G→R の対応表

# 成分ごとのラベル・オブジェクト接頭辞・列名
sources    <- c("Fertilizer", "Manure", "Deposition", "Fixation")
prefixes   <- c("INferdis",   "INmandis", "INdepdis",  "INfixdis")
value_cols <- c("Fert_Use_Grid", "ManureGrid", "N_deposition_grid", "N_fix")

years <- c(2010L, 2030L, 2050L, 2100L)

df_list <- list()
idx <- 1

for (i in seq_along(sources)) {
  for (yr in years) {
    obj_name <- paste0(prefixes[i], yr)  # 例: "INferdis2010"
    df_raw   <- get(obj_name)

    tmp <- df_raw %>%
      # G を R に変換
      left_join(MAP_RG, by = "G") %>%
      # その年の行だけ使う
      filter(YEMF == yr) %>%
      # 地域ごとに合計（グリッド→地域）
      group_by(R) %>%
      summarise(
        Value = sum(.data[[ value_cols[i] ]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Year   = yr,
        source = sources[i]
      )

    df_list[[idx]] <- tmp
    idx <- idx + 1
  }
}

df_input_long_comp <- bind_rows(df_list)
df_input_long_comp_base <- bind_rows(df_list)

df_input_long_comp <- df_input_long_comp %>%
  mutate(
    R = recode(R,
      "BRA" = "Brazil",
      "CAN" = "Canada",
      "CHN" = "China",
      "CIS" = "Former USSR",
      "IND" = "India",
      "JPN" = "Japan",
      "TUR" = "Turkey",
      "XAF" = "Sub-saharan",
      "XER" = "Other Europe",
      "XE25" = "EU25",
      "XLM" = "Other South America",
      "XME" = "Middle East",
      "XNF" = "North Africa",
      "XOC" = "Oceania",
      "XSA" = "Other Asia",
      "XSE" = "South-East Asia"
    ),
    R     = str_wrap(R, width = 12),
    Year  = factor(Year, levels = c(2010, 2030, 2050, 2100)),
    source = factor(source,
                    levels = c("Fertilizer", "Manure", "Deposition", "Fixation"))
  )

Input_stacked <- ggplot(
  df_input_long_comp,
  aes(x = Year, y = Value, fill = source)
) +
  geom_col(width = 0.7) +
  facet_wrap(~ R, scales = "free_y", ncol = 5) +
  labs(
    x    = "Year",
    y    = "Nitrogen Input (Tg N/yr)",  # 単位は必要に応じて変更
    fill = "Source"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    strip.background  = element_rect(fill = "gray85", color = NA),
    strip.text        = element_text(size = 23),
    axis.text.x       = element_text(size = 27, angle = 45, hjust = 1, vjust = 1),
    axis.text.y       = element_text(size = 28),
    axis.title        = element_text(size = 36),
    legend.title      = element_text(size = 28),
    legend.text       = element_text(size = 24),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank()
  )

ggsave(
  "../output/Input_17_stacked_components.png",
  plot   = Input_stacked,
  width  = 17,
  height = 12,
  dpi    = 500
)

df_input_long_comp_code <- df_input_long_comp_base %>%
  mutate(
    # R はコードのまま使う（折り返しだけしてもいいし、しなくてもよい）
    # R = str_wrap(R, width = 12),  # 長いコードが無ければこの行は省略でもOK
    Year = factor(Year, levels = c(2010, 2030, 2050, 2100)),
    source = factor(source,
                    levels = c("Fertilizer", "Manure", "Deposition", "Fixation"))
  )

Input_stacked_code <- ggplot(
  df_input_long_comp_code,
  aes(x = Year, y = Value, fill = source)
) +
  geom_col(width = 0.7) +
  facet_wrap(~ R, scales = "free_y", ncol = 5) +
  labs(
    x    = "Year",
    y    = "Nitrogen Input (Tg N/yr)",  # 単位は必要に応じて調整
    fill = "Source"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    strip.background  = element_rect(fill = "gray85", color = NA),
    strip.text        = element_text(size = 23),
    axis.text.x       = element_text(size = 24, angle = 45, hjust = 1, vjust = 1),
    axis.text.y       = element_text(size = 28),
    axis.title        = element_text(size = 36),
    legend.title      = element_text(size = 28),
    legend.text       = element_text(size = 24),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank()
  )

ggsave(
  "../output/Input_17_stacked_components_code.png",
  plot   = Input_stacked_code,
  width  = 14,
  height = 10,
  dpi    = 500
)


#Output Region Aggregate
MAP_RG <- rgdx.set(data_prep, "MAP_RG")  # すでに使っている G→R 対応表

# 成分ラベル・オブジェクト接頭辞・値の列名
sources    <- c("Crop uptake", "Residues", "Gaseous loss", "Leaching loss")
prefixes   <- c("OUTcropdis",  "OUTresdis","OUTgasdis",    "OUTleadis")
value_cols <- c("CropN",       "OUTresgrid","OUTgasgrid",  "OUTleagrid")

years <- c(2010L, 2030L, 2050L, 2100L)

df_list <- list()
idx <- 1

for (i in seq_along(sources)) {
  for (yr in years) {
    obj_name <- paste0(prefixes[i], yr)  # 例: "OUTcropdis2010"
    df_raw   <- get(obj_name)

    tmp <- df_raw %>%
      left_join(MAP_RG, by = "G") %>%   # G → R に変換
      filter(YEMF == yr) %>%            # その年の行だけ使う
      group_by(R) %>%                   # 地域ごとに合計
      summarise(
        Value = sum(.data[[ value_cols[i] ]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Year   = yr,
        source = sources[i]
      )

    df_list[[idx]] <- tmp
    idx <- idx + 1
  }
}

df_output_long_comp_base <- bind_rows(df_list)

df_output_long_comp_code <- df_output_long_comp_base %>%
  mutate(
    Year   = factor(Year, levels = c(2010, 2030, 2050, 2100)),
    source = factor(source,
                    levels = c("Crop uptake", "Residues",
                               "Gaseous loss", "Leaching loss"))
  )

OUT_stacked_code <- ggplot(
  df_output_long_comp_code,
  aes(x = Year, y = Value, fill = source)
) +
  geom_col(width = 0.7) +
  facet_wrap(~ R, scales = "free_y", ncol = 5) +
  labs(
    x    = "Year",
    y    = "Nitrogen Output (Tg N/yr)",  # 単位は必要に応じて調整
    fill = "Component"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    strip.background  = element_rect(fill = "gray85", color = NA),
    strip.text        = element_text(size = 23),
    axis.text.x       = element_text(size = 24, angle = 45, hjust = 1, vjust = 1),
    axis.text.y       = element_text(size = 28),
    axis.title        = element_text(size = 36),
    legend.title      = element_text(size = 28),
    legend.text       = element_text(size = 24),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank()
  )

ggsave(
  "../output/Output_17_finalrep.png",
  plot   = OUT_stacked_code,
  width  = 14,
  height = 10,
  dpi    = 500
)


#OUTgas Region Aggregate
OUTgas_R_2010 <- OUTgasdis %>%
  left_join(MAP_RG, by = "G") %>%
  group_by(YEMF, R) %>%
  summarise(OUTgasgrid = sum(OUTgasgrid)) %>%
  filter(YEMF == 2010)
OUTgas_R_2030 <- OUTgasdis %>%
  left_join(MAP_RG, by = "G") %>%
  group_by(YEMF, R) %>%
  summarise(OUTgasgrid = sum(OUTgasgrid)) %>%
  filter(YEMF == 2030)
OUTgas_R_2050 <- OUTgasdis %>%
  left_join(MAP_RG, by = "G") %>%
  group_by(YEMF, R) %>%
  summarise(OUTgasgrid = sum(OUTgasgrid)) %>%
  filter(YEMF == 2050)
OUTgas_R_2100 <- OUTgasdis %>%
  left_join(MAP_RG, by = "G") %>%
  group_by(YEMF, R) %>%
  summarise(OUTgasgrid = sum(OUTgasgrid)) %>%
  filter(YEMF == 2100)
OUTgas_R_2010 <- OUTgas_R_2010[, c(2,3)] %>%
  rename(OUTgas2010 = OUTgasgrid)
OUTgas_R_2030 <- OUTgas_R_2030[, c(2,3)] %>%
  rename(OUTgas2030 = OUTgasgrid)
OUTgas_R_2050 <- OUTgas_R_2050[, c(2,3)] %>%
  rename(OUTgas2050 = OUTgasgrid)
OUTgas_R_2100 <- OUTgas_R_2100[, c(2,3)] %>%
  rename(OUTgas2100 = OUTgasgrid)

OUTgas_R <- OUTgas_R_2010 %>%
  left_join(OUTgas_R_2030, by = "R") %>%
  left_join(OUTgas_R_2050, by = "R") %>%
  left_join(OUTgas_R_2100, by = "R") 

df_OUTgas_long <- OUTgas_R %>%  
  pivot_longer(
    cols = starts_with("OUTgas"), 
    names_to = "Year", 
    values_to = "Value"
  ) %>%
  mutate(
    Year = str_remove(Year, "OUTgas") %>% as.numeric()
  ) %>%
  mutate(R = recode(R,
    "BRA" = "Brazil",
    "CAN" = "Canada",
    "CHN" = "China",
    "CIS" = "Former USSR",
    "IND" = "India",
    "JPN" = "Japan",
    "TUR" = "Turkey",
    "XAF" = "Sub-saharan",
    "XER" = "Other Europe",
    "XR25" = "EU25",
    "XLM" = "Other South America",
    "XME" = "Middle East",
    "XNF" = "North Africa",
    "XOC" = "Oceania",
    "XSA" = "Other Asia",
    "XSE" = "South-East Asia"
  ))

df_OUTgas_long$R <- str_wrap(df_OUTgas_long$R, width = 12)

OUTgas_main <- ggplot(df_OUTgas_long, aes(x = factor(Year), y = Value, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.7) +
  facet_wrap(~ R, scales = "free_y", ncol = 9) +
  labs(
    x = "Year",
    y = "Gaseous Losses (Tg N/yr)"
  ) +
  scale_fill_brewer(palette = "Set2", guide = "none") +  
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "gray85", color = NA),
    strip.text = element_text(size = 25),
     axis.text.x = element_text(size = 27, angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 28),
    axis.title = element_text(size = 36),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("../output/OUTgas_17_2.png", plot = OUTgas_main, width = 28, height = 11, dpi = 500)


#image to figure
library(magick)

# 画像を読み込む
img1 <- image_read("../output/plot1_ver2.png")         # 左上
img2 <- image_read("../output/plot3_relative2.png")    # 右上
img3 <- image_read("../output/Input_17_revised2.png")  # 左下
img4 <- image_read("../output/OUTgas_17.png")          # 右下

img1 <- image_scale(img1, "800x685!")
img2 <- image_scale(img2, "800x685!")
img3 <- image_scale(img3, "700x700!")
img4 <- image_scale(img4, "700x700!")
# 上段と下段を横に並べる
top_row <- image_append(c(img1, img2))
bottom_row <- image_append(c(img3, img4))

# 上下を縦に並べて1枚に統合
final_img <- image_append(c(top_row, bottom_row), stack = TRUE)

image_write(final_img, path = "../output/IAMC_figure.png")




# 読み込み
img1 <- image_read("../output/plot1_ver2.png")         # 左上
img2 <- image_read("../output/plot3_relative2.png")    # 右上
img3 <- image_read("../output/Input_17_revised2.png")  # 左下
img4 <- image_read("../output/OUTgas_17.png")          # 右下

# サイズを統一
img1 <- image_scale(img1, "800x685!")
img2 <- image_scale(img2, "800x685!")
img3 <- image_scale(img3, "700x700!")
img4 <- image_scale(img4, "700x700!")

# 幅200の余白画像を作成（高さは下段と同じ）
spacer <- image_blank(width = 140, height = 700, color = "white")
spacer2 <- image_blank(width = 60, height = 700, color = "white")

# 下段の画像に余白を挟んで結合：img3 + spacer + img4 → 横幅1400 + 200 = 1600
bottom_row <- image_append(c(img3, spacer, img4, spacer2))

# 上段の画像を結合（幅1600）
top_row <- image_append(c(img1, img2))

# 縦方向に連結
final_img <- image_append(c(top_row, bottom_row), stack = TRUE)

# 保存
image_write(final_img, path = "../output/IAMC_figure.png")


#Leaching
# Leaching map 2010
OUTleadismap <- OUTleadis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2010)  #Substitute variable
OUTleadismap <- OUTleadismap[, c(2,3,4,5)] %>%
  left_join(gridarea2, by = c("lat", "lon")) %>% #kha
  mutate(OUTleagrid = OUTleagrid*1000000/area) %>% #kg/ha/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
OUTleadismap <- OUTleadismap[, c(1,2,6,7)]
custom_colors <- c("#f7fbff", "#fdae61", "#d7191c")

OUTleadismap_2010 <- OUTleadismap

plot1 <- ggplot() +
  geom_tile(data = OUTleadismap_2010, aes(x = x, y = y, fill = OUTleagrid)) +  
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.025, 0.05, 0.1, 0.15, 0.25, 0.5, 0.75, 1), 
    limits = c(0, 20), 
    breaks = c(0, 5, 10, 15, 20),  
    labels = c("0", "5", "10", "15", "20+"),  
    oob = scales::squish,
    name = "Leaching Losses\n(kg ha^-1 yr^-1)", 
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Leaching Losses"
  ) +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(size = 15), 
    legend.text = element_text(size = 12),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/outlea_2010.png",  
  plot = plot1,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)


#---------------------------
# 2100 Baseline（Leaching）
#---------------------------
OUTleadismap <- OUTleadis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2100)  # ★ Baseline シナリオの 2100 年に対応するよう OUTleadis を準備しておく

OUTleadismap <- OUTleadismap[, c(2,3,4,5)] %>%
  left_join(gridarea2, by = c("lat", "lon")) %>%    # kha
  mutate(OUTleagrid = OUTleagrid * 1000000 / area) %>%  # kg/ha/yr
  mutate(x = 0.5 * lon - 180.25,
         y = -0.5 * lat + 90.25)

OUTleadismap <- OUTleadismap[, c(1,2,6,7)]
custom_colors <- c("#f7fbff", "#fdae61", "#d7191c")

OUTleadismap_2100base <- OUTleadismap

plot2 <- ggplot() +
  geom_tile(data = OUTleadismap_2100base,
            aes(x = x, y = y, fill = OUTleagrid)) +
  scale_fill_gradientn(
    colors  = custom_colors,
    values  = c(0, 0.025, 0.05, 0.1, 0.15, 0.25, 0.5, 0.75, 1),
    limits  = c(0, 20),
    breaks  = c(0, 5, 10, 15, 20),
    labels  = c("0", "5", "10", "15", "20+"),
    oob     = scales::squish,
    name    = "Leaching Losses\n(kg ha^-1 yr^-1)",
    guide   = guide_colorbar(
      title.position = "top",
      title.hjust    = 0.5,
      barwidth       = unit(12, "cm"),
      barheight      = unit(0.3, "cm")
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) +
  theme_minimal() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Leaching Losses"
  ) +
  theme(
    legend.position = "bottom",
    legend.title    = element_text(size = 15),
    legend.text     = element_text(size = 12),
    plot.title      = element_text(size = 18, face = "bold", hjust = 0.5),
    panel.grid      = element_blank()
  )

#---------------------------
# 2100 500C（Leaching）
#---------------------------
# ★ ここも、500C シナリオに対応する OUTleadis をあらかじめ用意した上で同じ処理を流すイメージ
OUTleadismap <- OUTleadis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2100)

OUTleadismap <- OUTleadismap[, c(2,3,4,5)] %>%
  left_join(gridarea2, by = c("lat", "lon")) %>%    # kha
  mutate(OUTleagrid = OUTleagrid * 1000000 / area) %>%  # kg/ha/yr
  mutate(x = 0.5 * lon - 180.25,
         y = -0.5 * lat + 90.25)

OUTleadismap <- OUTleadismap[, c(1,2,6,7)]

OUTleadismap_2100500C <- OUTleadismap

plot3 <- ggplot() +
  geom_tile(data = OUTleadismap_2100500C,
            aes(x = x, y = y, fill = OUTleagrid)) +
  scale_fill_gradientn(
    colors  = custom_colors,
    values  = c(0, 0.025, 0.05, 0.1, 0.15, 0.25, 0.5, 0.75, 1),
    limits  = c(0, 20),
    breaks  = c(0, 5, 10, 15, 20),
    labels  = c("0", "5", "10", "15", "20+"),
    oob     = scales::squish,
    name    = "Leaching Losses\n(kg ha^-1 yr^-1)",
    guide   = guide_colorbar(
      title.position = "top",
      title.hjust    = 0.5,
      barwidth       = unit(12, "cm"),
      barheight      = unit(0.3, "cm")
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) +
  theme_minimal() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Leaching Losses"
  ) +
  theme(
    legend.position = "bottom",
    legend.title    = element_text(size = 15),
    legend.text     = element_text(size = 12),
    plot.title      = element_text(size = 18, face = "bold", hjust = 0.5),
    panel.grid      = element_blank()
  )

#---------------------------
# 3 枚を Gas と同様に合体
#---------------------------

library(cowplot)

plot1_clean <- plot1 + theme(legend.position = "none")
plot2_clean <- plot2 + theme(legend.position = "none")
plot3_clean <- plot3 + theme(legend.position = "none")

plot1_clean <- plot1_clean +
  annotate("text",
           x = 0, y = -55,
           label = "2010",
           size = 5, fontface = "bold") +
  labs(title = NULL)

plot2_clean <- plot2_clean +
  annotate("text",
           x = 0, y = -55,
           label = "Baseline 2100",
           size = 5, fontface = "bold") +
  labs(title = NULL)

plot3_clean <- plot3_clean +
  annotate("text",
           x = 0, y = -55,
           label = "500C 2100",
           size = 5, fontface = "bold") +
  labs(title = NULL)

add_black_border <- function(p) {
  p + theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.margin  = margin(1, 1, 1, 1, "mm")
  )
}

plot1_bordered <- add_black_border(plot1_clean)
plot2_bordered <- add_black_border(plot2_clean)
plot3_bordered <- add_black_border(plot3_clean)

legend_shared <- get_legend(
  plot1 +
    theme(
      legend.position = "bottom",
      legend.title    = element_text(size = 15),
      legend.text     = element_text(size = 12)
    )
)

top_row <- plot1_bordered
bottom_row <- plot_grid(plot2_bordered, plot3_bordered, ncol = 2, rel_widths = c(1, 1))

map_grid <- plot_grid(
  top_row,
  bottom_row,
  ncol = 1,
  rel_heights = c(1, 1),
  align = "v"
)

final_plot_lea <- plot_grid(
  map_grid,
  legend_shared,
  ncol = 1,
  rel_heights = c(1, 0.131)
)

ggsave(
  "../output/Outlea_combined.png",
  plot   = final_plot_lea,
  width  = 12,
  height = 7.5,
  dpi    = 300
)