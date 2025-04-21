Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:/GAMS/win64/26.1", sep = ";"))
Sys.which("gams")
library(dplyr)
library(tidyr)
library(ggplot2)
library(gdxrrw)
library(ncdf4)
library(readr)
library(Amelia)
library(stringr)
library(purrr)
library(terra)

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
  mutate(IR_Yield = IR_Yield_PDR+IR_Yield_C_B+IR_Yield_GRO+IR_Yield_OSD+IR_Yield_OTH_A+IR_Yield_WHT) %>%
  mutate(ALL_Yield = ALL_Yield_PDR+ALL_Yield_C_B+ALL_Yield_GRO+ALL_Yield_OSD+ALL_Yield_OTH_A+ALL_Yield_WHT)
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

#INman
livdis_nc <- nc_open("../data/livdis_SSP2_BaU_NoCC.nc") #input data(dry) #mg/m2/year
lon <- ncvar_get(livdis_nc, "lon") 
lat <- ncvar_get(livdis_nc, "lat") 
time <- ncvar_get(livdis_nc, "time") 
slnum <- ncvar_get(livdis_nc, "slnum") 
livestock_data <- ncvar_get(livdis_nc, "Livestock_number")
grid <- expand.grid(lon = lon, lat = lat, time = time, slnum = slnum)
final_df <- grid %>%
  mutate(Livestock_number = as.vector(livestock_data)) %>%
  filter(!is.na(Livestock_number) & Livestock_number != 0) %>%
  filter(time == 2010)
final_df$J <- (final_df$lon + 179.75) / 0.5 + 1
final_df$I <- (89.75 - final_df$lat) / 0.5 + 1 
final_df <- final_df[, c(3,4,5,6,7)] %>%
   mutate(Manure = case_when(
    slnum == 1 ~ Livestock_number * 70,
    slnum == 2 ~ Livestock_number * 42.5,
    slnum == 3 ~ Livestock_number * 45,
    slnum == 4 ~ Livestock_number * 10,
    slnum == 5 ~ Livestock_number * 9,
    slnum == 6 ~ Livestock_number * 11,
    slnum == 7 ~ Livestock_number * 0.5,
    slnum == 8 ~ Livestock_number * 0.5,
    TRUE ~ NA_real_ 
  ))

MAP_RIJ <- rgdx.set(data_prep, 'MAP_RIJ')
MAP_RIJ$I <- as.numeric(as.character(MAP_RIJ$I))
MAP_RIJ$J <- as.numeric(as.character(MAP_RIJ$J))
final_df2 <- final_df %>%
  left_join(MAP_RIJ, by = c("I", "J")) %>%
  filter(complete.cases(.)) %>%
  group_by(R, slnum) %>%
  summarise(ManureStable = sum(Manure)) %>% #kg
  mutate(ManureStable = case_when(
    slnum == 1 & R %in% c("USA", "CAN")~ ManureStable * (0.15*0.23+0.27*0.6+0.263*0.6+0.108*0.7+0.184*0.78)*(50/80),
    slnum == 1 & R %in% c("XE25")~ ManureStable * (0.357*0.6+0.368*0.6+0.2*0.7+0.07*0.78)*(50/80),
    slnum == 1 & R %in% c("XER")~ ManureStable * (0.175*0.6+0.6*0.6+0.18*0.7+0.025*0.78)*(50/80),
    slnum == 1 & R %in% c("XSE", "XOC")~ ManureStable * (0.16*0.23+0.01*0.6+0.76*0.7+0.08*0.78)*((50/80+40/60)/2),
    slnum == 1 & R %in% c("BRA", "XLM")~ ManureStable * (0.01*0.6+0.01*0.6+0.36*0.7+0.62*0.78)*(40/60),
    slnum == 1 & R %in% c("XAF", "XNF")~ ManureStable * (0.01*0.6+0.83*0.7+0.05*0.78)*((50/80+40/60)/2),
    slnum == 1 & R %in% c("XME")~ ManureStable * (0.01*0.6+0.02*0.6+0.8*0.7+0.02*0.78)*(40/60),
    slnum == 1 & R %in% c("CHN", "CIS", "JPN", "TUR", "XSA")~ ManureStable * (0.04*0.23+0.38*0.6+0.2*0.7+0.29*0.78+0.02*0.23)*((50/80+40/60)/2),
    slnum == 1 & R %in% c("IND")~ ManureStable * (0.01*0.6+0.27*0.7+0.19*0.78+0.01+0.23)*(40/60),
    slnum == 2 & R %in% c("USA", "CAN")~ ManureStable * (0.002*0.6+0.184*0.6+0.815*0.6)*(15/45),
    slnum == 2 & R %in% c("XE25")~ ManureStable * (0.252*0.6+0.39*0.5+0.32*0.6+0.018*0.6)*(15/45),
    slnum == 2 & R %in% c("XER")~ ManureStable * (0.225*0.6+0.44*0.5+0.2*0.6)*(15/45),
    slnum == 2 & R %in% c("XSE", "XOC")~ ManureStable * (0.09*0.6+0.91*0.6)*((15/45+10/40)/2),
    slnum == 2 & R %in% c("BRA", "XLM")~ ManureStable * (0.99*0.6)*(10/40),
    slnum == 2 & R %in% c("XAF", "XNF")~ ManureStable * (0.01*0.6+0.95*0.6+0.01*0.6)*((15/45+10/40)/2),
    slnum == 2 & R %in% c("XME")~ ManureStable * (0.01*0.6+0.79*0.6+0.02*0.6)*(10/40),
    slnum == 2 & R %in% c("CHN", "CIS", "JPN", "TUR", "XSA")~ ManureStable * (0.46*0.6+0.5*0.6+0.02*0.6)*((15/45+10/40)/2),
    slnum == 2 & R %in% c("IND")~ ManureStable * (0.01*0.6+0.04*0.6+0.22*0.6+0.2*0.6+0.01*0.6)*(10/40),
    slnum == 3 & R %in% c("USA", "CAN")~ ManureStable * (0.328*0.22+0.185*0.52+0.042*0.5+0.04*0.5+0.406*0.75)*(15/45),
    slnum == 3 & R %in% c("XE25")~ ManureStable * (0.087*0.22+0.137*0.5+0.028*0.75+0.698*0.75+0.02*0.78)*(15/45),
    slnum == 3 & R %in% c("XER")~ ManureStable * (0.03*0.22+0.42*0.5+0.247*0.75+0.247*0.75)*(15/45),
    slnum == 3 & R %in% c("XSE", "XOC")~ ManureStable * (0.54*0.22+0.03*0.5+0.15*0.5)*(15/45),
    slnum == 3 & R %in% c("BRA", "XLM")~ ManureStable * (0.08*0.52+0.1*0.5+0.41*0.5+0.02*0.78)*(15/45),
    slnum == 3 & R %in% c("XAF", "XNF")~ ManureStable * (0.06*0.52+0.06*0.5+0.87*0.5+0.01*0.75)*(15/45),
    slnum == 3 & R %in% c("XME")~ ManureStable * (0.14*0.52+0.69*0.5+0.17*0.75)*(15/45),
    slnum == 3 & R %in% c("CHN", "CIS", "JPN", "TUR", "XSA")~ ManureStable * (0.4*0.52+0.54*0.5)*(15/45),
    slnum == 3 & R %in% c("IND")~ ManureStable * (0.09*0.22+0.22*0.52+0.16*0.5+0.3*0.5+0.03*0.75+0.09*0.78)*(15/45),
    slnum == 4 ~ ManureStable * 0.72*(1/10),
    slnum == 5 ~ ManureStable * 0.72*(1/9),
    slnum == 6 ~ ManureStable * 0.64,
    slnum == 7 ~ ManureStable * 0.64,
    slnum == 8 ~ ManureStable * 0.64,
    TRUE ~ NA_real_ 
  )) %>%
  group_by(R) %>%
  summarise(ManureStable = sum(ManureStable)/1000) %>% #t
  rename(RegionCode = R)

INmandep <- final_df %>%
  left_join(MAP_RIJ, by = c("I", "J")) %>%
  filter(complete.cases(.)) %>%
  group_by(R, slnum) %>%
  summarise(ManureStable = sum(Manure)) %>% #kg
  mutate(ManureStable = case_when(
    slnum == 1 & R %in% c("USA", "XE25", "XER", "TUR", "XOC", "JPN", "CAN", "XAF")~ ManureStable * 0.92*(30/80),
    slnum == 1 & R %in% c("CHN", "IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF")~ ManureStable * 0.85*(20/60),
    slnum == 2 & R %in% c("USA", "XE25", "XER", "TUR", "XOC", "JPN", "CAN", "XAF")~ ManureStable * 0.92*(30/45),
    slnum == 2 & R %in% c("CHN", "IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF")~ ManureStable * 0.85*(30/40),
    slnum == 3 ~ ManureStable * 0.85*(30/45),
    slnum == 4 & R %in% c("USA", "XE25", "XER", "TUR", "XOC", "JPN", "CAN", "XAF")~ ManureStable * 0.96*(9/10),
    slnum == 4 & R %in% c("CHN", "IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF")~ ManureStable * 0.92*(9/10),
    slnum == 5 & R %in% c("USA", "XE25", "XER", "TUR", "XOC", "JPN", "CAN", "XAF")~ ManureStable * 0.96*(8/9),
    slnum == 5 & R %in% c("CHN", "IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF")~ ManureStable * 0.92*(8/9),
    slnum == 6 ~ ManureStable * 0.64*0,
    slnum == 7 ~ ManureStable * 0.64*0,
    slnum == 8 ~ ManureStable * 0.64*0,
    TRUE ~ NA_real_ 
  )) %>%
  group_by(R) %>%
  summarise(ManurePas = sum(ManureStable)*0.4/1000) %>% #t #other loss
  rename(RegionCode = R)

  IAMC_Template_liv <- IAMC_Template %>%
  filter(VEMF == "Agr_Pro_Liv")

IAMC_Template_liv <- IAMC_Template_liv %>%
  group_by(REMF, SCENARIO) %>%
  mutate(Basevalue_2015 = IAMC_Template[YEMF == 2015]) %>%
  mutate(Change_Rate = IAMC_Template / Basevalue_2015) %>%
  ungroup()
IAMC_Template_liv <- IAMC_Template_liv[, c(1,2,4,7)] %>%
  rename(RegionCode = REMF)

Manure_Stable <- IAMC_Template_liv %>%
  left_join(final_df2, by = "RegionCode") %>%
  mutate(ManureStable_Y = ManureStable*Change_Rate)
Manure_Stable <- Manure_Stable[, c(1,2,3,6)]

Manure_4 <- Manure_Stable %>%
  mutate(ManureStable_Y = ManureStable_Y*0.4) #other loss

Manure_6_A <- Manure_4 %>%
  filter(RegionCode %in% c("USA", "XE25", "XER", "TUR", "XOC", "JPN", "CAN", "XAF")) %>%
  mutate(ManureCrop = ManureStable_Y*0.66, ManurePas = ManureStable_Y*0.34)

Manure_6_B <- Manure_4 %>%
  filter(RegionCode %in% c("CHN", "IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF")) %>%
  mutate(ManureCrop = ManureStable_Y*0.9, ManurePas = ManureStable_Y*0.1)

Manure_6 <- bind_rows(Manure_6_A, Manure_6_B)
Manure_6 <- Manure_6[, c(1,2,3,5,6)]
Manure_6$YEMF <- as.numeric(as.character(Manure_6$YEMF)) 
Manure_6 <- Manure_6 %>%
  left_join(INmandep, by = c("RegionCode")) %>%
  mutate(ManurePas = ManurePas.x + ManurePas.y)

Manure_6 <- Manure_6[, c(1,2,3,4,7)]

VY_load_Crop <- VY_load2 %>%
  filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A"))
VY_load_Crop <- VY_load_Crop[, c(1,2,3,4,7)] %>%
  group_by(i1, i2, G) %>%
  summarise(LUAREA = sum(LUAREA, na.rm = TRUE)) %>%
  ungroup()

Sum_Cropland <- VY_load_Crop %>%
  group_by(i1, i2) %>%
  summarise(sum_LUAREA = sum(LUAREA))

VY_load_Crop2 <- VY_load_Crop %>%
  left_join(Sum_Cropland, by = c("i1", "i2")) %>%
  mutate(LURATIO = LUAREA/sum_LUAREA)
VY_load_Crop2 <- VY_load_Crop2[, c(1,2,3,6)] %>%
  rename(RegionCode = i1, YEMF = i2)

VY_load_PAS <- VY_load2 %>%
  filter(i3 %in% c("PAS"))
VY_load_PAS <- VY_load_PAS[, c(1,2,4,7)]

Sum_Pasture <- VY_load_PAS %>%
  group_by(i1, i2) %>%
  summarise(sum_LUAREA = sum(LUAREA))

VY_load_PAS2 <- VY_load_PAS %>%
  left_join(Sum_Pasture, by = c("i1", "i2")) %>%
  mutate(LURATIO = LUAREA/sum_LUAREA)
VY_load_PAS2 <- VY_load_PAS2[, c(1,2,3,6)] %>%
  rename(RegionCode = i1, YEMF = i2)

Manure_7_Crop <- VY_load_Crop2 %>%
  left_join(Manure_6[, c(1:4)], by = c("RegionCode", "YEMF")) %>%
  mutate(ManureCropGrid = ManureCrop*LURATIO)
Manure_7_Crop <- Manure_7_Crop[, c(1,2,3,5,7)] %>%
  mutate(ManureCropGrid = ManureCropGrid/1000000) #TgN/yr

Manure_7_Pas <- VY_load_PAS2 %>%
  left_join(Manure_6[, c(1,2,3,5)], by = c("RegionCode", "YEMF")) %>%
  mutate(ManurePasGrid = ManurePas*LURATIO)
Manure_7_Pas <- Manure_7_Pas[, c(1,2,3,5,7)] %>%
  mutate(ManurePasGrid = ManurePasGrid/1000000) #TgN/yr

Manure_7 <- Manure_7_Crop %>%
  full_join(Manure_7_Pas, by = c("RegionCode", "YEMF", "G", "SCENARIO")) %>%
  mutate(ManureCropGrid = ifelse(is.na(ManureCropGrid), 0, ManureCropGrid)) %>%
  mutate(ManurePasGrid = ifelse(is.na(ManurePasGrid), 0, ManurePasGrid)) %>%
  mutate(ManureGrid = ManureCropGrid + ManurePasGrid)
Manure_7 <- Manure_7[, c(1,2,3,4,7)]

INman <- Manure_7 %>%
  group_by(YEMF, SCENARIO) %>%
  summarise(INman = sum(ManureGrid))

INmancrop <- Manure_7_Crop %>%
  group_by(YEMF, SCENARIO) %>%
  summarise(INman = sum(ManureCropGrid))

INmanpas <- Manure_7_Pas %>%
  group_by(YEMF, SCENARIO) %>%
  summarise(INman = sum(ManurePasGrid))

INmandis <- Manure_7[, c(2,3,4,5)]
INmancropdis <- Manure_7_Crop[, c(2,3,4,5)]
INmanpasdis <- Manure_7_Pas[, c(2,3,4,5)]

#N deposition
nc <- nc_open("../data/0505NDRYMON_Base2015.nc") #input data(dry) #mg/m2/year
nc2 <- nc_open("../data/0505NWETMON_Base2015.nc") #input data(wet) #mg/year/4x5grid
ncdims <- names(nc$dim) #get netcdf dimensions
timevar <- ncdims[which(ncdims %in% c("time", "Time", "datetime", "Datetime", "date", "Date"))[1]] #find time variable
variablelist <- names(nc$var)
vart <- ncvar_get(nc,timevar)
ncdims2 <- names(nc2$dim) #get netcdf dimensions
timevar2 <- ncdims2[which(ncdims2 %in% c("time", "Time", "datetime", "Datetime", "date", "Date"))[1]] #find time variable
variablelist2 <- names(nc2$var)
vart2 <- ncvar_get(nc2,timevar)
lat <- 1:360
lon <- 1:720
outdf <- data.frame()
varx <- as.list(variablelist)
names(varx) <- as.vector(variablelist)
varx2 <- as.list(variablelist2)
names(varx2) <- as.vector(variablelist2)
    

  for (x in variablelist) {
    var_data <- ncvar_get(nc, x)
    }
    varx21 <- t(var_data[, , 1])
    varx22 <- t(var_data[, , 2])
    varx23 <- t(var_data[, , 3])
    varx24 <- t(var_data[, , 4])
    varx25 <- t(var_data[, , 5])
    varx26 <- t(var_data[, , 6])
    varx27 <- t(var_data[, , 7])
    varx28 <- t(var_data[, , 8])
    varx29 <- t(var_data[, , 9])
    varx210 <- t(var_data[, , 10])
    varx211 <- t(var_data[, , 11])
    varx212 <- t(var_data[, , 12])

    for (x in variablelist2) {
    var_data2 <- ncvar_get(nc2, x)
    }
    varx21b <- t(var_data2[, , 1])
    varx22b <- t(var_data2[, , 2])
    varx23b <- t(var_data2[, , 3])
    varx24b <- t(var_data2[, , 4])
    varx25b <- t(var_data2[, , 5])
    varx26b <- t(var_data2[, , 6])
    varx27b <- t(var_data2[, , 7])
    varx28b <- t(var_data2[, , 8])
    varx29b <- t(var_data2[, , 9])
    varx210b <- t(var_data2[, , 10])
    varx211b <- t(var_data2[, , 11])
    varx212b <- t(var_data2[, , 12])

var_list <- list(varx21, varx22, varx23, varx24, varx25, varx26, varx27, varx28, varx29, varx210, varx211, varx212)
var_list2 <- list(varx21b, varx22b, varx23b, varx24b, varx25b, varx26b, varx27b, varx28b, varx29b, varx210b, varx211b, varx212b)
outdf1 <- do.call(rbind, lapply(1:12, function(i) {
  data.frame(
    sector = rep(x, nrow(var_list[[i]]) * ncol(var_list[[i]])),
    lat = rep(lat, each = ncol(var_list[[i]])),
    lon = rep(lon, times = nrow(var_list[[i]])),
    month = i,
    Value = as.vector(t(var_list[[i]]))
  )
}))

outdf2 <- do.call(rbind, lapply(1:12, function(i) {
  data.frame(
    sector = rep(x, nrow(var_list2[[i]]) * ncol(var_list2[[i]])),
    lat = rep(lat, each = ncol(var_list2[[i]])),
    lon = rep(lon, times = nrow(var_list2[[i]])),
    month = i,
    Value = as.vector(t(var_list2[[i]]))
  )
})) 

gridarea <- rgdx.param(data_prep,"GAIJ")
colnames(gridarea)<-c("lat","lon","area")
gridarea2 <- gridarea %>%
     mutate(across(c("lat", "lon"), as.numeric))

merged_data1a <- left_join(outdf1, gridarea2, by=c("lat", "lon"))
merged_data1b <- left_join(outdf2, gridarea2, by=c("lat", "lon"))
merged_data2a <- merged_data1a %>%
  group_by(sector, lat, lon, area) %>%
  summarise(Value = sum(Value)/12) %>%
  mutate(N_deposition1 = Value*area/100000000) %>% #Tg/year
  mutate(i2 = 2010) #Substitute variable

merged_data2b <- merged_data1b %>%
  group_by(sector, lat, lon, area) %>%
  summarise(Value = sum(Value)/12) %>%
  mutate(N_deposition2 = Value/80000000000000000) %>% #Tg/year
  mutate(i2 = 2010) #Substitute variable

merged_data2 <- merged_data2a %>%
  left_join(merged_data2b, c("sector", "lat", "lon", "area", "i2")) %>%
  mutate(N_deposition = N_deposition1+N_deposition2)

merged_data3 <- merged_data2[, c("lat", "lon", "N_deposition","i2")] 

VY_load3 <- VY_load2 %>%
  left_join(merged_data3, c("lat","lon","i2"))

VY_load4 <- VY_load3[,c("i1","i2","i3","GA","LUAREA","lat","lon","N_deposition")]

LUGRIDCROP <- VY_load4 %>%
  filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A")) %>%
  filter(i2 == "2010") %>% #Year
  group_by(i1, i2, lat, lon, N_deposition, GA) %>%
  summarise(sum_LUAREA = sum(LUAREA)) %>%
  mutate(N_deposition_Crop = N_deposition*sum_LUAREA/(GA*1000)) %>%  #Tg/year
  filter(!is.na(N_deposition))

LUGRIDPAS <- VY_load4 %>%
  filter(i3 %in% c("PAS")) %>%
  filter(i2 == "2010") %>% #Year
  group_by(i1, i2, lat, lon, N_deposition, GA) %>%
  summarise(sum_LUAREA = sum(LUAREA)) %>%
  mutate(N_deposition_Pas = N_deposition*sum_LUAREA/(GA*1000)) %>% #TgN/year
  filter(!is.na(N_deposition))

LUGRIDTOTAL <- LUGRIDCROP %>%
 left_join(LUGRIDPAS, by = c("i1","i2","lat","lon"))
LUGRIDTOTAL <- LUGRIDTOTAL[,c(1,2,3,4,8,12)]

INdep <- LUGRIDTOTAL %>%
  group_by(i2) %>%
  mutate(
    INdepcrop = sum(N_deposition_Crop, na.rm = TRUE), INdeppas = sum(N_deposition_Pas, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(INdep = INdepcrop+INdeppas)

MAP_GIJ <- MAP_GIJ %>%
  mutate(
    lat = as.numeric(as.character(lat)),
    lon = as.numeric(as.character(lon))
  ) %>%
  mutate(lat = round(lat, 1), lon = round(lon, 1))

INdepdis <- INdep[, c(2,3,4,5,6)] %>%
  left_join(MAP_GIJ, by = c("lat", "lon")) %>%
  rename(YEMF = i2) %>%
  mutate(N_deposition_grid = N_deposition_Crop+N_deposition_Pas)
INdepdis <- INdepdis[, c(1,6,7)] %>%
  mutate(N_deposition_grid = ifelse(is.na(N_deposition_grid), 0, N_deposition_grid))

INdepcropdis <- INdep[, c(2,3,4,5)] %>%
  left_join(MAP_GIJ, by = c("lat", "lon")) %>%
  rename(YEMF = i2)
INdepcropdis <- INdepcropdis[,c(1,4,5)]

INdeppasdis <- INdep[, c(2,3,4,6)] %>%
  left_join(MAP_GIJ, by = c("lat", "lon")) %>%
  rename(YEMF = i2)
INdeppasdis <- INdeppasdis[,c(1,4,5)]

rm(list = ls(pattern = "^varx"))

#N fixation
VY_load5 <- VY_load4[, c("i1", "i2", "i3", "LUAREA", "lat", "lon")]
LUGRID1 <- VY_load5 %>%
  filter(i3 %in% c("PDRIR")) %>%
  mutate(N_fix1 = LUAREA*25/1000000000) %>% #TgN/year
  group_by(i1, i2, lat, lon) %>%
  summarise(N_fix_A = sum(N_fix1))

LUGRID2 <- VY_load5 %>%
  filter(i3 %in% c("OSDIR", "OSDRF")) %>%
  mutate(N_fix2 = LUAREA*60/1000000000) %>% #TgN/year
  group_by(i1, i2, lat, lon) %>%
  summarise(N_fix_B = sum(N_fix2))

LUGRID3 <- VY_load5 %>%
  filter(i3 %in% c("WHTIR", "GROIR", "PDRRF", "WHTRF", "GRORF", "OTH_A")) %>%
  mutate(N_fix3 = LUAREA*12/1000000000) %>% #TgN/year
  group_by(i1, i2, lat, lon) %>%
  summarise(N_fix_C = sum(N_fix3))

LUGRID4 <- VY_load5 %>%
  filter(i3 %in% c("PAS")) %>%
  mutate(N_fix4 = LUAREA*5/1000000000) %>% #TgN/year
  group_by(i1, i2, lat, lon) %>%
  summarise(N_fix_D = sum(N_fix4))

NFIXCROP0 <- LUGRID1 %>%
  full_join(LUGRID2, c("i1", "i2", "lat", "lon")) %>%
  full_join(LUGRID3, c("i1", "i2", "lat", "lon")) %>%
  mutate(N_fix = N_fix_A+N_fix_B+N_fix_C)
NFIXCROP0[is.na(NFIXCROP0)] <- 0
NFIXCROP <- NFIXCROP0[, c("i1", "i2", "lat", "lon", "N_fix")] %>%
  rename(N_fix_crop = N_fix)

NFIXPAS <- LUGRID4[, c("i1", "i2", "lat", "lon", "N_fix_D")] %>%
  rename(N_fix_pas = N_fix_D)

NFIXTOTAL <- NFIXCROP %>%
 full_join(NFIXPAS, by = c("i1","i2","lat","lon"))
NFIXTOTAL[is.na(NFIXTOTAL)] <- 0

INfix <- NFIXTOTAL %>%
  group_by(i2) %>%
  summarise(
    N_fix_crop = sum(N_fix_crop, na.rm = TRUE), N_fix_pas = sum(N_fix_pas, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(INfix = N_fix_crop+N_fix_pas)

INfixCropandPas <- NFIXTOTAL %>%
  group_by(i2) %>%
  summarise(
    N_fix_crop = sum(N_fix_crop, na.rm = TRUE), N_fix_pas = sum(N_fix_pas, na.rm = TRUE)
  ) %>%
  ungroup()

INfixdis <- NFIXTOTAL %>%
  mutate(N_fix = N_fix_crop+N_fix_pas) %>%
  left_join(MAP_GIJ, by = c("lat", "lon"))
INfixdis <- INfixdis[, c(2,7,8)] %>%
  rename(YEMF = i2) %>%
  mutate(N_fix = ifelse(is.na(N_fix), 0, N_fix))

INfixcropdis <- NFIXCROP %>%
  left_join(MAP_GIJ, by = c("lat", "lon"))
INfixcropdis <- INfixcropdis[,c(2,5,6)]
INfixcropdis[is.na(INfixcropdis)] <- 0

INfixpasdis <- NFIXPAS %>%
  left_join(MAP_GIJ, by = c("lat", "lon"))
INfixpasdis <- INfixpasdis[,c(2,5,6)]

#Residue output
ResidueN_by_Crop <- yieldgrid_save_combined[, c(1,2,4,6,8,10,12,14,15)] %>% #t
  mutate(ALL_Yield_PDR = ALL_Yield_PDR*1.4*9.05*0.85/1000000000,
  ALL_Yield_WHT = ALL_Yield_WHT*1.3*9.58*0.865/1000000000,
  ALL_Yield_C_B = ALL_Yield_C_B*1.6*2*0.372/1000000000,
  ALL_Yield_GRO = ALL_Yield_GRO*1.2*9.61*0.86/1000000000,
  ALL_Yield_OSD = ALL_Yield_OSD*1.5*15.95*0.8855/1000000000,
  ALL_Yield_OTH_A = ALL_Yield_OTH_A*2.89*0.4*0.876/1000000000) #TgN

ResidueN_by_Crop2 <- yieldgrid_save_combined[, c(1,2,4,6,8,10,12,14,15)] %>% #t
  mutate(ALL_Yield_PDR = ALL_Yield_PDR*1.4*9.05*0.85/1000000000,
  ALL_Yield_WHT = ALL_Yield_WHT*1.3*9.58*0.865/1000000000,
  ALL_Yield_C_B = ALL_Yield_C_B*1.6*2*0.372/1000000000,
  ALL_Yield_GRO = ALL_Yield_GRO*1.2*9.61*0.86/1000000000,
  ALL_Yield_OSD = ALL_Yield_OSD*1.5*15.95*0.8855/1000000000,
  ALL_Yield_OTH_A = ALL_Yield_OTH_A*2.89*0.4*0.876/1000000000) #TgN

Rate_df2 <- Rate_df %>%
  filter(REMF == "World", SCENARIO == "SSP2_BaU_NoCC") #Scenario
Rate_df2$YEMF <- as.numeric(as.character(Rate_df2$YEMF))
Rate_df2$Ratio <- as.numeric(as.character(Rate_df2$Ratio)) 
Rate_df2 <- Rate_df2 %>%
  rename(R = Ratio)
Rate_df2 <- Rate_df2 %>%
  mutate(R= R/Rate_df2[2,5])
Rate_df3 <- Rate_df2[, c(4,5)]

OUTres <- ResidueN_by_Crop2 %>%
  mutate(ALL_Yield_Total = ALL_Yield_PDR+ALL_Yield_C_B+ALL_Yield_GRO+ALL_Yield_OSD+ALL_Yield_OTH_A+ALL_Yield_WHT) %>%
  rename(YEMF = Year) %>%
  left_join(Rate_df3, by = "YEMF") %>%
  mutate(ALL_Yield_Total = ALL_Yield_Total*R) %>%
  group_by(YEMF) %>%
  summarise(OUTres = sum(ALL_Yield_Total)) %>%
  rename(Year = YEMF)

OUTresdis <- ResidueN_by_Crop2 %>%
  mutate(OUTresgrid = ALL_Yield_PDR+ALL_Yield_C_B+ALL_Yield_GRO+ALL_Yield_OSD+ALL_Yield_OTH_A+ALL_Yield_WHT)%>%
  rename(YEMF = Year) %>%
  left_join(Rate_df3, by = "YEMF") %>%
  mutate(OUTresgrid = OUTresgrid*R)
OUTresdis <- OUTresdis[, c(2,9,10)] 

#Add residue to INfer
INfer <- ResidueN_by_Crop %>%
  mutate(ALL_Yield_Total = ALL_Yield_PDR+ALL_Yield_C_B+ALL_Yield_GRO+ALL_Yield_OSD+ALL_Yield_OTH_A+ALL_Yield_WHT) %>%
  mutate(INfer_EX = case_when(
    RegionCode %in% c("USA", "XE25", "XER", "TUR", "XOC", "JPN", "CAN", "XAF")~ ALL_Yield_Total * 0.65,
    RegionCode %in% c("CHN", "IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF")~ ALL_Yield_Total * 0.50)) %>%
  group_by(Year) %>%
  summarise(INfer_EX = sum(INfer_EX)) %>%
  rename(YEMF = Year) %>%
  left_join(INfer, by = "YEMF") %>%
  mutate(INfer = INfer + INfer_EX) %>%
  left_join(Rate_df3, by = "YEMF") %>%
  mutate(INfer = INfer + INfer_EX*(R-1))
INfer <- INfer[, c(1,2,3,4)]

INfercrop <- ResidueN_by_Crop %>%
  mutate(ALL_Yield_Total = ALL_Yield_PDR+ALL_Yield_C_B+ALL_Yield_GRO+ALL_Yield_OSD+ALL_Yield_OTH_A+ALL_Yield_WHT) %>%
  mutate(INfer_EX = case_when(
    RegionCode %in% c("USA", "XE25", "XER", "TUR", "XOC", "JPN", "CAN", "XAF")~ ALL_Yield_Total * 0.65,
    RegionCode %in% c("CHN", "IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF")~ ALL_Yield_Total * 0.50)) %>%
  group_by(Year) %>%
  summarise(INfer_EX = sum(INfer_EX)) %>%
  rename(YEMF = Year) %>%
  left_join(INfercrop, by = "YEMF") %>%
  mutate(INfercrop = INfercrop + INfer_EX) %>%
  left_join(Rate_df3, by = "YEMF") %>%
  mutate(INfercrop = INfercrop + INfer_EX*(R-1))
INfercrop <- INfercrop[, c(1,2,3,4)]

INferdis <- ResidueN_by_Crop %>%
  mutate(ALL_Yield_Total = ALL_Yield_PDR+ALL_Yield_C_B+ALL_Yield_GRO+ALL_Yield_OSD+ALL_Yield_OTH_A+ALL_Yield_WHT) %>%
  mutate(INfer_EX = case_when(
    RegionCode %in% c("USA", "XE25", "XER", "TUR", "XOC", "JPN", "CAN", "XAF")~ ALL_Yield_Total * 0.65,
    RegionCode %in% c("CHN", "IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF")~ ALL_Yield_Total * 0.50)) %>%
  rename(YEMF = Year) %>%
  full_join(INferdis, by = c("G", "YEMF"))
INferdis[is.na(INferdis)] <- 0
INferdis <- INferdis %>%
  left_join(Rate_df3, by = "YEMF") %>%
  mutate(INfer_EX = INfer_EX*R)
INferdis <- INferdis %>%
  mutate(Fert_Use_Grid = Fert_Use_Grid + INfer_EX)
INferdis <- INferdis[, c(2,9,12,13)] 
  

INfercropdis <- ResidueN_by_Crop %>%
  mutate(ALL_Yield_Total = ALL_Yield_PDR+ALL_Yield_C_B+ALL_Yield_GRO+ALL_Yield_OSD+ALL_Yield_OTH_A+ALL_Yield_WHT) %>%
  mutate(INfer_EX = case_when(
    RegionCode %in% c("USA", "XE25", "XER", "TUR", "XOC", "JPN", "CAN", "XAF")~ ALL_Yield_Total * 0.65,
    RegionCode %in% c("CHN", "IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF")~ ALL_Yield_Total * 0.50)) %>%
  rename(YEMF = Year) %>%
  full_join(INfercropdis, by = c("G", "YEMF"))
INfercropdis[is.na(INfercropdis)] <- 0
INfercropdis <- INfercropdis %>%
  left_join(Rate_df3, by = "YEMF") %>%
  mutate(INfer_EX = INfer_EX*R)
INfercropdis <- INfercropdis %>%
  mutate(Fert_Use_Grid = Fert_Use_Grid + INfer_EX)
INfercropdis <- INfercropdis[, c(2,9,12,13)]


#Gas output
socref <- rast("../data/socref.tif")

target_res <- rast(ext = ext(socref), resolution = c(0.5, 0.5), crs = crs(socref))
resampled_image <- resample(socref, target_res, method = "average")
  
resampled_df <- as.data.frame(resampled_image, xy = TRUE, na.rm = TRUE)
colnames(resampled_df) <- c("x", "y", "SOC")
  
resampled_df$J <- (resampled_df$x + 179.75) / 0.5 + 1
resampled_df$I <- (89.75 - resampled_df$y) / 0.5 + 1.04406

resampled_df2 <- resampled_df[, c(4, 5, 3)] %>%
  rename(lat = I) %>%
  rename(lon = J) %>%
  mutate(lat = round(lat, 1), lon = round(lon, 1)) %>%
  left_join(MAP_GIJ, by = c("lat", "lon"))
resampled_df2 <- resampled_df2[, c(3,4)]

VY_load_CropPas <- VY_load2 %>%
  filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A", "PAS")) %>%
  group_by(G, i2) %>%
  summarise(LUAREA = sum(LUAREA)) %>%
  rename(GA = LUAREA, YEMF = i2)

SOC_Fert <- INferdis %>%
  left_join(resampled_df2, by = "G") %>%
  left_join(VY_load_CropPas, by = c("G", "YEMF")) 
  
SOC_Fert <- SOC_Fert  %>%
  full_join(INmandis, by = c("G", "YEMF", "SCENARIO"))
SOC_Fert[is.na(SOC_Fert)] <- 0
SOC_Fert <- SOC_Fert %>%
  mutate(Fert_Use_Grid = ((Fert_Use_Grid+ManureGrid)*1000000000)/(GA)) %>% #kgN/ha
  mutate(SOC = (SOC/1.6)*100/3000)  #% #1.6g/cm3
SOC_Fert <- SOC_Fert[, c(1,2,3,4,5,6)]

SOC_Fertcrop <- INfercropdis %>%
  left_join(resampled_df2, by = "G") %>%
  left_join(VY_load_CropPas, by = c( "G", "YEMF")) %>%
  full_join(INmancropdis, by = c("G", "YEMF", "SCENARIO"))
SOC_Fertcrop[is.na(SOC_Fertcrop)] <- 0
SOC_Fertcrop <- SOC_Fertcrop %>%
  mutate(Fert_Use_Grid = ((Fert_Use_Grid+ManureCropGrid)*1000000000)/(GA)) %>% #kgN/ha
  mutate(SOC = (SOC/1.6)*100/3000)  #% #1.6g/cm3
SOC_Fertcrop <- SOC_Fertcrop[, c(1,2,3,4,5,6)]


SOC_Fertpas <- INferpasdis %>%
  left_join(resampled_df2, by = "G") %>%
  left_join(VY_load_CropPas, by = c("G", "YEMF")) %>%
  full_join(INmanpasdis, by = c("G", "YEMF", "SCENARIO"))
SOC_Fertpas[is.na(SOC_Fertpas)] <- 0
SOC_Fertpas <- SOC_Fertpas %>%
  mutate(Fert_Use_Grid = ((Fert_Use_Grid+ManurePasGrid)*1000000000)/(GA)) %>% #kgN/ha
  mutate(SOC = (SOC/1.6)*100/3000)  #% #1.6g/cm3
SOC_Fertpas <- SOC_Fertpas[, c(1,2,3,4,5,6)]


PRECTOT <- nc_open("../data/0505PRECTOT_test.nc4") #mg/m2/year
ncdims_P <- names(PRECTOT$dim) #get netcdf dimensions
timevar_P <- ncdims_P[which(ncdims_P %in% c("time", "Time", "datetime", "Datetime", "date", "Date"))[1]] #find time variable
variablelist_P <- names(PRECTOT$var)
vart_P <- ncvar_get(PRECTOT,timevar_P)
outdf <- data.frame()
varx_P <- as.list(variablelist_P)
names(varx_P) <- as.vector(variablelist_P)

for (x in variablelist_P) {
    var_data_P <- ncvar_get(PRECTOT, x)
    }
    varx1P <- t(var_data_P[, , 1])
    varx2P <- t(var_data_P[, , 2])
    varx3P <- t(var_data_P[, , 3])
    varx4P <- t(var_data_P[, , 4])
    varx5P <- t(var_data_P[, , 5])
    varx6P <- t(var_data_P[, , 6])
    varx7P <- t(var_data_P[, , 7])
    varx8P <- t(var_data_P[, , 8])
    varx9P <- t(var_data_P[, , 9])
    varx10P <- t(var_data_P[, , 10])
    varx11P <- t(var_data_P[, , 11])
    varx12P <- t(var_data_P[, , 12])

var_list_P <- list(varx1P, varx2P, varx3P, varx4P, varx5P, varx6P, varx7P, varx8P, varx9P, varx10P, varx11P, varx12P)
outdf_P <- do.call(rbind, lapply(1:12, function(i) {
  data.frame(
    sector = rep(x, nrow(var_list_P[[i]]) * ncol(var_list_P[[i]])),
    lat = rep(lat, each = ncol(var_list_P[[i]])),
    lon = rep(lon, times = nrow(var_list_P[[i]])),
    month = i,
    Value = as.vector(t(var_list_P[[i]]))
  )
}))

outdf2_P <- outdf_P %>%
  group_by(lat, lon) %>%
  summarise(Value = sum(Value)) %>%
  mutate(lat = round(lat, 1), lon = round(lon, 1)) %>%
  left_join(MAP_GIJ, by = c("lat", "lon"))
outdf2_P <- outdf2_P[, c(3,4)]

SOC_Fert_P <- SOC_Fert %>%
  left_join(outdf2_P, by = "G") %>%
  rename(PREC = Value) %>%
  mutate(PREC = PREC) #mm/year

SOC_Fertcrop_P <- SOC_Fertcrop %>%
  left_join(outdf2_P, by = "G") %>%
  rename(PREC = Value) %>%
  mutate(PREC = PREC) #mm/year

SOC_Fertpas_P <- SOC_Fertpas %>%
  left_join(outdf2_P, by = "G") %>%
  rename(PREC = Value) %>%
  mutate(PREC = PREC) #mm/year

OUTgasdis <- SOC_Fert_P %>%
  mutate(OUTgasgrid = ((0.025+0.000855*PREC+0.01725*Fert_Use_Grid+0.117*SOC)+(0.025+0.000855*PREC+0.01725*Fert_Use_Grid+0.117*SOC)*(14/8)+0.113*Fert_Use_Grid)*GA/1000000000) #TgN/year
OUTgasdis <- OUTgasdis[, c(1,2,3,8)]
OUTgasdis[is.na(OUTgasdis)] <- 0

OUTgascropdis <- SOC_Fertcrop_P %>%
  full_join(SOC_Fertpas_P, by = c("YEMF", "G", "SCENARIO", "SOC", "GA", "PREC")) 
OUTgascropdis[is.na(OUTgascropdis)] <- 0
OUTgascropdis <- OUTgascropdis %>%
  mutate(OUTgascropgrid = (((0.025+0.000855*PREC+0.117*SOC)*(Fert_Use_Grid.x/(Fert_Use_Grid.x+Fert_Use_Grid.y))+0.01725*Fert_Use_Grid.x)
  +((0.025+0.000855*PREC+0.117*SOC)*(Fert_Use_Grid.x/(Fert_Use_Grid.x+Fert_Use_Grid.y))+0.01725*Fert_Use_Grid.x)*(14/8)+0.113*Fert_Use_Grid.x)*GA/1000000000) #TgN/year
OUTgascropdis <- OUTgascropdis[, c(1,2,3,9)]
OUTgascropdis[is.na(OUTgascropdis)] <- 0

OUTgaspasdis <- SOC_Fertpas_P %>%
  full_join(SOC_Fertcrop_P, by = c("YEMF", "G", "SCENARIO", "SOC", "GA", "PREC")) 
OUTgaspasdis[is.na(OUTgaspasdis)] <- 0
OUTgaspasdis <- OUTgaspasdis  %>%
  mutate(OUTgaspasgrid = (((0.025+0.000855*PREC+0.117*SOC)*(Fert_Use_Grid.x/(Fert_Use_Grid.x+Fert_Use_Grid.y))+0.01725*Fert_Use_Grid.x)
  +((0.025+0.000855*PREC+0.117*SOC)*(Fert_Use_Grid.x/(Fert_Use_Grid.x+Fert_Use_Grid.y))+0.01725*Fert_Use_Grid.x)*(14/8)+0.113*Fert_Use_Grid.x)*GA/1000000000) #TgN/year
OUTgaspasdis <- OUTgaspasdis[, c(1,2,3,9)]
OUTgaspasdis[is.na(OUTgaspasdis)] <- 0

OUTgas <- OUTgasdis %>%
  group_by(YEMF, SCENARIO) %>%
  summarise(OUTgas = sum(OUTgasgrid, na.rm = TRUE))

OUTgascrop <- OUTgascropdis %>%
  group_by(YEMF, SCENARIO) %>%
  summarise(OUTgascrop = sum(OUTgascropgrid, na.rm = TRUE))

OUTgaspas <- OUTgaspasdis %>%
  group_by(YEMF, SCENARIO) %>%
  summarise(OUTgaspas = sum(OUTgaspasgrid, na.rm = TRUE))


OUTgasdis_NOx <- SOC_Fert_P %>%
  mutate(OUTgasgrid = ((0.025+0.000855*PREC+0.01725*Fert_Use_Grid+0.117*SOC))*GA/1000000000) #TgN/year
OUTgasdis_NOx <- OUTgasdis_NOx[, c(1,2,3,8)]
OUTgasdis_NOx[is.na(OUTgasdis_NOx)] <- 0

OUTgascropdis_NOx <- SOC_Fertcrop_P %>%
  full_join(SOC_Fertpas_P, by = c("YEMF", "G", "SCENARIO", "SOC", "GA", "PREC")) 
OUTgascropdis_NOx[is.na(OUTgascropdis_NOx)] <- 0
OUTgascropdis_NOx <- OUTgascropdis_NOx %>%
  mutate(OUTgascropgrid = (((0.025+0.000855*PREC+0.117*SOC)*(Fert_Use_Grid.x/(Fert_Use_Grid.x+Fert_Use_Grid.y))+0.01725*Fert_Use_Grid.x))*GA/1000000000) #TgN/year
OUTgascropdis_NOx <- OUTgascropdis_NOx[, c(1,2,3,9)]
OUTgascropdis_NOx[is.na(OUTgascropdis_NOx)] <- 0

OUTgaspasdis_NOx <- SOC_Fertpas_P %>%
  full_join(SOC_Fertcrop_P, by = c("YEMF", "G", "SCENARIO", "SOC", "GA", "PREC")) 
OUTgaspasdis_NOx[is.na(OUTgaspasdis_NOx)] <- 0
OUTgaspasdis_NOx <- OUTgaspasdis_NOx  %>%
  mutate(OUTgaspasgrid = (((0.025+0.000855*PREC+0.117*SOC)*(Fert_Use_Grid.x/(Fert_Use_Grid.x+Fert_Use_Grid.y))+0.01725*Fert_Use_Grid.x))*GA/1000000000) #TgN/year
OUTgaspasdis_NOx <- OUTgaspasdis_NOx[, c(1,2,3,9)]
OUTgaspasdis_NOx[is.na(OUTgaspasdis_NOx)] <- 0

OUTgas_NOx <- OUTgasdis_NOx %>%
  group_by(YEMF, SCENARIO) %>%
  summarise(OUTgas = sum(OUTgasgrid, na.rm = TRUE))

OUTgascrop_NOx <- OUTgascropdis_NOx %>%
  group_by(YEMF, SCENARIO) %>%
  summarise(OUTgascrop = sum(OUTgascropgrid, na.rm = TRUE))

OUTgaspas_NOx <- OUTgaspasdis_NOx %>%
  group_by(YEMF, SCENARIO) %>%
  summarise(OUTgaspas = sum(OUTgaspasgrid, na.rm = TRUE))

OUTgasdis_NH3 <- SOC_Fert_P %>%
  mutate(OUTgasgrid = ((0.113*Fert_Use_Grid))*GA/1000000000) #TgN/year
OUTgasdis_NH3 <- OUTgasdis_NH3[, c(1,2,3,8)]
OUTgasdis_NH3[is.na(OUTgasdis_NH3)] <- 0

OUTgascropdis_NH3 <- SOC_Fertcrop_P %>%
  full_join(SOC_Fertpas_P, by = c("YEMF", "G", "SCENARIO", "SOC", "GA", "PREC")) 
OUTgascropdis_NH3[is.na(OUTgascropdis_NH3)] <- 0
OUTgascropdis_NH3 <- OUTgascropdis_NH3 %>%
  mutate(OUTgascropgrid = ((0.113*Fert_Use_Grid.x))*GA/1000000000) #TgN/year
OUTgascropdis_NH3 <- OUTgascropdis_NH3[, c(1,2,3,9)]
OUTgascropdis_NH3[is.na(OUTgascropdis_NH3)] <- 0

OUTgaspasdis_NH3 <- SOC_Fertpas_P %>%
  full_join(SOC_Fertcrop_P, by = c("YEMF", "G", "SCENARIO", "SOC", "GA", "PREC")) 
OUTgaspasdis_NH3[is.na(OUTgaspasdis_NH3)] <- 0
OUTgaspasdis_NH3 <- OUTgaspasdis_NH3  %>%
  mutate(OUTgaspasgrid = ((0.113*Fert_Use_Grid.x)*GA/1000000000)) #TgN/year
OUTgaspasdis_NH3 <- OUTgaspasdis_NH3[, c(1,2,3,9)]
OUTgaspasdis_NH3[is.na(OUTgaspasdis_NH3)] <- 0

OUTgas_NH3 <- OUTgasdis_NH3 %>%
  group_by(YEMF, SCENARIO) %>%
  summarise(OUTgas = sum(OUTgasgrid, na.rm = TRUE))

OUTgascrop_NH3 <- OUTgascropdis_NH3 %>%
  group_by(YEMF, SCENARIO) %>%
  summarise(OUTgascrop = sum(OUTgascropgrid, na.rm = TRUE))

OUTgaspas_NH3 <- OUTgaspasdis_NH3 %>%
  group_by(YEMF, SCENARIO) %>%
  summarise(OUTgaspas = sum(OUTgaspasgrid, na.rm = TRUE))

OUTgasdis_N2 <- SOC_Fert_P %>%
  mutate(OUTgasgrid = ((0.025+0.000855*PREC+0.01725*Fert_Use_Grid+0.117*SOC)*(14/8))*GA/1000000000) #TgN/year
OUTgasdis_N2 <- OUTgasdis_N2[, c(1,2,3,8)]
OUTgasdis_N2[is.na(OUTgasdis_N2)] <- 0

OUTgascropdis_N2 <- SOC_Fertcrop_P %>%
  full_join(SOC_Fertpas_P, by = c("YEMF", "G", "SCENARIO", "SOC", "GA", "PREC")) 
OUTgascropdis_N2[is.na(OUTgascropdis_N2)] <- 0
OUTgascropdis_N2 <- OUTgascropdis_N2 %>%
  mutate(OUTgascropgrid = (((0.025+0.000855*PREC+0.117*SOC)*(Fert_Use_Grid.x/(Fert_Use_Grid.x+Fert_Use_Grid.y))+0.01725*Fert_Use_Grid.x)*(14/8))*GA/1000000000) #TgN/year
OUTgascropdis_N2 <- OUTgascropdis_N2[, c(1,2,3,9)]
OUTgascropdis_N2[is.na(OUTgascropdis_N2)] <- 0

OUTgaspasdis_N2 <- SOC_Fertpas_P %>%
  full_join(SOC_Fertcrop_P, by = c("YEMF", "G", "SCENARIO", "SOC", "GA", "PREC")) 
OUTgaspasdis_N2[is.na(OUTgaspasdis_N2)] <- 0
OUTgaspasdis_N2 <- OUTgaspasdis_N2  %>%
  mutate(OUTgaspasgrid = (((0.025+0.000855*PREC+0.117*SOC)*(Fert_Use_Grid.x/(Fert_Use_Grid.x+Fert_Use_Grid.y))+0.01725*Fert_Use_Grid.x)*(14/8))*GA/1000000000) #TgN/year
OUTgaspasdis_N2 <- OUTgaspasdis_N2[, c(1,2,3,9)]
OUTgaspasdis_N2[is.na(OUTgaspasdis_N2)] <- 0

OUTgas_N2 <- OUTgasdis_N2 %>%
  group_by(YEMF, SCENARIO) %>%
  summarise(OUTgas = sum(OUTgasgrid, na.rm = TRUE))

OUTgascrop_N2 <- OUTgascropdis_N2 %>%
  group_by(YEMF, SCENARIO) %>%
  summarise(OUTgascrop = sum(OUTgascropgrid, na.rm = TRUE))

OUTgaspas_N2 <- OUTgaspasdis_N2 %>%
  group_by(YEMF, SCENARIO) %>%
  summarise(OUTgaspas = sum(OUTgaspasgrid, na.rm = TRUE))

#Crop output
Crop_N <- yieldgrid_save_combined[, c(1,2,4,6,8,10,12,14,15)] %>% #t
  mutate(ALL_Yield_PDR = ALL_Yield_PDR*12.44*0.85/1000000000,
  ALL_Yield_WHT = ALL_Yield_WHT*20.69*0.865/1000000000,
  ALL_Yield_C_B = ALL_Yield_C_B*3.52*0.372/1000000000,
  ALL_Yield_GRO = ALL_Yield_GRO*13.12*0.86/1000000000,
  ALL_Yield_OSD = ALL_Yield_OSD*40.9*0.8855/1000000000,
  ALL_Yield_OTH_A = ALL_Yield_OTH_A*2.81*0.876/1000000000) #TgN

 #Scenario
 INferdis <- INferdis %>%
   filter(SCENARIO %in% "SSP2_BaU_NoCC") #scenario
 INferdis <- INferdis[, c(1,2,4)]

 INfercropdis <- INfercropdis %>%
   filter(SCENARIO %in% "SSP2_BaU_NoCC") #scenario
 INfercropdis <- INfercropdis[, c(1,2,4)]

 INferpasdis <- INferpasdis %>%
   filter(SCENARIO %in% "SSP2_BaU_NoCC") #scenario
 INferpasdis <- INferpasdis[, c(1,2,4)]

 INmandis <- INmandis %>%
   filter(SCENARIO %in% "SSP2_BaU_NoCC") #scenario
 INmandis <- INmandis[, c(1,2,4)]

 INmancropdis <- INmancropdis %>%
   filter(SCENARIO %in% "SSP2_BaU_NoCC") #scenario
 INmancropdis <- INmancropdis[, c(1,2,4)]

INmanpasdis <- INmanpasdis %>%
   filter(SCENARIO %in% "SSP2_BaU_NoCC") #scenario
 INmanpasdis <- INmanpasdis[, c(1,2,4)]

OUTgasdis <- OUTgasdis %>%
   filter(SCENARIO %in% "SSP2_BaU_NoCC") #scenario
OUTgasdis <- OUTgasdis[, c(1,2,4)]

OUTgaspasdis <- OUTgaspasdis %>%
  filter(SCENARIO %in% "SSP2_BaU_NoCC") #scenario
OUTgaspasdis <- OUTgaspasdis[, c(1,2,4)]

OUTgascropdis <- OUTgascropdis %>%
  filter(SCENARIO %in% "SSP2_BaU_NoCC") #scenario
OUTgascropdis <- OUTgascropdis[, c(1,2,4)]

INdepdis$YEMF <- 2010 #Substitute variable
Inputdis <- INferdis %>%
  full_join(INmandis, by = c("YEMF", "G")) %>%
  full_join(INdepdis, by = c("YEMF", "G")) %>%
  full_join(INfixdis, by = c("YEMF", "G"))
Inputdis[is.na(Inputdis)] <- 0
Inputdis <- Inputdis %>%
  mutate(Input = Fert_Use_Grid+ManureGrid+N_deposition_grid+N_fix)
Inputdis <- Inputdis[, c(1,2,7)]

INfixpasdis <- INfixpasdis %>%
  rename(YEMF = i2)
INfixcropdis <- INfixcropdis %>%
  rename(YEMF = i2)

INdeppasdis[is.na(INdeppasdis)] <- 0

Inputpasdis <- INferpasdis %>%
  full_join(INmanpasdis, by = c("YEMF", "G")) %>%
  full_join(INdeppasdis, by = c("YEMF", "G")) %>%
  full_join(INfixpasdis, by = c("YEMF", "G"))
Inputpasdis[is.na(Inputpasdis)] <- 0
Inputpasdis <- Inputpasdis %>%
  mutate(Input = Fert_Use_Grid+ManurePasGrid+N_deposition_Pas+N_fix_pas)
Inputpasdis <- Inputpasdis[,c(1,2,7)]

OUTcroppas <- Inputpasdis %>%
  full_join(OUTgaspasdis, by = c("YEMF", "G"))
OUTcroppas[is.na(OUTcroppas)] <- 0
OUTcroppas <- OUTcroppas %>%
  mutate(OUTcroppas = (Input-OUTgaspasgrid)*0.6)
OUTcroppas <- OUTcroppas[,c(1,2,5)]

OUTcropdis <- Crop_N %>%
  rename(YEMF = Year) %>%
  full_join(OUTcroppas, by = c("G", "YEMF"))
OUTcropdis[is.na(OUTcropdis)] <- 0
OUTcropdis <- OUTcropdis %>%
  mutate(CropN = ALL_Yield_PDR+ALL_Yield_C_B+ALL_Yield_GRO+ALL_Yield_OSD+ALL_Yield_OTH_A+ALL_Yield_WHT+OUTcroppas)
OUTcropdis <- OUTcropdis[, c(2,9,11)]

OUTcropcropdis <- Crop_N %>%
  rename(YEMF = Year)
OUTcropcropdis[is.na(OUTcropcropdis)] <- 0
OUTcropcropdis <- OUTcropcropdis %>%
  mutate(CropN = ALL_Yield_PDR+ALL_Yield_C_B+ALL_Yield_GRO+ALL_Yield_OSD+ALL_Yield_OTH_A+ALL_Yield_WHT)
OUTcropcropdis <- OUTcropcropdis[, c(2,9,10)]

OUTcroppasdis <- OUTcroppas



OUTcropdis <- OUTcropdis %>%
  left_join(Rate_df2, by = "YEMF") %>%
  mutate(CropN = CropN*R)
OUTcropdis <- OUTcropdis[, c(1,2,3)]

OUTcropcropdis <- OUTcropcropdis %>%
  left_join(Rate_df2, by = "YEMF") %>%
  mutate(CropN = CropN*R)
OUTcropcropdis <- OUTcropcropdis[, c(1,2,3)]

OUTcroppasdis <- OUTcroppasdis %>%
  left_join(Rate_df2, by = "YEMF") %>%
  mutate(OUTcroppas = OUTcroppas*R)
OUTcroppasdis <- OUTcroppasdis[, c(1,2,3)]



OUTcrop <- OUTcropdis %>%
  group_by(YEMF) %>%
  summarise(OUTcrop = sum(CropN))

OUTcropcroptotal <- OUTcropcropdis %>%
  group_by(YEMF) %>%
  summarise(OUTcrop = sum(CropN))

OUTcroppastotal <- OUTcroppasdis %>%
  group_by(YEMF) %>%
  summarise(OUTcrop = sum(OUTcroppas))


  


#Leaching
OUTleadis <- Inputdis %>%
  full_join(OUTcropdis, by = c("G", "YEMF")) %>%
  full_join(OUTgasdis, by = c("G", "YEMF")) %>%
  full_join(OUTresdis, by = c("G", "YEMF"))
OUTleadis[is.na(OUTleadis)] <- 0
OUTleadis <- OUTleadis %>%
  mutate(OUTleagrid = Input-(CropN+OUTgasgrid+OUTresgrid))
OUTleadis <- OUTleadis[, c(1,2,7)]

OUTlea <- OUTleadis %>%
  group_by(YEMF) %>%
  summarise(OUTlea = sum(OUTleagrid))

OUTleapasdis <- Inputpasdis %>%
  full_join(OUTcroppasdis, by = c("G", "YEMF")) %>%
  full_join(OUTgaspasdis, by = c("G", "YEMF"))
OUTleapasdis[is.na(OUTleapasdis)] <- 0
OUTleapasdis <- OUTleapasdis %>%
  mutate(OUTleagrid = Input-(OUTcroppas+OUTgaspasgrid))
OUTleapasdis <- OUTleapasdis[, c(1,2,6)]

OUTleapas <- OUTleapasdis %>%
  group_by(YEMF) %>%
  summarise(OUTleapas = sum(OUTleagrid))

OUTleacropdis <- OUTleadis %>%
  full_join(OUTleapasdis, by = c("G", "YEMF")) 
OUTleacropdis[is.na(OUTleacropdis)] <- 0
OUTleacropdis <- OUTleacropdis %>%
  mutate(OUTleacrop = OUTleagrid.x - OUTleagrid.y)
OUTleacropdis <- OUTleacropdis[, c(1,2,5)]

OUTleacrop <- OUTleacropdis %>%
  group_by(YEMF) %>%
  summarise(OUTleacrop = sum(OUTleacrop))

#NH3 other loss
NH3_loss1 <- final_df %>%
  left_join(MAP_RIJ, by = c("I", "J")) %>%
  filter(complete.cases(.)) %>%
  group_by(R, slnum) %>%
  summarise(ManureStable = sum(Manure)*0.4) %>% #kg #other loss
  ungroup() %>%
  mutate(ManureStable = case_when(
    slnum == 1 & R %in% c("USA", "CAN")~ ManureStable * (1-(0.15*0.23+0.27*0.6+0.263*0.6+0.108*0.7+0.184*0.78))*(50/80),
    slnum == 1 & R %in% c("XE25")~ ManureStable * (1-(0.357*0.6+0.368*0.6+0.2*0.7+0.07*0.78))*(50/80),
    slnum == 1 & R %in% c("XER")~ ManureStable * (1-(0.175*0.6+0.6*0.6+0.18*0.7+0.025*0.78))*(50/80),
    slnum == 1 & R %in% c("XSE", "XOC")~ ManureStable * (1-(0.16*0.23+0.01*0.6+0.76*0.7+0.08*0.78))*((50/80+40/60)/2),
    slnum == 1 & R %in% c("BRA", "XLM")~ ManureStable * (1-(0.01*0.6+0.01*0.6+0.36*0.7+0.62*0.78))*(40/60),
    slnum == 1 & R %in% c("XAF", "XNF")~ ManureStable * (1-(0.01*0.6+0.83*0.7+0.05*0.78))*((50/80+40/60)/2),
    slnum == 1 & R %in% c("XME")~ ManureStable * (1-(0.01*0.6+0.02*0.6+0.8*0.7+0.02*0.78))*(40/60),
    slnum == 1 & R %in% c("CHN", "CIS", "JPN", "TUR", "XSA")~ ManureStable * (1-(0.04*0.23+0.38*0.6+0.2*0.7+0.29*0.78+0.02*0.23))*((50/80+40/60)/2),
    slnum == 1 & R %in% c("IND")~ ManureStable * (1-(0.01*0.6+0.27*0.7+0.19*0.78+0.01+0.23))*(40/60),
    slnum == 2 & R %in% c("USA", "CAN")~ ManureStable * (1-(0.002*0.6+0.184*0.6+0.815*0.6))*(15/45),
    slnum == 2 & R %in% c("XE25")~ ManureStable * (1-(0.252*0.6+0.39*0.5+0.32*0.6+0.018*0.6))*(15/45),
    slnum == 2 & R %in% c("XER")~ ManureStable * (1-(0.225*0.6+0.44*0.5+0.2*0.6))*(15/45),
    slnum == 2 & R %in% c("XSE", "XOC")~ ManureStable * (1-(0.09*0.6+0.91*0.6))*((15/45+10/40)/2),
    slnum == 2 & R %in% c("BRA", "XLM")~ ManureStable * (1-(0.99*0.6))*(10/40),
    slnum == 2 & R %in% c("XAF", "XNF")~ ManureStable * (1-(0.01*0.6+0.95*0.6+0.01*0.6))*((15/45+10/40)/2),
    slnum == 2 & R %in% c("XME")~ ManureStable * (1-(0.01*0.6+0.79*0.6+0.02*0.6))*(10/40),
    slnum == 2 & R %in% c("CHN", "CIS", "JPN", "TUR", "XSA")~ ManureStable * (1-(0.46*0.6+0.5*0.6+0.02*0.6))*((15/45+10/40)/2),
    slnum == 2 & R %in% c("IND")~ ManureStable * (1-(0.01*0.6+0.04*0.6+0.22*0.6+0.2*0.6+0.01*0.6))*(10/40),
    slnum == 3 & R %in% c("USA", "CAN")~ ManureStable * (1-(0.328*0.22+0.185*0.52+0.042*0.5+0.04*0.5+0.406*0.75))*(15/45),
    slnum == 3 & R %in% c("XE25")~ ManureStable * (1-(0.087*0.22+0.137*0.5+0.028*0.75+0.698*0.75+0.02*0.78))*(15/45),
    slnum == 3 & R %in% c("XER")~ ManureStable * (1-(0.03*0.22+0.42*0.5+0.247*0.75+0.247*0.75))*(15/45),
    slnum == 3 & R %in% c("XSE", "XOC")~ ManureStable * (1-(0.54*0.22+0.03*0.5+0.15*0.5))*(15/45),
    slnum == 3 & R %in% c("BRA", "XLM")~ ManureStable * (1-(0.08*0.52+0.1*0.5+0.41*0.5+0.02*0.78))*(15/45),
    slnum == 3 & R %in% c("XAF", "XNF")~ ManureStable * (1-(0.06*0.52+0.06*0.5+0.87*0.5+0.01*0.75))*(15/45),
    slnum == 3 & R %in% c("XME")~ ManureStable * (1-(0.14*0.52+0.69*0.5+0.17*0.75))*(15/45),
    slnum == 3 & R %in% c("CHN", "CIS", "JPN", "TUR", "XSA")~ ManureStable * (1-(0.4*0.52+0.54*0.5))*(15/45),
    slnum == 3 & R %in% c("IND")~ ManureStable * (1-(0.09*0.22+0.22*0.52+0.16*0.5+0.3*0.5+0.03*0.75+0.09*0.78))*(15/45),
    slnum == 4 ~ ManureStable * (1-0.72)*(1/10),
    slnum == 5 ~ ManureStable * (1-0.72)*(1/9),
    slnum == 6 ~ ManureStable * (1-0.64),
    slnum == 7 ~ ManureStable * (1-0.64),
    slnum == 8 ~ ManureStable * (1-0.64),
    TRUE ~ NA_real_  
  )) %>%
  summarise(ManureStable = sum(ManureStable)/1000000000)  #Tg


NH3_loss2 <- final_df %>%
  left_join(MAP_RIJ, by = c("I", "J")) %>%
  filter(complete.cases(.)) %>%
  group_by(R, slnum) %>%
  summarise(ManureStable = sum(Manure)*0.4) %>% #kg #other loss
  ungroup() %>%
  mutate(ManureStable = case_when(
    slnum == 1 & R %in% c("USA", "XE25", "XER", "TUR", "XOC", "JPN", "CAN", "XAF")~ ManureStable * (1-0.92)*(30/80),
    slnum == 1 & R %in% c("CHN", "IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF")~ ManureStable * (1-0.85)*(20/60),
    slnum == 2 & R %in% c("USA", "XE25", "XER", "TUR", "XOC", "JPN", "CAN", "XAF")~ ManureStable * (1-0.92)*(30/45),
    slnum == 2 & R %in% c("CHN", "IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF")~ ManureStable * (1-0.85)*(30/40),
    slnum == 3 ~ ManureStable * (1-0.85)*(30/45),
    slnum == 4 & R %in% c("USA", "XE25", "XER", "TUR", "XOC", "JPN", "CAN", "XAF")~ ManureStable * (1-0.96)*(9/10),
    slnum == 4 & R %in% c("CHN", "IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF")~ ManureStable * (1-0.92)*(9/10),
    slnum == 5 & R %in% c("USA", "XE25", "XER", "TUR", "XOC", "JPN", "CAN", "XAF")~ ManureStable * (1-0.96)*(8/9),
    slnum == 5 & R %in% c("CHN", "IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF")~ ManureStable * (1-0.92)*(8/9),
    slnum == 6 ~ ManureStable * (1-0.64)*0,
    slnum == 7 ~ ManureStable * (1-0.64)*0,
    slnum == 8 ~ ManureStable * (1-0.64)*0,
    TRUE ~ NA_real_ 
  )) %>%
  summarise(ManurePas = sum(ManureStable)/1000000000)  #Tg 

NH3_loss_total <- NH3_loss1[1,1] + NH3_loss2[1,1]
  


#check
INfercropcheck <- INfercropdis %>%
  group_by(YEMF) %>%
  summarise(Fert_Use_Grid = sum(Fert_Use_Grid))

INferpascheck <- INferpasdis %>%
  group_by(YEMF) %>%
  summarise(Fert_Use_Grid = sum(Fert_Use_Grid))

INmancropcheck <- INmancropdis %>%
  group_by(YEMF) %>%
  summarise(ManureCropGrid = sum(ManureCropGrid))

INmanpascheck <- INmanpasdis %>%
  group_by(YEMF) %>%
  summarise(ManurePasGrid = sum(ManurePasGrid))

INdepcropcheck <- INdepcropdis %>%
  group_by(YEMF) %>%
  summarise(N_deposition_Crop = sum(N_deposition_Crop))

INdeppascheck <- INdeppasdis %>%
  group_by(YEMF) %>%
  summarise(N_deposition_Pas = sum(N_deposition_Pas))

INfixcropcheck <- INfixcropdis %>%
  group_by(YEMF) %>%
  summarise(N_fix_crop = sum(N_fix_crop))

INfixpascheck <- INfixpasdis %>%
  group_by(YEMF) %>%
  summarise(N_fix_pas = sum(N_fix_pas))

OUTrescheck <- OUTresdis %>%
  group_by(YEMF) %>%
  summarise(OUTresgrid = sum(OUTresgrid))

OUTcropcheck <- OUTcropdis %>%
  group_by(YEMF) %>%
  summarise(CropN = sum(CropN))

OUTgascheck <- OUTgasdis %>%
  group_by(YEMF) %>%
  summarise(OUTgasgrid = sum(OUTgasgrid))


OUTgasdis_NOx <- SOC_Fert_P %>%
  mutate(OUTgasgrid = (0.000855*PREC)*GA*1000/1000000000) #TgN/year
OUTgasdis_NOx <- OUTgasdis_NOx[, c(1,2,3,8)]
OUTgasdis_NOx[is.na(OUTgasdis_NOx)] <- 0

OUTgas_NOx <- OUTgasdis_NOx %>%
  group_by(YEMF, SCENARIO) %>%
  summarise(OUTgas = sum(OUTgasgrid, na.rm = TRUE))



OUTleacheck <- OUTleadis %>%
  group_by(YEMF) %>%
  summarise(OUTleagrid = sum(OUTleagrid))

OUTleacropcheck <- OUTleacropdis %>%
  group_by(YEMF) %>%
  summarise(OUTleacrop = sum(OUTleacrop))


OUTleapascheck <- OUTleapasdis %>%
  group_by(YEMF) %>%
  summarise(OUTleagrid = sum(OUTleagrid))

Inputcheck <- Inputdis %>%
  group_by(YEMF) %>%
  summarise(Input = sum(Input))

#Region NUE
OUTenvdis <- Inputdis %>%
  full_join(OUTcropdis, by = c("G", "YEMF")) %>%
  full_join(OUTresdis, by = c("G", "YEMF"))
OUTenvdis[is.na(OUTenvdis)] <- 0
OUTenvdis <- OUTenvdis %>%
  mutate(OUTleagrid = Input-(CropN+OUTresgrid))
OUTenvdis <- OUTenvdis[, c(1,2,6)]

OUTenvdis <- OUTenvdis %>%
  left_join(MAP_RG, by = "G") %>%
  group_by(YEMF, R) %>%
  summarise(outenv = sum(OUTleagrid))