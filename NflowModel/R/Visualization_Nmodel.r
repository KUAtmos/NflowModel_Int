library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(patchwork)
library(cowplot)
library(scales)
library(grid)
data_prep <- "../data/data_prep.gdx"
MAP_RG <- rgdx.set(data_prep, 'MAP_RG')
Global_17 <- "../data/global_17_IAMC.gdx"
IAMC_Template <- rgdx.param(Global_17, 'IAMC_Template')
select <- dplyr::select

#NH3 emission Region Aggregate
    #CEDS
    NH3_df <- read_csv("../data/NH3_CEDS_emissions_by_country_sector_v_2025_03_18.csv")
    NH3_df2 <- NH3_df %>%
          pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "Value") %>%
          mutate(year = as.integer(str_remove(year, "X"))) %>%
          filter(year == 2010 | year == 2020) %>%
          filter(sector == "3D_Soil-emissions") %>%
          dplyr::select(country, year, Value)
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
    BRA  = "Brazil",
    CAN  = "Canada",
    CHN  = "China and\nMongolia",
    CIS  = "Former\nSoviet Union",
    IND  = "India",
    JPN  = "Japan",
    TUR  = "Turkey",
    USA  = "United States",
    XAF  = "Sub-Saharan\nAfrica",
    XER  = "Non-EU Europe",
    XE25 = "EU",
    XLM  = "Latin America",
    XME  = "Middle East",
    XNF  = "North Africa",
    XOC  = "New Zealand\nand Australia",
    XSA  = "Other Asia",
    XSE  = "Other East and\nSoutheast Asia"
  )

    NH3_comp <- NH3_comp %>%
    mutate(region = factor(region, levels = names(region_lab)))

    NH3emi_region <- ggplot(
    NH3_comp %>% filter(year == 2010),
    aes(x = Source, y = Value, fill = Source)
    ) +
    geom_col(position = "dodge") +
    facet_wrap(
        ~ region, scales = "free_y",
        labeller = as_labeller(region_lab) 
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

    # ------------------------------------------------------------
    # NH3 emission Region Aggregate: region code version
    # ------------------------------------------------------------

    NH3emi_region_code_2010 <- ggplot(
    NH3_comp %>% filter(year == 2010),
    aes(x = Source, y = Value, fill = Source)
    ) +
    geom_col(position = "dodge") +
    facet_wrap(
        ~ region, scales = "free_y"
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

    NH3emi_region_code_2020 <- ggplot(
    NH3_comp %>% filter(year == 2020),
    aes(x = Source, y = Value, fill = Source)
    ) +
    geom_col(position = "dodge") +
    facet_wrap(
        ~ region, scales = "free_y"
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

    dir.create("../output", showWarnings = FALSE, recursive = TRUE)

        # Japanese region-name version
        ggsave(
        filename = "../output/NH3_emiRegion_2010.png",
        plot     = NH3emi_region,
        width    = 10,
        height   = 7,
        dpi      = 300,
        bg       = "white"
        )

        ggsave(
        filename = "../output/NH3_emiRegion_2020.png",
        plot     = NH3emi_region2,
        width    = 10,
        height   = 7,
        dpi      = 300,
        bg       = "white"
        )

        # Region-code version
        ggsave(
        filename = "../output/NH3_emiRegion_code_2010.png",
        plot     = NH3emi_region_code_2010,
        width    = 10,
        height   = 7,
        dpi      = 300,
        bg       = "white"
        )

        ggsave(
        filename = "../output/NH3_emiRegion_code_2020.png",
        plot     = NH3emi_region_code_2020,
        width    = 10,
        height   = 7,
        dpi      = 300,
        bg       = "white"
        )

#Future nitrogen flows
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
        "INfer"   = "#0072B2",
        "INman"   = "#009E73",
        "INdep"   = "#56B4E9",
        "INfix"   = "#CCBB44",
        "OUTcrop" = "#CC79A7",
        "OUTgas"  = "#E69F00",
        "OUTlea"  = "#D55E00",
        "OUTres"  = "#666666"
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

library(raster)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(cowplot)
library(scales)
library(grid)

# ------------------------------------------------------------
# Map projection
# ------------------------------------------------------------

# WGS84 longitude-latitude
crs_lonlat <- sf::st_crs(4326)

# Mollweide projection
moll_proj4 <- paste(
  "+proj=moll",
  "+lon_0=0",
  "+datum=WGS84",
  "+units=m",
  "+no_defs"
)

crs_moll <- sf::st_crs(moll_proj4)

map_lat_min <- -60
map_lat_max <-  70

#Plotting: Input / Output region aggregate
    dir.create("../output", showWarnings = FALSE, recursive = TRUE)

    plot_years <- c(2010L, 2030L, 2050L, 2100L)

    format_region_labels <- function(df) {
    df %>%
        mutate(
        R = recode(
            R,
            "BRA"  = "Brazil",
            "CAN"  = "Canada",
            "CHN"  = "China",
            "CIS"  = "Former USSR",
            "IND"  = "India",
            "JPN"  = "Japan",
            "TUR"  = "Turkey",
            "XAF"  = "Sub-saharan",
            "XER"  = "Other Europe",
            "XE25" = "EU25",
            "XLM"  = "Other South America",
            "XME"  = "Middle East",
            "XNF"  = "North Africa",
            "XOC"  = "Oceania",
            "XSA"  = "Other Asia",
            "XSE"  = "South-East Asia"
        ),
        R = stringr::str_wrap(R, width = 12)
        )
    }

    get_case_object <- function(prefix,
                                scenario_tag,
                                year,
                                use_BaU2010_for_missing = FALSE) {
    obj_name <- paste0(prefix, "_", scenario_tag, "_", year)

    if (exists(obj_name, envir = globalenv())) {
        return(get(obj_name, envir = globalenv()))
    }

    if (use_BaU2010_for_missing && year == 2010L) {
        obj_name_bau <- paste0(prefix, "_BaU_2010")

        if (exists(obj_name_bau, envir = globalenv())) {
        return(get(obj_name_bau, envir = globalenv()))
        }
    }

    stop("No object found for: ", obj_name)
    }

    build_Input_R <- function(scenario_tag,
                            years = plot_years,
                            use_BaU2010_for_missing = FALSE) {
    df_list <- lapply(years, function(yr) {
        obj_name <- paste0("Input_R_", scenario_tag, "_", yr)

        if (exists(obj_name, envir = globalenv())) {
        return(get(obj_name, envir = globalenv()))
        }

        if (use_BaU2010_for_missing && yr == 2010L) {
        obj_name_bau <- "Input_R_BaU_2010"

        if (exists(obj_name_bau, envir = globalenv())) {
            return(get(obj_name_bau, envir = globalenv()))
        }
        }

        stop("No object found for: ", obj_name)
    })

    purrr::reduce(df_list, left_join, by = "R")
    }

    build_region_component_df <- function(scenario_tag,
                                        prefixes,
                                        value_cols,
                                        labels,
                                        years = plot_years,
                                        use_BaU2010_for_missing = FALSE) {
    if (!exists("MAP_RG", envir = globalenv())) {
        stop("No MAP_RG object found in the global environment. Please run the necessary preprocessing steps to create it.")
    }

    MAP_RG_local <- get("MAP_RG", envir = globalenv())

    purrr::map_dfr(seq_along(prefixes), function(i) {
        purrr::map_dfr(years, function(yr) {
        df_raw <- get_case_object(
            prefix = prefixes[i],
            scenario_tag = scenario_tag,
            year = yr,
            use_BaU2010_for_missing = use_BaU2010_for_missing
        )

        if (!value_cols[i] %in% names(df_raw)) {
            stop(
            "No column found for ",
            value_cols[i],
            " in ",
            prefixes[i],
            "_",
            scenario_tag,
            "_",
            yr
            )
        }

        df_raw %>%
            left_join(MAP_RG_local, by = "G") %>%
            filter(as.character(YEMF) == as.character(yr)) %>%
            group_by(R) %>%
            summarise(
            Value = sum(.data[[value_cols[i]]], na.rm = TRUE),
            .groups = "drop"
            ) %>%
            mutate(
            Year = yr,
            source = labels[i]
            )
        })
    })
    }

    save_input_total_plot <- function(Input_R_df, outfile) {
    df_input_long <- Input_R_df %>%
        pivot_longer(
        cols = starts_with("Input"),
        names_to = "Year",
        values_to = "Value"
        ) %>%
        mutate(
        Year = stringr::str_remove(Year, "Input") %>% as.integer()
        ) %>%
        format_region_labels()

    Input_main <- ggplot(
        df_input_long,
        aes(x = factor(Year), y = Value, fill = factor(Year))
    ) +
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

    ggsave(outfile, plot = Input_main, width = 14, height = 14, dpi = 500)
    }

    save_input_stacked_plot <- function(df_input_long_comp, outfile) {
    df_plot <- df_input_long_comp %>%
        format_region_labels() %>%
        mutate(
        Year = factor(Year, levels = c(2010, 2030, 2050, 2100)),
        source = factor(
            source,
            levels = c("Fertilizer", "Manure", "Deposition", "Fixation")
        )
        )

    Input_stacked <- ggplot(
        df_plot,
        aes(x = Year, y = Value, fill = source)
    ) +
        geom_col(width = 0.7) +
        facet_wrap(~ R, scales = "free_y", ncol = 5) +
        labs(
        x = "Year",
        y = "Nitrogen Input (Tg N/yr)",
        fill = "Source"
        ) +
        scale_fill_brewer(palette = "Set2") +
        theme_minimal() +
        theme(
        strip.background = element_rect(fill = "gray85", color = NA),
        strip.text = element_text(size = 23),
        axis.text.x = element_text(size = 27, angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 28),
        axis.title = element_text(size = 36),
        legend.title = element_text(size = 28),
        legend.text = element_text(size = 24),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        )

    ggsave(outfile, plot = Input_stacked, width = 17, height = 12, dpi = 500)
    }

    save_output_stacked_plot <- function(df_output_long_comp, outfile) {
    df_plot <- df_output_long_comp %>%
        mutate(
        Year = factor(Year, levels = c(2010, 2030, 2050, 2100)),
        source = factor(
            source,
            levels = c("Crop uptake", "Residues", "Gaseous loss", "Leaching loss")
        )
        )

    OUT_stacked_code <- ggplot(
        df_plot,
        aes(x = Year, y = Value, fill = source)
    ) +
        geom_col(width = 0.7) +
        facet_wrap(~ R, scales = "free_y", ncol = 5) +
        labs(
        x = "Year",
        y = "Nitrogen Output (Tg N/yr)",
        fill = "Component"
        ) +
        scale_fill_brewer(palette = "Set2") +
        theme_minimal() +
        theme(
        strip.background = element_rect(fill = "gray85", color = NA),
        strip.text = element_text(size = 23),
        axis.text.x = element_text(size = 24, angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 28),
        axis.title = element_text(size = 36),
        legend.title = element_text(size = 28),
        legend.text = element_text(size = 24),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        )

    ggsave(outfile, plot = OUT_stacked_code, width = 14, height = 10, dpi = 500)
    }

    Input_R_BaU <- build_Input_R(
    scenario_tag = "BaU",
    use_BaU2010_for_missing = FALSE
    )

    df_input_long_comp_BaU <- build_region_component_df(
    scenario_tag = "BaU",
    prefixes = c("INferdis", "INmandis", "INdepdis", "INfixdis"),
    value_cols = c(
        "Fert_Use_Grid",
        "ManureGrid",
        "N_deposition_grid",
        "N_fix"
    ),
    labels = c("Fertilizer", "Manure", "Deposition", "Fixation"),
    use_BaU2010_for_missing = FALSE
    )

    df_output_long_comp_BaU <- build_region_component_df(
    scenario_tag = "BaU",
    prefixes = c("OUTcropdis", "OUTresdis", "OUTgasdis", "OUTleadis"),
    value_cols = c(
        "CropN",
        "OUTresgrid",
        "OUTgasgrid",
        "OUTleagrid"
    ),
    labels = c("Crop uptake", "Residues", "Gaseous loss", "Leaching loss"),
    use_BaU2010_for_missing = FALSE
    )

    Input_R_500C <- build_Input_R(
    scenario_tag = "500C",
    use_BaU2010_for_missing = TRUE
    )

    df_input_long_comp_500C <- build_region_component_df(
    scenario_tag = "500C",
    prefixes = c("INferdis", "INmandis", "INdepdis", "INfixdis"),
    value_cols = c(
        "Fert_Use_Grid",
        "ManureGrid",
        "N_deposition_grid",
        "N_fix"
    ),
    labels = c("Fertilizer", "Manure", "Deposition", "Fixation"),
    use_BaU2010_for_missing = TRUE
    )

    df_output_long_comp_500C <- build_region_component_df(
    scenario_tag = "500C",
    prefixes = c("OUTcropdis", "OUTresdis", "OUTgasdis", "OUTleadis"),
    value_cols = c(
        "CropN",
        "OUTresgrid",
        "OUTgasgrid",
        "OUTleagrid"
    ),
    labels = c("Crop uptake", "Residues", "Gaseous loss", "Leaching loss"),
    use_BaU2010_for_missing = TRUE
    )

    save_input_total_plot(
    Input_R_BaU,
    "../output/Input_17_revised2.png"
    )

    save_input_stacked_plot(
    df_input_long_comp_BaU,
    "../output/Input_17_stacked_components.png"
    )

    save_output_stacked_plot(
    df_output_long_comp_BaU,
    "../output/Output_17_finalrep.png"
    )

    save_input_total_plot(
    Input_R_500C,
    "../output/Input_17_revised2_500C.png"
    )

    save_input_stacked_plot(
    df_input_long_comp_500C,
    "../output/Input_17_stacked_components_500C.png"
    )

    save_output_stacked_plot(
    df_output_long_comp_500C,
    "../output/Output_17_finalrep_500C.png"
    )

#Distribution
# ------------------------------------------------------------
# Prepare world boundaries
# ------------------------------------------------------------

world_lonlat <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

map_bbox_lonlat <- sf::st_bbox(
  c(
    xmin = -180,
    ymin = map_lat_min,
    xmax =  180,
    ymax = map_lat_max
  ),
  crs = crs_lonlat
)

world_crop <- suppressWarnings(
  sf::st_crop(world_lonlat, map_bbox_lonlat)
)

world_moll <- world_crop %>%
  sf::st_make_valid() %>%
  sf::st_transform(crs = crs_moll)

geom_types <- as.character(sf::st_geometry_type(world_moll))

world_moll <- world_moll[geom_types %in% c("POLYGON", "MULTIPOLYGON"), ]

# ------------------------------------------------------------
# Graticule (grid lines)
# ------------------------------------------------------------
graticule_lonlat <- sf::st_graticule(
  lon = seq(-180, 180, by = 60),
  lat = seq(-60, 60, by = 20),
  crs = crs_lonlat
)

graticule_moll <- sf::st_transform(
  graticule_lonlat,
  crs = crs_moll
)

# ------------------------------------------------------------
# Mollweide ellipse polygon
# ------------------------------------------------------------

make_mollweide_ellipse <- function(
    crs_from = crs_lonlat,
    crs_to = crs_moll,
    n = 1000
) {

  lat_seq <- seq(
    -89.999,
     89.999,
    length.out = n
  )

  right_sf <- sf::st_as_sf(
    data.frame(
      lon = rep(179.999, n),
      lat = lat_seq
    ),
    coords = c("lon", "lat"),
    crs = crs_from
  )

  left_sf <- sf::st_as_sf(
    data.frame(
      lon = rep(-179.999, n),
      lat = rev(lat_seq)
    ),
    coords = c("lon", "lat"),
    crs = crs_from
  )

  right_xy <- sf::st_coordinates(
    sf::st_transform(right_sf, crs_to)
  )

  left_xy <- sf::st_coordinates(
    sf::st_transform(left_sf, crs_to)
  )

  coords <- rbind(
    right_xy[, 1:2, drop = FALSE],
    left_xy[, 1:2, drop = FALSE]
  )

  coords <- rbind(
    coords,
    coords[1, , drop = FALSE]
  )

  sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(coords)),
      crs = crs_to
    )
  )
}

mollweide_ellipse <- make_mollweide_ellipse()

# ------------------------------------------------------------
# Plot window in Mollweide coordinates
# ------------------------------------------------------------
make_plot_window_moll <- function(
    lon_min = -180,
    lon_max = 180,
    lat_min = map_lat_min,
    lat_max = map_lat_max,
    n = 400
) {
  boundary_pts <- rbind(
    cbind(seq(lon_min, lon_max, length.out = n), rep(lat_max, n)), # top
    cbind(seq(lon_min, lon_max, length.out = n), rep(lat_min, n)), # bottom
    cbind(rep(lon_min, n), seq(lat_min, lat_max, length.out = n)), # left
    cbind(rep(lon_max, n), seq(lat_min, lat_max, length.out = n))  # right
  )

  pts_sf <- sf::st_as_sf(
    data.frame(
      lon = boundary_pts[, 1],
      lat = boundary_pts[, 2]
    ),
    coords = c("lon", "lat"),
    crs = crs_lonlat
  )

  pts_moll <- sf::st_transform(pts_sf, crs = crs_moll)
  xy <- sf::st_coordinates(pts_moll)

  list(
    xlim = range(xy[, 1], na.rm = TRUE),
    ylim = range(xy[, 2], na.rm = TRUE)
  )
}

plot_window_moll <- make_plot_window_moll()

# ------------------------------------------------------------
# Full-globe Mollweide boundary (for white ellipse fill)
# ------------------------------------------------------------
make_globe_boundary_lonlat <- function(
    lon_min = -180,
    lon_max = 180,
    lat_min = -89.999,
    lat_max =  89.999,
    n = 500
) {
  top <- cbind(
    seq(lon_min, lon_max, length.out = n),
    rep(lat_max, n)
  )

  right <- cbind(
    rep(lon_max, n),
    seq(lat_max, lat_min, length.out = n)
  )

  bottom <- cbind(
    seq(lon_max, lon_min, length.out = n),
    rep(lat_min, n)
  )

  left <- cbind(
    rep(lon_min, n),
    seq(lat_min, lat_max, length.out = n)
  )

  coords <- rbind(
    top,
    right,
    bottom,
    left,
    top[1, , drop = FALSE]
  )

  sf::st_sfc(
    sf::st_polygon(list(coords)),
    crs = crs_lonlat
  )
}

globe_boundary_lonlat <- make_globe_boundary_lonlat()

globe_boundary_moll <- sf::st_transform(
  globe_boundary_lonlat,
  crs = crs_moll
)

map_boundary_lonlat <- make_map_boundary_lonlat()

map_boundary_moll <- sf::st_transform(
  map_boundary_lonlat,
  crs = crs_moll
)

get_case_object <- function(prefix, scenario_tag, year) {
  obj_name <- paste0(prefix, "_", scenario_tag, "_", year)

  if (!exists(obj_name, envir = globalenv())) {
    stop("No object found for: ", obj_name)
  }

  get(obj_name, envir = globalenv())
}

check_map_objects <- function() {
  required <- c(
    "MAP_GIJ",
    "gridarea2",
    "Inputdis_BaU_2010",
    "Inputdis_BaU_2100",
    "Inputdis_500C_2100",
    "OUTgasdis_BaU_2010",
    "OUTgasdis_BaU_2100",
    "OUTgasdis_500C_2100",
    "OUTleadis_BaU_2010",
    "OUTleadis_BaU_2100",
    "OUTleadis_500C_2100",
    "VY_load2_BaU_2010",
    "VY_load2_BaU_2100",
    "VY_load2_500C_2100"
  )

  missing <- required[
    !vapply(required, exists, logical(1), envir = globalenv())
  ]

  if (length(missing) > 0) {
    stop(
      "No required objects found for map creation:\n",
      paste(missing, collapse = "\n")
    )
  }
}

check_map_objects()
make_input_map_df <- function(scenario_tag, year) {
  year <- as.integer(year)

  Inputdis_case <- get_case_object(
    "Inputdis",
    scenario_tag,
    year
  )

  VY_load2_case <- get_case_object(
    "VY_load2",
    scenario_tag,
    year
  )

  landuse_classes <- c(
    "PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR",
    "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF",
    "OTH_A", "PAS"
  )

  Totaldistribution <- VY_load2_case %>%
    dplyr::select(
      i2,
      i3,
      lat,
      lon,
      LUAREA
    ) %>%
    dplyr::filter(
      i2 == year,
      i3 %in% landuse_classes
    ) %>%
    dplyr::group_by(
      i2,
      lat,
      lon
    ) %>%
    dplyr::summarise(
      LUAREA = sum(LUAREA, na.rm = TRUE),
      .groups = "drop"
    )

  Inputdismap <- Inputdis_case %>%
    dplyr::left_join(
      MAP_GIJ,
      by = "G"
    ) %>%
    dplyr::filter(
      YEMF == year
    ) %>%
    dplyr::select(
      G,
      YEMF,
      Input,
      lat,
      lon
    ) %>%
    dplyr::left_join(
      Totaldistribution,
      by = c("lat", "lon")
    ) %>%
    dplyr::mutate(
      Input = dplyr::if_else(
        is.na(LUAREA) | LUAREA <= 0,
        NA_real_,
        Input * 1000000000 / LUAREA
      ),
      x = 0.5 * lon - 180.25,
      y = -0.5 * lat + 90.25
    ) %>%
    dplyr::select(
      Input,
      x,
      y
    )

  Inputdismap
}

# ------------------------------------------------------------
# Longitude labels
# ------------------------------------------------------------

make_lon_label_df <- function(label_lat = map_lat_min + 6) {
  lon_vals_plot <- c(-179.9, -120, -60, 0, 60, 120, 179.9)
  lon_labs <- c("180°", "120°W", "60°W", "0°", "60°E", "120°E", "180°")

  pts <- sf::st_as_sf(
    data.frame(
      lon = lon_vals_plot,
      lat = rep(label_lat, length(lon_vals_plot)),
      label = lon_labs
    ),
    coords = c("lon", "lat"),
    crs = crs_lonlat
  )

  pts_moll <- sf::st_transform(pts, crs = crs_moll)
  xy <- sf::st_coordinates(pts_moll)

  data.frame(
    x = xy[, 1],
    y = xy[, 2],
    label = lon_labs
  )
}

lon_label_df <- make_lon_label_df()

make_outgas_map_df <- function(scenario_tag, year) {
  year <- as.integer(year)

  OUTgasdis_case <- get_case_object("OUTgasdis", scenario_tag, year)

  OUTgasdismap <- OUTgasdis_case %>%
    dplyr::left_join(MAP_GIJ, by = "G") %>%
    dplyr::filter(YEMF == year) %>%
    dplyr::select(G, YEMF, OUTgasgrid, lat, lon) %>%
    dplyr::left_join(gridarea2, by = c("lat", "lon")) %>%
    dplyr::mutate(
      OUTgasgrid = if_else(
        is.na(area) | area <= 0,
        NA_real_,
        OUTgasgrid * 1000000 / area
      ),
      x = 0.5 * lon - 180.25,
      y = -0.5 * lat + 90.25
    ) %>%
    dplyr::select(OUTgasgrid, x, y)

  OUTgasdismap
}

make_outlea_map_df <- function(scenario_tag, year) {
  year <- as.integer(year)

  OUTleadis_case <- get_case_object("OUTleadis", scenario_tag, year)

  OUTleadismap <- OUTleadis_case %>%
    dplyr::left_join(MAP_GIJ, by = "G") %>%
    dplyr::filter(YEMF == year) %>%
    dplyr::select(G, YEMF, OUTleagrid, lat, lon) %>%
    dplyr::left_join(gridarea2, by = c("lat", "lon")) %>%
    dplyr::mutate(
      OUTleagrid = if_else(
        is.na(area) | area <= 0,
        NA_real_,
        OUTleagrid * 1000000 / area
      ),
      x = 0.5 * lon - 180.25,
      y = -0.5 * lat + 90.25
    ) %>%
    dplyr::select(OUTleagrid, x, y)

  OUTleadismap
}

# ------------------------------------------------------------
# Reproject regular 0.5-degree grid to Mollweide
# ------------------------------------------------------------

project_grid_to_mollweide <- function(
    df,
    value_col,
    target_resolution = 50000
) {

  required_cols <- c("x", "y", value_col)

  missing_cols <- setdiff(
    required_cols,
    names(df)
  )

  if (length(missing_cols) > 0) {
    stop(
      "必要な列がありません: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  df_crop <- df %>%
    dplyr::select(
      x,
      y,
      value = dplyr::all_of(value_col)
    ) %>%
    dplyr::filter(
      is.finite(x),
      is.finite(y),
      y >= map_lat_min,
      y <= map_lat_max,
      y > -89.999,
      y < 89.999
    )

  if (nrow(df_crop) == 0) {
    stop(
      "No data found for reprojection. Please check the x, y coordinates and latitude bounds."
    )
  }

  raster_lonlat <- raster::rasterFromXYZ(
    as.data.frame(df_crop),
    crs = "+proj=longlat +datum=WGS84 +no_defs"
  )

  names(raster_lonlat) <- value_col

  raster_lonlat <- raster::crop(
    raster_lonlat,
    raster::extent(
      -180,
      180,
      map_lat_min,
      map_lat_max
    )
  )

  raster_moll <- suppressWarnings(
    raster::projectRaster(
      raster_lonlat,
      crs = moll_proj4,
      res = target_resolution,
      method = "ngb"
    )
  )

  if (is.null(raster_moll)) {
    stop(
      "Failed to project raster to Mollweide. Check the input data and projection parameters."
    )
  }

df_moll <- as.data.frame(
  raster_moll,
  xy = TRUE,
  na.rm = TRUE
)

names(df_moll)[3] <- value_col

df_moll <- df_moll[
  is.finite(df_moll[[value_col]]),
  ,
  drop = FALSE
]

df_moll
}

plot_input_map <- function(
    df,
    legend_title = expression(
      atop(
        "Total N input",
        group("(", kg~N~ha^{-1}~yr^{-1}, ")")
      )
    )
) {
  breaks_vals <- seq(0, 250, by = 25)
  labels_vals <- c(
    "0", "25", "50", "75", "100", "125",
    "150", "175", "200", "225", "250+"
  )

custom_colors <- viridisLite::cividis(
  n = length(breaks_vals) - 1,
  begin = 0.08,
  end = 0.92,
  direction = -1
)
  ggplot() +
geom_sf(
  data = mollweide_ellipse,
  fill = "white",
  color = NA,
  inherit.aes = FALSE
) +
    geom_raster(
      data = df,
      aes(x = x, y = y, fill = Input)
    ) +
    geom_sf(
      data = graticule_moll,
      color = "grey70",
      linewidth = 0.25,
      inherit.aes = FALSE
    ) +
    geom_sf(
      data = world_moll,
      fill = NA,
      color = "grey50",
      linewidth = 0.15,
      inherit.aes = FALSE
    ) +
    geom_text(
      data = lon_label_df,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      size = 4
    ) +
    scale_fill_stepsn(
      colors = custom_colors,
      breaks = breaks_vals,
      limits = c(0, 250),
      labels = labels_vals,
      oob = scales::squish,
      na.value = "transparent",
      name = legend_title,
      guide = guide_colorsteps(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = grid::unit(12, "cm"),
        barheight = grid::unit(0.25, "cm"),
        even.steps = TRUE,
        show.limits = TRUE
      )
    ) +
    coord_sf(
      crs = crs_moll,
      datum = NA,
      xlim = plot_window_moll$xlim,
      ylim = plot_window_moll$ylim,
      expand = FALSE,
      clip = "on"
    )+
    labs(
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 15, hjust = 0.5),
      legend.text = element_text(size = 12),

      axis.title = element_text(size = 13),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 10),
      axis.ticks.x = element_blank(),

      panel.grid = element_blank(),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.5
      ),
      panel.background = element_rect(
      fill = "grey92",
      color = NA
    )
    )
}

plot_gas_map <- function(
    df,
    legend_title = expression(
      atop(
        "Gaseous N losses",
        group("(", kg~N~ha^{-1}~yr^{-1}, ")")
      )
    )
) {
  custom_colors <- c("#f7fbff", "#fdae61", "#d7191c")

  ggplot() +
    geom_sf(
    data = mollweide_ellipse,
    fill = "white",
    color = NA,
    inherit.aes = FALSE
  ) +
    geom_raster(
      data = df,
      aes(x = x, y = y, fill = OUTgasgrid)
    ) +
    geom_sf(
      data = graticule_moll,
      color = "grey70",
      linewidth = 0.25,
      inherit.aes = FALSE
    ) +
    geom_sf(
      data = world_moll,
      fill = NA,
      color = "grey50",
      linewidth = 0.15,
      inherit.aes = FALSE
    ) +
    geom_text(
      data = lon_label_df,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      size = 4
    ) +
    scale_fill_gradientn(
      colors = custom_colors,
      values = scales::rescale(c(0, 5, 20), from = c(0, 20)),
      limits = c(0, 20),
      breaks = c(0, 5, 10, 15, 20),
      labels = c("0", "5", "10", "15", "20+"),
      oob = scales::squish,
      na.value = "transparent",
      name = legend_title,
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = grid::unit(12, "cm"),
        barheight = grid::unit(0.25, "cm")
      )
    ) +
    coord_sf(
      crs = crs_moll,
      datum = NA,
      xlim = plot_window_moll$xlim,
      ylim = plot_window_moll$ylim,
      expand = FALSE,
      clip = "on"
    ) +
    labs(
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 15, hjust = 0.5),
      legend.text = element_text(size = 12),

      axis.title = element_text(size = 13),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 10),
      axis.ticks.x = element_blank(),

      panel.grid = element_blank(),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.5
      ),
      panel.background = element_rect(
        fill = "grey92",
        color = NA
      )
    )
}

plot_leaching_map <- function(
    df,
    legend_title = expression(
      atop(
        "Leaching N losses",
        group("(", kg~N~ha^{-1}~yr^{-1}, ")")
      )
    )
) {
  custom_colors <- c("#f7fbff", "#fdae61", "#d7191c")

  ggplot() +
    geom_sf(
      data = mollweide_ellipse,
      fill = "white",
      color = NA,
      inherit.aes = FALSE
    ) +
    geom_raster(
      data = df,
      aes(x = x, y = y, fill = OUTleagrid)
    ) +
    geom_sf(
      data = graticule_moll,
      color = "grey70",
      linewidth = 0.25,
      inherit.aes = FALSE
    ) +
    geom_sf(
      data = world_moll,
      fill = NA,
      color = "grey50",
      linewidth = 0.15,
      inherit.aes = FALSE
    ) +
    geom_text(
      data = lon_label_df,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      size = 4
    ) +
    scale_fill_gradientn(
      colors = custom_colors,
      values = scales::rescale(c(0, 5, 20), from = c(0, 20)),
      limits = c(0, 20),
      breaks = c(0, 5, 10, 15, 20),
      labels = c("0", "5", "10", "15", "20+"),
      oob = scales::squish,
      na.value = "transparent",
      name = legend_title,
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = grid::unit(12, "cm"),
        barheight = grid::unit(0.25, "cm")
      )
    ) +
    coord_sf(
      crs = crs_moll,
      datum = NA,
      xlim = plot_window_moll$xlim,
      ylim = plot_window_moll$ylim,
      expand = FALSE,
      clip = "on"
    ) +
    labs(
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 15, hjust = 0.5),
      legend.text = element_text(size = 12),
      axis.title = element_text(size = 13),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 10),
      axis.ticks.x = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.5
      ),
      panel.background = element_rect(
        fill = "grey92",
        color = NA
      )
    )
}

# ============================================================
# Combine three maps
# ============================================================
panel_label_xy <- sf::st_coordinates(
  sf::st_transform(
    sf::st_sfc(
      sf::st_point(c(0, map_lat_min + 18)),
      crs = crs_lonlat
    ),
    crs = crs_moll
  )
)[1, ]

add_panel_label <- function(p, label) {
  p +
    theme(
      legend.position = "none"
    ) +
    annotate(
      "text",
      x = panel_label_xy[1],
      y = panel_label_xy[2],
      label = label,
      size = 4.5,
      fontface = "bold"
    ) +
    labs(
      x = "Longitude",
      y = "Latitude",
      title = NULL
    )
}

add_black_border <- function(p) {
  p +
    theme(
      plot.margin = margin(1, 1, 1, 1, "mm")
    )
}

combine_three_maps <- function(p_2010,
                               p_base2100,
                               p_500c2100,
                               legend_from,
                               legend_title_size = 15,
                               legend_text_size = 12,
                               rel_legend_height = 0.11) {
  p_2010_clean <- add_black_border(
    add_panel_label(p_2010, "2010")
  )

  p_base_clean <- add_black_border(
    add_panel_label(p_base2100, "Baseline 2100")
  )

  p_500c_clean <- add_black_border(
    add_panel_label(p_500c2100, "500C 2100")
  )

  legend_shared <- cowplot::get_legend(
    legend_from +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = legend_title_size),
        legend.text = element_text(size = legend_text_size)
      )
  )

  top_row <- p_2010_clean

  bottom_row <- cowplot::plot_grid(
    p_base_clean,
    p_500c_clean,
    ncol = 2,
    rel_widths = c(1, 1)
  )

  map_grid <- cowplot::plot_grid(
    top_row,
    bottom_row,
    ncol = 1,
    rel_heights = c(1, 1),
    align = "v"
  )

  cowplot::plot_grid(
    map_grid,
    legend_shared,
    ncol = 1,
    rel_heights = c(1, rel_legend_height)
  )
}

# ============================================================
# Input maps
# ============================================================

Inputdismap_2010 <- make_input_map_df("BaU", 2010)
Inputdismap_2100Base <- make_input_map_df("BaU", 2100)
Inputdismap_2100500C <- make_input_map_df("500C", 2100)

Inputdismap_2010_moll <- project_grid_to_mollweide(
  df = Inputdismap_2010,
  value_col = "Input"
)

Inputdismap_2100Base_moll <- project_grid_to_mollweide(
  df = Inputdismap_2100Base,
  value_col = "Input"
)

Inputdismap_2100500C_moll <- project_grid_to_mollweide(
  df = Inputdismap_2100500C,
  value_col = "Input"
)

plot_input_2010 <- plot_input_map(
  Inputdismap_2010_moll
)

plot_input_base2100 <- plot_input_map(
  Inputdismap_2100Base_moll
)

plot_input_500c2100 <- plot_input_map(
  Inputdismap_2100500C_moll
)

ggsave(
  filename = "../output/Input_2010.png",
  plot = plot_input_2010,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "../output/Input_Base_2100_ver2.png",
  plot = plot_input_base2100,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "../output/Input_500C_2100.png",
  plot = plot_input_500c2100,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

Input_combined <- combine_three_maps(
  p_2010 = plot_input_2010,
  p_base2100 = plot_input_base2100,
  p_500c2100 = plot_input_500c2100,
  legend_from = plot_input_2010,
  legend_title_size = 15,
  legend_text_size = 12,
  rel_legend_height = 0.131
)

ggsave(
  filename = "../output/Input_combined_rev.png",
  plot = Input_combined,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

# ============================================================
# Gas maps
# ============================================================

OUTgasdismap_2010 <- make_outgas_map_df("BaU", 2010)
OUTgasdismap_2100base <- make_outgas_map_df("BaU", 2100)
OUTgasdismap_2100500C <- make_outgas_map_df("500C", 2100)

OUTgasdismap_2010_moll <- project_grid_to_mollweide(
  df = OUTgasdismap_2010,
  value_col = "OUTgasgrid"
)

OUTgasdismap_2100base_moll <- project_grid_to_mollweide(
  df = OUTgasdismap_2100base,
  value_col = "OUTgasgrid"
)

OUTgasdismap_2100500C_moll <- project_grid_to_mollweide(
  df = OUTgasdismap_2100500C,
  value_col = "OUTgasgrid"
)

plot_gas_2010 <- plot_gas_map(
  OUTgasdismap_2010_moll
)

plot_gas_base2100 <- plot_gas_map(
  OUTgasdismap_2100base_moll
)

plot_gas_500c2100 <- plot_gas_map(
  OUTgasdismap_2100500C_moll
)

ggsave(
  filename = "../output/outgas_2010.png",
  plot = plot_gas_2010,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "../output/outgas_Base_2100_ver2.png",
  plot = plot_gas_base2100,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "../output/outgas_500C_2100.png",
  plot = plot_gas_500c2100,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

Outgas_combined <- combine_three_maps(
  p_2010 = plot_gas_2010,
  p_base2100 = plot_gas_base2100,
  p_500c2100 = plot_gas_500c2100,
  legend_from = plot_gas_2010,
  legend_title_size = 15,
  legend_text_size = 12,
  rel_legend_height = 0.131
)

ggsave(
  filename = "../output/Outgas_combined.png",
  plot = Outgas_combined,
  width = 12,
  height = 7.5,
  dpi = 300,
  bg = "white"
)

# ============================================================
# Leaching maps
# ============================================================

OUTleadismap_2010 <- make_outlea_map_df("BaU", 2010)
OUTleadismap_2100base <- make_outlea_map_df("BaU", 2100)
OUTleadismap_2100500C <- make_outlea_map_df("500C", 2100)

OUTleadismap_2010_moll <- project_grid_to_mollweide(
  df = OUTleadismap_2010,
  value_col = "OUTleagrid"
)

OUTleadismap_2100base_moll <- project_grid_to_mollweide(
  df = OUTleadismap_2100base,
  value_col = "OUTleagrid"
)

OUTleadismap_2100500C_moll <- project_grid_to_mollweide(
  df = OUTleadismap_2100500C,
  value_col = "OUTleagrid"
)

plot_leaching_2010 <- plot_leaching_map(
  OUTleadismap_2010_moll
)

plot_leaching_base2100 <- plot_leaching_map(
  OUTleadismap_2100base_moll
)

plot_leaching_500c2100 <- plot_leaching_map(
  OUTleadismap_2100500C_moll
)

ggsave(
  filename = "../output/outlea_2010.png",
  plot = plot_leaching_2010,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "../output/outlea_Base_2100.png",
  plot = plot_leaching_base2100,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "../output/outlea_500C_2100.png",
  plot = plot_leaching_500c2100,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

Outlea_combined <- combine_three_maps(
  p_2010 = plot_leaching_2010,
  p_base2100 = plot_leaching_base2100,
  p_500c2100 = plot_leaching_500c2100,
  legend_from = plot_leaching_2010,
  legend_title_size = 15,
  legend_text_size = 12,
  rel_legend_height = 0.131
)

ggsave(
  filename = "../output/Outlea_combined.png",
  plot = Outlea_combined,
  width = 12,
  height = 7.5,
  dpi = 300,
  bg = "white"
)
