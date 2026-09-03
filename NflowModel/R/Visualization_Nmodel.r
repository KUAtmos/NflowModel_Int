library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(patchwork)
library(cowplot)
library(scales)
library(grid)
world <- ne_countries(scale = "medium", returnclass = "sf")
data_prep <- "../data/data_prep.gdx"
MAP_RG <- rgdx.set(data_prep, 'MAP_RG')
Global_17 <- "../data/global_17_IAMC.gdx"
IAMC_Template <- rgdx.param(Global_17, 'IAMC_Template')


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
    EDGAR_df <- read_csv("../../VISIToRN/Data_pd/EDGAR_NH3_1970_2022.csv") #Gg
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
        # Use the result calculated immediately before this plotting step.
        # Result_ver6.csv is retained as an archive but must not be used for
        # this manure-only rerun.
        if (exists("Result", envir = globalenv())) {
          df_raw <- get("Result", envir = globalenv())
        } else {
          df_raw <- read_csv("../data/Result.csv")
        }

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

    stop("作図用オブジェクトが見つかりません: ", obj_name)
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

        stop("Input_R用オブジェクトが見つかりません: ", obj_name)
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
        stop("MAP_RG が見つかりません。copy_plot_objects()で保存されているか確認してください。")
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
            "列が見つかりません: ",
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
world <- ne_countries(scale = "medium", returnclass = "sf")

get_case_object <- function(prefix, scenario_tag, year) {
  obj_name <- paste0(prefix, "_", scenario_tag, "_", year)

  if (!exists(obj_name, envir = globalenv())) {
    stop("作図用オブジェクトが見つかりません: ", obj_name)
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
    "VY_load2_BaU_2010",
    "VY_load2_BaU_2100",
    "VY_load2_500C_2100"
  )

  missing <- required[
    !vapply(required, exists, logical(1), envir = globalenv())
  ]

  if (length(missing) > 0) {
    stop(
      "地図作成に必要なオブジェクトが不足しています:\n",
      paste(missing, collapse = "\n")
    )
  }
}

check_map_objects()

make_input_map_df <- function(scenario_tag, year) {
  year <- as.integer(year)

  Inputdis_case <- get_case_object("Inputdis", scenario_tag, year)
  VY_load2_case <- get_case_object("VY_load2", scenario_tag, year)

  landuse_classes <- c(
    "PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR",
    "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF",
    "OTH_A", "PAS"
  )

  Totaldistribution <- VY_load2_case %>%
    select(i2, i3, lat, lon, LUAREA) %>%
    filter(i2 == year) %>%
    filter(i3 %in% landuse_classes) %>%
    group_by(i2, lat, lon) %>%
    summarise(
      LUAREA = sum(LUAREA, na.rm = TRUE),
      .groups = "drop"
    )

  Inputdismap <- Inputdis_case %>%
    left_join(MAP_GIJ, by = "G") %>%
    filter(YEMF == year) %>%
    select(G, YEMF, Input, lat, lon) %>%
    left_join(Totaldistribution, by = c("lat", "lon")) %>%
    mutate(
      Input = if_else(
        is.na(LUAREA) | LUAREA <= 0,
        NA_real_,
        Input * 100000000 / LUAREA
      ),
      x = 0.5 * lon - 180.25,
      y = -0.5 * lat + 90.25
    ) %>%
    select(Input, x, y)

  Inputdismap
}

make_outgas_map_df <- function(scenario_tag, year) {
  year <- as.integer(year)

  OUTgasdis_case <- get_case_object("OUTgasdis", scenario_tag, year)

  OUTgasdismap <- OUTgasdis_case %>%
    left_join(MAP_GIJ, by = "G") %>%
    filter(YEMF == year) %>%
    select(G, YEMF, OUTgasgrid, lat, lon) %>%
    left_join(gridarea2, by = c("lat", "lon")) %>%
    mutate(
      OUTgasgrid = if_else(
        is.na(area) | area <= 0,
        NA_real_,
        OUTgasgrid * 1000000 / area
      ),
      x = 0.5 * lon - 180.25,
      y = -0.5 * lat + 90.25
    ) %>%
    select(OUTgasgrid, x, y)

  OUTgasdismap
}

plot_input_map <- function(df, legend_title = "Total N input\n(g N m⁻² yr⁻¹)") {
  breaks_vals <- seq(0, 25, by = 2.5)
  labels_vals <- c(
    "0", "2.5", "5", "7.5", "10", "12.5",
    "15", "17.5", "20", "22.5", "25+"
  )

  custom_colors <- c(
    "#ffffff",
    "#deebf7",
    "#c7e9c0",
    "#74c476",
    "#ffffb2",
    "#fecc5c",
    "#fd8d3c",
    "#f16913",
    "#d94801",
    "#a63603"
  )

  ggplot() +
    geom_tile(data = df, aes(x = x, y = y, fill = Input)) +
    scale_fill_stepsn(
      colors = custom_colors,
      breaks = breaks_vals,
      limits = c(0, 25),
      labels = labels_vals,
      na.value = "white",
      name = legend_title,
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
      panel.grid = element_blank()
    )
}

plot_gas_map <- function(df, legend_title = "Gaseous Losses\n(kg ha⁻¹ yr⁻¹)") {
  custom_colors <- c("#f7fbff", "#fdae61", "#d7191c")

  ggplot() +
    geom_tile(data = df, aes(x = x, y = y, fill = OUTgasgrid)) +
    scale_fill_gradientn(
      colors = custom_colors,
      values = c(0, 0.025, 0.05, 0.1, 0.15, 0.25, 0.5, 0.75, 1),
      limits = c(0, 20),
      breaks = c(0, 5, 10, 15, 20),
      labels = c("0", "5", "10", "15", "20+"),
      oob = scales::squish,
      name = legend_title,
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(12, "cm"),
        barheight = unit(0.3, "cm")
      )
    ) +
    geom_sf(data = world, fill = NA, color = "black", size = 0.1) +
    coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 12),
      panel.grid = element_blank()
    )
}

# ============================================================
# Combine three maps
# ============================================================

add_panel_label <- function(p, label) {
  p +
    theme(legend.position = "none") +
    annotate(
      "text",
      x = 0,
      y = -55,
      label = label,
      size = 5,
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
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.5
      ),
      plot.margin = margin(1, 1, 1, 1, "mm")
    )
}

combine_three_maps <- function(p_2010,
                               p_base2100,
                               p_500c2100,
                               legend_from,
                               legend_title_size = 10,
                               legend_text_size = 8,
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

plot_input_2010 <- plot_input_map(Inputdismap_2010)
plot_input_base2100 <- plot_input_map(Inputdismap_2100Base)
plot_input_500c2100 <- plot_input_map(Inputdismap_2100500C)

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
  legend_title_size = 10,
  legend_text_size = 8,
  rel_legend_height = 0.11
)

ggsave(
  filename = "../output/Input_combined_rev.png",
  plot = Input_combined,
  width = 12,
  height = 7.5,
  dpi = 300,
  bg = "white"
)

# ============================================================
# Gas maps
# ============================================================

OUTgasdismap_2010 <- make_outgas_map_df("BaU", 2010)
OUTgasdismap_2100base <- make_outgas_map_df("BaU", 2100)
OUTgasdismap_2100500C <- make_outgas_map_df("500C", 2100)

plot_gas_2010 <- plot_gas_map(OUTgasdismap_2010)
plot_gas_base2100 <- plot_gas_map(OUTgasdismap_2100base)
plot_gas_500c2100 <- plot_gas_map(OUTgasdismap_2100500C)

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
