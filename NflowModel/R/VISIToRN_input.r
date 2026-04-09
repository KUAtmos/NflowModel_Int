#Process visit output
library(dplyr)
library(tidyr)
library(ncdf4)
library(gdxrrw)
library(terra)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)
library(scales)
library(ggrepel)
library(readr)
library(forcats)
library(purrr)
igdx("C:/GAMS/53")
data_prep <- "data_prep.gdx"
data_prep_data <- gdxrrw::rgdx(data_prep)


#Make VISIToRN input array
        make_visit_input_df <- function(YEAR, NMIP3 = FALSE) {
          year_num <- 2010

          if (NMIP3) {
            nc_nat <- nc_open("visit_20260311_atmosclus_NMIP3_TEST_fNleach_natonly.nc")
            nc_agr <- nc_open("visit_20260311_atmosclus_NMIP3_TEST_fNleach_agronly.nc")
          } else {
            nc_nat <- nc_open("visit_20250820_atmosclus_geos_TEST_fNleach_natonly.nc")
            nc_agr <- nc_open("visit_20250820_atmosclus_geos_TEST_fNleach_agronly.nc")
          }

          on.exit({
            nc_close(nc_nat)
            nc_close(nc_agr)
          })

          # VISIT natural
          time_vals <- ncvar_get(nc_nat, "time")
          origin_year <- 1850
          years <- origin_year + (time_vals / 12)

          target_idx <- which(years >= year_num & years < (year_num + 1))

          if (length(target_idx) == 0) {
            stop("No time index found in nc_nat for YEAR = ", year_num)
          }

          fNleach_slice <- ncvar_get(
            nc_nat, "n_leach",
            start = c(1, 1, 1, min(target_idx)),
            count = c(720, 360, 1, length(target_idx))
          )

          lon <- ncvar_get(nc_nat, "lon")
          lat <- ncvar_get(nc_nat, "lat")

          df_list <- lapply(seq_along(target_idx), function(ti) {
            year_val <- years[target_idx[ti]]
            df <- as.data.frame.table(fNleach_slice[, , ti], responseName = "Nleach")
            colnames(df) <- c("lon_idx", "lat_idx", "Nleach")

            df %>%
              mutate(
                lon = lon[as.integer(lon_idx)],
                lat = lat[as.integer(lat_idx)],
                year = year_val
              ) %>%
              select(year, lon, lat, Nleach)
          })

          fNleach_df <- bind_rows(df_list)

          # VISIT agricultural
          time_vals2 <- ncvar_get(nc_agr, "time")
          years2 <- origin_year + (time_vals2 / 12)

          target_idx2 <- which(years2 >= year_num & years2 < (year_num + 1))

          if (length(target_idx2) == 0) {
            stop("No time index found in nc_agr for YEAR = ", year_num)
          }

          fNleach_slice2 <- ncvar_get(
            nc_agr, "n_leach",
            start = c(1, 1, 2, min(target_idx2)),
            count = c(720, 360, 1, length(target_idx2))
          )

          lon2 <- ncvar_get(nc_agr, "lon")
          lat2 <- ncvar_get(nc_agr, "lat")

          df_list2 <- lapply(seq_along(target_idx2), function(ti) {
            year_val2 <- years2[target_idx2[ti]]
            df2 <- as.data.frame.table(fNleach_slice2[, , ti], responseName = "Nleach")
            colnames(df2) <- c("lon_idx", "lat_idx", "Nleach")

            df2 %>%
              mutate(
                lon = lon2[as.integer(lon_idx)],
                lat = lat2[as.integer(lat_idx)],
                year = year_val2
              ) %>%
              select(year, lon, lat, Nleach)
          })

          fNleach_df2 <- bind_rows(df_list2)

          list(
            fNleach_df = fNleach_df,
            fNleach_df2 = fNleach_df2
          )
        }

        make_array_from_df <- function(dat, value_col = "Nleach") {
          arr <- array(0, dim = c(720, 360, 12))
          year_vals <- sort(unique(dat$year))

          dat2 <- dat %>%
            mutate(
              lon_idx = round((lon + 179.75) / 0.5) + 1,
              lat_idx = round((89.75 - lat) / 0.5) + 1,
              month_idx = match(year, year_vals)
            )

          arr[cbind(dat2$lon_idx, dat2$lat_idx, dat2$month_idx)] <- dat2[[value_col]]
          arr
        }

        make_visit_array <- function(SCENARIOset, YEAR, NMIP3 = FALSE) {

          year_chr <- as.character(YEAR)
          year_num <- as.numeric(YEAR)

          is_mit <- SCENARIOset == "SSP2_500C_CACN_NoCC"
          suffix_mit <- if (is_mit) "mit" else ""
          suffix_nmip3 <- if (NMIP3) "_NMIP3" else ""

          YEAR2 <- paste0(year_chr, suffix_mit)
          arrayname <- paste0("VISITnl_", year_chr, suffix_mit, suffix_nmip3)
          arrayname_nat <- paste0("VISITnl_nat_", year_chr, suffix_mit, suffix_nmip3)

          message("Running: ", SCENARIOset, " / ", year_chr,
                  " / NMIP3=", NMIP3, " -> ", arrayname)

          visit_input <- make_visit_input_df(YEAR = YEAR, NMIP3 = NMIP3)
          fNleach_df  <- visit_input$fNleach_df
          fNleach_df2 <- visit_input$fNleach_df2

          # ================================================================
          # Natural only and agricultural only
          # ================================================================
          AIMPLUM_BaU <- paste0("results_", SCENARIOset, ".gdx")
          AIMPLUM_BaU_data <- gdxrrw::rgdx(AIMPLUM_BaU)

          VY_load <- rgdx.param(AIMPLUM_BaU, "VY_load") %>%
            rename(G = i4)

          GA <- rgdx.param(data_prep, "GA") # kha
          MAP_GIJ <- rgdx.set(data_prep, "MAP_GIJ") %>%
            rename(lat = I, lon = J)

          VY_load2 <- VY_load %>%
            left_join(GA, by = "G") %>%
            mutate(LUAREA = value * GA * 1000) # ha

          LUAREA_Region <- VY_load2 %>%
            group_by(i1, i2, i3) %>%
            summarise(sum_LUAREA = sum(LUAREA), .groups = "drop")

          VY_load2 <- VY_load2 %>%
            left_join(LUAREA_Region, by = c("i1", "i2", "i3")) %>%
            mutate(LURATIO = LUAREA / sum_LUAREA) %>%
            left_join(MAP_GIJ, by = "G")

          VY_load2$lat <- as.numeric(as.character(VY_load2$lat))
          VY_load2$lon <- as.numeric(as.character(VY_load2$lon))
          VY_load2$i2  <- as.numeric(as.character(VY_load2$i2))

          VY_load3 <- VY_load2 %>%
            filter(i2 == year_num) %>%
            filter(i3 %in% c("AFR", "OL", "FRS", "OTH_AIR", "OTH_ARF")) %>%
            select(i3, LUAREA, lat, lon) %>%
            group_by(lat, lon) %>%
            summarise(LU_nat = sum(LUAREA), .groups = "drop") %>%
            mutate(
              lon = 0.5 * lon - 180.25,
              lat = -0.5 * lat + 90.25
            )

          VY_load4 <- VY_load2 %>%
            filter(i2 == year_num) %>%
            filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR",
                            "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF",
                            "OTH_A", "PAS")) %>%
            select(i3, LUAREA, lat, lon) %>%
            group_by(lat, lon) %>%
            summarise(LU_agr = sum(LUAREA), .groups = "drop") %>%
            mutate(
              LU_agr = ifelse(LU_agr < 0, 0, LU_agr),
              lon = 0.5 * lon - 180.25,
              lat = -0.5 * lat + 90.25
            )

          VISIT_nat <- fNleach_df %>%
            full_join(VY_load3, by = c("lon", "lat")) %>%
            mutate(
              Nleach = replace_na(Nleach, 0),
              LU_nat = replace_na(LU_nat, 0),
              Nleach_grid = Nleach * LU_nat * 10000 * 3600 * 24
            ) %>%
            select(year, lon, lat, Nleach_grid)

          VISIT_nat_only <- VISIT_nat %>%
              transmute(year, lon, lat, Nleach = Nleach_grid)

          VISIT_agr <- fNleach_df2 %>%
            full_join(VY_load4, by = c("lon", "lat")) %>%
            mutate(
              Nleach = replace_na(Nleach, 0),
              LU_agr = replace_na(LU_agr, 0),
              Nleach_grid2 = Nleach * LU_agr * 10000 * 3600 * 24
            ) %>%
            select(year, lon, lat, Nleach_grid2)

          VISIT_nat_agr <- VISIT_nat %>%
            left_join(VISIT_agr, by = c("year", "lon", "lat")) %>%
            mutate(Nleach = Nleach_grid + Nleach_grid2) %>%
            select(year, lon, lat, Nleach)

          VISITnl_nat_ag_PLUMLU <- array(0, dim = c(720, 360, 12))
          year_vals <- sort(unique(VISIT_nat_agr$year))

          VISIT_nat_agr <- VISIT_nat_agr %>%
            mutate(
              lon_idx = round((lon + 179.75) / 0.5) + 1,
              lat_idx = round((89.75 - lat) / 0.5) + 1,
              month_idx = match(year, year_vals)
            )

          VISITnl_nat_ag_PLUMLU[
            cbind(VISIT_nat_agr$lon_idx, VISIT_nat_agr$lat_idx, VISIT_nat_agr$month_idx)
          ] <- VISIT_nat_agr$Nleach

          # ================================================================
          # Make nitrogen budget model Nleach array
          # Make VISIToRN input (Nleach)
          # ================================================================
          load(file.path("Data_pd", paste0("OUTleadis", YEAR2, ".rdat")))

          OUTlea_ag <- OUTleadis %>%
            mutate(OUTleagrid = OUTleagrid * 1000000000 / 365.25) %>%
            left_join(MAP_GIJ, by = "G") %>%
            mutate(
              lon = as.numeric(as.character(lon)),
              lat = as.numeric(as.character(lat)),
              lon = 0.5 * lon - 180.25,
              lat = -0.5 * lat + 90.25
            ) %>%
            filter(YEMF == year_chr) %>%
            select(OUTleagrid, lat, lon)

          VISIT_Nbud <- VISIT_nat %>%
            left_join(OUTlea_ag, by = c("lat", "lon")) %>%
            replace_na(list(
              Nleach_grid = 0,
              OUTleagrid = 0
            )) %>%
            mutate(
              Nleach = Nleach_grid + OUTleagrid,
              Nleach = ifelse(Nleach < 0, 0, Nleach)
            ) %>%
            select(year, lon, lat, Nleach)

          tmparray <- array(0, dim = c(720, 360, 12))
          year_vals <- sort(unique(VISIT_Nbud$year))

        # ================================================================
        # Natural-only array
        # ================================================================
        VISIT_nat_only <- VISIT_nat %>%
          transmute(year, lon, lat, Nleach = Nleach_grid)

        nat_array <- array(0, dim = c(720, 360, 12))
        year_vals_nat <- sort(unique(VISIT_nat_only$year))

        VISIT_nat_only <- VISIT_nat_only %>%
          mutate(
            lon_idx = round((lon + 179.75) / 0.5) + 1,
            lat_idx = round((89.75 - lat) / 0.5) + 1,
            month_idx = match(year, year_vals_nat)
          )

        nat_array[
          cbind(VISIT_nat_only$lon_idx, VISIT_nat_only$lat_idx, VISIT_nat_only$month_idx)
        ] <- VISIT_nat_only$Nleach

        arrayname_nat <- paste0("VISITnl_nat_", year_chr, suffix_mit, suffix_nmip3)

        assign(arrayname_nat, nat_array, envir = .GlobalEnv)

        save(
          list = arrayname_nat,
          file = file.path("Data_pd", paste0(arrayname_nat, ".rdat"))
        )

        # ================================================================
        # Combined array
        # ================================================================
        VISIT_Nbud <- VISIT_Nbud %>%
          mutate(
            lon_idx = round((lon + 179.75) / 0.5) + 1,
            lat_idx = round((89.75 - lat) / 0.5) + 1,
            month_idx = match(year, year_vals)
          )

        tmparray[
          cbind(VISIT_Nbud$lon_idx, VISIT_Nbud$lat_idx, VISIT_Nbud$month_idx)
        ] <- VISIT_Nbud$Nleach

        assign(arrayname, tmparray, envir = .GlobalEnv)

        save(
          list = arrayname,
          file = file.path("Data_pd", paste0(arrayname, ".rdat"))
        )

        invisible(list(
          combined = arrayname,
          natural_only = arrayname_nat
        ))
        }

      # =========================================================
      # run
      # =========================================================
      NMIP3 <- TRUE
      
      run_plan <- data.frame(
        SCENARIOset = c(
          "SSP2_BaU_NoCC",
          "SSP2_BaU_NoCC",
          "SSP2_BaU_NoCC",
          "SSP2_BaU_NoCC",
          "SSP2_500C_CACN_NoCC",
          "SSP2_500C_CACN_NoCC",
          "SSP2_500C_CACN_NoCC"
        ),
        YEAR = c("2010", "2030", "2050", "2100", "2030", "2050", "2100"),
        stringsAsFactors = FALSE
      )

      for (i in seq_len(nrow(run_plan))) {
        make_visit_array(
          SCENARIOset = run_plan$SCENARIOset[i],
          YEAR = run_plan$YEAR[i],
          NMIP3 = NMIP3
        )
      }
      #=================================================

#River aggregate
        # =========================================================
        # Settings
        # =========================================================
        data_dir <- "Data_pd"
        demo_file <- "Demo_VISIToRN.R"
        NMIP3 <- TRUE

        # =========================================================
        # run
        # =========================================================
        source("Run_VISIToRN.r")

#Visualization
        load("Data_pd/df_river_all.rdat")
        load("Data_pd/df_river_all_mit.rdat")

        df_base <- df_river_all %>%
          mutate(scenario = "Baseline")

        df_mit_2010 <- df_river_all %>%
          filter(year == 2010) %>%
          mutate(scenario = "500C")

        df_mit_future <- df_river_all_mit %>%
          mutate(scenario = "500C")

        df_mit <- bind_rows(df_mit_2010, df_mit_future)

        df_both <- bind_rows(df_base, df_mit) %>%
          mutate(
            scenario = factor(scenario, levels = c("Baseline", "500C"))
          )

        top_rivers14 <- df_river_all %>%
          filter(year == 2010) %>%
          arrange(desc(Nload)) %>%
          slice_head(n = 14) %>%
          pull(river_basin_name)

        df_top14 <- df_both %>%
          filter(river_basin_name %in% top_rivers14)

        df_total_all <- df_both %>%
          group_by(scenario, year) %>%
          summarise(
            Nload = sum(Nload, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(river_basin_name = "All basins")

        df_plot <- bind_rows(df_top14, df_total_all)

        facet_levels <- c(sort(unique(top_rivers14)), "All basins")

        river_labels <- setNames(
          sub("_\\(.*$", "", facet_levels), 
          facet_levels   
        )

        df_plot <- df_plot %>%
          mutate(
            river_basin_name = factor(river_basin_name, levels = facet_levels),
            year_f = factor(year, levels = c(2010, 2030, 2050, 2100))
          )


        ggplot(df_plot,
              aes(x = year_f, y = Nload,
                  color = scenario, group = scenario)) +
          geom_line(linewidth = 0.9) +
          geom_point(size = 1.8) +
          scale_x_discrete(drop = FALSE) +
          scale_y_continuous(
            limits = c(0, NA) 
          ) +
          scale_color_manual(values = c(
            "Baseline"   = "red",
            "500C" = "cyan3"
          )) +
          labs(
            x = "Year",
            y = "N load [Gg N yr⁻¹]",
            color = "SCENARIO",
            title = "Temporal change of river N load (Top 14 basins and global total)"
          ) +
          facet_wrap(
            ~ river_basin_name,
            ncol = 5,  
            scales = "free_y",
            labeller = labeller(river_basin_name = river_labels)  
          ) +
          theme_bw(base_size = 12) +
          theme(
          
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
          
            panel.border     = element_blank(),
            axis.line        = element_line(colour = "black"),
            axis.text.x      = element_text(angle = 45, hjust = 1),
            legend.position  = "bottom",
            strip.background = element_rect(fill = "grey90", colour = NA),
            strip.text       = element_text(face = "bold")
          )

#Compare with observation (He et al.)
obs_raw <- read_csv("River_Nitrogen_Load_Data.csv", show_col_types = FALSE, guess_max = 100000)


names(obs_raw) <- str_replace_all(names(obs_raw), "\\s+", "_")

name_col <- names(obs_raw)[str_detect(names(obs_raw), regex("^name$", ignore_case = TRUE))]
if (length(name_col) == 0) name_col <- names(obs_raw)[1]


din_col <- names(obs_raw)[str_detect(names(obs_raw), regex("DIN", ignore_case = TRUE))]
if (length(din_col) == 0) {
  stop("観測CSVに DIN を含む列が見つかりません。列名をご確認ください。")
}

obs <- obs_raw %>%
  transmute(
    obs_name = trimws(.data[[name_col]]),
    DIN_load = suppressWarnings(as.numeric(.data[[din_col[1]]]))
  )

lines <- readLines("River_obs_analysis.txt", encoding = "UTF-8")
lines <- lines[nzchar(trimws(lines))]                       
lines <- lines[!str_starts(trimws(lines), "#")]          

split2 <- strsplit(lines, ",")
map <- tibble(
  csv_name = trimws(vapply(split2, `[`, character(1), 1)),
  df_name  = trimws(vapply(split2, `[`, character(1), 2))
)

map <- map %>% filter(csv_name != "", df_name != "")

model <- df_river_named_mouth_2010 %>%
  transmute(
    df_name = trimws(river_basin_name),
    Nload   = as.numeric(Nload/1000000)
  )

dat_raw <- obs %>%
  rename(csv_name = obs_name) %>%        
  inner_join(map, by = "csv_name") %>%
  inner_join(model, by = "df_name") %>%
  transmute(
    csv_name,
    df_name,
    DIN_load_kt = as.numeric(DIN_load),     
    Nload_kt    = as.numeric(Nload)      
  ) %>%
  filter(!is.na(DIN_load_kt), !is.na(Nload_kt))


dat <- dat_raw %>%
  group_by(csv_name) %>%
  summarise(
    DIN_load_kt = first(DIN_load_kt),             
    Nload_kt    = sum(Nload_kt, na.rm = TRUE),       
    n_sources   = n(),
    merged_from = paste(unique(df_name), collapse = " + "),
    .groups = "drop"
  )

dat_pos <- dat %>% filter(DIN_load_kt > 0, Nload_kt > 0)

label_top <- dat_pos %>%
  mutate(ratio = Nload_kt / DIN_load_kt,
         dev = abs(log10(ratio))) %>%
  slice_max(dev, n = 20)

lbl_num <- if (exists("cut_short_scale", where = asNamespace("scales"))) {
  scales::label_number(scale_cut = scales::cut_short_scale())
} else {
  scales::label_number()
}

all_vals <- c(dat_pos$DIN_load_kt, dat_pos$Nload_kt)
all_vals <- all_vals[is.finite(all_vals) & all_vals > 0]
lo <- 10^floor(log10(min(all_vals)))
hi <- 10^ceiling(log10(max(all_vals)))

fit <- lm(log10(Nload_kt) ~ log10(DIN_load_kt), data = dat_pos)
a  <- unname(coef(fit)[1]); b <- unname(coef(fit)[2])
r2 <- summary(fit)$r.squared

xnew <- data.frame(DIN_load_kt = 10^seq(log10(lo), log10(hi), length.out = 200))
pr   <- predict(fit, newdata = xnew, se.fit = TRUE)
df_line <- transform(
  xnew,
  y_fit = 10^pr$fit,
  y_lo  = 10^(pr$fit - 1.96*pr$se.fit),
  y_hi  = 10^(pr$fit + 1.96*pr$se.fit)
)

p <- ggplot(dat_pos, aes(x = DIN_load_kt, y = Nload_kt)) +
  geom_point(alpha = 0.85, size = 2) +
  geom_function(fun = function(x) x) +
  geom_function(fun = function(x) 2 * x, linetype = "dashed") +
  geom_function(fun = function(x) 0.5 * x, linetype = "dashed") +
  ggrepel::geom_text_repel(
    data = label_top, aes(label = csv_name),
    size = 3, max.overlaps = 100
  ) +
  geom_ribbon(data = df_line,
              aes(x = DIN_load_kt, ymin = y_lo, ymax = y_hi),
              alpha = 0.12, inherit.aes = FALSE) +
  geom_line(data = df_line,
            aes(x = DIN_load_kt, y = y_fit),
            linewidth = 1, inherit.aes = FALSE) +
scale_x_log10(limits = c(lo, hi), labels = lbl_num, name = "Observed N load (kt N yr-1)",
              expand = ggplot2::expansion(mult = 0.02)) +
scale_y_log10(limits = c(lo, hi), labels = lbl_num, name = "Simulated N load (kt N yr-1)",
              expand = ggplot2::expansion(mult = 0.02)) +
coord_equal() +   
  theme_bw(base_size = 12) +
  ggtitle("河川ごとの窒素負荷の本研究における推計値および観測値（He et al., 2011）（kt-N）",
          subtitle = "実線: 1:1, 破線: ×2 / ×0.5")

print(p)

ggsave("260120Obs_compare.png", p, width = 6, height = 6, dpi = 300)

    #index
calc_metrics <- function(df, obs_col = "DIN_load_kt", mod_col = "Nload_kt") {

  dat <- df %>%
    transmute(
      O = as.numeric(.data[[obs_col]]),
      M = as.numeric(.data[[mod_col]])
    ) %>%
    filter(is.finite(O), is.finite(M), O > 0, M > 0)

  e  <- dat$M - dat$O
  pe <- 100 * e / dat$O  
  ape <- abs(pe)   

  fit_lin <- lm(M ~ O, data = dat)
  R2      <- summary(fit_lin)$r.squared
  RMSE    <- sqrt(mean(e^2))
  MAE     <- mean(abs(e))
  NRMSE_meanO     <- RMSE / mean(dat$O)
  NRMSE_meanO_pct <- 100 * NRMSE_meanO
  MAPE  <- mean(ape)                    
  MdAPE <- median(ape)       
  MPE   <- mean(pe)               
  PBIAS <- 100 * sum(e) / sum(dat$O)  
  fit_log <- lm(log10(M) ~ log10(O), data = dat)
  R2_log10   <- summary(fit_log)$r.squared
  RMSE_log10 <- sqrt(mean((log10(dat$M) - log10(dat$O))^2))
  typical_factor_from_RMSElog10 <- 10^RMSE_log10
  tibble::tibble(
    n = nrow(dat),
    R2 = R2,
    RMSE = RMSE,
    MAE = MAE,
    NRMSE_meanO = NRMSE_meanO,
    NRMSE_meanO_pct = NRMSE_meanO_pct,
    MAPE = MAPE,
    MdAPE = MdAPE,
    MPE = MPE,
    PBIAS = PBIAS,
    R2_log10 = R2_log10,
    RMSE_log10 = RMSE_log10,
    typical_factor_from_RMSElog10 = typical_factor_from_RMSElog10
  )
}

metrics <- calc_metrics(dat_pos)
metrics

#Sewage
    load("Data_pd/HumanN.rdat")
    Global <- "Data_pd/SSP_update.gdx" #input data
    Global_data <- gdxrrw::rgdx(Global)
    GDP <- rgdx.param(Global, 'GDP')
    POP <- rgdx.param(Global, 'POP')

    regionSSP.map <- read.delim("Data_pd/regionSSP.map", 
                                header = FALSE, 
                                stringsAsFactors = FALSE)
    regionSSP.map <- regionSSP.map[, c(1,3)]


    colnames(regionSSP.map) <- c("country", "region")

    IAMC_Template_GDP <- GDP %>%
      left_join(regionSSP.map, by = c("i1" = "country")) %>%
      select(-i1) %>%
      select(-i3) %>%
      rename(SCENARIO = i2) %>%
      rename(YEMF = i4) %>%
      rename(REMF = region) %>%
      rename(IAMC_Template = value)

    IAMC_Template_GDP <- IAMC_Template_GDP %>%
      filter(YEMF %in% c(2005, seq(2010, 2100, by = 10))) %>%
      filter(SCENARIO == "SSP2")

    IAMC_Template_POP <- POP %>%
      left_join(regionSSP.map, by = c("i1" = "country")) %>%
      select(-i1) %>%
      select(-i3) %>%
      rename(SCENARIO = i2) %>%
      rename(YEMF = i4) %>%
      rename(REMF = region) %>%
      rename(IAMC_Template = value)

    IAMC_Template_POP <- IAMC_Template_POP %>%
      filter(YEMF %in% c(2005, seq(2010, 2100, by = 10))) %>%
      filter(SCENARIO == "SSP2")

    MAP_RIJ <- rgdx.set(data_prep, 'MAP_RIJ')
    MAP_RIJ$I <- as.numeric(as.character(MAP_RIJ$I))
    MAP_RIJ$J <- as.numeric(as.character(MAP_RIJ$J))
    MAP_RIJ <- MAP_RIJ %>%
    rename(REMF = R)

    POPratio <- IAMC_Template_POP %>%
      left_join(MAP_RIJ, by = "REMF")

    base2010 <- POPratio %>%
      filter(YEMF == 2010) %>%
      group_by(SCENARIO, REMF, I, J) %>%
      summarise(pop2010 = sum(IAMC_Template, na.rm = TRUE), .groups = "drop")

    POPratio_rel2010 <- POPratio %>%
      group_by(SCENARIO, REMF, I, J, YEMF) %>%
      summarise(pop = sum(IAMC_Template, na.rm = TRUE), .groups = "drop") %>%
      left_join(base2010, by = c("SCENARIO","REMF","I","J")) %>%
      mutate(pop_ratio_2010 = if_else(is.na(pop2010) | pop2010 <= 0, NA_real_, pop / pop2010)) %>%
      arrange(SCENARIO, REMF, I, J, YEMF)


    SCNR <- IAMC_Template_GDP %>%
      mutate(
        SC = 22.969 * log(IAMC_Template * 1000) - 162.12,
        NR = 0.0013 * IAMC_Template * 1000 - 2.9578
      ) %>%
      mutate(
        SC = pmin(SC, 100),
        NR = pmin(NR, 90)
      )

    SCratio <- SCNR %>%
      left_join(MAP_RIJ, by = "REMF")

    SC2010 <- SCratio %>%
      filter(YEMF == 2010) %>%
      group_by(SCENARIO, REMF, I, J) %>%
      summarise(sc2010 = sum(SC, na.rm = TRUE), .groups = "drop")

    SCratio_rel2010 <- SCratio %>%
      group_by(SCENARIO, REMF, I, J, YEMF) %>%
      summarise(sc = sum(SC, na.rm = TRUE), .groups = "drop") %>%
      left_join(SC2010, by = c("SCENARIO","REMF","I","J")) %>%
      mutate(sc_ratio_2010 = if_else(is.na(sc2010) | sc2010 <= 0, NA_real_, sc / sc2010)) %>%
      arrange(SCENARIO, REMF, I, J, YEMF)

    NR2010 <- SCratio %>%
      filter(YEMF == 2010) %>%
      group_by(SCENARIO, REMF, I, J) %>%
      summarise(nr2010 = sum(NR, na.rm = TRUE), .groups = "drop")

    NRratio_rel2010 <- SCratio %>%
      group_by(SCENARIO, REMF, I, J, YEMF) %>%
      summarise(nr = sum(NR, na.rm = TRUE), .groups = "drop") %>%
      left_join(NR2010, by = c("SCENARIO","REMF","I","J")) %>%
      mutate(nr_ratio_2010 = if_else(is.na(nr2010) | nr2010 <= 0, NA_real_, nr / nr2010)) %>%
      arrange(SCENARIO, REMF, I, J, YEMF)

    base2010 <- HumanN[,,50]
    stopifnot(nrow(base2010) == 720, ncol(base2010) == 360)

    target_years <- c(2010, 2030, 2050, 2100)

    make_future_map <- function(scn, year,
                                base_mat = HumanN[,,50],
                                POPratio_rel2010,
                                SCratio_rel2010,
                                NRratio_rel2010) {
      if (year == 2010) {
        return(base_mat)
      }

      eps <- 1e-6

      # --- POP: POPfuture / POP2010 ---
      popY <- POPratio_rel2010 %>%
        dplyr::filter(SCENARIO == scn, YEMF == year) %>%
        dplyr::select(I, J, pop, pop2010) %>%
        dplyr::mutate(
          pop      = pmax(pop, 0),
          pop2010  = pmax(pop2010, 0),
          pop_ratio = ifelse(pop2010 > 0, pop / pmax(pop2010, eps), 1)
        ) %>%
        dplyr::distinct(I, J, .keep_all = TRUE)

      # --- SC ---
      scY <- SCratio_rel2010 %>%
        dplyr::filter(SCENARIO == scn, YEMF == year) %>%
        dplyr::select(I, J, sc) %>%
        dplyr::mutate(
          sc = pmin(pmax(sc, 0), 100) / 100  # % → 小数
        ) %>%
        dplyr::distinct(I, J, .keep_all = TRUE)

      # --- NR ---
      nrY <- NRratio_rel2010 %>%
        dplyr::filter(SCENARIO == scn, YEMF == year) %>%
        dplyr::select(I, J, nr) %>%
        dplyr::mutate(
          nr = pmin(pmax(nr, 0), 100) / 100  # % → 小数
        ) %>%
        dplyr::distinct(I, J, .keep_all = TRUE)

      df <- popY %>%
        dplyr::full_join(scY, by = c("I", "J")) %>%
        dplyr::full_join(nrY, by = c("I", "J")) %>%
        dplyr::filter(I >= 1, I <= 720, J >= 1, J <= 360) %>%
        dplyr::mutate(
          pop_ratio = dplyr::coalesce(pop_ratio, 1),
          sc        = dplyr::coalesce(sc, 0),
          nr        = dplyr::coalesce(nr, 0),
          mult_no    = pop_ratio * pmax(1 - sc, 0),
          mult_sewer = pop_ratio * sc * pmax(1 - nr, 0),
          mult = mult_no + mult_sewer
        )
      mult_mat <- matrix(1, nrow = 720, ncol = 360)
      mult_mat[cbind(df$I, df$J)] <- df$mult

      base_mat * mult_mat
    }

    scenarios <- sort(unique(POPratio_rel2010$SCENARIO))
    HumanN_future <- setNames(vector("list", length(scenarios)), scenarios)

    for (scn in scenarios) {
      HumanN_future[[scn]] <- setNames(vector("list", length(target_years)),
                                      as.character(target_years))
      for (yr in target_years) {
        HumanN_future[[scn]][[as.character(yr)]] <-
          make_future_map(scn = scn, year = yr,
                          POPratio_rel2010 = POPratio_rel2010,
                          SCratio_rel2010  = SCratio_rel2010,
                          NRratio_rel2010  = NRratio_rel2010)
      }
    }

    save(HumanN_future, file = "Data_pd/HumanN_future.rdat")