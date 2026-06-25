current_wd <- getwd()
current_wd_norm <- normalizePath(current_wd, winslash = "/", mustWork = TRUE)

if (basename(current_wd_norm) != "VISIToRN") {
  visitorn_dir_candidate <- file.path(current_wd_norm, "../../VISIToRN")

  if (!dir.exists(visitorn_dir_candidate)) {
    stop(
      "VISIToRN directory was not found.\n",
      "Current working directory: ", current_wd_norm, "\n",
      "Expected VISIToRN path: ", normalizePath(visitorn_dir_candidate, winslash = "/", mustWork = FALSE)
    )
  }

  visitorn_dir <- normalizePath(
    visitorn_dir_candidate,
    winslash = "/",
    mustWork = TRUE
  )

  message("Working directory changed:")
  message("  from: ", current_wd_norm)
  message("  to  : ", visitorn_dir)

  setwd(visitorn_dir)

} else {
  message("Already in VISIToRN directory: ", current_wd_norm)
}

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

if (.Platform$OS.type == "windows") {
  gams_dir <- "C:/GAMS/53"
} else {
  gams_dir <- normalizePath(
    path.expand("~/gams39.3_linux_x64_64_sfx"),
    mustWork = TRUE
  )
}

Sys.setenv(
  PATH = paste(gams_dir, Sys.getenv("PATH"), sep = .Platform$path.sep)
)

gdxrrw::igdx(gams_dir)

message("GAMS path: ", Sys.which("gams"))

rgdx.param <- function(gdx_file, symbol_name) {
  x <- gdxrrw::rgdx(
    gdx_file,
    list(
      name = symbol_name,
      form = "sparse"
    )
  )

  df <- as.data.frame(x$val, stringsAsFactors = FALSE)

  if (ncol(df) < 2) {
    stop("Unexpected GDX parameter format: ", symbol_name)
  }

  n_dim <- ncol(df) - 1

  if (!is.null(x$domains) && length(x$domains) == n_dim) {
    names(df)[seq_len(n_dim)] <- x$domains
  } else {
    names(df)[seq_len(n_dim)] <- paste0("i", seq_len(n_dim))
  }

  names(df)[ncol(df)] <- "value"

  df[[symbol_name]] <- df$value

  df
}


rgdx.set <- function(gdx_file, symbol_name) {
  x <- gdxrrw::rgdx(
    gdx_file,
    list(
      name = symbol_name,
      form = "sparse"
    )
  )

  df <- as.data.frame(x$val, stringsAsFactors = FALSE)

  if (ncol(df) < 1) {
    stop("Unexpected GDX set format: ", symbol_name)
  }

  if (ncol(df) >= 2) {
    last_col <- suppressWarnings(as.numeric(df[[ncol(df)]]))

    if (!all(is.na(last_col)) && all(last_col %in% c(0, 1), na.rm = TRUE)) {
      df <- df[, -ncol(df), drop = FALSE]
    }
  }

  if (!is.null(x$domains) && length(x$domains) == ncol(df)) {
    names(df) <- x$domains
  } else {
    names(df) <- paste0("i", seq_len(ncol(df)))
  }

  if (symbol_name == "MAP_GIJ" && ncol(df) == 3) {
    names(df) <- c("G", "I", "J")
  }

  if (symbol_name == "MAP_RIJ" && ncol(df) == 3) {
    names(df) <- c("R", "I", "J")
  }

  if (symbol_name == "MAP_RG" && ncol(df) == 2) {
    names(df) <- c("R", "G")
  }

  df
}
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
        load("Data_pd/df_river_all_NMIP3.rdat")
        load("Data_pd/df_river_all_mit_NMIP3.rdat")

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


         # output folder
        out_dir <- "output"
        dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

        p_river_top14 <- ggplot(
          df_plot,
          aes(x = year_f, y = Nload, color = scenario, group = scenario)
        ) +
          geom_line(linewidth = 0.9) +
          geom_point(size = 1.8) +
          scale_x_discrete(drop = FALSE) +
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          scale_color_manual(values = c(
            "Baseline" = "red",
            "500C"     = "cyan3"
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

        print(p_river_top14)

        ggsave(
          filename = file.path(out_dir, paste0("River_Nload_top14", suffix_save, ".png")),
          plot = p_river_top14,
          width = 12,
          height = 8,
          dpi = 300,
          bg = "white"
        )
