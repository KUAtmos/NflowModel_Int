#POP
#Area/POP
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(purrr)
    library(readr)
    library(gdxrrw)

    target_years <- c(2010, 2030, 2050, 2100)

    crop_classes <- c(
      "PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR",
      "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF",
      "OTH_A"
    )

    pasture_classes <- c("PAS")

    idx_to_lon <- function(lon_idx) {
      0.5 * as.numeric(lon_idx) - 180.25
    }

    idx_to_lat <- function(lat_idx) {
      -0.5 * as.numeric(lat_idx) + 90.25
    }

    lon_to_idx <- function(lon) {
      as.integer(round((as.numeric(lon) + 179.75) / 0.5) + 1)
    }

    lat_to_idx <- function(lat) {
      as.integer(round((89.75 - as.numeric(lat)) / 0.5) + 1)
    }

    standardize_grid_index <- function(df,
                                      lat_col = "lat",
                                      lon_col = "lon") {

      if (!lat_col %in% names(df)) {
        stop("lat column not found: ", lat_col)
      }

      if (!lon_col %in% names(df)) {
        stop("lon column not found: ", lon_col)
      }

      lat_raw <- as.numeric(as.character(df[[lat_col]]))
      lon_raw <- as.numeric(as.character(df[[lon_col]]))

      lon_range <- range(lon_raw, na.rm = TRUE)
      lat_range <- range(lat_raw, na.rm = TRUE)

      # 1-based grid index: lon = 1..720, lat = 1..360
      is_1based_index <- (
        lon_range[1] >= 1 && lon_range[2] <= 720 &&
          lat_range[1] >= 1 && lat_range[2] <= 360
      )

      # 0-based grid index: lon = 0..719, lat = 0..359
      is_0based_index <- (
        lon_range[1] >= 0 && lon_range[2] <= 719 &&
          lat_range[1] >= 0 && lat_range[2] <= 359
      )

      if (is_1based_index) {
        df %>%
          mutate(
            lon_idx = as.integer(round(lon_raw)),
            lat_idx = as.integer(round(lat_raw)),
            lon_center = idx_to_lon(lon_idx),
            lat_center = idx_to_lat(lat_idx)
          )

      } else if (is_0based_index) {
        df %>%
          mutate(
            lon_idx = as.integer(round(lon_raw)) + 1L,
            lat_idx = as.integer(round(lat_raw)) + 1L,
            lon_center = idx_to_lon(lon_idx),
            lat_center = idx_to_lat(lat_idx)
          )

      } else {
        # actual lon/lat
        df %>%
          mutate(
            lon_idx = lon_to_idx(lon_raw),
            lat_idx = lat_to_idx(lat_raw),
            lon_center = as.numeric(lon_raw),
            lat_center = as.numeric(lat_raw)
          )
      }
    }

    make_basin_grid <- function(rivnum_matrix) {

      data.frame(
        lon_idx = rep(1:720, each = 360),
        lat_idx = rep(1:360, times = 720),
        rivnum  = as.vector(rivnum_matrix)
      ) %>%
        mutate(
          lon_idx = as.integer(lon_idx),
          lat_idx = as.integer(lat_idx),
          rivnum  = as.integer(rivnum),
          lon_center = idx_to_lon(lon_idx),
          lat_center = idx_to_lat(lat_idx)
        ) %>%
        filter(rivnum > 0)
    }

    # =========================================================
    # Population data
    # =========================================================

    Global <- "Data_pd/SSP_update.gdx"
    POP <- rgdx.param(Global, "POP")

    regionSSP.map <- read.delim(
      "Data_pd/regionSSP.map",
      header = FALSE,
      stringsAsFactors = FALSE
    )

    regionSSP.map <- regionSSP.map[, c(1, 3)]
    colnames(regionSSP.map) <- c("country", "region")

    IAMC_Template_POP <- POP %>%
      left_join(regionSSP.map, by = c("i1" = "country")) %>%
      select(-i1, -i3) %>%
      rename(
        SCENARIO = i2,
        YEMF = i4,
        REMF = region,
        IAMC_Template = value
      ) %>%
      mutate(
        YEMF = as.integer(as.character(YEMF)),
        IAMC_Template = as.numeric(IAMC_Template)
      ) %>%
      filter(
        YEMF %in% c(2005, seq(2010, 2100, by = 10)),
        SCENARIO == "SSP2"
      )

    # data_prep がまだ定義されていない場合のみ指定
    # 必要に応じてパスは調整してください
    if (!exists("data_prep")) {
      data_prep <- "Data_pd/data_prep.gdx"
    }

    MAP_RIJ <- rgdx.set(data_prep, "MAP_RIJ") %>%
      mutate(
        lat_idx = as.integer(as.character(I)),
        lon_idx = as.integer(as.character(J))
      ) %>%
      rename(REMF = R) %>%
      select(REMF, lat_idx, lon_idx)

    # lat/lon という列名は残すが、中身は「グリッド番号」として統一
    POPratio <- IAMC_Template_POP %>%
      left_join(MAP_RIJ, by = "REMF") %>%
      mutate(
        lat = lat_idx,
        lon = lon_idx,
        lat_center = idx_to_lat(lat_idx),
        lon_center = idx_to_lon(lon_idx)
      )

    # =========================================================
    # Aggregate population by river basin
    # =========================================================

    aggregate_basin_population <- function(POPratio,
                                          rivnum_matrix,
                                          riv_table,
                                          target_years = NULL,
                                          scenario = "SSP2") {

      basin_grid <- make_basin_grid(rivnum_matrix)

      pop_grid <- POPratio %>%
        filter(SCENARIO == scenario) %>%
        standardize_grid_index(lat_col = "lat", lon_col = "lon") %>%
        mutate(
          year = as.integer(as.character(YEMF)),
          population_million = as.numeric(IAMC_Template)
        )

      if (!is.null(target_years)) {
        pop_grid <- pop_grid %>%
          filter(year %in% target_years)
      }

      # 同じグリッドに複数行ある場合に備えて、先にグリッド単位で合計
      pop_grid_sum <- pop_grid %>%
        group_by(year, lon_idx, lat_idx) %>%
        summarise(
          population_million = sum(population_million, na.rm = TRUE),
          .groups = "drop"
        )

      basin_pop <- pop_grid_sum %>%
        left_join(
          basin_grid %>% select(lon_idx, lat_idx, rivnum),
          by = c("lon_idx", "lat_idx")
        ) %>%
        filter(!is.na(rivnum), rivnum > 0) %>%
        group_by(year, rivnum) %>%
        summarise(
          population_million = sum(population_million, na.rm = TRUE),
          population = population_million * 1e6,
          .groups = "drop"
        ) %>%
        left_join(
          riv_table %>%
            mutate(rivnum = as.integer(as.character(rivnum))),
          by = "rivnum"
        )

      basin_pop
    }

    df_basin_pop <- aggregate_basin_population(
      POPratio = POPratio,
      rivnum_matrix = rivnum_matrix,
      riv_table = riv_table,
      target_years = target_years,
      scenario = "SSP2"
    )

    # =========================================================
    # Aggregate cropland + pasture area by river basin
    # =========================================================

    aggregate_one_vyload2_area_pop <- function(VY_load2_one,
                                              scenario_case,
                                              target_year,
                                              rivnum_matrix,
                                              riv_table,
                                              df_basin_pop) {

      required_cols <- c("i2", "i3", "LUAREA", "lat", "lon")
      missing_cols <- setdiff(required_cols, names(VY_load2_one))

      if (length(missing_cols) > 0) {
        stop(
          "VY_load2 object に必要な列がありません: ",
          paste(missing_cols, collapse = ", ")
        )
      }

      basin_grid <- make_basin_grid(rivnum_matrix)

      vy <- VY_load2_one %>%
        standardize_grid_index(lat_col = "lat", lon_col = "lon") %>%
        mutate(
          year = as.integer(as.character(i2)),
          land_class = as.character(i3),
          LUAREA = as.numeric(LUAREA),
          land_group = case_when(
            land_class %in% crop_classes ~ "cropland",
            land_class %in% pasture_classes ~ "pasture",
            TRUE ~ NA_character_
          )
        ) %>%
        filter(
          year == target_year,
          !is.na(land_group)
        )

      if (nrow(vy) == 0) {
        stop(
          "対象となる土地利用データがありません: ",
          scenario_case,
          " / ",
          target_year
        )
      }

    basin_area_long <- vy %>%
      dplyr::group_by(year, lon_idx, lat_idx, land_group) %>%
      dplyr::summarise(
        area_ha = sum(LUAREA, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::left_join(
        basin_grid %>% dplyr::select(lon_idx, lat_idx, rivnum),
        by = c("lon_idx", "lat_idx")
      ) %>%
      dplyr::filter(!is.na(rivnum), rivnum > 0) %>%
      dplyr::group_by(year, rivnum, land_group) %>%
      dplyr::summarise(
        area_ha = sum(area_ha, na.rm = TRUE),
        .groups = "drop"
      )

    basin_area <- basin_area_long %>%
      tidyr::pivot_wider(
        names_from = land_group,
        values_from = area_ha,
        values_fill = list(area_ha = 0)
      )

    if (!"cropland" %in% names(basin_area)) {
      basin_area$cropland <- 0
    }

    if (!"pasture" %in% names(basin_area)) {
      basin_area$pasture <- 0
    }

    basin_area <- basin_area %>%
      dplyr::mutate(
        cropland_area_ha = dplyr::coalesce(as.numeric(cropland), 0),
        pasture_area_ha  = dplyr::coalesce(as.numeric(pasture), 0),
        ag_area_ha       = cropland_area_ha + pasture_area_ha
      ) %>%
      dplyr::select(
        year,
        rivnum,
        cropland_area_ha,
        pasture_area_ha,
        ag_area_ha
      )

      pop_for_join <- df_basin_pop %>%
        mutate(
          year = as.integer(year),
          rivnum = as.integer(as.character(rivnum)),
          population_million = as.numeric(population_million),
          population = as.numeric(population)
        ) %>%
        select(
          year,
          rivnum,
          population_million,
          population
        )

      out <- basin_area %>%
        left_join(pop_for_join, by = c("year", "rivnum")) %>%
        left_join(
          riv_table %>%
            mutate(rivnum = as.integer(as.character(rivnum))),
          by = "rivnum"
        ) %>%
        mutate(
          scenario_case = scenario_case,

          ag_area_ha_per_person = if_else(
            !is.na(population) & population > 0,
            ag_area_ha / population,
            NA_real_
          ),

          ag_area_m2_per_person = if_else(
            !is.na(population) & population > 0,
            ag_area_ha * 10000 / population,
            NA_real_
          ),

          cropland_m2_per_person = if_else(
            !is.na(population) & population > 0,
            cropland_area_ha * 10000 / population,
            NA_real_
          ),

          pasture_m2_per_person = if_else(
            !is.na(population) & population > 0,
            pasture_area_ha * 10000 / population,
            NA_real_
          )
        ) %>%
        select(
          scenario_case,
          year,
          rivnum,
          river_basin_name,
          cropland_area_ha,
          pasture_area_ha,
          ag_area_ha,
          population_million,
          population,
          ag_area_ha_per_person,
          ag_area_m2_per_person,
          cropland_m2_per_person,
          pasture_m2_per_person
        )

      out
    }

    # =========================================================
    # Detect VY_load2 objects
    # =========================================================

    vy_objects <- ls(envir = .GlobalEnv, pattern = "^VY_load2_.*_[0-9]{4}$")

    vy_plan <- tibble(object_name = vy_objects) %>%
      mutate(
        m = str_match(object_name, "^VY_load2_(.*)_([0-9]{4})$"),
        scenario_case = m[, 2],
        year = as.integer(m[, 3])
      ) %>%
      select(-m) %>%
      filter(
        !is.na(scenario_case),
        !is.na(year),
        year %in% target_years
      ) %>%
      arrange(scenario_case, year)

    print(vy_plan)

    if (nrow(vy_plan) == 0) {
      stop("VY_load2_<scenario>_<year> 形式のオブジェクトが見つかりません。")
    }

    # =========================================================
    # Run aggregation for all VY_load2 objects
    # =========================================================

    df_basin_agarea_per_pop_all <- bind_rows(
      lapply(seq_len(nrow(vy_plan)), function(k) {

        obj_name <- vy_plan$object_name[k]

        message("Processing: ", obj_name)

        aggregate_one_vyload2_area_pop(
          VY_load2_one = get(obj_name, envir = .GlobalEnv),
          scenario_case = vy_plan$scenario_case[k],
          target_year = vy_plan$year[k],
          rivnum_matrix = rivnum_matrix,
          riv_table = riv_table,
          df_basin_pop = df_basin_pop
        )
      })
    )

    # =========================================================
    # Check
    # =========================================================

    df_basin_agarea_per_pop_all %>%
      count(scenario_case, year) %>%
      print(n = Inf)

    df_basin_agarea_per_pop_all %>%
      summarise(
        n_total = n(),
        n_missing_population = sum(is.na(population)),
        n_missing_ag_area = sum(is.na(ag_area_ha)),
        n_missing_area_per_pop = sum(is.na(ag_area_m2_per_person)),
        min_ag_area_m2_per_person = min(ag_area_m2_per_person, na.rm = TRUE),
        max_ag_area_m2_per_person = max(ag_area_m2_per_person, na.rm = TRUE)
      ) %>%
      print()

    # =========================================================
    # Save
    # =========================================================
    save(
      df_basin_pop,
      file = "Data_pd/df_basin_population_SSP2.rdat"
    )

    save(
      df_basin_agarea_per_pop_all,
      file = "Data_pd/df_basin_agarea_per_pop_all.rdat"
    )

#Ninput/Area
  # =========================================================
  # Input / cropland + pasture area by river basin
  # =========================================================

  aggregate_one_input_per_area <- function(Inputdis_one,
                                          VY_load2_one,
                                          scenario_case,
                                          target_year,
                                          rivnum_matrix,
                                          riv_table,
                                          df_basin_agarea_per_pop_all) {

    # ---------------------------------------------------------
    # Check Inputdis columns
    # ---------------------------------------------------------
    required_input_cols <- c("G", "YEMF")
    missing_input_cols <- setdiff(required_input_cols, names(Inputdis_one))

    if (length(missing_input_cols) > 0) {
      stop(
        "Inputdis object に必要な列がありません: ",
        paste(missing_input_cols, collapse = ", ")
      )
    }

    input_col <- if ("input" %in% names(Inputdis_one)) {
      "input"
    } else if ("Input" %in% names(Inputdis_one)) {
      "Input"
    } else {
      stop("Inputdis object に input または Input 列が見つかりません。")
    }

    # ---------------------------------------------------------
    # Check VY_load2 columns
    # ---------------------------------------------------------
    required_vy_cols <- c("G", "i2", "lat", "lon")
    missing_vy_cols <- setdiff(required_vy_cols, names(VY_load2_one))

    if (length(missing_vy_cols) > 0) {
      stop(
        "VY_load2 object に必要な列がありません: ",
        paste(missing_vy_cols, collapse = ", ")
      )
    }

    # ---------------------------------------------------------
    # Basin grid
    # ---------------------------------------------------------
    basin_grid <- make_basin_grid(rivnum_matrix) %>%
      dplyr::select(lon_idx, lat_idx, rivnum)

    # ---------------------------------------------------------
    # Make G -> rivnum table from VY_load2
    # ---------------------------------------------------------
    g_to_basin <- VY_load2_one %>%
      standardize_grid_index(lat_col = "lat", lon_col = "lon") %>%
      dplyr::mutate(
        G = as.character(G),
        year = as.integer(as.character(i2))
      ) %>%
      dplyr::filter(year == target_year) %>%
      dplyr::distinct(G, lon_idx, lat_idx) %>%
      dplyr::left_join(basin_grid, by = c("lon_idx", "lat_idx")) %>%
      dplyr::filter(!is.na(rivnum), rivnum > 0) %>%
      dplyr::select(G, rivnum)

    # ---------------------------------------------------------
    # Aggregate input by river basin
    # ---------------------------------------------------------
    basin_input <- Inputdis_one %>%
      dplyr::mutate(
        G = as.character(G),
        year = as.integer(as.character(YEMF)),
        input_value = as.numeric(.data[[input_col]])
      ) %>%
      dplyr::filter(year == target_year) %>%
      dplyr::group_by(year, G) %>%
      dplyr::summarise(
        input_value = sum(input_value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::left_join(g_to_basin, by = "G") %>%
      dplyr::filter(!is.na(rivnum), rivnum > 0) %>%
      dplyr::group_by(year, rivnum) %>%
      dplyr::summarise(
        input_value = sum(input_value, na.rm = TRUE),
        .groups = "drop"
      )

    # ---------------------------------------------------------
    # Area denominator from previous result
    # ---------------------------------------------------------
    basin_area <- df_basin_agarea_per_pop_all %>%
      dplyr::filter(
        scenario_case == !!scenario_case,
        year == !!target_year
      ) %>%
      dplyr::mutate(
        year = as.integer(year),
        rivnum = as.integer(as.character(rivnum))
      ) %>%
      dplyr::select(
        year,
        rivnum,
        river_basin_name,
        cropland_area_ha,
        pasture_area_ha,
        ag_area_ha
      )

    # ---------------------------------------------------------
    # Join and calculate input / area
    # ---------------------------------------------------------
    out <- basin_input %>%
      dplyr::mutate(
        rivnum = as.integer(as.character(rivnum))
      ) %>%
      dplyr::left_join(basin_area, by = c("year", "rivnum")) %>%
      dplyr::left_join(
        riv_table %>%
          dplyr::mutate(rivnum = as.integer(as.character(rivnum))) %>%
          dplyr::select(rivnum, river_basin_name_from_table = river_basin_name),
        by = "rivnum"
      ) %>%
      dplyr::mutate(
        river_basin_name = dplyr::coalesce(
          river_basin_name,
          river_basin_name_from_table
        ),
        scenario_case = scenario_case,

        # input_value が Tg N yr-1 の場合
        input_TgN_per_ha = dplyr::if_else(
          !is.na(ag_area_ha) & ag_area_ha > 0,
          input_value / ag_area_ha,
          NA_real_
        ),

        # input_value が Tg N yr-1 の場合: kg N ha-1 yr-1
        input_kgN_per_ha = dplyr::if_else(
          !is.na(ag_area_ha) & ag_area_ha > 0,
          input_value * 1e9 / ag_area_ha,
          NA_real_
        ),

        # 参考: kg N km-2 yr-1
        input_kgN_per_km2 = dplyr::if_else(
          !is.na(ag_area_ha) & ag_area_ha > 0,
          input_value * 1e9 / (ag_area_ha * 0.01),
          NA_real_
        )
      ) %>%
      dplyr::select(
        scenario_case,
        year,
        rivnum,
        river_basin_name,
        input_value,
        cropland_area_ha,
        pasture_area_ha,
        ag_area_ha,
        input_TgN_per_ha,
        input_kgN_per_ha,
        input_kgN_per_km2
      )

    out
  }

  # =========================================================
  # Detect Inputdis objects and matching VY_load2 objects
  # =========================================================

  input_objects <- ls(envir = .GlobalEnv, pattern = "^Inputdis_.*_[0-9]{4}$")

  input_plan <- tibble::tibble(object_input = input_objects) %>%
    dplyr::mutate(
      m = stringr::str_match(object_input, "^Inputdis_(.*)_([0-9]{4})$"),
      scenario_case = m[, 2],
      year = as.integer(m[, 3]),
      object_vy = paste0("VY_load2_", scenario_case, "_", year)
    ) %>%
    dplyr::select(-m) %>%
    dplyr::filter(
      !is.na(scenario_case),
      !is.na(year),
      year %in% target_years
    ) %>%
    dplyr::arrange(scenario_case, year)

  print(input_plan)

  if (nrow(input_plan) == 0) {
    stop("Inputdis_<scenario>_<year> 形式のオブジェクトが見つかりません。")
  }

  missing_vy <- input_plan$object_vy[
    !vapply(input_plan$object_vy, exists, logical(1), envir = .GlobalEnv)
  ]

  if (length(missing_vy) > 0) {
    stop(
      "対応する VY_load2 オブジェクトが見つかりません: ",
      paste(missing_vy, collapse = ", ")
    )
  }

  # =========================================================
  # Run aggregation
  # =========================================================

  df_basin_input_per_area_all <- dplyr::bind_rows(
    lapply(seq_len(nrow(input_plan)), function(k) {

      input_name <- input_plan$object_input[k]
      vy_name    <- input_plan$object_vy[k]

      message("Processing: ", input_name, " + ", vy_name)

      aggregate_one_input_per_area(
        Inputdis_one = get(input_name, envir = .GlobalEnv),
        VY_load2_one = get(vy_name, envir = .GlobalEnv),
        scenario_case = input_plan$scenario_case[k],
        target_year = input_plan$year[k],
        rivnum_matrix = rivnum_matrix,
        riv_table = riv_table,
        df_basin_agarea_per_pop_all = df_basin_agarea_per_pop_all
      )
    })
  )

  # =========================================================
  # Check
  # =========================================================

  df_basin_input_per_area_all %>%
    dplyr::count(scenario_case, year) %>%
    print(n = Inf)

  df_basin_input_per_area_all %>%
    dplyr::group_by(scenario_case, year) %>%
    dplyr::summarise(
      n_basin = dplyr::n(),
      n_unique_input = dplyr::n_distinct(input_value),
      n_unique_ag_area = dplyr::n_distinct(ag_area_ha),
      n_unique_input_per_area = dplyr::n_distinct(input_kgN_per_ha),
      n_missing_input = sum(is.na(input_value)),
      n_missing_area = sum(is.na(ag_area_ha)),
      n_missing_input_per_area = sum(is.na(input_kgN_per_ha)),
      min_input_kgN_per_ha = min(input_kgN_per_ha, na.rm = TRUE),
      max_input_kgN_per_ha = max(input_kgN_per_ha, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    print(n = Inf)

  # =========================================================
  # Save
  # =========================================================

  save(
    df_basin_input_per_area_all,
    file = "Data_pd/df_basin_input_per_area_all.rdat"
  )

  #kgN ha-1 yr-1

#Nleach/Ninput
  # =========================================================
  # Nleach / Ninput by river basin
  # =========================================================

  aggregate_one_leach_per_input <- function(OUTleadis_one,
                                            VY_load2_one,
                                            scenario_case,
                                            target_year,
                                            rivnum_matrix,
                                            riv_table,
                                            df_basin_input_per_area_all) {

    # ---------------------------------------------------------
    # Check OUTleadis columns
    # ---------------------------------------------------------
    required_leach_cols <- c("G", "YEMF", "OUTleagrid")
    missing_leach_cols <- setdiff(required_leach_cols, names(OUTleadis_one))

    if (length(missing_leach_cols) > 0) {
      stop(
        "OUTleadis object に必要な列がありません: ",
        paste(missing_leach_cols, collapse = ", ")
      )
    }

    # ---------------------------------------------------------
    # Check VY_load2 columns
    # ---------------------------------------------------------
    required_vy_cols <- c("G", "i2", "lat", "lon")
    missing_vy_cols <- setdiff(required_vy_cols, names(VY_load2_one))

    if (length(missing_vy_cols) > 0) {
      stop(
        "VY_load2 object に必要な列がありません: ",
        paste(missing_vy_cols, collapse = ", ")
      )
    }

    # ---------------------------------------------------------
    # Basin grid
    # ---------------------------------------------------------
    basin_grid <- make_basin_grid(rivnum_matrix) %>%
      dplyr::select(lon_idx, lat_idx, rivnum)

    # ---------------------------------------------------------
    # Make G -> rivnum table from VY_load2
    # ---------------------------------------------------------
    g_to_basin <- VY_load2_one %>%
      standardize_grid_index(lat_col = "lat", lon_col = "lon") %>%
      dplyr::mutate(
        G = as.character(G),
        year = as.integer(as.character(i2))
      ) %>%
      dplyr::filter(year == target_year) %>%
      dplyr::distinct(G, lon_idx, lat_idx) %>%
      dplyr::left_join(basin_grid, by = c("lon_idx", "lat_idx")) %>%
      dplyr::filter(!is.na(rivnum), rivnum > 0) %>%
      dplyr::mutate(
        rivnum = as.integer(as.character(rivnum))
      ) %>%
      dplyr::select(G, rivnum) %>%
      dplyr::distinct()

    # ---------------------------------------------------------
    # Aggregate N leaching by river basin
    # ---------------------------------------------------------
    basin_leach <- OUTleadis_one %>%
      dplyr::mutate(
        G = as.character(G),
        year = as.integer(as.character(YEMF)),
        Nleach_value = as.numeric(OUTleagrid)
      ) %>%
      dplyr::filter(year == target_year) %>%
      dplyr::group_by(year, G) %>%
      dplyr::summarise(
        Nleach_value = sum(Nleach_value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::left_join(g_to_basin, by = "G") %>%
      dplyr::filter(!is.na(rivnum), rivnum > 0) %>%
      dplyr::group_by(year, rivnum) %>%
      dplyr::summarise(
        Nleach_value = sum(Nleach_value, na.rm = TRUE),
        .groups = "drop"
      )

    # ---------------------------------------------------------
    # Basin-level N input from previous result
    # ---------------------------------------------------------
    basin_input <- df_basin_input_per_area_all %>%
      dplyr::filter(
        .data$scenario_case == .env$scenario_case,
        .data$year == .env$target_year
      ) %>%
      dplyr::mutate(
        year = as.integer(year),
        rivnum = as.integer(as.character(rivnum)),
        Ninput_value = as.numeric(input_value)
      ) %>%
      dplyr::group_by(year, rivnum) %>%
      dplyr::summarise(
        river_basin_name = dplyr::first(river_basin_name),
        Ninput_value = sum(Ninput_value, na.rm = TRUE),
        ag_area_ha = dplyr::first(ag_area_ha),
        input_kgN_per_ha = dplyr::first(input_kgN_per_ha),
        .groups = "drop"
      )

    # ---------------------------------------------------------
    # Join and calculate Nleach / Ninput
    # ---------------------------------------------------------
    out <- basin_leach %>%
      dplyr::full_join(basin_input, by = c("year", "rivnum")) %>%
      dplyr::left_join(
        riv_table %>%
          dplyr::mutate(rivnum = as.integer(as.character(rivnum))) %>%
          dplyr::select(rivnum, river_basin_name_from_table = river_basin_name),
        by = "rivnum"
      ) %>%
      dplyr::mutate(
        river_basin_name = dplyr::coalesce(
          river_basin_name,
          river_basin_name_from_table
        ),
        scenario_case = scenario_case,

        Nleach_Ninput_ratio = dplyr::if_else(
          !is.na(Ninput_value) & Ninput_value > 0,
          Nleach_value / Ninput_value,
          NA_real_
        ),

        Nleach_Ninput_percent = 100 * Nleach_Ninput_ratio
      ) %>%
      dplyr::select(
        scenario_case,
        year,
        rivnum,
        river_basin_name,
        Nleach_value,
        Ninput_value,
        Nleach_Ninput_ratio,
        Nleach_Ninput_percent,
        ag_area_ha,
        input_kgN_per_ha
      )

    out
  }

  # =========================================================
  # Detect OUTleadis objects and matching VY_load2 objects
  # =========================================================

  leach_objects <- ls(envir = .GlobalEnv, pattern = "^OUTleadis_.*_[0-9]{4}$")

  leach_plan <- tibble::tibble(object_leach = leach_objects) %>%
    dplyr::mutate(
      m = stringr::str_match(object_leach, "^OUTleadis_(.*)_([0-9]{4})$"),
      scenario_case = m[, 2],
      year = as.integer(m[, 3]),
      object_vy = paste0("VY_load2_", scenario_case, "_", year)
    ) %>%
    dplyr::select(-m) %>%
    dplyr::filter(
      !is.na(scenario_case),
      !is.na(year),
      year %in% target_years
    ) %>%
    dplyr::arrange(scenario_case, year)

  print(leach_plan)

  if (nrow(leach_plan) == 0) {
    stop("OUTleadis_<scenario>_<year> 形式のオブジェクトが見つかりません。")
  }

  missing_vy <- leach_plan$object_vy[
    !vapply(leach_plan$object_vy, exists, logical(1), envir = .GlobalEnv)
  ]

  if (length(missing_vy) > 0) {
    stop(
      "対応する VY_load2 オブジェクトが見つかりません: ",
      paste(missing_vy, collapse = ", ")
    )
  }

  if (!exists("df_basin_input_per_area_all")) {
    stop("df_basin_input_per_area_all が見つかりません。先に Ninput/Area の計算を実行してください。")
  }

  # =========================================================
  # Run aggregation
  # =========================================================

  df_basin_leach_per_input_all <- dplyr::bind_rows(
    lapply(seq_len(nrow(leach_plan)), function(k) {

      leach_name <- leach_plan$object_leach[k]
      vy_name    <- leach_plan$object_vy[k]

      message("Processing: ", leach_name, " + ", vy_name)

      aggregate_one_leach_per_input(
        OUTleadis_one = get(leach_name, envir = .GlobalEnv),
        VY_load2_one = get(vy_name, envir = .GlobalEnv),
        scenario_case = leach_plan$scenario_case[k],
        target_year = leach_plan$year[k],
        rivnum_matrix = rivnum_matrix,
        riv_table = riv_table,
        df_basin_input_per_area_all = df_basin_input_per_area_all
      )
    })
  )

  # =========================================================
  # Check
  # =========================================================

  df_basin_leach_per_input_all %>%
    dplyr::count(scenario_case, year) %>%
    print(n = Inf)

  df_basin_leach_per_input_all %>%
    dplyr::group_by(scenario_case, year) %>%
    dplyr::summarise(
      n_basin = dplyr::n(),
      n_unique_Nleach = dplyr::n_distinct(Nleach_value),
      n_unique_Ninput = dplyr::n_distinct(Ninput_value),
      n_unique_ratio = dplyr::n_distinct(Nleach_Ninput_ratio),
      n_missing_Nleach = sum(is.na(Nleach_value)),
      n_missing_Ninput = sum(is.na(Ninput_value)),
      n_missing_ratio = sum(is.na(Nleach_Ninput_ratio)),
      min_ratio = min(Nleach_Ninput_ratio, na.rm = TRUE),
      max_ratio = max(Nleach_Ninput_ratio, na.rm = TRUE),
      min_percent = min(Nleach_Ninput_percent, na.rm = TRUE),
      max_percent = max(Nleach_Ninput_percent, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    print(n = Inf)

  # =========================================================
  # Save
  # =========================================================

  save(
    df_basin_leach_per_input_all,
    file = "Data_pd/df_basin_leach_per_input_all.rdat"
  )

#Nload/Nleach
      # =========================================================
    # Nload / Nleach by river basin
    # NOTE:
    #   Raw Nload in df_river_named_mouth_* is kg N yr-1
    #   Nleach_value is assumed to be Tg N yr-1
    # =========================================================

    aggregate_one_load_per_leach <- function(load_df,
                                            case_label,
                                            df_basin_leach_per_input_all) {

      year <- as.integer(stringr::str_extract(case_label, "[0-9]{4}"))

      scenario_case <- if (stringr::str_detect(case_label, "mit$")) {
        "500C"
      } else {
        "BaU"
      }

      if (!"rivnum" %in% names(load_df)) {
        stop("Nload data frame に rivnum 列が見つかりません: ", case_label)
      }

      if (!"Nload" %in% names(load_df)) {
        stop("Nload data frame に Nload 列が見つかりません: ", case_label)
      }

      name_col <- if ("river_basin_name" %in% names(load_df)) {
        "river_basin_name"
      } else {
        NA_character_
      }

      # ---------------------------------------------------------
      # Nload table
      # Raw Nload is kg N yr-1
      # ---------------------------------------------------------
      load_tbl <- load_df %>%
        dplyr::mutate(
          rivnum = as.integer(as.character(rivnum)),
          Nload_kgN = as.numeric(Nload),
          Nload_GgN = Nload_kgN / 1e6,
          Nload_TgN = Nload_kgN / 1e9
        )

      if (!is.na(name_col)) {
        load_tbl <- load_tbl %>%
          dplyr::transmute(
            scenario_case = scenario_case,
            year = year,
            rivnum,
            river_basin_name_load = .data[[name_col]],
            Nload_kgN,
            Nload_GgN,
            Nload_TgN
          )
      } else {
        load_tbl <- load_tbl %>%
          dplyr::transmute(
            scenario_case = scenario_case,
            year = year,
            rivnum,
            river_basin_name_load = NA_character_,
            Nload_kgN,
            Nload_GgN,
            Nload_TgN
          )
      }

      # ---------------------------------------------------------
      # Nleach table
      # Nleach_value is assumed to be Tg N yr-1
      # ---------------------------------------------------------
      leach_tbl <- df_basin_leach_per_input_all %>%
        dplyr::filter(
          .data$scenario_case == .env$scenario_case,
          .data$year == .env$year
        ) %>%
        dplyr::mutate(
          rivnum = as.integer(as.character(rivnum)),
          Nleach_TgN = as.numeric(Nleach_value),
          Nleach_GgN = Nleach_TgN * 1000,
          Nleach_kgN = Nleach_TgN * 1e9
        ) %>%
        dplyr::select(
          scenario_case,
          year,
          rivnum,
          river_basin_name,
          Nleach_kgN,
          Nleach_GgN,
          Nleach_TgN,
          Ninput_value,
          Nleach_Ninput_ratio,
          Nleach_Ninput_percent
        )

      out <- load_tbl %>%
        dplyr::full_join(
          leach_tbl,
          by = c("scenario_case", "year", "rivnum")
        ) %>%
        dplyr::mutate(
          river_basin_name = dplyr::coalesce(
            river_basin_name,
            river_basin_name_load
          ),

          Nload_Nleach_ratio = dplyr::if_else(
            !is.na(Nleach_kgN) & Nleach_kgN > 0,
            Nload_kgN / Nleach_kgN,
            NA_real_
          ),

          Nload_Nleach_percent = 100 * Nload_Nleach_ratio
        ) %>%
        dplyr::select(
          scenario_case,
          year,
          rivnum,
          river_basin_name,
          Nload_kgN,
          Nload_GgN,
          Nload_TgN,
          Nleach_kgN,
          Nleach_GgN,
          Nleach_TgN,
          Nload_Nleach_ratio,
          Nload_Nleach_percent,
          Ninput_value,
          Nleach_Ninput_ratio,
          Nleach_Ninput_percent
        )

      out
    }

    # =========================================================
    # Detect df_river_named_mouth objects
    # =========================================================

    load_objects <- ls(
      envir = .GlobalEnv,
      pattern = "^df_river_named_mouth_[0-9]{4}(mit)?$"
    )

    load_plan <- tibble::tibble(object_load = load_objects) %>%
      dplyr::mutate(
        case_label = stringr::str_remove(object_load, "^df_river_named_mouth_"),
        year = as.integer(stringr::str_extract(case_label, "[0-9]{4}")),
        scenario_case = dplyr::if_else(
          stringr::str_detect(case_label, "mit$"),
          "500C",
          "BaU"
        )
      ) %>%
      dplyr::filter(
        !is.na(year),
        year %in% target_years
      ) %>%
      dplyr::arrange(scenario_case, year)

    print(load_plan)

    if (nrow(load_plan) == 0) {
      stop("df_river_named_mouth_YYYY または df_river_named_mouth_YYYYmit 形式のオブジェクトが見つかりません。")
    }

    if (!exists("df_basin_leach_per_input_all")) {
      stop("df_basin_leach_per_input_all が見つかりません。先に Nleach/Ninput の計算を実行してください。")
    }

    df_basin_load_per_leach_all <- dplyr::bind_rows(
      lapply(seq_len(nrow(load_plan)), function(k) {

        load_name  <- load_plan$object_load[k]
        case_label <- load_plan$case_label[k]

        message("Processing: ", load_name)

        aggregate_one_load_per_leach(
          load_df = get(load_name, envir = .GlobalEnv),
          case_label = case_label,
          df_basin_leach_per_input_all = df_basin_leach_per_input_all
        )
      })
    )

    df_basin_load_per_leach_all %>%
      dplyr::count(scenario_case, year) %>%
      print(n = Inf)

    save(
      df_basin_load_per_leach_all,
      file = "Data_pd/df_basin_load_per_leach_all.rdat"
    )

#LMDI
    # =========================================================
    # 1. Prepare factor table
    # =========================================================

    df_factor_identity_all <- df_basin_load_per_leach_all %>%
      dplyr::mutate(
        scenario_case = as.character(scenario_case),
        year = as.integer(year),
        rivnum = as.integer(as.character(rivnum)),
        Nload_GgN = as.numeric(Nload_GgN),
        Nleach_Ninput = as.numeric(Nleach_Ninput_ratio),
        Nload_Nleach = as.numeric(Nload_Nleach_ratio)
      ) %>%
      dplyr::select(
        scenario_case,
        year,
        rivnum,
        river_basin_name,
        Nload_GgN,
        Nleach_Ninput,
        Nload_Nleach
      ) %>%
      dplyr::left_join(
        df_basin_agarea_per_pop_all %>%
          dplyr::mutate(
            scenario_case = as.character(scenario_case),
            year = as.integer(year),
            rivnum = as.integer(as.character(rivnum)),
            POP = as.numeric(population),
            Area_POP_ha_per_person = as.numeric(ag_area_ha_per_person),
            Area_POP_m2_per_person = as.numeric(ag_area_m2_per_person),
            ag_area_ha = as.numeric(ag_area_ha)
          ) %>%
          dplyr::select(
            scenario_case,
            year,
            rivnum,
            POP,
            Area_POP_ha_per_person,
            Area_POP_m2_per_person,
            ag_area_ha
          ),
        by = c("scenario_case", "year", "rivnum")
      ) %>%
      dplyr::left_join(
        df_basin_input_per_area_all %>%
          dplyr::mutate(
            scenario_case = as.character(scenario_case),
            year = as.integer(year),
            rivnum = as.integer(as.character(rivnum)),
            Ninput_TgN = as.numeric(input_value),
            Ninput_GgN = Ninput_TgN * 1000,
            Ninput_Area_GgN_per_ha = Ninput_GgN / as.numeric(ag_area_ha),
            Ninput_Area_kgN_per_ha = as.numeric(input_kgN_per_ha)
          ) %>%
          dplyr::select(
            scenario_case,
            year,
            rivnum,
            Ninput_TgN,
            Ninput_GgN,
            Ninput_Area_GgN_per_ha,
            Ninput_Area_kgN_per_ha
          ),
        by = c("scenario_case", "year", "rivnum")
      ) %>%
      dplyr::mutate(
        # 恒等式の再構成値
        # POP [person]
        # Area_POP [ha person-1]
        # Ninput_Area [Gg N ha-1 yr-1]
        # ratios [-]
        Nload_reconstructed_GgN =
          POP *
          Area_POP_ha_per_person *
          Ninput_Area_GgN_per_ha *
          Nleach_Ninput *
          Nload_Nleach,

        reconstruction_error_GgN = Nload_reconstructed_GgN - Nload_GgN,
        reconstruction_error_pct = 100 * reconstruction_error_GgN / Nload_GgN
      )

    # =========================================================
    # 2. Check identity
    # =========================================================

    df_factor_identity_all %>%
      dplyr::group_by(scenario_case, year) %>%
      dplyr::summarise(
        n = dplyr::n(),
        mean_abs_error_GgN = mean(abs(reconstruction_error_GgN), na.rm = TRUE),
        max_abs_error_GgN = max(abs(reconstruction_error_GgN), na.rm = TRUE),
        mean_abs_error_pct = mean(abs(reconstruction_error_pct), na.rm = TRUE),
        max_abs_error_pct = max(abs(reconstruction_error_pct), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      print(n = Inf)

    # =========================================================
    # 3. LMDI utility functions
    # =========================================================

    log_mean <- function(x, y) {
      out <- rep(NA_real_, length(x))

      valid <- is.finite(x) & is.finite(y) & x > 0 & y > 0

      equal <- valid & abs(x - y) <= sqrt(.Machine$double.eps) * pmax(abs(x), abs(y), 1)

      out[equal] <- x[equal]

      not_equal <- valid & !equal
      out[not_equal] <- (x[not_equal] - y[not_equal]) / (log(x[not_equal]) - log(y[not_equal]))

      out
    }

    replace_nonpositive_by_epsilon <- function(x) {
      positive_min <- suppressWarnings(min(x[is.finite(x) & x > 0], na.rm = TRUE))

      if (!is.finite(positive_min)) {
        stop("正の値が存在しないため、LMDI 用の epsilon を設定できません。")
      }

      eps <- positive_min * 1e-12

      dplyr::if_else(
        is.finite(x) & x > 0,
        x,
        eps
      )
    }

    # =========================================================
    # 4. Prepare base year and future year pairs
    # =========================================================

    factor_vars <- c(
      "POP",
      "Area_POP_ha_per_person",
      "Ninput_Area_GgN_per_ha",
      "Nleach_Ninput",
      "Nload_Nleach",
      "Nload_reconstructed_GgN",
      "Nload_GgN"
    )

    df_factor_for_lmdi <- df_factor_identity_all %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(factor_vars),
          replace_nonpositive_by_epsilon
        )
      )

    # 2010年基準値
    # 500C についても BaU 2010 を基準にする
    base_2010 <- df_factor_for_lmdi %>%
      dplyr::filter(scenario_case == "BaU", year == 2010) %>%
      dplyr::select(
        rivnum,
        river_basin_name_base = river_basin_name,
        POP_0 = POP,
        Area_POP_0 = Area_POP_ha_per_person,
        Ninput_Area_0 = Ninput_Area_GgN_per_ha,
        Nleach_Ninput_0 = Nleach_Ninput,
        Nload_Nleach_0 = Nload_Nleach,
        Nload_0_GgN = Nload_reconstructed_GgN,
        Nload_original_0_GgN = Nload_GgN
      )

    future_values <- df_factor_for_lmdi %>%
      dplyr::filter(
        year != 2010,
        scenario_case %in% c("BaU", "500C")
      ) %>%
      dplyr::select(
        scenario_case,
        year,
        rivnum,
        river_basin_name,
        POP_t = POP,
        Area_POP_t = Area_POP_ha_per_person,
        Ninput_Area_t = Ninput_Area_GgN_per_ha,
        Nleach_Ninput_t = Nleach_Ninput,
        Nload_Nleach_t = Nload_Nleach,
        Nload_t_GgN = Nload_reconstructed_GgN,
        Nload_original_t_GgN = Nload_GgN
      )

    lmdi_pairs <- future_values %>%
      dplyr::inner_join(base_2010, by = "rivnum") %>%
      dplyr::mutate(
        river_basin_name = dplyr::coalesce(river_basin_name, river_basin_name_base)
      )

      # =========================================================
    # 5. Additive LMDI decomposition
    # =========================================================

    df_lmdi_decomp_summary <- lmdi_pairs %>%
      dplyr::mutate(
        L_weight = log_mean(Nload_t_GgN, Nload_0_GgN),

        contrib_POP_GgN =
          L_weight * log(POP_t / POP_0),

        contrib_Area_POP_GgN =
          L_weight * log(Area_POP_t / Area_POP_0),

        contrib_Ninput_Area_GgN =
          L_weight * log(Ninput_Area_t / Ninput_Area_0),

        contrib_Nleach_Ninput_GgN =
          L_weight * log(Nleach_Ninput_t / Nleach_Ninput_0),

        contrib_Nload_Nleach_GgN =
          L_weight * log(Nload_Nleach_t / Nload_Nleach_0),

        delta_Nload_reconstructed_GgN =
          Nload_t_GgN - Nload_0_GgN,

        delta_Nload_original_GgN =
          Nload_original_t_GgN - Nload_original_0_GgN,

        sum_contribution_GgN =
          contrib_POP_GgN +
          contrib_Area_POP_GgN +
          contrib_Ninput_Area_GgN +
          contrib_Nleach_Ninput_GgN +
          contrib_Nload_Nleach_GgN,

        residual_GgN =
          delta_Nload_reconstructed_GgN - sum_contribution_GgN
      ) %>%
      dplyr::select(
        scenario_case,
        year,
        rivnum,
        river_basin_name,

        Nload_0_GgN,
        Nload_t_GgN,
        delta_Nload_reconstructed_GgN,
        delta_Nload_original_GgN,

        contrib_POP_GgN,
        contrib_Area_POP_GgN,
        contrib_Ninput_Area_GgN,
        contrib_Nleach_Ninput_GgN,
        contrib_Nload_Nleach_GgN,

        sum_contribution_GgN,
        residual_GgN,

        POP_0,
        POP_t,
        Area_POP_0,
        Area_POP_t,
        Ninput_Area_0,
        Ninput_Area_t,
        Nleach_Ninput_0,
        Nleach_Ninput_t,
        Nload_Nleach_0,
        Nload_Nleach_t
      )

    df_lmdi_decomp_summary %>%
      dplyr::group_by(scenario_case, year) %>%
      dplyr::summarise(
        n = dplyr::n(),
        mean_abs_residual_GgN = mean(abs(residual_GgN), na.rm = TRUE),
        max_abs_residual_GgN = max(abs(residual_GgN), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      print(n = Inf)

      # =========================================================
    # 6. Long format for plotting
    # =========================================================

    df_lmdi_decomp_long <- df_lmdi_decomp_summary %>%
      dplyr::select(
        scenario_case,
        year,
        rivnum,
        river_basin_name,
        delta_Nload_reconstructed_GgN,
        dplyr::starts_with("contrib_")
      ) %>%
      tidyr::pivot_longer(
        cols = dplyr::starts_with("contrib_"),
        names_to = "driver",
        values_to = "contribution_GgN"
      ) %>%
      dplyr::mutate(
        driver = dplyr::recode(
          driver,
          "contrib_POP_GgN" = "POP",
          "contrib_Area_POP_GgN" = "Area/POP",
          "contrib_Ninput_Area_GgN" = "Ninput/Area",
          "contrib_Nleach_Ninput_GgN" = "Nleach/Ninput",
          "contrib_Nload_Nleach_GgN" = "Nload/Nleach"
        ),
        driver = factor(
          driver,
          levels = c(
            "POP",
            "Area/POP",
            "Ninput/Area",
            "Nleach/Ninput",
            "Nload/Nleach"
          )
        )
      )

     # =========================================================
# 7. Plot function
#    Top 14 basins selected by absolute Nload in 2010
# =========================================================

clean_river_name <- function(x) {
  x %>%
    stringr::str_replace("_\\(.*$", "") %>%
    stringr::str_replace("\\s*\\(.*\\)$", "") %>%
    stringr::str_replace_all("_", " ")
}

# ---------------------------------------------------------
# Select Top 14 basins by absolute Nload in 2010
# Baseline/BaU 2010 is used as the common reference.
# ---------------------------------------------------------

top_rivers14_lmdi <- df_factor_identity_all %>%
  dplyr::filter(
    scenario_case == "BaU",
    year == 2010
  ) %>%
  dplyr::arrange(dplyr::desc(Nload_GgN)) %>%
  dplyr::slice_head(n = 14) %>%
  dplyr::mutate(
    river_label = clean_river_name(river_basin_name)
  ) %>%
  dplyr::select(
    rivnum,
    river_basin_name,
    river_label,
    Nload_2010_GgN = Nload_GgN
  )

print(top_rivers14_lmdi)

# ---------------------------------------------------------
# Plot LMDI decomposition
# ---------------------------------------------------------

plot_lmdi_decomposition_top14 <- function(scenario_name,
                                          out_dir = "output") {

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  summary_s <- df_lmdi_decomp_summary %>%
    dplyr::filter(scenario_case == scenario_name)

  long_s <- df_lmdi_decomp_long %>%
    dplyr::filter(scenario_case == scenario_name)

  if (nrow(summary_s) == 0) {
    warning("No data for scenario: ", scenario_name)
    return(NULL)
  }

  basin_levels <- top_rivers14_lmdi %>%
    dplyr::arrange(dplyr::desc(Nload_2010_GgN)) %>%
    dplyr::pull(river_label)

  plot_long <- long_s %>%
    dplyr::inner_join(
      top_rivers14_lmdi %>%
        dplyr::select(rivnum, river_label),
      by = "rivnum"
    ) %>%
    dplyr::mutate(
      river_label = factor(river_label, levels = rev(basin_levels)),
      year_f = factor(year, levels = c(2030, 2050, 2100))
    )

  plot_total <- summary_s %>%
    dplyr::inner_join(
      top_rivers14_lmdi %>%
        dplyr::select(rivnum, river_label),
      by = "rivnum"
    ) %>%
    dplyr::mutate(
      river_label = factor(river_label, levels = rev(basin_levels)),
      year_f = factor(year, levels = c(2030, 2050, 2100))
    )

  p <- ggplot2::ggplot(
    plot_long,
    ggplot2::aes(
      x = river_label,
      y = contribution_GgN,
      fill = driver
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_point(
      data = plot_total,
      ggplot2::aes(
        x = river_label,
        y = delta_Nload_reconstructed_GgN
      ),
      inherit.aes = FALSE,
      size = 1.8
    ) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(
      ~ year_f,
      ncol = 3,
      scales = "free_y"
    ) +
    ggplot2::theme_bw(base_size = 18) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border     = ggplot2::element_blank(),
      axis.line        = ggplot2::element_line(colour = "black"),
      legend.position  = "bottom",
      strip.background = ggplot2::element_rect(fill = "grey90", colour = NA),
      strip.text       = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::labs(
      title = paste0("Decomposition of river N load change from 2010: ", scenario_name),
      subtitle = "Top 14 basins selected by absolute N load in 2010. Points indicate total ΔNload.",
      x = "River basin",
      y = expression(paste("Contribution to ", Delta, "N load [Gg N yr"^{-1}, "]")),
      fill = "Factor"
    )

  print(p)

  ggplot2::ggsave(
    filename = file.path(
      out_dir,
      paste0("LMDI_Nload_decomposition_", scenario_name, "_top14_absNload2010.png")
    ),
    plot = p,
    width = 15,
    height = 8,
    dpi = 300,
    bg = "white"
  )

  p
}

# =========================================================
# 8. Draw plots
# =========================================================

p_lmdi_BaU <- plot_lmdi_decomposition_top14(
  scenario_name = "BaU",
  out_dir = "output"
)

p_lmdi_500C <- plot_lmdi_decomposition_top14(
  scenario_name = "500C",
  out_dir = "output"
)

    # =========================================================
    # 9. Save decomposition results
    # =========================================================

    dir.create("Data_pd", showWarnings = FALSE, recursive = TRUE)

    save(
      df_factor_identity_all,
      df_lmdi_decomp_summary,
      df_lmdi_decomp_long,
      file = "Data_pd/df_Nload_factor_decomposition_LMDI.rdat"
    )

    readr::write_csv(
      df_factor_identity_all,
      "Data_pd/df_Nload_factor_identity_all.csv"
    )

    readr::write_csv(
      df_lmdi_decomp_summary,
      "Data_pd/df_Nload_factor_decomposition_LMDI_summary.csv"
    )

    readr::write_csv(
      df_lmdi_decomp_long,
      "Data_pd/df_Nload_factor_decomposition_LMDI_long.csv"
    )
