library(dplyr)
library(purrr)
library(tibble)
library(readr)

BASE_SCENARIOset <- "SSP2_BaU_NoCC"
MIT_SCENARIOset  <- "SSP2_500C_CACN_NoCC"

run_table_result <- tibble::tribble(
  ~SCENARIO,  ~SCENARIOset,      ~YEAR,
  "Base2010", BASE_SCENARIOset,  "2010",
  "Base2030", BASE_SCENARIOset,  "2030",
  "Base2050", BASE_SCENARIOset,  "2050",
  "Base2100", BASE_SCENARIOset,  "2100",
  "Mit2030",  MIT_SCENARIOset,   "2030",
  "Mit2050",  MIT_SCENARIOset,   "2050",
  "Mit2100",  MIT_SCENARIOset,   "2100"
)

run_table_exec <- bind_rows(
  run_table_result,
  tibble::tibble(
    SCENARIO    = "Base2020",
    SCENARIOset = BASE_SCENARIOset,
    YEAR        = "2020"
  )
) %>%
  distinct(SCENARIOset, YEAR, .keep_all = TRUE)

scenario_info <- tibble::tribble(
  ~scenario_tag, ~SCENARIOset,
  "BaU",         BASE_SCENARIOset,
  "500C",        MIT_SCENARIOset
)

get_scenario_tag <- function(scenario_set) {
  scenario_tag <- scenario_info$scenario_tag[
    match(scenario_set, scenario_info$SCENARIOset)
  ]

  if (is.na(scenario_tag)) {
    stop("未登録のSCENARIOsetです: ", scenario_set)
  }

  scenario_tag
}

to_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

filter_by_year_scenario <- function(df,
                                    year = NULL,
                                    scenario_set = NULL,
                                    year_col = NULL,
                                    scenario_col = NULL) {
  if (!is.null(year_col)) {
    if (!year_col %in% names(df)) {
      stop("列が見つかりません: ", year_col)
    }

    df <- df %>%
      filter(as.character(.data[[year_col]]) == as.character(year))
  }

  if (!is.null(scenario_col)) {
    if (!scenario_col %in% names(df)) {
      stop("列が見つかりません: ", scenario_col)
    }

    df <- df %>%
      filter(as.character(.data[[scenario_col]]) == as.character(scenario_set))
  }

  df
}

get_value <- function(env,
                      obj_name,
                      value_col,
                      year = NULL,
                      scenario_set = NULL,
                      year_col = NULL,
                      scenario_col = NULL,
                      use_first = FALSE) {
  if (!exists(obj_name, envir = env)) {
    stop("オブジェクトが見つかりません: ", obj_name)
  }

  df <- get(obj_name, envir = env)

  if (!value_col %in% names(df)) {
    stop(obj_name, " に列 ", value_col, " が見つかりません。")
  }

  df2 <- filter_by_year_scenario(
    df = df,
    year = year,
    scenario_set = scenario_set,
    year_col = year_col,
    scenario_col = scenario_col
  )

  if (nrow(df2) == 0) {
    stop(
      "該当行が見つかりません: ",
      obj_name,
      " / ",
      value_col,
      " / YEAR = ",
      year,
      " / SCENARIOset = ",
      scenario_set
    )
  }

  values <- to_num(df2[[value_col]])

  if (use_first) {
    return(values[1])
  }

  sum(values, na.rm = TRUE)
}

extract_nflow_one_case <- function(env, scenario_label, scenario_set, year) {
  # Fertilizer
  INfer_total <- get_value(
    env, "INfer", "INfer",
    year = year,
    scenario_set = scenario_set,
    year_col = "YEMF",
    scenario_col = "SCENARIO"
  )

  INfer_crop <- get_value(
    env, "INfercrop", "INfercrop",
    year = year,
    scenario_set = scenario_set,
    year_col = "YEMF",
    scenario_col = "SCENARIO"
  )

  INfer_pas <- get_value(
    env, "INferpas", "INferpas",
    year = year,
    scenario_set = scenario_set,
    year_col = "YEMF",
    scenario_col = "SCENARIO"
  )

  # Manure
  INman_total <- get_value(
    env, "INman", "INman",
    year = year,
    scenario_set = scenario_set,
    year_col = "YEMF",
    scenario_col = "SCENARIO"
  )

  INman_crop <- get_value(
    env, "INmancrop", "INman",
    year = year,
    scenario_set = scenario_set,
    year_col = "YEMF",
    scenario_col = "SCENARIO"
  )

  INman_pas <- get_value(
    env, "INmanpas", "INman",
    year = year,
    scenario_set = scenario_set,
    year_col = "YEMF",
    scenario_col = "SCENARIO"
  )

  # Deposition
  INdep_total <- get_value(
    env, "INdep", "INdep",
    use_first = TRUE
  )

  INdep_crop <- get_value(
    env, "INdep", "INdepcrop",
    use_first = TRUE
  )

  INdep_pas <- get_value(
    env, "INdep", "INdeppas",
    use_first = TRUE
  )

  # Fixation
  INfix_total <- get_value(
    env, "INfix", "INfix",
    year = year,
    year_col = "i2"
  )

  INfix_crop <- get_value(
    env, "INfix", "N_fix_crop",
    year = year,
    year_col = "i2"
  )

  INfix_pas <- get_value(
    env, "INfix", "N_fix_pas",
    year = year,
    year_col = "i2"
  )

  # Crop uptake / recovery
  OUTcrop_total <- get_value(
    env, "OUTcrop", "OUTcrop",
    year = year,
    year_col = "YEMF"
  )

  OUTcrop_crop <- get_value(
    env, "OUTcropcroptotal", "OUTcrop",
    year = year,
    year_col = "YEMF"
  )

  OUTcrop_pas <- get_value(
    env, "OUTcroppastotal", "OUTcrop",
    year = year,
    year_col = "YEMF"
  )

  # Residue
  OUTres_total <- get_value(
    env, "OUTres", "OUTres",
    year = year,
    year_col = "YEMF"
  )

  OUTres_crop <- OUTres_total
  OUTres_pas <- 0

  # Gas
  OUTgas_total <- get_value(
    env, "OUTgas", "OUTgas",
    year = year,
    scenario_set = scenario_set,
    year_col = "YEMF",
    scenario_col = "SCENARIO"
  )

  OUTgas_crop <- get_value(
    env, "OUTgascrop", "OUTgascrop",
    year = year,
    scenario_set = scenario_set,
    year_col = "YEMF",
    scenario_col = "SCENARIO"
  )

  OUTgas_pas <- get_value(
    env, "OUTgaspas", "OUTgaspas",
    year = year,
    scenario_set = scenario_set,
    year_col = "YEMF",
    scenario_col = "SCENARIO"
  )

  # Leaching
  OUTlea_total <- get_value(
    env, "OUTlea", "OUTlea",
    year = year,
    year_col = "YEMF"
  )

  OUTlea_crop <- get_value(
    env, "OUTleacrop", "OUTleacrop",
    year = year,
    year_col = "YEMF"
  )

  OUTlea_pas <- get_value(
    env, "OUTleapas", "OUTleapas",
    year = year,
    year_col = "YEMF"
  )

  tibble::tribble(
    ~Nf,        ~SCENARIO,       ~Cropland,     ~Pasture,     ~Total,
    "INfer",   scenario_label,   INfer_crop,    INfer_pas,    INfer_total,
    "INman",   scenario_label,   INman_crop,    INman_pas,    INman_total,
    "INdep",   scenario_label,   INdep_crop,    INdep_pas,    INdep_total,
    "INfix",   scenario_label,   INfix_crop,    INfix_pas,    INfix_total,
    "OUTcrop", scenario_label,   OUTcrop_crop,  OUTcrop_pas,  OUTcrop_total,
    "OUTres",  scenario_label,   OUTres_crop,   OUTres_pas,   OUTres_total,
    "OUTgas",  scenario_label,   OUTgas_crop,   OUTgas_pas,   OUTgas_total,
    "OUTlea",  scenario_label,   OUTlea_crop,   OUTlea_pas,   OUTlea_total
  )
}

get_value_safe <- function(env,
                           obj_name,
                           value_col,
                           year,
                           scenario_set,
                           year_col = "YEMF",
                           scenario_col = "SCENARIO") {
  tryCatch(
    {
      get_value(
        env = env,
        obj_name = obj_name,
        value_col = value_col,
        year = year,
        scenario_set = scenario_set,
        year_col = year_col,
        scenario_col = scenario_col
      )
    },
    error = function(e) {
      warning(
        "取得できませんでした: ",
        obj_name,
        " / ",
        value_col,
        " / YEAR = ",
        year,
        " / SCENARIOset = ",
        scenario_set,
        " -> NA\n",
        conditionMessage(e)
      )
      NA_real_
    }
  )
}


extract_outgas_species_one_case <- function(env,
                                            scenario_label,
                                            scenario_set,
                                            year) {
  total <- tibble::tibble(
    Species = "Total",
    Cropland = get_value(
      env, "OUTgascrop", "OUTgascrop",
      year = year,
      scenario_set = scenario_set,
      year_col = "YEMF",
      scenario_col = "SCENARIO"
    ),
    Pasture = get_value(
      env, "OUTgaspas", "OUTgaspas",
      year = year,
      scenario_set = scenario_set,
      year_col = "YEMF",
      scenario_col = "SCENARIO"
    ),
    Total = get_value(
      env, "OUTgas", "OUTgas",
      year = year,
      scenario_set = scenario_set,
      year_col = "YEMF",
      scenario_col = "SCENARIO"
    )
  )

  nh3 <- tibble::tibble(
    Species = "NH3",
    Cropland = get_value(
      env, "OUTgascrop_NH3", "OUTgascrop",
      year = year,
      scenario_set = scenario_set,
      year_col = "YEMF",
      scenario_col = "SCENARIO"
    ),
    Pasture = get_value(
      env, "OUTgaspas_NH3", "OUTgaspas",
      year = year,
      scenario_set = scenario_set,
      year_col = "YEMF",
      scenario_col = "SCENARIO"
    ),
    Total = get_value(
      env, "OUTgas_NH3", "OUTgas",
      year = year,
      scenario_set = scenario_set,
      year_col = "YEMF",
      scenario_col = "SCENARIO"
    )
  )

  # NH3 emitted before manure N enters the agricultural-soil budget is
  # calculated separately near the end of RawDataProcess.R.  NH3_loss1 is
  # volatilization from manure management systems (MMS), whereas NH3_loss2 is
  # volatilization associated with direct excretion on pasture.  OUTgas_NH3
  # above is the subsequent agricultural-soil emission from fertilizer and
  # retained manure N, so the three terms do not overlap.
  nh3_mms_total <- get_value(
    env, "NH3_loss1", "ManureStable"
  )
  nh3_direct_excretion_total <- get_value(
    env, "NH3_loss2", "ManurePas"
  )

  nh3_mms <- tibble::tibble(
    Species = "NH3_MMS",
    Cropland = 0,
    Pasture = 0,
    Manure_management_systems = nh3_mms_total,
    Direct_excretion = 0,
    Total = nh3_mms_total
  )

  nh3_direct_excretion <- tibble::tibble(
    Species = "NH3_direct_excretion",
    Cropland = 0,
    Pasture = 0,
    Manure_management_systems = 0,
    Direct_excretion = nh3_direct_excretion_total,
    Total = nh3_direct_excretion_total
  )

  nh3_agriculture_total <- tibble::tibble(
    Species = "NH3_agriculture_total",
    Cropland = nh3$Cropland,
    Pasture = nh3$Pasture,
    Manure_management_systems = nh3_mms_total,
    Direct_excretion = nh3_direct_excretion_total,
    Total = nh3$Total + nh3_mms_total + nh3_direct_excretion_total
  )

  n2 <- tibble::tibble(
    Species = "N2",
    Cropland = get_value(
      env, "OUTgascrop_N2", "OUTgascrop",
      year = year,
      scenario_set = scenario_set,
      year_col = "YEMF",
      scenario_col = "SCENARIO"
    ),
    Pasture = get_value(
      env, "OUTgaspas_N2", "OUTgaspas",
      year = year,
      scenario_set = scenario_set,
      year_col = "YEMF",
      scenario_col = "SCENARIO"
    ),
    Total = get_value(
      env, "OUTgas_N2", "OUTgas",
      year = year,
      scenario_set = scenario_set,
      year_col = "YEMF",
      scenario_col = "SCENARIO"
    )
  )

  nox_n2o_total <- tibble::tibble(
    Species = "NOx_N2O_total",
    Cropland = get_value(
      env, "OUTgascrop_NOx", "OUTgascrop",
      year = year,
      scenario_set = scenario_set,
      year_col = "YEMF",
      scenario_col = "SCENARIO"
    ),
    Pasture = get_value(
      env, "OUTgaspas_NOx", "OUTgaspas",
      year = year,
      scenario_set = scenario_set,
      year_col = "YEMF",
      scenario_col = "SCENARIO"
    ),
    Total = get_value(
      env, "OUTgas_NOx", "OUTgas",
      year = year,
      scenario_set = scenario_set,
      year_col = "YEMF",
      scenario_col = "SCENARIO"
    )
  )

  nox <- tibble::tibble(
    Species = "NOx",
    Cropland = nox_n2o_total$Cropland * 5 / 14,
    Pasture  = nox_n2o_total$Pasture  * 5 / 14,
    Total    = nox_n2o_total$Total    * 5 / 14
  )

  n2o <- tibble::tibble(
    Species = "N2O",
    Cropland = nox_n2o_total$Cropland * 9 / 14,
    Pasture  = nox_n2o_total$Pasture  * 9 / 14,
    Total    = nox_n2o_total$Total    * 9 / 14
  )

  dplyr::bind_rows(
    n2,
    nh3,
    nh3_mms,
    nh3_direct_excretion,
    nh3_agriculture_total,
    nox,
    n2o,
    total
  ) %>%
    dplyr::mutate(
      Manure_management_systems = dplyr::coalesce(
        Manure_management_systems,
        0
      ),
      Direct_excretion = dplyr::coalesce(Direct_excretion, 0),
      Nf = "OUTgas",
      SCENARIO = scenario_label,
      SCENARIOset = scenario_set,
      YEAR = as.character(year)
    ) %>%
    dplyr::select(
      Nf, Species, SCENARIO, SCENARIOset, YEAR,
      Cropland, Pasture, Manure_management_systems,
      Direct_excretion, Total
    )
}

outgas_species_basis <- c(
  "N2" = "Direct model output: OUTgas_N2",
  "NH3" = "Agricultural-soil output: OUTgas_NH3",
  "NOx" = "5/14 of model OUTgas_NOx pool",
  "N2O" = "9/14 of model OUTgas_NOx pool",
  "Total" = "Direct model output: OUTgas"
)

outgas_species_csv_basis <- c(
  outgas_species_basis,
  "NH3_MMS" = paste0(
    "NH3_loss1: volatilization from manure management systems before ",
    "manure application"
  ),
  "NH3_direct_excretion" = paste0(
    "NH3_loss2: volatilization associated with direct excretion on ",
    "pasture before retained N enters the soil budget"
  ),
  "NH3_agriculture_total" = paste0(
    "OUTgas_NH3 + NH3_loss1 + NH3_loss2: total agricultural-sector ",
    "NH3 emission"
  )
)

get_map_rg_for_case <- function(env) {
  if (exists("MAP_RG", envir = env, inherits = FALSE)) {
    return(get("MAP_RG", envir = env, inherits = FALSE))
  }

  if (exists("MAP_RG", envir = globalenv(), inherits = FALSE)) {
    return(get("MAP_RG", envir = globalenv(), inherits = FALSE))
  }

  stop("MAP_RG が見つからないため、17地域別のガス排出量を集計できません。")
}

extract_outgas_species_region_one_case <- function(env,
                                                   scenario_label,
                                                   scenario_set,
                                                   year) {
  MAP_RG_local <- get_map_rg_for_case(env)

  species_specs <- tibble::tribble(
    ~Species, ~object_suffix, ~fraction,
    "N2",    "N2",          1,
    "NH3",   "NH3",         1,
    "NOx",   "NOx",         5 / 14,
    "N2O",   "NOx",         9 / 14,
    "Total", "",             1
  )

  land_specs <- tibble::tribble(
    ~Land_type,  ~object_stem, ~value_col,
    "Cropland", "OUTgascrop", "OUTgascropgrid",
    "Pasture",  "OUTgaspas",  "OUTgaspasgrid",
    "Total",    "OUTgas",     "OUTgasgrid"
  )

  purrr::pmap_dfr(
    species_specs,
    function(Species, object_suffix, fraction) {
      species_value <- Species

      purrr::pmap_dfr(
        land_specs,
        function(Land_type, object_stem, value_col) {
          land_type_value <- Land_type

          object_name <- if (nzchar(object_suffix)) {
            paste0(object_stem, "dis_", object_suffix)
          } else {
            paste0(object_stem, "dis")
          }

          if (!exists(object_name, envir = env, inherits = FALSE)) {
            stop("ガス化学種の地域集計に必要なオブジェクトがありません: ", object_name)
          }

          gas_grid <- get(object_name, envir = env, inherits = FALSE)

          if (!value_col %in% names(gas_grid)) {
            stop(object_name, " に列 ", value_col, " がありません。")
          }

          gas_grid_case <- gas_grid %>%
            filter(as.character(YEMF) == as.character(year))

          # Some cropland/pasture grid objects are already scoped to the
          # scenario-specific RawDataProcess environment and therefore do not
          # retain a SCENARIO column.  Filter it only when it is present.
          if ("SCENARIO" %in% names(gas_grid_case)) {
            gas_grid_case <- gas_grid_case %>%
              filter(as.character(SCENARIO) == as.character(scenario_set))
          }

          gas_grid_case %>%
            left_join(MAP_RG_local, by = "G") %>%
            filter(!is.na(R)) %>%
            group_by(R) %>%
            summarise(
              Value_Tg_N_yr = sum(.data[[value_col]], na.rm = TRUE) * fraction,
              .groups = "drop"
            ) %>%
            mutate(
              SCENARIO = scenario_label,
              SCENARIOset = scenario_set,
              YEAR = as.integer(year),
              Species = species_value,
              Land_type = land_type_value,
              Calculation_basis = unname(
                outgas_species_basis[[species_value]]
              )
            ) %>%
            select(
              R, SCENARIO, SCENARIOset, YEAR, Species, Land_type,
              Value_Tg_N_yr, Calculation_basis
            )
        }
      )
    }
  )
}

copy_if_exists <- function(env, src_candidates, dst_name) {
  for (src in src_candidates) {
    if (exists(src, envir = env)) {
      assign(dst_name, get(src, envir = env), envir = globalenv())
      return(TRUE)
    }
  }

  warning(
    "No object found for: ",
    dst_name,
    " / Search candidates: ",
    paste(src_candidates, collapse = ", ")
  )

  FALSE
}

save_outleadis_for_visitorn <- function(env, scenario_set, year) {
  year_chr <- as.character(year)

  if (!year_chr %in% c("2010", "2030", "2050", "2100")) {
    return(invisible(NULL))
  }

  if (year_chr == "2010" && scenario_set != BASE_SCENARIOset) {
    return(invisible(NULL))
  }

  if (!scenario_set %in% c(BASE_SCENARIOset, MIT_SCENARIOset)) {
    return(invisible(NULL))
  }

  if (!exists("OUTleadis", envir = env)) {
    warning(
      "OUTleadis が見つからないため保存できません: ",
      scenario_set,
      " / ",
      year_chr
    )
    return(invisible(NULL))
  }

  out_dir <- "../../VISIToRN/Data_pd"
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # BaU: OUTleadis2030.rdat
  # 500C: OUTleadis2030mit.rdat
  file_suffix <- if (scenario_set == MIT_SCENARIOset) "mit" else ""

  out_file <- file.path(
    out_dir,
    paste0("OUTleadis", year_chr, file_suffix, ".rdat")
  )

  save(
    list = "OUTleadis",
    file = out_file,
    envir = env
  )

  message("Saved OUTleadis: ", out_file)

  invisible(NULL)
}


copy_plot_objects <- function(env, scenario_set, year) {
  year_chr <- as.character(year)
  scenario_tag <- get_scenario_tag(scenario_set)

MAP_RG_local <- NULL

if (exists("MAP_RG", envir = env)) {
  MAP_RG_local <- get("MAP_RG", envir = env)

} else if (exists("MAP_RG", envir = globalenv())) {
  MAP_RG_local <- get("MAP_RG", envir = globalenv())

} else if (exists("data_prep", envir = env) && exists("rgdx.set")) {
  MAP_RG_local <- rgdx.set(get("data_prep", envir = env), "MAP_RG")

} else if (exists("data_prep", envir = globalenv()) && exists("rgdx.set")) {
  MAP_RG_local <- rgdx.set(get("data_prep", envir = globalenv()), "MAP_RG")
}

if (!is.null(MAP_RG_local)) {
  assign("MAP_RG", MAP_RG_local, envir = globalenv())
} else {
  warning(
    "MAP_RGを作成できませんでした。Input_R_BaU_2010などは作成されません。"
  )
}

  if (
    scenario_set == BASE_SCENARIOset &&
    year_chr %in% c("2010", "2020")
  ) {
    if (exists("OUTgasdis_NH3", envir = env)) {
      assign(
        paste0("OUTgasdis_NH3_", year_chr),
        get("OUTgasdis_NH3", envir = env),
        envir = globalenv()
      )

      assign(
        paste0("OUTgasdis_NH3_", scenario_tag, "_", year_chr),
        get("OUTgasdis_NH3", envir = env),
        envir = globalenv()
      )
    } else {
      warning(
        "OUTgasdis_NH3 が見つかりません: ",
        scenario_set,
        " / ",
        year_chr
      )
    }
  }

  # ------------------------------------------------------------
  # Input/Output地域図用
  # こちらは2010,2030,2050,2100だけ保存する
  # 2020はNH3図専用なのでここで終了してよい
  # ------------------------------------------------------------
  if (!year_chr %in% c("2010", "2030", "2050", "2100")) {
    return(invisible(NULL))
  }

  save_with_scenario_year <- function(obj_name) {
    if (exists(obj_name, envir = env)) {
      assign(
        paste0(obj_name, "_", scenario_tag, "_", year_chr),
        get(obj_name, envir = env),
        envir = globalenv()
      )
    } else {
      warning(
        "作図用オブジェクトが見つかりません: ",
        obj_name,
        " / ",
        scenario_set,
        " / ",
        year_chr
      )
    }
  }

  save_with_scenario_year("Inputdis")

  save_with_scenario_year("INferdis")
  save_with_scenario_year("INmandis")
  save_with_scenario_year("INdepdis")
  save_with_scenario_year("INfixdis")

  save_with_scenario_year("OUTcropdis")
  save_with_scenario_year("OUTresdis")
  save_with_scenario_year("OUTgasdis")
  save_with_scenario_year("OUTleadis")

    save_with_scenario_year("Inputdis")
    save_with_scenario_year("OUTgasdis")
    save_with_scenario_year("VY_load2")

    if (exists("MAP_GIJ", envir = env)) {
    assign("MAP_GIJ", get("MAP_GIJ", envir = env), envir = globalenv())
    }

    if (exists("gridarea2", envir = env)) {
    assign("gridarea2", get("gridarea2", envir = env), envir = globalenv())
    }

  if (exists("Inputdis", envir = env) && !is.null(MAP_RG_local)) {
    Input_R_one <- get("Inputdis", envir = env) %>%
      left_join(MAP_RG_local, by = "G") %>%
      filter(as.character(YEMF) == year_chr) %>%
      group_by(R) %>%
      summarise(
        Input = sum(Input, na.rm = TRUE),
        .groups = "drop"
      )

    if (year_chr == "2010") {
      Input_R_one <- Input_R_one %>%
        mutate(Input = Input * (238 / 243))
    }

    names(Input_R_one)[names(Input_R_one) == "Input"] <- paste0("Input", year_chr)

    assign(
      paste0("Input_R_", scenario_tag, "_", year_chr),
      Input_R_one,
      envir = globalenv()
    )
  }

  invisible(NULL)
}

save_manure_diagnostics <- function(env, scenario_set, year) {
  diagnostic_objects <- c(
    "nflow_livestock_diagnostics",
    "nflow_manure_allocation_diagnostics"
  )
  diagnostic_dir <- "../output/manure_diagnostics"
  dir.create(diagnostic_dir, showWarnings = FALSE, recursive = TRUE)

  for (object_name in diagnostic_objects) {
    if (!exists(object_name, envir = env, inherits = FALSE)) {
      warning("Manure diagnostic object was not created: ", object_name)
      next
    }
    output_file <- file.path(
      diagnostic_dir,
      paste0(object_name, "_", scenario_set, "_", year, ".csv")
    )
    write_csv(get(object_name, envir = env), output_file)
    message("Saved manure diagnostics: ", output_file)
  }

  invisible(NULL)
}

run_rawdataprocess_once <- function(scenario_label,
                                    scenario_set,
                                    year,
                                    make_result = TRUE) {
  message("Running: ", scenario_set, " / ", year, " -> ", scenario_label)

  env <- new.env(parent = globalenv())

  env$SCENARIOset <- scenario_set
  env$YEAR <- as.character(year)

  sys.source("RawDataProcess.R", envir = env)

  save_manure_diagnostics(
    env = env,
    scenario_set = scenario_set,
    year = year
  )

  save_outleadis_for_visitorn(
    env = env,
    scenario_set = scenario_set,
    year = year
  )

  copy_plot_objects(
    env = env,
    scenario_set = scenario_set,
    year = year
  )

  if (!make_result) {
    return(list(
      nflow = tibble(
        Nf = character(),
        SCENARIO = character(),
        Cropland = numeric(),
        Pasture = numeric(),
        Total = numeric()
      ),
      outgas_species = tibble(
        Nf = character(),
        Species = character(),
        SCENARIO = character(),
        SCENARIOset = character(),
        YEAR = character(),
        Cropland = numeric(),
        Pasture = numeric(),
        Manure_management_systems = numeric(),
        Direct_excretion = numeric(),
        Total = numeric()
      ),
      outgas_species_region = tibble(
        R = character(),
        SCENARIO = character(),
        SCENARIOset = character(),
        YEAR = integer(),
        Species = character(),
        Land_type = character(),
        Value_Tg_N_yr = numeric(),
        Calculation_basis = character()
      )
    ))
  }

  list(
    nflow = extract_nflow_one_case(
      env = env,
      scenario_label = scenario_label,
      scenario_set = scenario_set,
      year = year
    ),
    outgas_species = extract_outgas_species_one_case(
      env = env,
      scenario_label = scenario_label,
      scenario_set = scenario_set,
      year = year
    ),
    outgas_species_region = extract_outgas_species_region_one_case(
      env = env,
      scenario_label = scenario_label,
      scenario_set = scenario_set,
      year = year
    )
  )
}


run_outputs <- run_table_exec %>%
  pmap(function(SCENARIO, SCENARIOset, YEAR) {
    run_rawdataprocess_once(
      scenario_label = SCENARIO,
      scenario_set = SCENARIOset,
      year = YEAR,
      make_result = SCENARIO %in% run_table_result$SCENARIO
    )
  })


Result <- run_outputs %>%
  map_dfr("nflow") %>%
  mutate(
    SCENARIO = factor(SCENARIO, levels = run_table_result$SCENARIO),
    Cropland = round(Cropland, 1),
    Pasture  = round(Pasture, 1),
    Total    = round(Total, 1)
  ) %>%
  arrange(SCENARIO) %>%
  select(Nf, SCENARIO, Cropland, Pasture, Total)

OUTgas_species_all_unrounded <- run_outputs %>%
  map_dfr("outgas_species")

soil_gas_species <- c("N2", "NH3", "NOx", "N2O", "Total")

# Keep the original soil-gas table for chemical-species reconciliation and
# the global/17-region summary.  The public OUTgas_species.csv additionally
# reports the non-soil NH3 sources and their agricultural-sector sum.
OUTgas_species_unrounded <- OUTgas_species_all_unrounded %>%
  filter(Species %in% soil_gas_species)

OUTgas_species <- OUTgas_species_all_unrounded %>%
  mutate(
    SCENARIO = factor(SCENARIO, levels = run_table_result$SCENARIO),
    Species = factor(
      Species,
      levels = c(
        "N2", "NH3", "NH3_MMS", "NH3_direct_excretion",
        "NH3_agriculture_total", "NOx", "N2O", "Total"
      )
    ),
    Cropland = round(Cropland, 1),
    Pasture  = round(Pasture, 1),
    Manure_management_systems = round(Manure_management_systems, 1),
    Direct_excretion = round(Direct_excretion, 1),
    Calculation_basis = unname(
      outgas_species_csv_basis[as.character(Species)]
    ),
    Unit = "Tg N yr^-1",
    Total    = round(Total, 1)
  ) %>%
  arrange(SCENARIO, Species) %>%
  select(
    Nf, Species, SCENARIO, SCENARIOset, YEAR,
    Cropland, Pasture, Manure_management_systems,
    Direct_excretion, Total, Unit, Calculation_basis
  )

OUTgas_species_17regions_long <- run_outputs %>%
  map_dfr("outgas_species_region") %>%
  mutate(
    SCENARIO = factor(SCENARIO, levels = run_table_result$SCENARIO),
    Species = factor(Species, levels = c("N2", "NH3", "NOx", "N2O", "Total")),
    Land_type = factor(Land_type, levels = c("Cropland", "Pasture", "Total")),
    Value_Tg_N_yr = round(Value_Tg_N_yr, 6)
  ) %>%
  arrange(SCENARIO, R, Species, Land_type)

OUTgas_species_17regions <- OUTgas_species_17regions_long %>%
  select(
    R, SCENARIO, SCENARIOset, YEAR, Species, Land_type,
    Value_Tg_N_yr, Calculation_basis
  ) %>%
  tidyr::pivot_wider(
    names_from = Land_type,
    values_from = Value_Tg_N_yr
  ) %>%
  arrange(SCENARIO, R, Species)

OUTgas_species_global_long <- OUTgas_species_unrounded %>%
  mutate(
    Spatial_level = "Global",
    Region = "Global",
    YEAR = as.integer(YEAR),
    Calculation_basis = unname(outgas_species_basis[Species])
  ) %>%
  tidyr::pivot_longer(
    cols = c(Cropland, Pasture, Total),
    names_to = "Land_type",
    values_to = "Value_Tg_N_yr"
  ) %>%
  select(
    Spatial_level, Region, SCENARIO, SCENARIOset, YEAR, Species,
    Land_type, Value_Tg_N_yr, Calculation_basis
  )

OUTgas_species_summary <- OUTgas_species_17regions_long %>%
  transmute(
    Spatial_level = "AIM17",
    Region = R,
    SCENARIO = as.character(SCENARIO),
    SCENARIOset,
    YEAR,
    Species = as.character(Species),
    Land_type = as.character(Land_type),
    Value_Tg_N_yr,
    Calculation_basis
  ) %>%
  bind_rows(
    OUTgas_species_global_long %>%
      mutate(
        SCENARIO = as.character(SCENARIO),
        Species = as.character(Species),
        Land_type = as.character(Land_type)
      )
  ) %>%
  mutate(
    Value_Tg_N_yr = round(Value_Tg_N_yr, 6),
    Unit = "Tg N yr^-1"
  ) %>%
  arrange(SCENARIO, Spatial_level, Region, Species, Land_type)

OUTgas_species_validation <- OUTgas_species_summary %>%
  group_by(
    Spatial_level, Region, SCENARIO, SCENARIOset, YEAR, Land_type
  ) %>%
  summarise(
    Component_sum_Tg_N_yr = sum(
      Value_Tg_N_yr[Species != "Total"],
      na.rm = TRUE
    ),
    Reported_total_Tg_N_yr = Value_Tg_N_yr[Species == "Total"][1],
    Difference_Tg_N_yr = Component_sum_Tg_N_yr - Reported_total_Tg_N_yr,
    Pass = abs(Difference_Tg_N_yr) <= 1e-5,
    .groups = "drop"
  )

if (any(!OUTgas_species_validation$Pass)) {
  warning(
    "ガス化学種の合計とOUTgasの差が許容値を超えた行があります。",
    " OUTgas_species_validation.csv を確認してください。"
  )
}


dir.create("../data", showWarnings = FALSE, recursive = TRUE)

write_csv(Result, "../data/Result.csv")
write_csv(OUTgas_species, "../data/OUTgas_species.csv")
write_csv(OUTgas_species_17regions, "../data/OUTgas_species_17regions.csv")
write_csv(OUTgas_species_summary, "../data/OUTgas_species_summary.csv")
write_csv(OUTgas_species_validation, "../data/OUTgas_species_validation.csv")
write_csv(Result, "../output/Result.csv")
write_csv(OUTgas_species, "../output/OUTgas_species.csv")
write_csv(OUTgas_species_17regions, "../output/OUTgas_species_17regions.csv")
write_csv(OUTgas_species_summary, "../output/OUTgas_species_summary.csv")
write_csv(OUTgas_species_validation, "../output/OUTgas_species_validation.csv")
