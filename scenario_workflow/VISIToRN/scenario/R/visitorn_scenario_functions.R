plot_text_scale <- 2

# These helpers are also sourced directly by the lightweight batch test, so
# set the bitmap backend here rather than relying only on the main runner.
# This prevents grDevices::png() from trying to open an X11 display on a
# headless compute node.
if (isTRUE(capabilities("cairo"))) {
  options(bitmapType = "cairo")
} else if (!nzchar(Sys.getenv("DISPLAY"))) {
  stop(
    "PNG plotting requires Cairo on a headless node, but this R build has ",
    "no Cairo support."
  )
}

load_rdata_environment <- function(path) {
  if (!file.exists(path)) {
    stop("Required RData file does not exist: ", path)
  }
  environment <- new.env(parent = emptyenv())
  load(path, envir = environment)
  environment
}

file_content_signature <- function(paths) {
  paths <- unique(normalizePath(
    paths,
    winslash = "/",
    mustWork = TRUE
  ))
  hashes <- unname(tools::md5sum(paths))
  if (anyNA(hashes)) {
    stop("Could not calculate an input checksum.")
  }
  paste(paste(paths, hashes, sep = "="), collapse = "|")
}

load_single_array <- function(path) {
  environment <- load_rdata_environment(path)
  names_found <- ls(environment, all.names = TRUE)
  arrays <- names_found[vapply(names_found, function(name) {
    value <- get(name, envir = environment)
    is.array(value) && length(dim(value)) == 3L
  }, logical(1))]
  if (length(arrays) != 1L) {
    stop(
      "Expected one three-dimensional array in ", path,
      "; found: ", paste(arrays, collapse = ", ")
    )
  }
  get(arrays[[1]], envir = environment)
}

normalize_natural_land_scenario <- function(scenario) {
  scenario <- as.character(scenario)[1]
  aliases <- c(
    SSP2_BaU_NoCC = "SSP2_BaU_NoCC_No",
    SSP2_BaU_NoCC_No = "SSP2_BaU_NoCC_No",
    SSP2_500C_CACN_NoCC = "SSP2_500C_CACN_NoCC_No",
    SSP2_500C_CACN_NoCC_No = "SSP2_500C_CACN_NoCC_No",
    SSP2_BaU_DEMFWR_No = "SSP2_BaU_DEMFWR_No",
    SSP2_500C_CACN_DEMFWR_No = "SSP2_500C_CACN_DEMFWR_No",
    baseline = "SSP2_BaU_NoCC_No",
    nue = "SSP2_BaU_NoCC_No",
    air_pollution = "SSP2_BaU_NoCC_No",
    nue_air_pollution = "SSP2_BaU_NoCC_No",
    `500c` = "SSP2_500C_CACN_NoCC_No",
    `500c_nue` = "SSP2_500C_CACN_NoCC_No",
    demfwr = "SSP2_BaU_DEMFWR_No",
    demfwr_nue = "SSP2_BaU_DEMFWR_No",
    `500c_demfwr` = "SSP2_500C_CACN_DEMFWR_No",
    `500c_demfwr_nue` = "SSP2_500C_CACN_DEMFWR_No",
    # Legacy aliases retained for older output manifests.
    eatlancet = "SSP2_BaU_DEMFWR_No",
    eatlancet_nue = "SSP2_BaU_DEMFWR_No",
    `500c_eatlancet` = "SSP2_500C_CACN_DEMFWR_No",
    `500c_eatlancet_nue` = "SSP2_500C_CACN_DEMFWR_No"
  )
  normalized <- unname(aliases[scenario])
  if (length(normalized) == 0L || is.na(normalized)) {
    stop(
      "No scenario-specific natural-land mapping is registered for: ",
      scenario
    )
  }
  normalized
}

natural_array_path <- function(visitorn_dir, year, land_scenario, nmip3) {
  land_scenario <- normalize_natural_land_scenario(land_scenario)
  nmip3_suffix <- if (isTRUE(nmip3)) "_NMIP3" else ""
  file.path(
    visitorn_dir,
    "Data_pd",
    "natural_by_landuse",
    land_scenario,
    paste0(
      "VISITnl_nat_",
      as.integer(year),
      nmip3_suffix,
      ".rdat"
    )
  )
}

load_outleaching_with_coordinates <- function(scenario_dir, year) {
  outlea_path <- file.path(
    scenario_dir,
    paste0("OUTleadis", as.integer(year), ".rdat")
  )
  audit_path <- file.path(
    scenario_dir,
    paste0("grid_audit_", as.integer(year), ".csv")
  )
  if (!file.exists(outlea_path)) {
    stop("Nitrogen-flow leaching output is missing: ", outlea_path)
  }
  if (!file.exists(audit_path)) {
    stop("Nitrogen-flow grid audit is missing: ", audit_path)
  }

  environment <- load_rdata_environment(outlea_path)
  if (!exists("OUTleadis", envir = environment, inherits = FALSE)) {
    stop(outlea_path, " does not contain OUTleadis.")
  }
  outlea <- get("OUTleadis", envir = environment)
  required_outlea <- c("G", "YEMF", "OUTleagrid")
  missing <- setdiff(required_outlea, names(outlea))
  if (length(missing) > 0L) {
    stop("OUTleadis is missing columns: ", paste(missing, collapse = ", "))
  }

  coordinates <- readr::read_csv(
    audit_path,
    show_col_types = FALSE,
    progress = FALSE
  )
  required_coordinates <- c("G", "longitude", "latitude")
  missing_coordinates <- setdiff(required_coordinates, names(coordinates))
  if (length(missing_coordinates) > 0L) {
    stop(
      "Grid audit is missing map coordinates: ",
      paste(missing_coordinates, collapse = ", "),
      ". Re-run the nitrogen-flow scenario with the current runner."
    )
  }
  coordinates <- unique(data.frame(
    G = as.character(coordinates$G),
    longitude = as.numeric(coordinates$longitude),
    latitude = as.numeric(coordinates$latitude),
    stringsAsFactors = FALSE
  ))
  if (anyDuplicated(coordinates$G)) {
    stop("Some grids have multiple coordinate mappings in ", audit_path, ".")
  }

  outlea <- data.frame(
    G = as.character(outlea$G),
    year = suppressWarnings(as.integer(as.character(outlea$YEMF))),
    leaching_tg_n_year = as.numeric(outlea$OUTleagrid),
    stringsAsFactors = FALSE
  )
  outlea <- outlea[outlea$year == as.integer(year), , drop = FALSE]
  if (nrow(outlea) == 0L) {
    stop("OUTleadis contains no rows for year ", year, ".")
  }
  outlea <- stats::aggregate(
    outlea$leaching_tg_n_year,
    by = list(G = outlea$G),
    FUN = sum,
    na.rm = TRUE
  )
  names(outlea)[2] <- "leaching_tg_n_year"
  outlea <- merge(outlea, coordinates, by = "G", all.x = TRUE)

  # G is the one-based linear index of the 720 x 360 global 0.5-degree
  # grid: G = (latitude_index - 1) * 720 + longitude_index.  MAP_GIJ is
  # intentionally sparse in some input data, although OUTleadis can still
  # contain those (normally zero-flow) cells.  Recover only missing
  # coordinates from the grid identifier and retain coordinates supplied by
  # the audit wherever they exist.
  grid_number <- suppressWarnings(as.integer(outlea$G))
  valid_grid_number <- (
    !is.na(grid_number) &
      grid_number >= 1L &
      grid_number <= 720L * 360L
  )
  longitude_index <- ((grid_number - 1L) %% 720L) + 1L
  latitude_index <- ((grid_number - 1L) %/% 720L) + 1L
  derived_longitude <- 0.5 * longitude_index - 180.25
  derived_latitude <- -0.5 * latitude_index + 90.25
  fill_longitude <- !is.finite(outlea$longitude) & valid_grid_number
  fill_latitude <- !is.finite(outlea$latitude) & valid_grid_number
  outlea$longitude[fill_longitude] <- derived_longitude[fill_longitude]
  outlea$latitude[fill_latitude] <- derived_latitude[fill_latitude]

  if (any(!stats::complete.cases(
    outlea[, c("longitude", "latitude"), drop = FALSE]
  ))) {
    missing_grids <- outlea$G[!stats::complete.cases(
      outlea[, c("longitude", "latitude"), drop = FALSE]
    )]
    stop(
      "Some OUTleadis grids have no longitude/latitude mapping and cannot ",
      "be decoded as 0.5-degree grid identifiers: ",
      paste(utils::head(missing_grids, 10L), collapse = ", ")
    )
  }
  outlea
}

make_visitorn_input <- function(natural_array, outlea) {
  if (!identical(dim(natural_array), c(720L, 360L, 12L))) {
    stop(
      "Natural VISIToRN input has unexpected dimensions: ",
      paste(dim(natural_array), collapse = " x ")
    )
  }

  longitude_index <- round((outlea$longitude + 179.75) / 0.5) + 1L
  latitude_index <- round((89.75 - outlea$latitude) / 0.5) + 1L
  valid <- (
    longitude_index >= 1L & longitude_index <= 720L &
      latitude_index >= 1L & latitude_index <= 360L
  )
  if (!all(valid)) {
    stop("Some nitrogen-flow grids fall outside the VISIToRN 0.5-degree grid.")
  }

  if (any(!is.finite(outlea$leaching_tg_n_year))) {
    stop("Nitrogen-flow leaching output contains non-finite values.")
  }
  # Preserve the signed agricultural residual until it is combined with the
  # natural-soil flux.  This matches the GMD workflow: only the final total
  # supplied to VISIToRN is constrained to be non-negative.
  daily_kg_n <- outlea$leaching_tg_n_year * 1e9 / 365.25
  indexed <- stats::aggregate(
    daily_kg_n,
    by = list(
      longitude_index = longitude_index,
      latitude_index = latitude_index
    ),
    FUN = sum
  )
  names(indexed)[3] <- "daily_kg_n"
  agricultural_array <- array(0, dim = c(720, 360, 12))
  for (month in seq_len(12L)) {
    agricultural_array[
      cbind(
        indexed$longitude_index,
        indexed$latitude_index,
        rep(month, nrow(indexed))
      )
    ] <- indexed$daily_kg_n
  }

  combined_raw <- natural_array + agricultural_array
  combined <- pmax(combined_raw, 0)

  list(
    combined = combined,
    agricultural = agricultural_array,
    summary = data.frame(
      natural_mean_daily_kg_n = mean(apply(natural_array, 3, sum)),
      agricultural_daily_kg_n = sum(daily_kg_n),
      combined_mean_daily_kg_n = mean(apply(
        combined,
        3,
        sum
      )),
      negative_nflow_cells = sum(
        outlea$leaching_tg_n_year < 0,
        na.rm = TRUE
      ),
      negative_combined_cells_clamped = sum(
        combined_raw < 0,
        na.rm = TRUE
      ),
      stringsAsFactors = FALSE
    )
  )
}

save_visitorn_input <- function(input, metadata, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  VISITnl_input <- input
  input_metadata <- metadata
  save(VISITnl_input, input_metadata, file = path, compress = TRUE)
  invisible(path)
}

load_visitorn_static_inputs <- function(visitorn_dir) {
  data_dir <- file.path(visitorn_dir, "Data_pd")
  required_files <- c(
    LandMask_Loop = "LandMask_Loop.rdat",
    DistMat = "DistMat.rdat",
    VISITro_nat_ag = "VISITro_nat_ag.rdat",
    RivArea = "RivArea.rdat",
    HumanN_future = "HumanN_future.rdat",
    dat_tas = "dat_tas.rdat",
    dat_dir = "dat_dir.rdat"
  )

  result <- new.env(parent = emptyenv())
  for (expected_name in names(required_files)) {
    path <- file.path(data_dir, required_files[[expected_name]])
    environment <- load_rdata_environment(path)
    if (!exists(expected_name, envir = environment, inherits = FALSE)) {
      stop(path, " does not contain ", expected_name, ".")
    }
    assign(
      expected_name,
      get(expected_name, envir = environment),
      envir = result
    )
  }

  rivnum_values <- scan(
    file.path(visitorn_dir, "rivnum05.asc"),
    what = numeric(),
    quiet = TRUE
  )
  if (length(rivnum_values) != 720L * 360L) {
    stop("rivnum05.asc does not contain a 720 x 360 grid.")
  }
  result$rivnum_matrix <- matrix(
    rivnum_values,
    ncol = 720,
    nrow = 360,
    byrow = TRUE
  )
  result$riv_table <- utils::read.table(
    file.path(visitorn_dir, "rivnum05.txt"),
    header = TRUE,
    stringsAsFactors = FALSE
  )[, c("rivnum", "river_basin_name"), drop = FALSE]

  trip_path <- file.path(visitorn_dir, "trip_0.5x0.5.nc")
  trip <- ncdf4::nc_open(trip_path)
  on.exit(ncdf4::nc_close(trip), add = TRUE)
  longitude <- ncdf4::ncvar_get(trip, "lon")
  latitude <- ncdf4::ncvar_get(trip, "lat")
  river_path <- ncdf4::ncvar_get(trip, "rpaths")
  grid <- expand.grid(longitude = longitude, latitude = latitude)
  result$river_grid <- data.frame(
    rpaths = as.vector(river_path),
    longitude_index = round((grid$longitude + 179.75) / 0.5) + 1L,
    latitude_index = round((89.75 - grid$latitude) / 0.5) + 1L
  )
  result$source_paths <- c(
    unname(file.path(data_dir, required_files)),
    file.path(visitorn_dir, "rivnum05.asc"),
    file.path(visitorn_dir, "rivnum05.txt"),
    trip_path,
    file.path(visitorn_dir, "VISIToRN_func.R"),
    file.path(
      visitorn_dir,
      "scenario",
      "R",
      "visitorn_scenario_functions.R"
    ),
    file.path(visitorn_dir, "scenario", "run_visitorn_scenarios.R")
  )
  result
}

compile_visitorn <- function(visitorn_dir) {
  if (!exists("VISIToRN", mode = "function", envir = .GlobalEnv)) {
    sys.source(
      file.path(visitorn_dir, "VISIToRN_func.R"),
      envir = .GlobalEnv
    )
  }
  if (!exists("VISIToRN", mode = "function", envir = .GlobalEnv)) {
    stop("VISIToRN_func.R did not define VISIToRN.")
  }
  invisible(TRUE)
}

run_visitorn_routing <- function(input_array,
                                 year,
                                 static,
                                 visitorn_dir,
                                 par_fit = c(940.7691253, 12.04265895,
                                             2.959583837)) {
  compile_visitorn(visitorn_dir)
  human_year <- as.character(as.integer(year))
  human_n <- static$HumanN_future[["SSP2"]][[human_year]]
  if (is.null(human_n)) {
    stop("HumanN_future has no SSP2 data for year ", year, ".")
  }
  initial_zero <- matrix(0, nrow = 720, ncol = 360)

  initial <- VISIToRN(
    dim3 = c(720, 360, 361),
    dat_dir = static$dat_dir,
    Land_mask = static$LandMask_Loop,
    dat_dis = static$DistMat,
    Sriv = initial_zero,
    SrivN = initial_zero,
    VISITro = static$VISITro_nat_ag,
    VISITnl = input_array,
    RivArea = static$RivArea,
    dat_humanN = human_n,
    dat_tas = static$dat_tas,
    ParFit = as.numeric(par_fit)
  )
  initial_sriv <- initial$Sriv[, , 360]
  initial_sriv_n <- initial$SrivN[, , 360]
  rm(initial)
  invisible(gc())

  simulation <- VISIToRN(
    dim3 = c(720, 360, 361),
    dat_dir = static$dat_dir,
    Land_mask = static$LandMask_Loop,
    dat_dis = static$DistMat,
    Sriv = initial_sriv,
    SrivN = initial_sriv_n,
    VISITro = static$VISITro_nat_ag,
    VISITnl = input_array,
    RivArea = static$RivArea,
    dat_humanN = human_n,
    dat_tas = static$dat_tas,
    ParFit = as.numeric(par_fit)
  )

  qout_n <- simulation$QoutNint
  qout_water <- apply(simulation$Qout, c(1, 2), sum, na.rm = TRUE)
  rm(simulation, initial_sriv, initial_sriv_n)
  invisible(gc())
  list(qout_n = qout_n, qout_water = qout_water)
}

aggregate_river_mouth_scenario <- function(qout_n, static) {
  grid <- data.frame(
    longitude_index = rep(seq_len(720), each = 360),
    latitude_index = rep(seq_len(360), times = 720),
    n_out_kg = as.vector(t(qout_n)),
    rivnum = as.vector(static$rivnum_matrix)
  )
  grid <- merge(
    grid,
    static$river_grid,
    by = c("longitude_index", "latitude_index"),
    all.x = TRUE
  )
  mouths <- grid[
    grid$rivnum > 0 & grid$rpaths %in% c(9, 12),
    ,
    drop = FALSE
  ]
  river <- stats::aggregate(
    mouths$n_out_kg,
    by = list(rivnum = mouths$rivnum),
    FUN = sum,
    na.rm = TRUE
  )
  names(river)[2] <- "n_load_kg"
  river <- merge(river, static$riv_table, by = "rivnum", all.x = TRUE)
  river$n_load_gg <- river$n_load_kg / 1e6
  river
}

add_river_plot_labels <- function(river) {
  required <- c("rivnum", "river_basin_name")
  missing <- setdiff(required, names(river))
  if (length(missing) > 0L) {
    stop("River table is missing: ", paste(missing, collapse = ", "))
  }

  raw_name <- trimws(as.character(river$river_basin_name))
  name_available <- !is.na(raw_name) & nzchar(raw_name) &
    !grepl("^unknown[-_ ]*$", raw_name, ignore.case = TRUE)
  display_name <- gsub("_", " ", raw_name, fixed = TRUE)
  display_name <- sub("[- ]+$", "", display_name)
  display_name[!name_available] <- paste0(
    "Unnamed basin (ID ",
    as.character(river$rivnum[!name_available]),
    ")"
  )

  river$river_name_available <- name_available
  river$river_plot_label <- display_name
  river
}

select_top_river_records <- function(river, count, ranking_source = NULL) {
  labelled <- add_river_plot_labels(river)
  ranking_labelled <- if (is.null(ranking_source)) {
    labelled
  } else {
    add_river_plot_labels(ranking_source)
  }
  ranking <- ranking_labelled |>
    dplyr::group_by(
      rivnum,
      river_basin_name,
      river_plot_label,
      river_name_available
    ) |>
    dplyr::summarise(
      maximum_load = max(n_load_gg, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(maximum_load), rivnum) |>
    dplyr::mutate(overall_rank = dplyr::row_number())

  # The source table assigns the same placeholder name to thousands of
  # distinct basin IDs. Keep those records in totals and in the ranking audit,
  # but select named basins for figures so they are never merged into a single
  # misleading "Unknown" series.
  named_ranking <- ranking |>
    dplyr::filter(river_name_available) |>
    dplyr::mutate(named_rank = dplyr::row_number())
  selected_ids <- utils::head(named_ranking$rivnum, as.integer(count))
  ranking <- ranking |>
    dplyr::left_join(
      named_ranking |>
        dplyr::select(rivnum, named_rank),
      by = "rivnum"
    ) |>
    dplyr::mutate(selected_for_figure = rivnum %in% selected_ids)

  selected_ranking <- named_ranking |>
    dplyr::filter(rivnum %in% selected_ids) |>
    dplyr::arrange(named_rank)
  top <- labelled |>
    dplyr::filter(rivnum %in% selected_ids)
  top$river_plot_label <- factor(
    top$river_plot_label,
    levels = selected_ranking$river_plot_label
  )
  list(records = top, ranking = ranking)
}

load_reference_river_year <- function(path, reference_year = 2010L) {
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    reference <- readr::read_csv(
      path,
      show_col_types = FALSE,
      progress = FALSE
    )
    required <- c("rivnum", "river_basin_name", "n_load_gg", "year")
    missing <- setdiff(required, names(reference))
    if (length(missing) > 0L) {
      stop(
        "Reference river CSV is missing: ",
        paste(missing, collapse = ", ")
      )
    }
    reference <- data.frame(
      rivnum = suppressWarnings(as.integer(reference$rivnum)),
      river_basin_name = as.character(reference$river_basin_name),
      n_load_gg = suppressWarnings(as.numeric(reference$n_load_gg)),
      year = suppressWarnings(as.integer(reference$year)),
      stringsAsFactors = FALSE
    )
  } else {
  environment <- load_rdata_environment(path)
  if (!exists("df_river_all", envir = environment, inherits = FALSE)) {
    stop(path, " does not contain df_river_all.")
  }
  reference <- get("df_river_all", envir = environment)
  required <- c("rivnum", "river_basin_name", "Nload", "year")
  missing <- setdiff(required, names(reference))
  if (length(missing) > 0L) {
    stop(
      "Reference river table is missing: ",
      paste(missing, collapse = ", ")
    )
  }
  reference <- data.frame(
    rivnum = suppressWarnings(as.integer(as.character(reference$rivnum))),
    river_basin_name = as.character(reference$river_basin_name),
    n_load_gg = suppressWarnings(as.numeric(reference$Nload)),
    year = suppressWarnings(as.integer(as.character(reference$year))),
    stringsAsFactors = FALSE
  )
  }
  reference <- reference[
    reference$year == as.integer(reference_year),
    ,
    drop = FALSE
  ]
  if (nrow(reference) == 0L) {
    stop(
      "Reference river table contains no records for ",
      as.integer(reference_year),
      ": ",
      path
    )
  }
  if (anyNA(reference$rivnum) || any(!is.finite(reference$n_load_gg))) {
    stop("Reference river table contains invalid basin IDs or N loads.")
  }
  reference <- reference |>
    dplyr::group_by(rivnum, river_basin_name, year) |>
    dplyr::summarise(
      n_load_gg = sum(n_load_gg, na.rm = TRUE),
      .groups = "drop"
    )
  if (anyDuplicated(reference$rivnum)) {
    stop("Reference river table assigns multiple names to a basin ID.")
  }
  reference$n_load_kg <- reference$n_load_gg * 1e6
  reference
}

add_common_reference_year <- function(river,
                                      manifests,
                                      reference_river,
                                      reference_year = 2010L) {
  required_reference <- c(
    "rivnum", "river_basin_name", "n_load_kg", "n_load_gg", "year"
  )
  missing <- setdiff(required_reference, names(reference_river))
  if (length(missing) > 0L) {
    stop(
      "Reference river data is missing: ",
      paste(missing, collapse = ", ")
    )
  }
  reference_year <- as.integer(reference_year)
  if (any(reference_river$year != reference_year)) {
    stop("Reference river data contains years other than ", reference_year, ".")
  }

  common_reference <- dplyr::bind_rows(lapply(
    seq_len(nrow(manifests)),
    function(index) {
      result <- reference_river
      result$scenario_name <- as.character(
        manifests$scenario_name[[index]]
      )
      result$scenario_label <- as.character(
        manifests$scenario_label[[index]]
      )
      result$is_common_reference_year <- TRUE
      result
    }
  ))
  river$is_common_reference_year <- FALSE
  # The common historical baseline is authoritative if a scenario output
  # happens to contain the same year.
  river <- river[river$year != reference_year, , drop = FALSE]
  dplyr::bind_rows(common_reference, river)
}

make_concentration_summary <- function(qout_n, qout_water, threshold = 2.5) {
  concentration <- qout_n / qout_water * 1e6
  concentration[qout_water <= 0 | !is.finite(concentration)] <- NA_real_
  finite <- concentration[is.finite(concentration)]
  if (length(finite) == 0L) {
    summary <- data.frame(
      threshold_mg_n_l = threshold,
      valid_cell_count = 0L,
      mean_mg_n_l = NA_real_,
      median_mg_n_l = NA_real_,
      p90_mg_n_l = NA_real_,
      p95_mg_n_l = NA_real_,
      max_mg_n_l = NA_real_,
      cells_over_threshold = 0L,
      stringsAsFactors = FALSE
    )
    return(list(concentration = concentration, summary = summary))
  }
  summary <- data.frame(
    threshold_mg_n_l = threshold,
    valid_cell_count = length(finite),
    mean_mg_n_l = mean(finite),
    median_mg_n_l = stats::median(finite),
    p90_mg_n_l = as.numeric(stats::quantile(finite, 0.90)),
    p95_mg_n_l = as.numeric(stats::quantile(finite, 0.95)),
    max_mg_n_l = max(finite),
    cells_over_threshold = sum(finite > threshold),
    stringsAsFactors = FALSE
  )
  list(concentration = concentration, summary = summary)
}

matrix_to_geographic_table <- function(matrix_value, value_name) {
  value <- matrix_value[, ncol(matrix_value):1, drop = FALSE]
  result <- expand.grid(
    longitude = seq(-179.75, 179.75, by = 0.5),
    latitude = seq(-89.75, 89.75, by = 0.5)
  )
  result[[value_name]] <- as.vector(value)
  result
}

save_routing_maps <- function(qout_n,
                              concentration,
                              scenario_label,
                              year,
                              output_dir,
                              threshold = 2.5) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  world <- ggplot2::map_data("world")

  load_table <- matrix_to_geographic_table(qout_n, "n_load_kg")
  load_table$log10_n_load <- log10(pmax(load_table$n_load_kg, 0) + 1)
  load_plot <- ggplot2::ggplot(load_table) +
    ggplot2::geom_raster(
      ggplot2::aes(longitude, latitude, fill = log10_n_load)
    ) +
    ggplot2::geom_polygon(
      data = world,
      ggplot2::aes(long, lat, group = group),
      fill = NA,
      color = "grey30",
      linewidth = 0.15
    ) +
    ggplot2::scale_fill_viridis_c(option = "C") +
    ggplot2::coord_quickmap(
      xlim = c(-180, 180),
      ylim = c(-60, 90),
      expand = FALSE
    ) +
    ggplot2::labs(
      title = paste0(scenario_label, " / ", year),
      subtitle = "Cumulative river nitrogen outflow",
      fill = "log10(kg N + 1)",
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 16 * plot_text_scale) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 21 * plot_text_scale),
      plot.subtitle = ggplot2::element_text(size = 17 * plot_text_scale),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 16 * plot_text_scale),
      legend.text = ggplot2::element_text(size = 15 * plot_text_scale)
    )
  ggplot2::ggsave(
    file.path(output_dir, "cumulative_n_outflow.png"),
    load_plot,
    width = 24,
    height = 15,
    dpi = 260
  )

  ratio_table <- matrix_to_geographic_table(
    concentration / threshold,
    "threshold_ratio"
  )
  ratio_table$class <- cut(
    ratio_table$threshold_ratio,
    breaks = c(0, 0.4, 0.8, 1.2, 2.1, 3, Inf),
    labels = c("<=0.4", "0.4-0.8", "0.8-1.2", "1.2-2.1", "2.1-3", ">3"),
    include.lowest = TRUE
  )
  concentration_plot <- ggplot2::ggplot(ratio_table) +
    ggplot2::geom_raster(
      ggplot2::aes(longitude, latitude, fill = class)
    ) +
    ggplot2::geom_polygon(
      data = world,
      ggplot2::aes(long, lat, group = group),
      fill = NA,
      color = "grey30",
      linewidth = 0.15
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "<=0.4" = "#08306B",
        "0.4-0.8" = "#4292C6",
        "0.8-1.2" = "#C6DBEF",
        "1.2-2.1" = "#FEE0D2",
        "2.1-3" = "#FB6A4A",
        ">3" = "#CB181D"
      ),
      na.value = "transparent",
      drop = FALSE
    ) +
    ggplot2::coord_quickmap(
      xlim = c(-180, 180),
      ylim = c(-60, 90),
      expand = FALSE
    ) +
    ggplot2::labs(
      title = paste0(scenario_label, " / ", year),
      subtitle = paste0(
        "River N concentration relative to ",
        threshold,
        " mg N/L"
      ),
      fill = "Threshold ratio",
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 16 * plot_text_scale) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 21 * plot_text_scale),
      plot.subtitle = ggplot2::element_text(size = 17 * plot_text_scale),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 16 * plot_text_scale),
      legend.text = ggplot2::element_text(size = 15 * plot_text_scale)
    )
  ggplot2::ggsave(
    file.path(output_dir, "concentration_threshold_ratio.png"),
    concentration_plot,
    width = 24,
    height = 15,
    dpi = 260
  )
  invisible(TRUE)
}

save_scenario_river_plots <- function(river, scenario_label, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (nrow(river) == 0L) {
    stop("No river-mouth load records are available for ", scenario_label, ".")
  }
  totals <- stats::aggregate(
    river$n_load_gg,
    by = list(year = river$year),
    FUN = sum,
    na.rm = TRUE
  )
  names(totals)[2] <- "n_load_gg"
  totals$n_load_tg <- totals$n_load_gg / 1000
  total_plot <- ggplot2::ggplot(
    totals,
    ggplot2::aes(factor(year), n_load_tg, group = 1)
  )
  if (length(unique(totals$year)) > 1L) {
    total_plot <- total_plot +
      ggplot2::geom_line(linewidth = 0.8, color = "#0072B2")
  }
  total_plot <- total_plot +
    ggplot2::geom_point(size = 2.5, color = "#0072B2") +
    ggplot2::labs(
      title = scenario_label,
      x = "Year",
      y = expression("Total river-mouth N load (Tg N yr"^{-1}*")")
    ) +
    ggplot2::theme_minimal(base_size = 18 * plot_text_scale)
  ggplot2::ggsave(
    file.path(output_dir, "total_river_load.png"),
    total_plot,
    width = 20,
    height = 13,
    dpi = 260
  )

  selected <- select_top_river_records(river, 20L)
  top <- selected$records
  top$river_plot_label <- factor(
    top$river_plot_label,
    levels = rev(levels(top$river_plot_label))
  )
  top$n_load_tg <- top$n_load_gg / 1000
  top_plot <- ggplot2::ggplot(
    top,
    ggplot2::aes(river_plot_label, n_load_tg, fill = factor(year))
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste0(scenario_label, ": top 20 river basins"),
      x = NULL,
      y = expression("River-mouth N load (Tg N yr"^{-1}*")"),
      fill = "Year"
    ) +
    ggplot2::theme_minimal(base_size = 16 * plot_text_scale) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text = ggplot2::element_text(size = 14 * plot_text_scale),
      axis.title = ggplot2::element_text(size = 17 * plot_text_scale),
      legend.title = ggplot2::element_text(size = 16 * plot_text_scale),
      legend.text = ggplot2::element_text(size = 15 * plot_text_scale),
      plot.margin = ggplot2::margin(10, 20, 15, 25)
    )
  ggplot2::ggsave(
    file.path(output_dir, "river_load_top20.png"),
    top_plot,
    width = 26,
    height = 26,
    dpi = 260
  )
  invisible(TRUE)
}

river_scenario_styles <- function(manifests) {
  # Keep registered scenario encodings in every subset and comparison. A NUE
  # variant keeps its underlying policy color and adds dashes/triangles.
  scenario <- as.character(manifests$scenario_name)
  scenario <- gsub("eatlancet", "demfwr", scenario, fixed = TRUE)
  has_nue <- grepl("(^|_)nue($|_)", scenario)
  family <- sub("^nue_", "", sub("_nue$", "", scenario))
  family[family %in% c("baseline", "nue")] <- "baseline"
  palette <- c(
    baseline = "#333333",
    `500c` = "#D55E00",
    demfwr = "#0072B2",
    `500c_demfwr` = "#009E73",
    air_pollution = "#CC79A7"
  )
  unknown <- sort(setdiff(unique(family), names(palette)))
  if (length(unknown) > 0L) {
    # Retain support for custom scenarios without changing registered colors.
    palette <- c(palette, stats::setNames(
      grDevices::hcl.colors(length(unknown), palette = "Dark 3"), unknown
    ))
  }
  data.frame(
    scenario_name = as.character(manifests$scenario_name),
    scenario_label = as.character(manifests$scenario_label),
    colour = unname(palette[family]),
    linetype = ifelse(has_nue, "longdash", "solid"),
    shape = ifelse(has_nue, 17L, 16L),
    stringsAsFactors = FALSE
  )
}

river_comparison_scales <- function(styles) {
  # Identical names/breaks merge color, line and marker into one legend.
  labels <- styles$scenario_label
  list(
    ggplot2::scale_colour_manual(
      name = "Scenario", breaks = labels,
      values = stats::setNames(styles$colour, labels), drop = FALSE
    ),
    ggplot2::scale_linetype_manual(
      name = "Scenario", breaks = labels,
      values = stats::setNames(styles$linetype, labels), drop = FALSE
    ),
    ggplot2::scale_shape_manual(
      name = "Scenario", breaks = labels,
      values = stats::setNames(styles$shape, labels), drop = FALSE
    ),
    ggplot2::guides(
      colour = ggplot2::guide_legend(nrow = 2, byrow = TRUE),
      linetype = ggplot2::guide_legend(nrow = 2, byrow = TRUE),
      shape = ggplot2::guide_legend(nrow = 2, byrow = TRUE)
    )
  )
}

save_comparison_outputs <- function(river,
                                    manifests,
                                    output_dir,
                                    reference_scenario,
                                    reference_river = NULL,
                                    reference_year = 2010L) {
  if (nrow(river) == 0L) {
    stop("No river-mouth load records are available for comparison.")
  }
  if (!reference_scenario %in% river$scenario_name) {
    stop("Reference scenario is absent from river results: ", reference_scenario)
  }
  if (!is.null(reference_river)) {
    river <- add_common_reference_year(
      river,
      manifests,
      reference_river,
      reference_year
    )
  }
  dir.create(file.path(output_dir, "tables"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "figures"), recursive = TRUE, showWarnings = FALSE)
  scenario_levels <- as.character(manifests$scenario_label)
  scenario_styles <- river_scenario_styles(manifests)
  river$scenario_label <- factor(
    as.character(river$scenario_label),
    levels = scenario_levels
  )
  readr::write_csv(
    river,
    file.path(output_dir, "tables", "river_load_comparison.csv")
  )
  manifests$comparison_order <- seq_len(nrow(manifests))
  manifests$is_reference <- manifests$scenario_name == reference_scenario
  readr::write_csv(
    manifests,
    file.path(output_dir, "comparison_manifest.csv")
  )

  totals <- river |>
    dplyr::group_by(scenario_name, scenario_label, year) |>
    dplyr::summarise(
      n_load_tg = sum(n_load_gg, na.rm = TRUE) / 1000,
      .groups = "drop"
    )
  total_plot <- ggplot2::ggplot(
    totals,
    ggplot2::aes(
      year, n_load_tg, color = scenario_label,
      linetype = scenario_label, shape = scenario_label,
      group = scenario_label
    )
  )
  if (length(unique(totals$year)) > 1L) {
    total_plot <- total_plot + ggplot2::geom_line(linewidth = 1.2)
  }
  total_plot <- total_plot +
    ggplot2::geom_point(size = 3.8) +
    river_comparison_scales(scenario_styles) +
    ggplot2::scale_x_continuous(
      breaks = c(2010, 2030, 2050, 2100),
      limits = c(2010, 2100),
      minor_breaks = NULL
    ) +
    ggplot2::labs(
      x = "Year",
      y = expression("Total river-mouth N load (Tg N yr"^{-1}*")"),
      color = "Scenario"
    ) +
    ggplot2::theme_minimal(base_size = 18 * plot_text_scale) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.key.width = grid::unit(3, "cm"),
      legend.title = ggplot2::element_text(size = 17 * plot_text_scale),
      legend.text = ggplot2::element_text(size = 16 * plot_text_scale)
    )
  ggplot2::ggsave(
    file.path(output_dir, "figures", "total_river_load_comparison.png"),
    total_plot,
    width = 27,
    height = 16,
    dpi = 260
  )

  ranking_source <- river |>
    dplyr::filter(
      scenario_name == reference_scenario,
      year == as.integer(reference_year)
    )
  if (nrow(ranking_source) == 0L) {
    stop(
      "No river records are available for the Top 14 reference: ",
      reference_scenario,
      " / ",
      as.integer(reference_year)
    )
  }
  selected <- select_top_river_records(
    river,
    14L,
    ranking_source = ranking_source
  )
  ranking <- selected$ranking
  top <- selected$records
  top$river_plot_label <- factor(
    as.character(top$river_plot_label),
    levels = sort(unique(as.character(top$river_plot_label)))
  )
  top$n_load_tg <- top$n_load_gg / 1000
  readr::write_csv(
    ranking,
    file.path(output_dir, "tables", "river_basin_ranking.csv")
  )
  top_plot <- ggplot2::ggplot(
    top,
    ggplot2::aes(
      year, n_load_tg, color = scenario_label,
      linetype = scenario_label, shape = scenario_label,
      group = scenario_label
    )
  )
  if (length(unique(top$year)) > 1L) {
    top_plot <- top_plot + ggplot2::geom_line(linewidth = 1.15)
  }
  top_plot <- top_plot +
    ggplot2::geom_point(size = 3.2) +
    river_comparison_scales(scenario_styles) +
    ggplot2::scale_x_continuous(
      breaks = c(2010, 2030, 2050, 2100),
      limits = c(2010, 2100),
      minor_breaks = NULL
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(river_plot_label),
      scales = "free_y",
      ncol = 4,
      labeller = ggplot2::label_wrap_gen(width = 28)
    ) +
    ggplot2::labs(
      x = "Year",
      y = expression("River-mouth N load (Tg N yr"^{-1}*")"),
      color = "Scenario"
    ) +
    ggplot2::theme_minimal(base_size = 16 * plot_text_scale) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.key.width = grid::unit(3, "cm"),
      axis.text = ggplot2::element_text(size = 14 * plot_text_scale),
      axis.title = ggplot2::element_text(size = 17 * plot_text_scale),
      legend.title = ggplot2::element_text(size = 16 * plot_text_scale),
      legend.text = ggplot2::element_text(size = 15 * plot_text_scale),
      strip.text = ggplot2::element_text(
        size = 15 * plot_text_scale,
        margin = ggplot2::margin(5, 4, 5, 4)
      ),
      plot.margin = ggplot2::margin(12, 20, 22, 22)
    )
  ggplot2::ggsave(
    file.path(output_dir, "figures", "top14_river_load_comparison.png"),
    top_plot,
    width = 38,
    height = 34,
    dpi = 260,
    limitsize = FALSE
  )

  reference <- river |>
    dplyr::filter(scenario_name == reference_scenario) |>
    dplyr::select(
      rivnum,
      year,
      reference_load_gg = n_load_gg
    )
  changes <- river |>
    dplyr::left_join(reference, by = c("rivnum", "year")) |>
    dplyr::mutate(
      difference_from_reference_gg = n_load_gg - reference_load_gg,
      ratio_to_reference = dplyr::if_else(
        reference_load_gg > 0,
        n_load_gg / reference_load_gg,
        NA_real_
      ),
      reference_scenario = reference_scenario
    )
  readr::write_csv(
    changes,
    file.path(output_dir, "tables", "river_load_change_from_reference.csv")
  )
  invisible(TRUE)
}
