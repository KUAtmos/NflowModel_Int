script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(script_arg) > 0L) {
  sub("^--file=", "", script_arg[[1]])
} else {
  "prepare_natural_inputs.R"
}
script_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
visitorn_dir_default <- normalizePath(
  file.path(script_dir, ".."),
  winslash = "/",
  mustWork = TRUE
)
repository_dir_default <- normalizePath(
  file.path(visitorn_dir_default, ".."),
  winslash = "/",
  mustWork = TRUE
)

required_packages <- c("dplyr", "ncdf4", "gdxrrw", "readr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing packages required for natural-input preparation: ",
    paste(missing_packages, collapse = ", ")
  )
}

source(file.path(
  script_dir,
  "R",
  "visitorn_scenario_functions.R"
), local = .GlobalEnv)
source(file.path(
  repository_dir_default,
  "NflowModel",
  "R",
  "input_data_config.R"
), local = .GlobalEnv)
source(file.path(
  visitorn_dir_default,
  "safe_save_rdata.R"
), local = .GlobalEnv)

split_csv <- function(value) {
  result <- trimws(strsplit(as.character(value), ",", fixed = TRUE)[[1]])
  result[nzchar(result)]
}

parse_on_off <- function(value, key) {
  value <- tolower(trimws(as.character(value)))
  if (!value %in% c("on", "off")) {
    stop(key, " must be on or off.")
  }
  identical(value, "on")
}

parse_args <- function(arguments) {
  result <- list(
    visitorn_dir = visitorn_dir_default,
    nflow_data_dir = file.path(repository_dir_default, "NflowModel", "data"),
    scenarios = "all",
    years = "2030,2050,2100",
    nmip3 = "on",
    force = "off",
    flux_year = "2010",
    gams_dir = Sys.getenv(
      "GAMS_DIR",
      unset = if (.Platform$OS.type == "windows") "C:/GAMS/53" else ""
    )
  )
  valid <- names(result)

  index <- 1L
  while (index <= length(arguments)) {
    argument <- arguments[[index]]
    if (grepl("^--[^=]+=", argument)) {
      key <- sub("^--([^=]+)=.*$", "\\1", argument)
      value <- sub("^--[^=]+=", "", argument)
    } else if (startsWith(argument, "--")) {
      key <- sub("^--", "", argument)
      index <- index + 1L
      if (index > length(arguments)) {
        stop(argument, " requires a value.")
      }
      value <- arguments[[index]]
    } else {
      stop("Unknown argument: ", argument)
    }
    key <- gsub("-", "_", key)
    if (!key %in% valid) {
      stop("Unknown argument key: ", key)
    }
    result[[key]] <- value
    index <- index + 1L
  }

  result$visitorn_dir <- normalizePath(
    result$visitorn_dir,
    winslash = "/",
    mustWork = TRUE
  )
  result$nflow_data_dir <- normalizePath(
    result$nflow_data_dir,
    winslash = "/",
    mustWork = TRUE
  )
  result$nmip3 <- parse_on_off(result$nmip3, "--nmip3")
  result$force <- parse_on_off(result$force, "--force")
  result$flux_year <- suppressWarnings(as.integer(result$flux_year))
  if (length(result$flux_year) != 1L || is.na(result$flux_year)) {
    stop("--flux-year must be one integer year.")
  }
  if (!nzchar(result$gams_dir) || !dir.exists(result$gams_dir)) {
    stop("GAMS_DIR does not point to an existing directory: ", result$gams_dir)
  }
  result$gams_dir <- normalizePath(
    result$gams_dir,
    winslash = "/",
    mustWork = TRUE
  )
  result
}

all_land_scenarios <- c(
  "SSP2_BaU_NoCC_No",
  "SSP2_500C_CACN_NoCC_No",
  "SSP2_BaU_DEMFWR_No",
  "SSP2_500C_CACN_DEMFWR_No"
)

resolve_requested_scenarios <- function(value) {
  if (identical(value, "all")) {
    return(all_land_scenarios)
  }
  unique(vapply(
    split_csv(value),
    normalize_natural_land_scenario,
    character(1)
  ))
}

resolve_requested_years <- function(value) {
  if (identical(value, "all")) {
    return(c(2030L, 2050L, 2100L))
  }
  years <- suppressWarnings(as.integer(split_csv(value)))
  if (length(years) == 0L || anyNA(years)) {
    stop("--years must be all or comma-separated integer years.")
  }
  unique(years)
}

read_natural_flux <- function(path, flux_year) {
  dataset <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(dataset), add = TRUE)

  time_values <- ncdf4::ncvar_get(dataset, "time")
  decimal_years <- 1850 + time_values / 12
  indices <- which(
    decimal_years >= flux_year & decimal_years < flux_year + 1
  )
  if (length(indices) != 12L) {
    stop(
      "Expected 12 monthly natural-leaching slices for ", flux_year,
      "; found ", length(indices), " in ", path
    )
  }

  result <- ncdf4::ncvar_get(
    dataset,
    "n_leach",
    start = c(1, 1, 1, min(indices)),
    count = c(720, 360, 1, length(indices))
  )
  if (!identical(dim(result), c(720L, 360L, 12L))) {
    stop(
      "Natural-leaching NetCDF has unexpected extracted dimensions: ",
      paste(dim(result), collapse = " x ")
    )
  }
  result[!is.finite(result)] <- 0
  result
}

read_land_use_parameter <- function(path) {
  for (parameter_name in c("VY_load", "VYL")) {
    candidate <- tryCatch(
      gdxrrw::rgdx.param(path, parameter_name),
      error = function(error) NULL
    )
    if (!is.null(candidate) && nrow(candidate) > 0L) {
      if (!all(c("i1", "i2", "i3", "i4", "value") %in% names(candidate))) {
        stop(
          parameter_name,
          " does not contain the expected i1/i2/i3/i4/value columns in ",
          path
        )
      }
      return(list(name = parameter_name, data = candidate))
    }
  }
  stop("AIM-PLUM input contains neither VY_load nor VYL: ", path)
}

make_grid_reference <- function(data_prep_path) {
  ga <- gdxrrw::rgdx.param(data_prep_path, "GA")
  map <- gdxrrw::rgdx.set(data_prep_path, "MAP_GIJ") |>
    dplyr::transmute(
      G = as.character(G),
      latitude_index = suppressWarnings(as.integer(as.character(I))),
      longitude_index = suppressWarnings(as.integer(as.character(J)))
    ) |>
    dplyr::distinct()

  if (!all(c("G", "GA") %in% names(ga))) {
    stop("GA in data_prep.gdx does not contain G and GA columns.")
  }
  ga <- ga |>
    dplyr::transmute(G = as.character(G), GA = as.numeric(GA))
  if (anyDuplicated(ga$G)) {
    stop("GA contains duplicate grid identifiers.")
  }
  if (anyDuplicated(map$G)) {
    stop("MAP_GIJ contains multiple coordinate mappings for a grid.")
  }
  if (anyNA(map$latitude_index) || anyNA(map$longitude_index)) {
    stop("MAP_GIJ contains non-integer grid coordinates.")
  }
  list(ga = ga, map = map)
}

make_natural_land_area <- function(land_use, grid_reference, year) {
  natural_classes <- c("AFR", "OL", "FRS", "OTH_AIR", "OTH_ARF")
  land_use <- land_use |>
    dplyr::transmute(
      G = as.character(i4),
      year = suppressWarnings(as.integer(as.character(i2))),
      land_class = as.character(i3),
      land_fraction = pmax(as.numeric(value), 0)
    ) |>
    dplyr::filter(
      .data$year == .env$year,
      .data$land_class %in% .env$natural_classes
    ) |>
    dplyr::left_join(grid_reference$ga, by = "G") |>
    dplyr::mutate(area_ha = land_fraction * GA * 1000) |>
    dplyr::group_by(G) |>
    dplyr::summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(grid_reference$map, by = "G")

  if (nrow(land_use) == 0L) {
    stop("AIM-PLUM land-use data contain no natural land for year ", year, ".")
  }
  if (any(!is.finite(land_use$area_ha))) {
    stop("Natural land area contains non-finite values for year ", year, ".")
  }

  # MAP_GIJ is intentionally sparse and does not contain every grid that can
  # appear in AIM-PLUM VYL/VY_load. G is the one-based linear index of the
  # 720 x 360 grid, so recover only missing coordinates from G. Existing
  # MAP_GIJ coordinates remain authoritative.
  grid_number <- suppressWarnings(as.integer(land_use$G))
  valid_grid_number <- (
    !is.na(grid_number) &
      grid_number >= 1L &
      grid_number <= 720L * 360L
  )
  missing_latitude <- !is.finite(land_use$latitude_index)
  missing_longitude <- !is.finite(land_use$longitude_index)
  land_use$coordinate_derived <- missing_latitude | missing_longitude
  fill_latitude <- missing_latitude & valid_grid_number
  fill_longitude <- missing_longitude & valid_grid_number
  land_use$latitude_index[fill_latitude] <-
    ((grid_number[fill_latitude] - 1L) %/% 720L) + 1L
  land_use$longitude_index[fill_longitude] <-
    ((grid_number[fill_longitude] - 1L) %% 720L) + 1L

  if (anyNA(land_use$latitude_index) || anyNA(land_use$longitude_index)) {
    stop(
      "Natural land grids have neither MAP_GIJ coordinates nor a valid ",
      "numeric grid identifier for year ", year, "."
    )
  }
  if (any(
    land_use$longitude_index < 1L | land_use$longitude_index > 720L |
      land_use$latitude_index < 1L | land_use$latitude_index > 360L
  )) {
    stop("Natural land grids fall outside the 720 x 360 domain for year ", year, ".")
  }
  land_use
}

make_natural_array <- function(natural_flux, land_area) {
  area_ha <- matrix(0, nrow = 720L, ncol = 360L)
  area_ha[cbind(
    land_area$longitude_index,
    land_area$latitude_index
  )] <- land_area$area_ha
  multiplier <- area_ha * 10000 * 86400
  result <- sweep(natural_flux, c(1L, 2L), multiplier, "*")
  if (any(!is.finite(result))) {
    stop("Prepared natural nitrogen input contains non-finite values.")
  }
  result
}

is_current_output <- function(path, sources) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  output_time <- file.info(path)$mtime
  source_times <- file.info(sources)$mtime
  is.finite(as.numeric(output_time)) &&
    all(is.finite(as.numeric(source_times))) &&
    output_time >= max(source_times)
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
scenarios <- resolve_requested_scenarios(args$scenarios)
years <- resolve_requested_years(args$years)

Sys.setenv(GAMS_DIR = args$gams_dir)
Sys.setenv(PATH = paste(
  Sys.getenv("PATH"),
  args$gams_dir,
  sep = .Platform$path.sep
))
gdxrrw::igdx(args$gams_dir)

natural_nc <- file.path(
  args$visitorn_dir,
  if (args$nmip3) {
    "visit_20260311_atmosclus_NMIP3_TEST_fNleach_natonly.nc"
  } else {
    "visit_20250820_atmosclus_geos_TEST_fNleach_natonly.nc"
  }
)
data_prep_path <- file.path(args$nflow_data_dir, "data_prep.gdx")
required_files <- c(natural_nc, data_prep_path)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Required natural-input source files are missing: ", paste(
    missing_files,
    collapse = ", "
  ))
}

plan <- expand.grid(
  land_scenario = scenarios,
  year = years,
  stringsAsFactors = FALSE
)
plan$plum_file <- vapply(plan$land_scenario, function(scenario) {
  nflow_resolve_plum_file(
    args$nflow_data_dir,
    scenario,
    input_set = nflow_get_input_set()
  )
}, character(1))
plan$output_file <- mapply(
  natural_array_path,
  MoreArgs = list(visitorn_dir = args$visitorn_dir, nmip3 = args$nmip3),
  year = plan$year,
  land_scenario = plan$land_scenario,
  USE.NAMES = FALSE
)
plan$needs_update <- mapply(function(output_file, plum_file) {
  args$force || !is_current_output(
    output_file,
    c(
      natural_nc,
      data_prep_path,
      plum_file,
      script_path,
      file.path(script_dir, "R", "visitorn_scenario_functions.R")
    )
  )
}, plan$output_file, plan$plum_file)

if (!any(plan$needs_update)) {
  message("All scenario-specific natural nitrogen inputs are current.")
  quit(save = "no", status = 0)
}

message("Reading fixed natural-leaching flux for ", args$flux_year, "...")
natural_flux <- read_natural_flux(natural_nc, args$flux_year)
message("Reading N-flow grid area and coordinate mapping...")
grid_reference <- make_grid_reference(data_prep_path)

metadata_rows <- list()
metadata_index <- 0L
for (scenario in scenarios) {
  scenario_plan <- plan[
    plan$land_scenario == scenario & plan$needs_update,
    ,
    drop = FALSE
  ]
  if (nrow(scenario_plan) == 0L) {
    next
  }
  plum_path <- unique(scenario_plan$plum_file)
  if (length(plum_path) != 1L) {
    stop("Multiple AIM-PLUM files resolved for one land scenario: ", scenario)
  }
  message("Reading AIM-PLUM land cover: ", scenario)
  land_parameter <- read_land_use_parameter(plum_path)
  message("  parameter: ", land_parameter$name)

  for (row_index in seq_len(nrow(scenario_plan))) {
    year <- scenario_plan$year[[row_index]]
    output_file <- scenario_plan$output_file[[row_index]]
    message("Preparing natural nitrogen input: ", scenario, " / ", year)
    land_area <- make_natural_land_area(
      land_parameter$data,
      grid_reference,
      year
    )
    VISITnl_nat <- make_natural_array(natural_flux, land_area)
    safe_save_rdata(
      "VISITnl_nat",
      output_file,
      envir = environment(),
      compress = TRUE
    )

    metadata_index <- metadata_index + 1L
    metadata_rows[[metadata_index]] <- data.frame(
      land_scenario = scenario,
      year = year,
      nmip3 = args$nmip3,
      flux_year = args$flux_year,
      natural_nc = normalizePath(natural_nc, winslash = "/"),
      aim_plum_file = normalizePath(plum_path, winslash = "/"),
      land_use_parameter = land_parameter$name,
      natural_land_area_ha = sum(land_area$area_ha),
      coordinate_derived_grid_count = sum(land_area$coordinate_derived),
      natural_mean_daily_kg_n = mean(apply(VISITnl_nat, 3, sum)),
      output_file = normalizePath(output_file, winslash = "/"),
      created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      stringsAsFactors = FALSE
    )
    readr::write_csv(
      metadata_rows[[metadata_index]],
      file.path(dirname(output_file), paste0(
        "VISITnl_nat_",
        year,
        if (args$nmip3) "_NMIP3" else "",
        "_metadata.csv"
      ))
    )
    rm(VISITnl_nat, land_area)
    invisible(gc())
  }
  rm(land_parameter)
  invisible(gc())
}

message(
  "Scenario-specific natural nitrogen inputs: ",
  normalizePath(
    file.path(args$visitorn_dir, "Data_pd", "natural_by_landuse"),
    winslash = "/",
    mustWork = TRUE
  )
)
