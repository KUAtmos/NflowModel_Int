args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || length(args) > 2L) {
  stop("Usage: Rscript validate_input_schemas.R DATA_DIR [GAMS_DIR]")
}
data_dir <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
gams_dir <- if (length(args) == 2L) args[[2]] else Sys.getenv("GAMS_DIR")
if (!nzchar(gams_dir)) stop("GAMS_DIR is required.")
gams_dir <- normalizePath(gams_dir, winslash = "/", mustWork = TRUE)

script_argument <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- sub("^--file=", "", script_argument[[1]])
test_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
source(file.path(test_dir, "..", "..", "R", "input_data_config.R"))
source(file.path(test_dir, "..", "..", "R", "manure_future_helpers.R"))

gdxrrw::igdx(gams_dir)
scenarios <- c(
  "SSP2_BaU_NoCC_No",
  "SSP2_500C_CACN_NoCC_No",
  "SSP2_BaU_DEMFWR_No",
  "SSP2_500C_CACN_DEMFWR_No"
)
years <- c(2030L, 2050L, 2100L)
baseline_inventory <- NULL

validate_deposition_file <- function(path, scenario, input_name) {
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  if (!"NOx" %in% names(nc$var)) {
    stop("Deposition input has no NOx variable for ", scenario, ": ", input_name)
  }
  dimensions <- vapply(
    nc$var$NOx$dim,
    function(dimension) dimension$name,
    character(1)
  )
  if (!identical(dimensions, c("lon", "lat", "time"))) {
    stop(
      "Unexpected deposition dimensions for ", scenario, " / ", input_name,
      ": ", paste(dimensions, collapse = ", ")
    )
  }
  dimension_lengths <- vapply(nc$var$NOx$dim, `[[`, numeric(1), "len")
  if (any(!is.finite(dimension_lengths)) || any(dimension_lengths <= 0)) {
    stop("Empty deposition dimension for ", scenario, " / ", input_name)
  }
  invisible(TRUE)
}

iamc_file <- nflow_resolve_input_file(data_dir, "global_17_IAMC")
iamc <- gdxrrw::rgdx.param(iamc_file, "IAMC_Template")
missing_iamc <- setdiff(scenarios, unique(as.character(iamc$SCENARIO)))
if (length(missing_iamc) > 0L) {
  stop("IAMC_Template is missing scenarios: ", paste(missing_iamc, collapse = ", "))
}

legacy_scenarios <- c("SSP2_BaU_NoCC", "SSP2_500C_CACN_NoCC")
legacy_iamc_file <- nflow_resolve_aimhub_file(data_dir, legacy_scenarios[[1]])
legacy_iamc <- gdxrrw::rgdx.param(legacy_iamc_file, "IAMC_Template")
missing_legacy <- setdiff(
  legacy_scenarios,
  unique(as.character(legacy_iamc$SCENARIO))
)
if (length(missing_legacy) > 0L) {
  stop(
    "Legacy IAMC_Template is missing scenarios: ",
    paste(missing_legacy, collapse = ", ")
  )
}

for (scenario in scenarios) {
  inventory <- nflow_validate_scenario_inputs(data_dir, scenario, years)
  if (identical(scenario, "SSP2_BaU_NoCC_No")) {
    baseline_inventory <- inventory
  }
  plum_file <- inventory$path[inventory$input == "aim_plum"]
  land_use <- tryCatch(
    gdxrrw::rgdx.param(plum_file, "VY_load"),
    error = function(error) gdxrrw::rgdx.param(plum_file, "VYL")
  )
  required_columns <- c("i1", "i2", "i3", "i4", "value")
  if (!all(required_columns %in% names(land_use))) {
    stop("Unexpected AIM-PLUM schema for ", scenario)
  }
  available_years <- unique(as.integer(as.character(land_use$i2)))
  if (!all(years %in% available_years)) {
    stop("AIM-PLUM years are incomplete for ", scenario)
  }

  livestock_file <- inventory$path[inventory$input == "livestock"]
  livestock <- ncdf4::nc_open(livestock_file)
  livestock_dimensions <- vapply(
    livestock$var$Livestock_number$dim,
    function(dimension) dimension$name,
    character(1)
  )
  livestock_years <- as.integer(ncdf4::ncvar_get(livestock, "time"))
  livestock_shape <- vapply(
    livestock$var$Livestock_number$dim,
    `[[`,
    numeric(1),
    "len"
  )
  ncdf4::nc_close(livestock)
  if (!identical(livestock_dimensions, c("lon", "lat", "slnum", "time"))) {
    stop("Unexpected livestock dimensions for ", scenario)
  }
  if (!all(c(2010L, years) %in% livestock_years)) {
    stop("Livestock years are incomplete for ", scenario)
  }

  body_weight_file <- inventory$path[inventory$input == "body_weight"]
  body_weight_values <- nflow_read_body_weight_table(
    body_weight_file,
    n_species = livestock_shape[[3]],
    n_latitude = livestock_shape[[2]],
    n_longitude = livestock_shape[[1]]
  )
  if (!any(is.finite(body_weight_values) & body_weight_values > 0)) {
    stop("Body-weight input has no positive values for ", scenario)
  }

  deposition_rows <- startsWith(inventory$input, "deposition_")
  for (row in which(deposition_rows)) {
    validate_deposition_file(
      inventory$path[[row]],
      scenario,
      inventory$input[[row]]
    )
  }

  message(
    "SCHEMA_OK ", scenario,
    " land_rows=", nrow(land_use),
    " livestock_years=", paste(livestock_years, collapse = ","),
    " body_weight_values=", length(body_weight_values),
    " deposition_files=", sum(deposition_rows)
  )
  rm(land_use, body_weight_values)
  invisible(gc(verbose = FALSE))
}

ap_deposition_prefix <- "SSP2_BaU_NoCC_No_APMTFR"
ap_inventory <- nflow_validate_scenario_inputs(
  data_dir,
  "SSP2_BaU_NoCC_No",
  years,
  deposition_prefix = ap_deposition_prefix
)
ap_deposition_rows <- startsWith(ap_inventory$input, "deposition_")
if (sum(ap_deposition_rows) != 2L * length(years)) {
  stop("APMTFR deposition inventory is incomplete.")
}

non_deposition <- !ap_deposition_rows
baseline_non_deposition <- !startsWith(
  baseline_inventory$input,
  "deposition_"
)
ap_base <- ap_inventory[non_deposition, c("input", "path"), drop = FALSE]
baseline_base <- baseline_inventory[
  baseline_non_deposition,
  c("input", "path"),
  drop = FALSE
]
ap_base <- ap_base[order(ap_base$input), , drop = FALSE]
baseline_base <- baseline_base[order(baseline_base$input), , drop = FALSE]
rownames(ap_base) <- NULL
rownames(baseline_base) <- NULL
if (!identical(ap_base, baseline_base)) {
  stop("APMTFR must reuse the Baseline non-deposition inputs.")
}

for (row in which(ap_deposition_rows)) {
  validate_deposition_file(
    ap_inventory$path[[row]],
    ap_deposition_prefix,
    ap_inventory$input[[row]]
  )
}
message(
  "SCHEMA_OK air_pollution",
  " base_scenario=SSP2_BaU_NoCC_No",
  " deposition_prefix=", ap_deposition_prefix,
  " deposition_files=", sum(ap_deposition_rows)
)

cat(
  "All four raw-input scenarios, both legacy AIM-Hub cases, and six ",
  "APMTFR deposition files passed schema validation.\n"
)
