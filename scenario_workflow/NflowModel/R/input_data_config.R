# Input-data selection shared by the nitrogen-flow and VISIToRN scripts.
#
# The default is "current", which preserves the historical file names.
# Set NFLOW_INPUT_SET=gmd (or options(nflow.input_set = "gmd")) to read
# the archived GMD inputs whose file names end in "_gmd.gdx".

nflow_normalize_input_set <- function(input_set) {
  input_set <- tolower(trimws(as.character(input_set)[1]))

  aliases <- c(
    current = "current",
    default = "current",
    latest = "current",
    gmd = "gmd"
  )

  normalized <- unname(aliases[input_set])
  if (length(normalized) == 0L || is.na(normalized)) {
    stop(
      "Unknown nitrogen-flow input set: ", input_set, "\n",
      "Use 'current' or 'gmd'."
    )
  }

  normalized
}

nflow_get_input_set <- function() {
  environment_value <- Sys.getenv("NFLOW_INPUT_SET", unset = "")
  option_value <- getOption("nflow.input_set", "current")
  input_set <- if (nzchar(environment_value)) {
    environment_value
  } else {
    option_value
  }

  nflow_normalize_input_set(input_set)
}

nflow_input_suffix <- function(input_set = nflow_get_input_set()) {
  input_set <- nflow_normalize_input_set(input_set)
  if (identical(input_set, "gmd")) "_gmd" else ""
}

nflow_resolve_input_file <- function(directory,
                                     file_stem,
                                     input_set = nflow_get_input_set(),
                                     must_work = TRUE) {
  input_set <- nflow_normalize_input_set(input_set)
  path <- file.path(
    directory,
    paste0(file_stem, nflow_input_suffix(input_set), ".gdx")
  )

  if (must_work && !file.exists(path)) {
    stop(
      "Nitrogen-flow input file was not found.\n",
      "  input set: ", input_set, "\n",
      "  expected file: ",
      normalizePath(path, winslash = "/", mustWork = FALSE), "\n",
      "Check NFLOW_INPUT_SET and the input-file location."
    )
  }

  normalizePath(path, winslash = "/", mustWork = must_work)
}

nflow_report_input_file <- function(label, path, input_set = nflow_get_input_set()) {
  message(
    "N-flow input [", nflow_normalize_input_set(input_set), "] ",
    label, ": ", path
  )
  invisible(path)
}

nflow_first_existing_file <- function(paths, label, must_work = TRUE) {
  paths <- unique(normalizePath(
    paths,
    winslash = "/",
    mustWork = FALSE
  ))
  existing <- paths[file.exists(paths)]
  if (length(existing) > 0L) {
    return(normalizePath(existing[[1]], winslash = "/", mustWork = TRUE))
  }

  if (must_work) {
    stop(
      label, " was not found. Tried:\n  ",
      paste(paths, collapse = "\n  ")
    )
  }
  paths[[1]]
}

nflow_scenario_input_spec <- function(scenario) {
  scenario <- as.character(scenario)[1]
  specs <- list(
    SSP2_BaU_NoCC = list(
      climate = "BaU",
      demfwr = FALSE,
      deposition_prefix = "Base",
      aimhub_stems = c("global_17_IAMC_legacy", "global_17_IAMC"),
      body_weight_stems = c("BW_map_BaU", "BW_map_BaU_No")
    ),
    SSP2_500C_CACN_NoCC = list(
      climate = "500C",
      demfwr = FALSE,
      deposition_prefix = "500C",
      aimhub_stems = c("global_17_IAMC_legacy", "global_17_IAMC"),
      body_weight_stems = c("BW_map_500C", "BW_map_500C_No")
    ),
    SSP2_BaU_NoCC_No = list(
      climate = "BaU",
      demfwr = FALSE,
      deposition_prefix = "SSP2_BaU_NoCC_No",
      deposition_2010_prefix = "Base",
      aimhub_stems = "global_17_IAMC",
      body_weight_stems = c("BW_map_BaU_No", "BW_map_BaU")
    ),
    SSP2_500C_CACN_NoCC_No = list(
      climate = "500C",
      demfwr = FALSE,
      deposition_prefix = "SSP2_500C_CACN_NoCC_No",
      deposition_2010_prefix = "Base",
      aimhub_stems = "global_17_IAMC",
      body_weight_stems = c("BW_map_500C_No", "BW_map_500C")
    ),
    SSP2_BaU_DEMFWR_No = list(
      climate = "BaU",
      demfwr = TRUE,
      deposition_prefix = "SSP2_BaU_DEMFWR_No",
      deposition_2010_prefix = "Base",
      aimhub_stems = "global_17_IAMC",
      body_weight_stems = c("BW_map_BaU_EAT_No", "BW_map_BaU_No")
    ),
    SSP2_500C_CACN_DEMFWR_No = list(
      climate = "500C",
      demfwr = TRUE,
      deposition_prefix = "SSP2_500C_CACN_DEMFWR_No",
      deposition_2010_prefix = "Base",
      aimhub_stems = "global_17_IAMC",
      body_weight_stems = c("BW_map_500C_EAT_No", "BW_map_500C_No")
    )
  )

  spec <- specs[[scenario]]
  if (is.null(spec)) {
    stop(
      "No nitrogen-flow input mapping is registered for scenario: ",
      scenario
    )
  }
  spec$scenario <- scenario
  spec
}

nflow_resolve_aimhub_file <- function(directory,
                                      scenario,
                                      input_set = nflow_get_input_set(),
                                      must_work = TRUE) {
  input_set <- nflow_normalize_input_set(input_set)
  spec <- nflow_scenario_input_spec(scenario)
  suffix <- nflow_input_suffix(input_set)
  stems <- spec$aimhub_stems
  if (identical(input_set, "gmd")) {
    # Preserve the historical global_17_IAMC_gmd.gdx convention as a fallback.
    stems <- unique(c(stems, "global_17_IAMC"))
  }
  nflow_first_existing_file(
    file.path(directory, paste0(stems, suffix, ".gdx")),
    "AIM-Hub input",
    must_work
  )
}

nflow_resolve_plum_file <- function(directory,
                                    scenario,
                                    input_set = nflow_get_input_set(),
                                    must_work = TRUE) {
  input_set <- nflow_normalize_input_set(input_set)
  suffix <- nflow_input_suffix(input_set)
  stems <- c(
    paste0("result_", scenario),
    paste0("results_", scenario)
  )
  paths <- file.path(directory, paste0(stems, suffix, ".gdx"))
  nflow_first_existing_file(paths, "AIM-PLUM input", must_work)
}

nflow_resolve_livestock_file <- function(directory,
                                         scenario,
                                         must_work = TRUE) {
  nflow_scenario_input_spec(scenario)
  nflow_first_existing_file(
    file.path(directory, paste0("livdis_", scenario, ".nc")),
    "AIM-PLUM livestock input",
    must_work
  )
}

nflow_resolve_body_weight_file <- function(directory,
                                            scenario,
                                            must_work = TRUE) {
  spec <- nflow_scenario_input_spec(scenario)
  nflow_first_existing_file(
    file.path(directory, paste0(spec$body_weight_stems, ".csv")),
    "Livestock body-weight input",
    must_work
  )
}

nflow_resolve_deposition_file <- function(directory,
                                           scenario,
                                           year,
                                           type = c("dry", "wet"),
                                           must_work = TRUE,
                                           deposition_prefix = NULL) {
  type <- match.arg(type)
  spec <- nflow_scenario_input_spec(scenario)
  default_prefix <- if (
    as.integer(year) == 2010L &&
      !is.null(spec$deposition_2010_prefix)
  ) {
    spec$deposition_2010_prefix
  } else {
    spec$deposition_prefix
  }
  requested_prefix <- if (
    is.null(deposition_prefix) ||
      length(deposition_prefix) == 0L ||
      is.na(as.character(deposition_prefix)[1])
  ) {
    ""
  } else {
    as.character(deposition_prefix)[1]
  }
  use_registered_prefix <- !nzchar(requested_prefix) ||
    identical(requested_prefix, spec$deposition_prefix)
  prefix <- if (use_registered_prefix) {
    default_prefix
  } else {
    requested_prefix
  }
  stem <- if (identical(type, "dry")) "0505NDRYMON_" else "0505NWETMON_"
  nflow_first_existing_file(
    file.path(
      directory,
      paste0(stem, prefix, as.integer(year), ".nc")
    ),
    paste("GEOS-Chem", type, "deposition input"),
    must_work
  )
}

nflow_validate_scenario_inputs <- function(directory,
                                           scenario,
                                           years,
                                           input_set = nflow_get_input_set(),
                                           deposition_prefix = NULL) {
  input_set <- nflow_normalize_input_set(input_set)
  spec <- nflow_scenario_input_spec(scenario)
  files <- list(
    aimhub = nflow_resolve_aimhub_file(
      directory, scenario, input_set = input_set
    ),
    aim_plum = nflow_resolve_plum_file(
      directory,
      scenario,
      input_set = input_set
    ),
    livestock = nflow_resolve_livestock_file(directory, scenario),
    body_weight = nflow_resolve_body_weight_file(directory, scenario)
  )
  for (year in unique(as.integer(years))) {
    files[[paste0("deposition_dry_", year)]] <- nflow_resolve_deposition_file(
      directory, scenario, year, "dry",
      deposition_prefix = deposition_prefix
    )
    files[[paste0("deposition_wet_", year)]] <- nflow_resolve_deposition_file(
      directory, scenario, year, "wet",
      deposition_prefix = deposition_prefix
    )
  }

  data.frame(
    scenario = scenario,
    climate = spec$climate,
    demfwr = spec$demfwr,
    input_set = input_set,
    input = names(files),
    path = unname(unlist(files, use.names = FALSE)),
    stringsAsFactors = FALSE
  )
}
