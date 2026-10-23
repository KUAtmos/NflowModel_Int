script_argument <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(script_argument) > 0L) {
  sub("^--file=", "", script_argument[[1]])
} else {
  "run_nflow_scenarios.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
script_dir <- dirname(script_path)

source(file.path(script_dir, "R", "scenario_registry.R"), local = .GlobalEnv)
source(file.path(script_dir, "R", "config.R"), local = .GlobalEnv)
source(file.path(script_dir, "R", "scenario_extensions.R"), local = .GlobalEnv)
source(file.path(script_dir, "..", "R", "grid_emissions_csv.R"), local = .GlobalEnv)
source(
  file.path(script_dir, "..", "R", "input_data_config.R"),
  local = .GlobalEnv
)

parse_args <- function(args) {
  result <- list(
    config = file.path(script_dir, "scenario_config.env"),
    dry_run = FALSE,
    list_only = FALSE,
    case = ""
  )
  index <- 1L
  while (index <= length(args)) {
    argument <- args[[index]]
    if (argument == "--dry-run") {
      result$dry_run <- TRUE
    } else if (argument == "--list") {
      result$list_only <- TRUE
    } else if (startsWith(argument, "--config=")) {
      result$config <- sub("^--config=", "", argument)
    } else if (argument == "--config") {
      index <- index + 1L
      if (index > length(args)) stop("--config requires a path.")
      result$config <- args[[index]]
    } else if (startsWith(argument, "--case=")) {
      result$case <- sub("^--case=", "", argument)
    } else if (argument == "--case") {
      index <- index + 1L
      if (index > length(args)) stop("--case requires a scenario ID.")
      result$case <- args[[index]]
    } else {
      stop("Unknown argument: ", argument)
    }
    index <- index + 1L
  }
  result
}

write_csv_atomic <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = paste0(".", basename(path)), tmpdir = dirname(path))
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  utils::write.csv(x, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) {
    if (!file.copy(temporary, path, overwrite = TRUE)) {
      stop("Could not write output: ", path)
    }
    unlink(temporary)
  }
  invisible(path)
}

write_lines_atomic <- function(lines, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = paste0(".", basename(path)), tmpdir = dirname(path))
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  writeLines(lines, temporary, useBytes = TRUE)
  if (!file.rename(temporary, path)) {
    if (!file.copy(temporary, path, overwrite = TRUE)) {
      stop("Could not write output: ", path)
    }
    unlink(temporary)
  }
  invisible(path)
}

save_rdata_atomic <- function(object_names, path, env) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = paste0(".", basename(path)), tmpdir = dirname(path))
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  save(list = object_names, file = temporary, envir = env)
  if (!file.rename(temporary, path)) {
    if (!file.copy(temporary, path, overwrite = TRUE)) {
      stop("Could not write output: ", path)
    }
    unlink(temporary)
  }
  invisible(path)
}

case_switch_table <- function(cases) {
  axes <- scenario_axis_registry()
  names(cases)[match(axes$id, names(cases))] <- axes$config_key
  cases
}

validate_case_inputs <- function(cfg) {
  raw_data_dir <- normalizePath(
    file.path(dirname(cfg$raw_model_script), "..", "data"),
    winslash = "/",
    mustWork = TRUE
  )
  scenario_inventory <- nflow_validate_scenario_inputs(
    raw_data_dir,
    cfg$base_scenario,
    cfg$years,
    cfg$input_set,
    deposition_prefix = cfg$deposition_prefix_override
  )
  reference_inventory <- nflow_validate_scenario_inputs(
    raw_data_dir,
    cfg$summary_reference_scenario,
    cfg$summary_reference_year,
    cfg$input_set
  )
  scenario_inventory$purpose <- "scenario"
  reference_inventory$purpose <- "summary_reference"
  inventory <- unique(rbind(scenario_inventory, reference_inventory))

  specification <- nflow_scenario_input_spec(cfg$base_scenario)
  expected_climate <- if (cfg$scenario_500c) "500C" else "BaU"
  if (!identical(specification$climate, expected_climate) ||
      !identical(isTRUE(specification$demfwr), cfg$scenario_demfwr)) {
    stop(
      "Raw input mapping does not match the selected switches for ",
      cfg$scenario_name, ": ", cfg$base_scenario
    )
  }
  inventory
}

report_case <- function(cfg, inventory, schedule = NULL) {
  message("Scenario: ", cfg$scenario_name, " (", cfg$scenario_label, ")")
  message("  raw input: ", cfg$base_scenario)
  message(
    "  GMD non-_No input family: ",
    if (cfg$scenario_gmd_inputs) "on" else "off"
  )
  message("  500C: ", if (cfg$scenario_500c) "on" else "off")
  message("  DEMFWR: ", if (cfg$scenario_demfwr) "on" else "off")
  message("  NUE: ", if (cfg$nue_mitigation) "on" else "off")
  message(
    "  air pollution control: ",
    if (cfg$scenario_air_pollution) "on" else "off"
  )
  if (nzchar(cfg$deposition_prefix_override)) {
    message("  deposition prefix: ", cfg$deposition_prefix_override)
  }
  message("  years: ", paste(cfg$years, collapse = ", "))
  message("  validated input files: ", nrow(inventory))
  if (cfg$nue_mitigation && !is.null(schedule)) {
    targets <- vapply(
      cfg$years,
      function(year) interpolate_schedule(schedule, year)$nue_target,
      numeric(1)
    )
    message(
      "  NUE targets: ",
      paste0(cfg$years, "=", format(targets, trim = TRUE), collapse = ", ")
    )
  }
}

save_case_outputs <- function(env, cfg, year, case_dir) {
  nflow_export_grid_emissions(
    env, year, cfg$base_scenario, cfg$scenario_name, case_dir, cfg$scenario_label
  )
  selected_names <- c(
    "Inputdis", "Inputpasdis",
    "INferdis", "INfercropdis", "INferpasdis",
    "INmandis", "INmancropdis", "INmanpasdis",
    "INmanmanagedpasdis", "INmandirectpasdis",
    "INdepdis", "INdepcropdis", "INdeppasdis",
    "INfixdis", "INfixcropdis", "INfixpasdis",
    "OUTcropdis", "OUTcropcropdis", "OUTcroppasdis", "OUTresdis",
    "OUTgasdis", "OUTgascropdis", "OUTgaspasdis",
    "OUTgasdis_N2", "OUTgasdis_NH3", "OUTgasdis_NOx",
    "OUTgascropdis_N2", "OUTgaspasdis_N2",
    "OUTgascropdis_NH3", "OUTgaspasdis_NH3",
    "OUTgascropdis_NOx", "OUTgaspasdis_NOx",
    "OUTgasdis_NO_agriculture", "OUTgasdis_NH3_agriculture",
    "NH3_loss1", "NH3_loss2", "NH3_loss_total",
    "NH3_loss1dis", "NH3_loss2dis",
    "OUTleadis", "OUTleacropdis", "OUTleapasdis",
    "residue_reuse_grid",
    "nflow_livestock_base_time", "nflow_livestock_change_time",
    "nflow_livestock_diagnostics", "nflow_manure_allocation_diagnostics",
    "scenario_audit", "scenario_summary"
  )
  selected_names <- selected_names[vapply(
    selected_names,
    exists,
    logical(1),
    envir = env,
    inherits = FALSE
  )]
  save_rdata_atomic(
    selected_names,
    file.path(case_dir, paste0("nflow_", year, ".rdat")),
    env
  )
  save_rdata_atomic(
    "OUTleadis",
    file.path(case_dir, paste0("OUTleadis", year, ".rdat")),
    env
  )
  write_csv_atomic(
    get("scenario_audit", envir = env),
    file.path(case_dir, paste0("grid_audit_", year, ".csv"))
  )
  if (exists("nflow_livestock_diagnostics", envir = env, inherits = FALSE)) {
    write_csv_atomic(
      get("nflow_livestock_diagnostics", envir = env),
      file.path(case_dir, paste0("livestock_body_weight_audit_", year, ".csv"))
    )
  }
  if (exists(
    "nflow_manure_allocation_diagnostics",
    envir = env,
    inherits = FALSE
  )) {
    write_csv_atomic(
      get("nflow_manure_allocation_diagnostics", envir = env),
      file.path(case_dir, paste0("manure_allocation_audit_", year, ".csv"))
    )
  }
  if (cfg$export_visitorn) {
    visitorn_dir <- file.path(cfg$visitorn_output_dir, cfg$scenario_name)
    save_rdata_atomic(
      "OUTleadis",
      file.path(visitorn_dir, paste0("OUTleadis", year, ".rdat")),
      env
    )
  }
}

write_resolved_case_config <- function(cfg, path) {
  lines <- c(
    paste0("SCENARIO_NAME=", cfg$scenario_name),
    paste0("SCENARIO_LABEL=", cfg$scenario_label),
    paste0("RAW_SCENARIO=", cfg$base_scenario),
    paste0(
      "SCENARIO_GMD_INPUTS=",
      if (cfg$scenario_gmd_inputs) "on" else "off"
    ),
    paste0("SCENARIO_500C=", if (cfg$scenario_500c) "on" else "off"),
    paste0("SCENARIO_DEMFWR=", if (cfg$scenario_demfwr) "on" else "off"),
    paste0("SCENARIO_NUE=", if (cfg$nue_mitigation) "on" else "off"),
    paste0(
      "SCENARIO_AIR_POLLUTION=",
      if (cfg$scenario_air_pollution) "on" else "off"
    ),
    paste0("DEPOSITION_PREFIX=", cfg$deposition_prefix_override),
    paste0("YEARS=", paste(cfg$years, collapse = ",")),
    paste0("SUMMARY_REFERENCE_YEAR=", cfg$summary_reference_year),
    paste0("SUMMARY_REFERENCE_SCENARIO=", cfg$summary_reference_scenario),
    paste0("RANDOM_SEED_BASE=", cfg$random_seed_base),
    paste0(
      "INCLUDE_RESIDUE_IN_NUE=",
      if (cfg$include_residue_in_nue) "on" else "off"
    ),
    paste0("NUE_TARGET_FILE=", cfg$nue_target_file),
    paste0("SOURCE_CONFIG=", cfg$config_path)
  )
  write_lines_atomic(lines, path)
}

run_one_case <- function(cfg, dry_run = FALSE) {
  inventory <- validate_case_inputs(cfg)
  schedule <- if (cfg$nue_mitigation) {
    read_nue_schedule(cfg$nue_target_file)
  } else {
    NULL
  }
  report_case(cfg, inventory, schedule)
  if (dry_run) {
    message("  preflight passed")
    return(invisible(inventory))
  }

  Sys.setenv(NFLOW_INPUT_SET = cfg$input_set)
  if (nzchar(cfg$gams_dir)) Sys.setenv(GAMS_DIR = cfg$gams_dir)

  case_dir <- file.path(cfg$output_dir, cfg$scenario_name)
  if (dir.exists(case_dir) &&
      length(list.files(case_dir, all.files = TRUE, no.. = TRUE))) {
    stop("Output directory is not empty: ", case_dir,
         ". Select a new OUTPUT_DIR for a new calculation; use run_nflow_plots.R to replot.")
  }
  dir.create(case_dir, recursive = TRUE, showWarnings = FALSE)
  completion_path <- file.path(case_dir, "completion.csv")
  if (file.exists(completion_path) && !file.remove(completion_path)) {
    stop("Could not clear the old completion marker: ", completion_path)
  }
  write_resolved_case_config(cfg, file.path(case_dir, "scenario.env"))
  file_information <- file.info(inventory$path)
  inventory$size_bytes <- as.numeric(file_information$size)
  inventory$md5 <- unname(tools::md5sum(inventory$path))
  inventory$modified_at <- format(
    file_information$mtime,
    "%Y-%m-%dT%H:%M:%S%z"
  )
  write_csv_atomic(inventory, file.path(case_dir, "input_manifest.csv"))

  reference_cfg <- cfg
  reference_cfg$base_scenario <- cfg$summary_reference_scenario
  reference_cfg$scenario_500c <- FALSE
  reference_cfg$scenario_demfwr <- FALSE
  reference_cfg$nue_mitigation <- FALSE
  reference_cfg$scenario_air_pollution <- FALSE
  reference_cfg$deposition_prefix_override <- ""
  reference_cfg$export_visitorn <- FALSE

  # Persist the historical reference once under the Baseline of its input
  # family (baseline or gmd_baseline), rather than duplicating grid files.
  save_common_reference <- cfg$scenario_name %in% c("baseline", "gmd_baseline") &&
    identical(cfg$base_scenario, cfg$summary_reference_scenario)

  run_specs <- c(
    list(list(
      year = cfg$summary_reference_year,
      cfg = reference_cfg,
      save_outputs = save_common_reference,
      reference = TRUE
    )),
    lapply(cfg$years[cfg$years != cfg$summary_reference_year], function(year) {
      list(year = year, cfg = cfg, save_outputs = TRUE, reference = FALSE)
    })
  )

  original_directory <- getwd()
  on.exit(setwd(original_directory), add = TRUE)
  setwd(dirname(cfg$raw_model_script))
  summaries <- vector("list", length(run_specs))

  for (index in seq_along(run_specs)) {
    specification <- run_specs[[index]]
    run_cfg <- specification$cfg
    year <- specification$year
    message(
      "Running raw nitrogen-flow model: ", run_cfg$base_scenario,
      " / ", year,
      if (specification$reference) " (summary reference)" else ""
    )
    run_seed <- scenario_seed(
      run_cfg$random_seed_base,
      run_cfg$base_scenario,
      year
    )
    set.seed(
      run_seed,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection"
    )
    message("  deterministic seed: ", run_seed)
    environment <- new.env(parent = globalenv())
    environment$SCENARIOset <- run_cfg$base_scenario
    environment$NFLOW_DEPOSITION_PREFIX <- run_cfg$deposition_prefix_override
    environment$YEAR <- as.character(year)
    sys.source(run_cfg$raw_model_script, envir = environment)
    apply_nflow_scenario(
      env = environment,
      cfg = run_cfg,
      year = year,
      nue_schedule = if (run_cfg$nue_mitigation) schedule else NULL
    )
    if (specification$save_outputs) {
      save_case_outputs(environment, run_cfg, year, case_dir)
    }
    summaries[[index]] <- get("scenario_summary", envir = environment)
    rm(environment)
    invisible(gc(verbose = FALSE))
  }

  summary_table <- do.call(rbind, summaries)
  write_csv_atomic(summary_table, file.path(case_dir, "summary.csv"))
  output_years <- sort(unique(c(
    cfg$years,
    if (save_common_reference) cfg$summary_reference_year else integer()
  )))
  manifest <- data.frame(
    scenario_name = cfg$scenario_name,
    scenario_label = cfg$scenario_label,
    raw_scenario = cfg$base_scenario,
    scenario_gmd_inputs = cfg$scenario_gmd_inputs,
    scenario_500c = cfg$scenario_500c,
    scenario_demfwr = cfg$scenario_demfwr,
    scenario_nue = cfg$nue_mitigation,
    scenario_air_pollution = cfg$scenario_air_pollution,
    # Record the effective prefix for both policy scenarios and externally
    # supplied feedback deposition.  An empty value retains legacy behavior.
    deposition_prefix = cfg$deposition_prefix_override,
    years = paste(output_years, collapse = ","),
    nue_target_file = if (cfg$nue_mitigation) cfg$nue_target_file else "",
    source_config = cfg$config_path,
    random_seed_base = cfg$random_seed_base,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    stringsAsFactors = FALSE
  )
  write_csv_atomic(manifest, file.path(case_dir, "manifest.csv"))
  expected_outputs <- c(
    "summary.csv",
    "grid_emissions_columns.csv",
    "manifest.csv",
    "input_manifest.csv",
    "scenario.env",
    unlist(lapply(
      output_years,
      function(output_year) {
      c(
        paste0("nflow_", output_year, ".rdat"),
        paste0("OUTleadis", output_year, ".rdat"),
        paste0("grid_audit_", output_year, ".csv"),
        paste0("grid_emissions_", output_year, ".csv"),
        paste0("grid_emissions_negative_", output_year, ".csv"),
        paste0("grid_emissions_totals_", output_year, ".csv")
      )
    }), use.names = FALSE)
  )
  missing_outputs <- expected_outputs[
    !file.exists(file.path(case_dir, expected_outputs))
  ]
  if (length(missing_outputs) > 0L) {
    stop(
      "Scenario output set is incomplete for ", cfg$scenario_name, ": ",
      paste(missing_outputs, collapse = ", ")
    )
  }
  completion <- data.frame(
    status = "completed",
    scenario_name = cfg$scenario_name,
    scenario_label = cfg$scenario_label,
    years = paste(output_years, collapse = ","),
    expected_file_count = length(expected_outputs),
    completed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    stringsAsFactors = FALSE
  )
  # This marker is written last. Plot-only jobs therefore never consume a
  # partially written or failed calculation.
  write_csv_atomic(completion, completion_path)
  message("Scenario outputs: ", normalizePath(case_dir, winslash = "/"))
  invisible(manifest)
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
master_cfg <- read_scenario_config(args$config)
cases <- expand_scenario_cases(master_cfg)

if (nzchar(args$case)) {
  cases <- cases[cases$scenario_id == args$case, , drop = FALSE]
  if (nrow(cases) != 1L) {
    stop("Requested scenario ID is not selected by this config: ", args$case)
  }
}

if (args$list_only) {
  print(case_switch_table(cases), row.names = FALSE)
  quit(save = "no", status = 0)
}

if (nrow(cases) > 1L && !nzchar(args$case)) {
  message("Preflight validation for ", nrow(cases), " scenario combinations")
  for (index in seq_len(nrow(cases))) {
    cfg <- make_case_config(master_cfg, cases[index, , drop = FALSE])
    run_one_case(cfg, dry_run = TRUE)
  }
  if (args$dry_run) {
    message("All scenario combinations passed preflight validation.")
    quit(save = "no", status = 0)
  }

  rscript <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )
  statuses <- rep(NA_integer_, nrow(cases))
  for (index in seq_len(nrow(cases))) {
    scenario_id <- cases$scenario_id[[index]]
    message("===== Running ", scenario_id, " =====")
    statuses[[index]] <- system2(
      rscript,
      c(
        shQuote(script_path),
        "--config", shQuote(master_cfg$config_path),
        "--case", shQuote(scenario_id)
      )
    )
    if (statuses[[index]] != 0L && master_cfg$fail_fast) break
  }
  matrix_manifest <- case_switch_table(cases)
  matrix_manifest$status <- ifelse(
    is.na(statuses),
    "not_run",
    ifelse(statuses == 0L, "completed", "failed")
  )
  matrix_manifest$exit_code <- statuses
  matrix_manifest$created_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  write_csv_atomic(
    matrix_manifest,
    file.path(master_cfg$output_dir, "scenario_matrix_manifest.csv")
  )
  failed <- !is.na(statuses) & statuses != 0L
  not_run <- is.na(statuses)
  if (any(failed) || any(not_run)) {
    stop(
      "The scenario matrix did not complete. Failed: ",
      paste(cases$scenario_id[failed], collapse = ", "),
      if (any(not_run)) {
        paste0("; not run: ", paste(cases$scenario_id[not_run], collapse = ", "))
      } else {
        ""
      }
    )
  }
  message("All scenario combinations completed.")
  quit(save = "no", status = 0)
}

cfg <- make_case_config(master_cfg, cases[1, , drop = FALSE])
run_one_case(cfg, dry_run = args$dry_run)
