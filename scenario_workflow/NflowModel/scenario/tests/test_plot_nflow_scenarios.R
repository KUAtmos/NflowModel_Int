script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- sub("^--file=", "", script_arg[[1]])
test_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
scenario_dir <- normalizePath(file.path(test_dir, ".."), winslash = "/")
plot_script <- file.path(scenario_dir, "plot_nflow_scenarios.R")
plot_runner <- file.path(scenario_dir, "run_nflow_plots.R")

fixture_workspace <- tempfile("nflow_plot_fixture_")
fixture_root <- file.path(fixture_workspace, "output", "scenarios")
dir.create(fixture_root, recursive = TRUE)
on.exit(unlink(fixture_workspace, recursive = TRUE, force = TRUE), add = TRUE)

write_fixture <- function(name, scenario_500c, nue_mitigation) {
  scenario_path <- file.path(fixture_root, name)
  dir.create(scenario_path)
  completed_years <- if (identical(name, "baseline")) {
    c(2010L, 2030L, 2050L)
  } else {
    c(2030L, 2050L)
  }
  manifest <- data.frame(
    scenario_name = name,
    scenario_label = if (identical(name, "baseline")) {
      "Baseline"
    } else {
      "500C + NUE"
    },
    raw_scenario = if (scenario_500c) {
      "SSP2_500C_CACN_NoCC_No"
    } else {
      "SSP2_BaU_NoCC_No"
    },
    scenario_500c = scenario_500c,
    scenario_demfwr = FALSE,
    scenario_nue = nue_mitigation,
    scenario_air_pollution = FALSE,
    years = paste(completed_years, collapse = ","),
    aimhub_input = "",
    created_at = "2026-01-01T00:00:00+0000",
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    manifest,
    file.path(scenario_path, "manifest.csv"),
    row.names = FALSE
  )

  for (year in completed_years) {
    scale <- if (scenario_500c) 0.85 else 1
    grid <- expand.grid(
      grid_i = 120:121,
      grid_j = 360:361,
      KEEP.OUT.ATTRS = FALSE
    )
    grid$G <- paste0("g", seq_len(nrow(grid)))
    grid$YEMF <- year
    grid$RegionCode <- rep(c("JPN", "CHN"), each = 2)
    grid$longitude <- 0.5 * grid$grid_j - 180.25
    grid$latitude <- -0.5 * grid$grid_i + 90.25
    grid$GA <- 100000
    grid$fert_crop <- 0.20 * scale
    grid$manure_crop <- 0.04
    grid$dep_crop <- 0.02
    grid$fix_crop <- 0.01
    grid$fert_pas <- 0.03 * scale
    grid$manure_pas <- 0.02
    grid$dep_pas <- 0.01
    grid$fix_pas <- 0.005
    grid$crop_n <- 0.16
    grid$pasture_n <- 0.03
    grid$residue_n <- 0.04
    grid$gas_total <- 0.03
    # Include a negative residual leaching value to ensure plotting preserves
    # mass-balance diagnostics rather than silently dropping rows at y = 0.
    grid$leach_total <- c(-0.01, -0.01, 0.04, 0.04) * scale
    grid$input_crop <- (
      grid$fert_crop + grid$manure_crop + grid$dep_crop + grid$fix_crop
    )
    grid$nue_output <- grid$crop_n + grid$residue_n
    grid$nue_target <- if (nue_mitigation) 0.65 else NA_real_
    grid$nue_infeasible <- FALSE
    utils::write.csv(
      grid,
      file.path(scenario_path, paste0("grid_audit_", year, ".csv")),
      row.names = FALSE,
      na = ""
    )
  }
  completion <- data.frame(
    status = "completed",
    scenario_name = name,
    scenario_label = manifest$scenario_label,
    years = paste(completed_years, collapse = ","),
    expected_file_count = 2L,
    completed_at = "2026-01-01T00:00:00+0000",
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    completion,
    file.path(scenario_path, "completion.csv"),
    row.names = FALSE
  )
}

write_fixture("baseline", FALSE, FALSE)
write_fixture("500c_nue", TRUE, TRUE)

plot_output <- file.path(fixture_root, "plots")
rscript <- file.path(
  R.home("bin"),
  if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
)
command_output <- system2(
  rscript,
  args = c(
    shQuote(plot_script),
    "--scenario-root", shQuote(fixture_root),
    "--output-dir", shQuote(plot_output),
    "--scenarios", "baseline,500c_nue",
    "--years", "2030,2050",
    "--maps", "on"
  ),
  stdout = TRUE,
  stderr = TRUE
)
status <- attr(command_output, "status")
if (is.null(status)) {
  status <- 0L
}
if (status != 0L) {
  cat(paste(command_output, collapse = "\n"), "\n")
  stop("Scenario plotting command failed with status ", status, ".")
}
if (any(grepl("Removed [0-9]+ rows", command_output))) {
  cat(paste(command_output, collapse = "\n"), "\n")
  stop("Scenario plotting silently removed component rows.")
}

expected <- c(
  "regional_components.csv",
  "global_components.csv",
  "global_nue.csv",
  "plot_manifest.csv",
  "global_inputs.png",
  "global_outputs.png",
  "regional_inputs.png",
  "regional_outputs.png",
  "nue_comparison.png",
  "map_total_input.png",
  "map_n_uptake.png",
  "map_gaseous_loss.png",
  "map_leaching.png"
)
missing <- expected[!file.exists(file.path(plot_output, expected))]
if (length(missing) > 0L) {
  stop("Plot test did not create: ", paste(missing, collapse = ", "))
}

gmd_expected <- c(
  file.path("by_scenario", "baseline", "Input_17_stacked_components.png"),
  file.path("by_scenario", "baseline", "Output_17_finalrep.png"),
  file.path("by_scenario", "500c_nue", "Input_17_stacked_components.png"),
  file.path("by_scenario", "500c_nue", "Output_17_finalrep.png")
)
missing_gmd <- gmd_expected[!file.exists(file.path(plot_output, gmd_expected))]
if (length(missing_gmd) > 0L) {
  stop("GMD-style plot test did not create: ", paste(missing_gmd, collapse = ", "))
}

expected_input_components <- c(
  "Deposition", "Fertilizer", "Fixation", "Manure"
)
expected_output_components <- c(
  "Gaseous loss", "Leaching", "Residues", "Uptake"
)
global_components <- utils::read.csv(
  file.path(plot_output, "global_components.csv"),
  stringsAsFactors = FALSE
)
expected_labels <- c(
  baseline = "REF",
  `500c_nue` = "CM + NUE"
)
stopifnot(all(
  global_components$scenario_label ==
    unname(expected_labels[global_components$scenario_name])
))

plot_manifest <- utils::read.csv(
  file.path(plot_output, "plot_manifest.csv"),
  stringsAsFactors = FALSE
)
stopifnot(
  identical(plot_manifest$scenario_name, c("baseline", "500c_nue")),
  identical(plot_manifest$scenario_label, c("REF", "CM + NUE"))
)
for (scenario_name in c("baseline", "500c_nue")) {
  scenario_components <- utils::read.csv(
    file.path(
      plot_output,
      "by_scenario",
      scenario_name,
      "regional_components.csv"
    ),
    stringsAsFactors = FALSE
  )
  stopifnot(
    nrow(scenario_components) == 32L,
    identical(unique(scenario_components$scenario_name), scenario_name),
    !anyNA(scenario_components$value),
    all(is.finite(scenario_components$value)),
    identical(
      sort(unique(scenario_components$component[
        scenario_components$flow_type == "Input"
      ])),
      sort(expected_input_components)
    ),
    identical(
      sort(unique(scenario_components$component[
        scenario_components$flow_type == "Output"
      ])),
      sort(expected_output_components)
    )
  )
}
stopifnot(!any(grepl(
  "Mongolia",
  readLines(plot_script, warn = FALSE),
  fixed = TRUE
)))
plot_source <- paste(readLines(plot_script, warn = FALSE), collapse = "\n")
stopifnot(
  grepl("plot_text_scale <- 2", plot_source, fixed = TRUE),
  grepl("dplyr::filter(.data$year != 2010L)", plot_source, fixed = TRUE),
  length(gregexpr("ncol = 3", plot_source, fixed = TRUE)[[1]]) == 2L
)

common_reference_output <- file.path(fixture_root, "common_reference")
common_reference_command <- system2(
  rscript,
  args = c(
    shQuote(plot_script),
    "--scenario-root", shQuote(fixture_root),
    "--output-dir", shQuote(common_reference_output),
    "--scenarios", "baseline,500c_nue",
    "--years", "2010,2030",
    "--maps", "off"
  ),
  stdout = TRUE,
  stderr = TRUE
)
common_reference_status <- attr(common_reference_command, "status")
if (is.null(common_reference_status)) common_reference_status <- 0L
if (common_reference_status != 0L) {
  cat(paste(common_reference_command, collapse = "\n"), "\n")
  stop("Common *_No reference plotting failed.")
}
common_reference_components <- readr::read_csv(
  file.path(common_reference_output, "global_components.csv"),
  show_col_types = FALSE
)
reference_components <- common_reference_components |>
  dplyr::filter(year == 2010L) |>
  dplyr::arrange(component, scenario_name)
baseline_reference <- reference_components |>
  dplyr::filter(scenario_name == "baseline") |>
  dplyr::arrange(component)
mitigation_reference <- reference_components |>
  dplyr::filter(scenario_name == "500c_nue") |>
  dplyr::arrange(component)
stopifnot(
  nrow(baseline_reference) > 0L,
  all(c("baseline", "500c_nue") %in% reference_components$scenario_name),
  identical(baseline_reference$component, mitigation_reference$component),
  all(baseline_reference$value == mitigation_reference$value)
)

default_output_command <- system2(
  rscript,
  args = c(
    shQuote(plot_script),
    "--scenario-root", shQuote(fixture_root),
    "--scenarios", "baseline,500c_nue",
    "--years", "2030",
    "--maps", "off"
  ),
  stdout = TRUE,
  stderr = TRUE
)
default_status <- attr(default_output_command, "status")
if (is.null(default_status)) {
  default_status <- 0L
}
if (default_status != 0L) {
  cat(paste(default_output_command, collapse = "\n"), "\n")
  stop("Default comparison-folder command failed with status ", default_status)
}
default_output <- file.path(
  fixture_workspace,
  "output",
  "comparisons",
  "baseline__500c_nue"
)
if (!file.exists(file.path(default_output, "plot_manifest.csv"))) {
  stop("Default scenario-combination output folder was not created.")
}

missing_year_command <- suppressWarnings(system2(
  rscript,
  args = c(
    shQuote(plot_script),
    "--scenario-root", shQuote(fixture_root),
    "--output-dir", shQuote(file.path(fixture_root, "missing_year")),
    "--scenarios", "baseline",
    "--years", "2100",
    "--maps", "off"
  ),
  stdout = TRUE,
  stderr = TRUE
))
missing_year_status <- attr(missing_year_command, "status")
if (is.null(missing_year_status) || missing_year_status == 0L) {
  stop("Plotting accepted a year that was not part of the completed run.")
}
stopifnot(any(grepl(
  "was not completed for requested years: 2100",
  missing_year_command,
  fixed = TRUE
)))

# The configuration-driven plot runner must work even when calculation-only
# source paths do not exist, proving that saved results can be replotted alone.
wrapper_output <- file.path(fixture_workspace, "wrapper_output")
wrapper_config <- file.path(fixture_workspace, "plot_only.env")
writeLines(
  c(
    "RUN_ALL_COMBINATIONS=off",
    "SCENARIO_500C=off",
    "SCENARIO_DEMFWR=off",
    "SCENARIO_NUE=off",
    "SCENARIO_AIR_POLLUTION=off",
    "PLOT_SCENARIOS=baseline",
    "PLOT_YEARS=2030",
    "PLOT_MAPS=off",
    paste0("PLOT_OUTPUT_DIR=", wrapper_output),
    paste0("OUTPUT_DIR=", fixture_root),
    "NUE_TARGET_FILE=missing_nue_targets.csv",
    "RAW_MODEL_SCRIPT=missing_raw_model.R"
  ),
  wrapper_config,
  useBytes = TRUE
)
wrapper_command <- system2(
  rscript,
  args = c(shQuote(plot_runner), "--config", shQuote(wrapper_config)),
  stdout = TRUE,
  stderr = TRUE
)
wrapper_status <- attr(wrapper_command, "status")
if (is.null(wrapper_status)) wrapper_status <- 0L
if (wrapper_status != 0L) {
  cat(paste(wrapper_command, collapse = "\n"), "\n")
  stop("Configuration-driven plot-only runner failed.")
}
if (!file.exists(file.path(wrapper_output, "plot_manifest.csv"))) {
  stop("Plot-only runner did not create the selected comparison output.")
}

cat("Scenario plotting test passed.\n")
