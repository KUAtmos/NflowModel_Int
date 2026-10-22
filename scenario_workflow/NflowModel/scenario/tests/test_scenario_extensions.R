script_argument <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- sub("^--file=", "", script_argument[[1]])
test_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
scenario_dir <- normalizePath(file.path(test_dir, ".."), winslash = "/")

source(file.path(scenario_dir, "R", "scenario_registry.R"))
source(file.path(scenario_dir, "R", "config.R"))
source(file.path(scenario_dir, "R", "scenario_extensions.R"))
source(file.path(scenario_dir, "..", "R", "input_data_config.R"))

expect_equal <- function(actual, expected, tolerance = 1e-10) {
  if (!isTRUE(all.equal(actual, expected, tolerance = tolerance))) {
    stop(
      "Expected ", paste(expected, collapse = ", "),
      "; received ", paste(actual, collapse = ", ")
    )
  }
}

expect_error <- function(expression) {
  tryCatch(
    {
      force(expression)
      FALSE
    },
    error = function(error) TRUE
  )
}

make_test_grid <- function() {
  data.frame(
    G = c("g1", "g2"),
    YEMF = c(2030L, 2030L),
    RegionCode = c("R1", "R2"),
    fert_crop = c(0.8, 0.2),
    residue_recycled_model = c(0.1, 0.05),
    manure_crop = c(0.2, 0.1),
    dep_crop = c(0.05, 0.1),
    fix_crop = c(0.05, 0.1),
    fert_pas = c(0.1, 0.1),
    manure_pas = c(0.05, 0.05),
    dep_pas = c(0.01, 0.01),
    fix_pas = c(0.01, 0.01),
    crop_n = c(0.6, 0.1),
    pasture_n = c(0.05, 0.05),
    residue_n = c(0.1, 0.05),
    SOC = c(1, 1),
    GA = c(1e6, 1e6),
    PREC = c(1000, 1000),
    base_gas_crop = c(0.1, 0.05),
    base_gas_pas = c(0.02, 0.02),
    base_leach_crop = c(0.2, 0.2),
    base_leach_pas = c(0.02, 0.02),
    stringsAsFactors = FALSE
  )
}

# With NUE off, the scenario layer must leave the old-model flows unchanged.
base <- make_test_grid()
off <- apply_scenario_to_grid(base, nue_mitigation = FALSE)
expect_equal(off$fert_crop, base$fert_crop)
expect_equal(off$manure_crop, base$manure_crop)
expect_equal(off$crop_n, base$crop_n)
expect_equal(off$gas_crop, base$base_gas_crop)
expect_equal(off$leach_crop, base$base_leach_crop)
expect_equal(off$gas_pas, base$base_gas_pas)
expect_equal(off$leach_pas, base$base_leach_pas)
expect_equal(off$residue_recycled, base$residue_recycled_model)

# NUE solves total INfer from crop+residue recovery while holding recycled
# residue fixed. Only the synthetic portion may be reduced.
nue <- apply_scenario_to_grid(
  base,
  nue_mitigation = TRUE,
  nue_target = 0.70,
  include_residue_in_nue = TRUE
)
expect_equal(nue$nue_required_total_input[[1]], 1.0)
expect_equal(nue$nue_required_infer_crop[[1]], 0.70)
expect_equal(nue$nue_required_synthetic_fert_crop[[1]], 0.60)
expect_equal(nue$residue_recycled[[1]], 0.10)
expect_equal(nue$synthetic_fert_crop[[1]], 0.60)
expect_equal(nue$fert_crop[[1]], 0.70)
expect_equal(nue$crop_n, base$crop_n)
expect_equal(nue$residue_n, base$residue_n)
expect_equal(nue$nue_achieved[[1]], 0.70)
stopifnot(
  nue$nue_target_met[[1]],
  all(nue$crop_n_preserved),
  all(nue$residue_n_preserved)
)
expect_equal(
  nue$input_crop,
  nue$crop_n + nue$residue_n + nue$gas_crop + nue$leach_crop
)
expect_equal(
  nue$input_pas,
  nue$pasture_n + nue$gas_pas + nue$leach_pas
)
expect_equal(
  nue$input_total,
  nue$crop_n + nue$residue_n + nue$pasture_n +
    nue$gas_total + nue$leach_total
)

# Positive residuals are unchanged by removing the lower bound. This fixture
# still has positive crop and pasture residuals after its fertilizer reduction.
stopifnot(all(nue$leach_crop > 0), all(nue$leach_pas > 0))
expect_equal(
  nue$leach_crop,
  pmax(nue$input_crop - nue$crop_n - nue$residue_n - nue$gas_crop, 0)
)
expect_equal(
  nue$leach_pas,
  pmax(nue$input_pas - nue$pasture_n - nue$gas_pas, 0)
)

# NUE must preserve signed crop and pasture residuals, just as REF does.
# The target is already met, so this fixture needs no fertilizer change. Its
# base gas context is complete: differences cannot be caused by missing GA.
negative <- base[1, , drop = FALSE]
negative$crop_n <- 0.89
negative$pasture_n <- 0.16
negative_gas <- calculate_gas_losses(
  fert_crop = negative$fert_crop,
  manure_crop = negative$manure_crop,
  fert_pas = negative$fert_pas,
  manure_pas = negative$manure_pas,
  soc = negative$SOC,
  ga = negative$GA,
  prec = negative$PREC
)
negative$base_gas_crop <- negative_gas$gas_crop
negative$base_gas_pas <- negative_gas$gas_pas
negative$base_leach_crop <- with(
  negative,
  fert_crop + manure_crop + dep_crop + fix_crop -
    crop_n - residue_n - base_gas_crop
)
negative$base_leach_pas <- with(
  negative,
  fert_pas + manure_pas + dep_pas + fix_pas -
    pasture_n - base_gas_pas
)
negative_off <- apply_scenario_to_grid(negative, nue_mitigation = FALSE)
negative_nue <- apply_scenario_to_grid(
  negative,
  nue_mitigation = TRUE,
  nue_target = 0.70,
  include_residue_in_nue = TRUE
)
stopifnot(
  negative_nue$nue_target_met,
  negative_nue$leach_crop < 0,
  negative_nue$leach_pas < 0,
  negative_nue$leach_total < 0
)
expect_equal(negative_nue$fert_crop, negative_off$fert_crop)
expect_equal(negative_nue$crop_n, negative_off$crop_n)
expect_equal(negative_nue$residue_n, negative_off$residue_n)
expect_equal(negative_nue$pasture_n, negative_off$pasture_n)
expect_equal(negative_off$leach_crop, negative$base_leach_crop)
expect_equal(negative_off$leach_pas, negative$base_leach_pas)
expect_equal(negative_nue$leach_crop, negative_off$leach_crop)
expect_equal(negative_nue$leach_pas, negative_off$leach_pas)
expect_equal(negative_nue$leach_total, negative_off$leach_total)
expect_equal(
  negative_nue$leach_crop,
  negative_nue$input_crop - negative_nue$crop_n -
    negative_nue$residue_n - negative_nue$gas_crop
)
expect_equal(
  negative_nue$leach_pas,
  negative_nue$input_pas - negative_nue$pasture_n - negative_nue$gas_pas
)
expect_equal(
  negative_nue$input_total,
  negative_nue$crop_n + negative_nue$residue_n + negative_nue$pasture_n +
    negative_nue$gas_total + negative_nue$leach_total
)

# The residual helper changes no inputs, recovery, gas, or policy diagnostics.
for (candidate in list(nue, negative_nue)) {
  signed_result <- calculate_signed_leaching(candidate)
  unchanged_columns <- setdiff(
    names(candidate), c("leach_crop", "leach_pas", "leach_total")
  )
  stopifnot(identical(
    signed_result[, unchanged_columns, drop = FALSE],
    candidate[, unchanged_columns, drop = FALSE]
  ))
  expect_equal(signed_result$leach_crop, candidate$leach_crop)
  expect_equal(signed_result$leach_pas, candidate$leach_pas)
  expect_equal(signed_result$leach_total, candidate$leach_total)
}
empty_grid <- negative_nue[FALSE, , drop = FALSE]
stopifnot(identical(calculate_signed_leaching(empty_grid), empty_grid))

# Harvest-only NUE changes the policy numerator, not the physical N balance:
# crop-residue N must still be subtracted when leaching is recalculated.
harvest_only <- apply_scenario_to_grid(
  base[1, , drop = FALSE],
  nue_mitigation = TRUE,
  nue_target = 0.50,
  include_residue_in_nue = FALSE
)
expect_equal(harvest_only$nue_output, harvest_only$crop_n)
expect_equal(
  harvest_only$leach_crop,
  harvest_only$input_crop - harvest_only$crop_n -
    harvest_only$residue_n - harvest_only$gas_crop
)
expect_equal(
  harvest_only$input_crop,
  harvest_only$crop_n + harvest_only$residue_n +
    harvest_only$gas_crop + harvest_only$leach_crop
)

# Adjusted grid outputs keep the legacy schemas used by downstream scripts.
schema_env <- new.env(parent = emptyenv())
assign_grid_to_environment(schema_env, nue, "SSP2_BaU_NoCC_No")
stopifnot(
  identical(names(schema_env$INferdis), c("G", "YEMF", "Fert_Use_Grid")),
  identical(names(schema_env$INmandis), c("G", "YEMF", "ManureGrid")),
  identical(names(schema_env$OUTgasdis), c("G", "YEMF", "OUTgasgrid")),
  identical(
    names(schema_env$OUTgasdis_NH3),
    c("G", "YEMF", "SCENARIO", "OUTgasgrid")
  )
)

# Export to the existing VISIToRN-facing schema must retain the signed values.
signed_schema_env <- new.env(parent = emptyenv())
assign_grid_to_environment(signed_schema_env, negative_nue, "SSP2_BaU_NoCC_No")
expect_equal(signed_schema_env$OUTleadis$OUTleagrid, negative_nue$leach_total)
expect_equal(signed_schema_env$OUTleacropdis$OUTleacrop, negative_nue$leach_crop)
expect_equal(signed_schema_env$OUTleapasdis$OUTleagrid, negative_nue$leach_pas)

# Agricultural GEOS-Chem feedback grids retain only the NO share of the
# combined NOx+N2O pool and include all three agricultural NH3 components.
emission_env <- new.env(parent = emptyenv())
emission_env$OUTgasdis_NOx <- data.frame(
  G = c("g1", "g2"), YEMF = 2030L,
  SCENARIO = "SSP2_BaU_NoCC_No", OUTgasgrid = c(1.4, 2.8)
)
emission_env$OUTgasdis_NH3 <- data.frame(
  G = c("g1", "g2"), YEMF = 2030L,
  SCENARIO = "SSP2_BaU_NoCC_No", OUTgasgrid = c(0.1, 0.2)
)
emission_env$NH3_loss1dis <- data.frame(
  G = c("g1", "g2"), YEMF = 2030L,
  SCENARIO = "SSP2_BaU_NoCC_No", NH3_MMS_TgN_yr = c(0.3, 0.4)
)
emission_env$NH3_loss2dis <- data.frame(
  G = c("g1", "g2"), YEMF = 2030L,
  SCENARIO = "SSP2_BaU_NoCC_No", NH3_direct_TgN_yr = c(0.05, 0.15)
)
emission_env$NH3_loss1 <- data.frame(ManureStable = 0.7)
emission_env$NH3_loss2 <- data.frame(ManurePas = 0.2)
rebuild_agricultural_emission_grids(emission_env)
expect_equal(
  emission_env$OUTgasdis_NO_agriculture$OUTgasgrid,
  c(0.5, 1.0)
)
expect_equal(
  emission_env$OUTgasdis_NH3_agriculture$OUTgasgrid,
  c(0.45, 0.75)
)
stopifnot(
  identical(
    names(emission_env$OUTgasdis_NO_agriculture),
    c("G", "YEMF", "SCENARIO", "OUTgasgrid")
  ),
  identical(
    names(emission_env$OUTgasdis_NH3_agriculture),
    c("G", "YEMF", "SCENARIO", "OUTgasgrid")
  )
)
bad_emission_env <- list2env(as.list(emission_env), parent = emptyenv())
bad_emission_env$NH3_loss1 <- data.frame(ManureStable = 0.8)
stopifnot(expect_error(
  rebuild_agricultural_emission_grids(bad_emission_env)
))

# Fixed residue return can be the lower bound when a target calls for less
# total INfer than the model already recycles.
residue_floor <- base[1, , drop = FALSE]
residue_floor$residue_recycled_model <- 0.5
residue_floor$fert_crop <- 0.6
residue_floor$crop_n <- 0.1
residue_floor$residue_n <- 0
floor_result <- apply_scenario_to_grid(
  residue_floor,
  nue_mitigation = TRUE,
  nue_target = 0.75,
  include_residue_in_nue = TRUE
)
stopifnot(
  floor_result$nue_fertilizer_floor_limited[[1]],
  floor_result$residue_recycled[[1]] == 0.5,
  floor_result$synthetic_fert_crop[[1]] >= 0
)

summary_cfg <- list(
  scenario_name = "demfwr_nue",
  scenario_500c = FALSE,
  scenario_demfwr = TRUE,
  nue_mitigation = TRUE,
  include_residue_in_nue = TRUE,
  nue_adjustment_mode = "fertilizer_input",
  manure_recycling_mitigation = FALSE,
  baseline_manure_recycling_rate = 0.4
)
summary <- make_scenario_summary(nue, summary_cfg, 2030L)
stopifnot(
  summary$scenario_demfwr,
  summary$nue_mitigation,
  identical(summary$flow_unit, "Tg N yr-1")
)
expect_equal(summary$recycled_residue_crop, sum(base$residue_recycled_model))

# The registry generates every combination supported by the available raw
# inputs. APMTFR is currently complete only for Baseline, with NUE off/on.
master <- read_scenario_config(file.path(scenario_dir, "scenario_config.env"))
stopifnot(
  !master$scenario_gmd_inputs,
  scenario_seed(master$random_seed_base, "SSP2_BaU_NoCC_No", 2030L) ==
    scenario_seed(master$random_seed_base, "SSP2_BaU_NoCC_No", 2030L),
  scenario_seed(master$random_seed_base, "SSP2_BaU_NoCC_No", 2030L) !=
    scenario_seed(master$random_seed_base, "SSP2_BaU_NoCC_No", 2050L)
)

# Plot-only configuration parsing must not depend on calculation source files.
plot_only_config <- tempfile(fileext = ".env")
plot_only_lines <- readLines(
  file.path(scenario_dir, "scenario_config.env"),
  warn = FALSE
)
plot_only_lines <- sub(
  "^NUE_TARGET_FILE=.*$",
  "NUE_TARGET_FILE=missing_nue_targets.csv",
  plot_only_lines
)
plot_only_lines <- sub(
  "^RAW_MODEL_SCRIPT=.*$",
  "RAW_MODEL_SCRIPT=missing_raw_model.R",
  plot_only_lines
)
writeLines(plot_only_lines, plot_only_config, useBytes = TRUE)
plot_only_master <- read_scenario_config(
  plot_only_config,
  validate_calculation_files = FALSE
)
stopifnot(
  identical(plot_only_master$plot_scenarios, "all"),
  expect_error(read_scenario_config(plot_only_config))
)
unlink(plot_only_config)

single_master <- master
single_master$run_all_combinations <- FALSE
single_master$axis_switches <- lapply(single_master$axis_switches, function(x) FALSE)
selected_case <- expand_scenario_cases(single_master)
stopifnot(
  nrow(selected_case) == 1L,
  identical(selected_case$scenario_id, "baseline")
)
master$run_all_combinations <- TRUE
cases <- expand_scenario_cases(master)
expected_ids <- c(
  "baseline", "500c", "demfwr", "nue",
  "air_pollution", "500c_demfwr", "500c_nue", "demfwr_nue",
  "nue_air_pollution",
  "500c_demfwr_nue"
)
stopifnot(identical(cases$scenario_id, expected_ids))
expected_raw <- c(
  "SSP2_BaU_NoCC_No",
  "SSP2_500C_CACN_NoCC_No",
  "SSP2_BaU_DEMFWR_No",
  "SSP2_BaU_NoCC_No",
  "SSP2_BaU_NoCC_No",
  "SSP2_500C_CACN_DEMFWR_No",
  "SSP2_500C_CACN_NoCC_No",
  "SSP2_BaU_DEMFWR_No",
  "SSP2_BaU_NoCC_No",
  "SSP2_500C_CACN_DEMFWR_No"
)
stopifnot(identical(cases$raw_scenario, expected_raw))

# The GMD-paper family uses the historical scenario names without the _No
# suffix. In run-all mode it contains exactly Baseline and 500C, and uses the
# matching non-_No Baseline as the shared 2010 reference.
gmd_master <- master
gmd_master$scenario_gmd_inputs <- TRUE
gmd_master$run_all_combinations <- TRUE
gmd_cases <- expand_scenario_cases(gmd_master)
stopifnot(
  identical(gmd_cases$scenario_id, c("gmd_baseline", "gmd_500c")),
  identical(
    gmd_cases$raw_scenario,
    c("SSP2_BaU_NoCC", "SSP2_500C_CACN_NoCC")
  ),
  identical(gmd_cases$deposition_prefix, c("", ""))
)
gmd_500c_cfg <- make_case_config(
  gmd_master,
  gmd_cases[gmd_cases$scenario_id == "gmd_500c", , drop = FALSE]
)
stopifnot(
  gmd_500c_cfg$scenario_gmd_inputs,
  gmd_500c_cfg$scenario_500c,
  identical(gmd_500c_cfg$base_scenario, "SSP2_500C_CACN_NoCC"),
  identical(gmd_500c_cfg$summary_reference_scenario, "SSP2_BaU_NoCC")
)

gmd_single <- gmd_master
gmd_single$run_all_combinations <- FALSE
gmd_single$axis_switches <- lapply(gmd_single$axis_switches, function(x) FALSE)
stopifnot(identical(
  expand_scenario_cases(gmd_single)$scenario_id,
  "gmd_baseline"
))
gmd_single$axis_switches$`500c` <- TRUE
stopifnot(identical(
  expand_scenario_cases(gmd_single)$scenario_id,
  "gmd_500c"
))
gmd_single$axis_switches$nue <- TRUE
stopifnot(expect_error(expand_scenario_cases(gmd_single)))

# APMTFR changes only the deposition prefix and cannot be combined with 500C
# or DEMFWR until those GEOS-Chem calculations are complete.
ap_case <- cases[cases$scenario_id == "air_pollution", , drop = FALSE]
ap_cfg <- make_case_config(master, ap_case)
stopifnot(
  identical(ap_cfg$base_scenario, "SSP2_BaU_NoCC_No"),
  identical(
    ap_cfg$deposition_prefix_override,
    "SSP2_BaU_NoCC_No_APMTFR"
  )
)

# A blank external override leaves the registry untouched, while an explicit
# feedback prefix takes precedence without creating a new policy scenario.
stopifnot(identical(master$deposition_prefix_override, ""))
feedback_master <- master
feedback_master$deposition_prefix_override <- "FB_baseline_iter01_"
feedback_cfg <- make_case_config(
  feedback_master,
  cases[cases$scenario_id == "baseline", , drop = FALSE]
)
stopifnot(
  identical(feedback_cfg$scenario_name, "baseline"),
  identical(feedback_cfg$base_scenario, "SSP2_BaU_NoCC_No"),
  identical(
    feedback_cfg$deposition_prefix_override,
    "FB_baseline_iter01_"
  )
)
unsupported <- single_master
unsupported$axis_switches$`500c` <- TRUE
unsupported$axis_switches$air_pollution <- TRUE
stopifnot(expect_error(expand_scenario_cases(unsupported)))

ap_path <- nflow_resolve_deposition_file(
  tempdir(),
  "SSP2_BaU_NoCC_No",
  2030L,
  "dry",
  must_work = FALSE,
  deposition_prefix = "SSP2_BaU_NoCC_No_APMTFR"
)
stopifnot(endsWith(
  ap_path,
  "0505NDRYMON_SSP2_BaU_NoCC_No_APMTFR2030.nc"
))

# Native *_No cases use their matching GEOS-Chem deposition in future years,
# while the shared 2010 reference retains the historical Base deposition.
bau_future_path <- nflow_resolve_deposition_file(
  tempdir(),
  "SSP2_BaU_NoCC_No",
  2030L,
  "dry",
  must_work = FALSE
)
bau_reference_path <- nflow_resolve_deposition_file(
  tempdir(),
  "SSP2_BaU_NoCC_No",
  2010L,
  "wet",
  must_work = FALSE
)
# RawDataProcess passes the registered prefix explicitly. It must still retain
# the special common-reference mapping in 2010.
bau_raw_reference_path <- nflow_resolve_deposition_file(
  tempdir(),
  "SSP2_BaU_NoCC_No",
  2010L,
  "dry",
  must_work = FALSE,
  deposition_prefix = "SSP2_BaU_NoCC_No"
)
climate_future_path <- nflow_resolve_deposition_file(
  tempdir(),
  "SSP2_500C_CACN_NoCC_No",
  2100L,
  "wet",
  must_work = FALSE
)
stopifnot(
  endsWith(
    bau_future_path,
    "0505NDRYMON_SSP2_BaU_NoCC_No2030.nc"
  ),
  endsWith(bau_reference_path, "0505NWETMON_Base2010.nc"),
  endsWith(bau_raw_reference_path, "0505NDRYMON_Base2010.nc"),
  endsWith(
    climate_future_path,
    "0505NWETMON_SSP2_500C_CACN_NoCC_No2100.nc"
  )
)

# Input resolver agrees with the registry for all four native raw cases.
for (index in seq_len(nrow(cases))) {
  specification <- nflow_scenario_input_spec(cases$raw_scenario[[index]])
  stopifnot(
    specification$climate == if (cases$`500c`[[index]]) "500C" else "BaU",
    specification$demfwr == cases$demfwr[[index]]
  )
}

schedule <- read_nue_schedule(file.path(scenario_dir, "data", "nue_targets.csv"))
expect_equal(interpolate_schedule(schedule, 2030)$nue_target, 0.65)
expect_equal(interpolate_schedule(schedule, 2050)$nue_target, 0.70)
expect_equal(interpolate_schedule(schedule, 2100)$nue_target, 0.75)

cat("All scenario registry and NUE-extension tests passed.\n")
