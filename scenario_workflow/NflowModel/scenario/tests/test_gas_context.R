script_argument <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- sub("^--file=", "", script_argument[[1]])
test_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
scenario_dir <- normalizePath(file.path(test_dir, ".."), winslash = "/")
source(file.path(scenario_dir, "R", "scenario_extensions.R"))

expect_equal <- function(actual, expected, tolerance = 1e-12) {
  comparison <- all.equal(actual, expected, tolerance = tolerance)
  if (!isTRUE(comparison)) stop(paste(comparison, collapse = "; "))
}

expect_error <- function(expression, label) {
  error <- tryCatch({ force(expression); NULL }, error = identity)
  if (!inherits(error, "error")) stop("Expected an error: ", label)
  invisible(error)
}

target_year <- 2030L
target_scenario <- "SSP2_BaU_NoCC_No"
ids <- c("101", "102", "103", "104", "105")
context <- data.frame(
  G = ids, YEMF = target_year, SCENARIO = target_scenario,
  SOC = c(1.1, 2.2, 1.7, 2.0, 0),
  GA = c(100000, 240000, 360000, 180000, 0),
  PREC = c(800, 1400, 1000, 600, 0), stringsAsFactors = FALSE
)

make_context_env <- function() {
  env <- new.env(parent = emptyenv())
  # 101: cropland only; 102: pasture only; 103: overlap; 104: total fallback.
  # All contexts use agricultural (cropland + pasture) area, as in the raw model.
  env$SOC_Fertcrop_P <- context[c(1, 3), , drop = FALSE]
  env$SOC_Fertpas_P <- context[c(2, 3), , drop = FALSE]
  env$SOC_Fert_P <- context[c(3, 4), , drop = FALSE]
  env
}

clone_env <- function(env) list2env(as.list(env), parent = emptyenv())
sort_context <- function(x) {
  out <- x[order(x$G), c("G", "YEMF", "SOC", "GA", "PREC"), drop = FALSE]
  rownames(out) <- NULL
  out
}

# Contexts are a union, not a first-available-table fallback. Shared rows must
# collapse once without changing values or multiplying the grid nitrogen flows.
env <- make_context_env()
actual <- extract_gas_context(env, target_year, target_scenario)
expected <- sort_context(context[1:4, ])
expect_equal(sort_context(actual), expected)
stopifnot(nrow(actual) == 4L, !anyDuplicated(actual$G))

# Each individual source is supported, including a legacy total-only table.
for (source_name in c("SOC_Fertcrop_P", "SOC_Fertpas_P", "SOC_Fert_P")) {
  one <- new.env(parent = emptyenv())
  one[[source_name]] <- context[1:4, ]
  expect_equal(sort_context(extract_gas_context(one, target_year, target_scenario)),
               expected)
}
empty_first <- make_context_env()
empty_first$SOC_Fertcrop_P <- context[FALSE, , drop = FALSE]
empty_first$SOC_Fert_P <- context[1:4, , drop = FALSE]
expect_equal(sort_context(extract_gas_context(empty_first, target_year, target_scenario)),
             expected)
expect_error(extract_gas_context(new.env(parent = emptyenv()), target_year,
                                 target_scenario), "all context sources missing")

# A zero-area placeholder cannot hide valid context from another source.
# A genuinely zero-area row is preserved separately below for compatibility
# with the original raw model, rather than fabricating agricultural land area.
placeholder <- make_context_env()
placeholder$SOC_Fert_P[placeholder$SOC_Fert_P$G == "103", "GA"] <- 0
expect_equal(sort_context(extract_gas_context(placeholder, target_year, target_scenario)),
             expected)

# Filtering precedes duplicate validation: other scenario/year values cannot
# supply the selected grid context and cannot cause false conflicts.
filtered <- clone_env(env)
irrelevant_year <- context[1:4, ]
irrelevant_year$YEMF <- 2050L
irrelevant_year$GA <- irrelevant_year$GA * 100
irrelevant_scenario <- context[1:4, ]
irrelevant_scenario$SCENARIO <- "SSP2_500C_CACN_NoCC_No"
irrelevant_scenario$SOC <- irrelevant_scenario$SOC + 30
filtered$SOC_Fertcrop_P <- rbind(filtered$SOC_Fertcrop_P,
                               irrelevant_year, irrelevant_scenario)
expect_equal(sort_context(extract_gas_context(filtered, target_year, target_scenario)),
             expected)

# Identical repeated source rows are legitimate joins; conflicting rows are not.
duplicates <- clone_env(env)
duplicates$SOC_Fertcrop_P <- rbind(duplicates$SOC_Fertcrop_P,
                                 duplicates$SOC_Fertcrop_P)
expect_equal(sort_context(extract_gas_context(duplicates, target_year, target_scenario)),
             expected)
for (field in c("SOC", "GA", "PREC")) {
  conflict <- clone_env(env)
  conflict$SOC_Fertpas_P[conflict$SOC_Fertpas_P$G == "103", field] <-
    conflict$SOC_Fertpas_P[conflict$SOC_Fertpas_P$G == "103", field] + 1
  expect_error(extract_gas_context(conflict, target_year, target_scenario),
               paste("conflicting cross-source", field))
  conflict <- clone_env(env)
  duplicate <- conflict$SOC_Fertcrop_P[1, , drop = FALSE]
  duplicate[[field]] <- duplicate[[field]] + 1
  conflict$SOC_Fertcrop_P <- rbind(conflict$SOC_Fertcrop_P, duplicate)
  expect_error(extract_gas_context(conflict, target_year, target_scenario),
               paste("conflicting within-source", field))
}

# A tiny complete raw-model environment exercises merging and active-grid
# validation, not only standalone context extraction. No real outputs are read.
flows <- data.frame(
  fert_crop = c(.08, 0, .06, .05, 0),
  manure_crop = c(.02, 0, .01, .01, 0),
  dep_crop = c(.005, 0, .005, .005, 0),
  fix_crop = c(.005, 0, .005, .005, 0),
  fert_pas = c(0, .015, .008, .006, 0),
  manure_pas = c(0, .025, .012, .010, 0),
  dep_pas = c(0, .002, .002, .002, 0),
  fix_pas = c(0, .003, .003, .003, 0),
  crop_n = c(.085, 0, .062, .055, 0),
  residue_n = c(.005, 0, .006, .003, 0),
  pasture_n = c(0, .045, .022, .019, 0)
)

# Independent transcription of RawDataProcess.R crop/pasture gas equations.
# These are deliberately not obtained by calling calculate_gas_losses().
crop_amount <- flows$fert_crop + flows$manure_crop
pasture_amount <- flows$fert_pas + flows$manure_pas
total_amount <- crop_amount + pasture_amount
crop_share <- ifelse(total_amount > 0, crop_amount / total_amount, 0)
pasture_share <- ifelse(total_amount > 0, pasture_amount / total_amount, 0)
background <- .025 + .000855 * context$PREC + .117 * context$SOC
nox_crop <- .01725 * crop_amount + background * crop_share * context$GA / 1e9
nox_pas <- .01725 * pasture_amount + background * pasture_share * context$GA / 1e9
flows$base_gas_crop <- nox_crop * (1 + 14 / 8) + .113 * crop_amount
flows$base_gas_pas <- nox_pas * (1 + 14 / 8) + .113 * pasture_amount
flows$base_leach_crop <- with(flows,
  fert_crop + manure_crop + dep_crop + fix_crop -
    (crop_n + residue_n) - base_gas_crop)
flows$base_leach_pas <- with(flows,
  fert_pas + manure_pas + dep_pas + fix_pas - pasture_n - base_gas_pas)

specifications <- list(
  c("INfercropdis", "Fert_Use_Grid", "fert_crop"),
  c("INmancropdis", "ManureCropGrid", "manure_crop"),
  c("INdepcropdis", "N_deposition_Crop", "dep_crop"),
  c("INfixcropdis", "N_fix_crop", "fix_crop"),
  c("INferpasdis", "Fert_Use_Grid", "fert_pas"),
  c("INmanpasdis", "ManurePasGrid", "manure_pas"),
  c("INdeppasdis", "N_deposition_Pas", "dep_pas"),
  c("INfixpasdis", "N_fix_pas", "fix_pas"),
  c("OUTcropcropdis", "CropN", "crop_n"),
  c("OUTcroppasdis", "OUTcroppas", "pasture_n"),
  c("OUTresdis", "OUTresgrid", "residue_n"),
  c("OUTgascropdis", "OUTgascropgrid", "base_gas_crop"),
  c("OUTgaspasdis", "OUTgaspasgrid", "base_gas_pas"),
  c("OUTleacropdis", "OUTleacrop", "base_leach_crop"),
  c("OUTleapasdis", "OUTleagrid", "base_leach_pas")
)
make_raw_env <- function() {
  raw <- make_context_env()
  for (spec in specifications) {
    part <- data.frame(G = ids, YEMF = target_year,
                       SCENARIO = target_scenario, value = flows[[spec[[3]]]],
                       stringsAsFactors = FALSE)
    names(part)[4] <- spec[[2]]
    raw[[spec[[1]]]] <- part
  }
  raw$grid_region_map <- data.frame(G = ids, RegionCode = "R1",
                                    stringsAsFactors = FALSE)
  raw
}
raw <- make_raw_env()
raw_snapshot <- as.list(raw)
grid <- build_scenario_grid(raw, target_year, target_scenario)
stopifnot(identical(as.list(raw), raw_snapshot))
stopifnot(identical(grid$G, ids), nrow(grid) == length(ids))
expect_equal(grid$GA, context$GA)
expect_equal(grid$SOC, context$SOC)
expect_equal(grid$PREC, context$PREC)
for (field in names(flows)) expect_equal(grid[[field]], flows[[field]])

# NUE is already met in each cropland grid. With fertilizer unchanged, gas
# recalculation must reproduce the original emissions, including pasture-only.
off <- apply_scenario_to_grid(grid, nue_mitigation = FALSE)
nue <- apply_scenario_to_grid(grid, nue_mitigation = TRUE, nue_target = .70,
                              include_residue_in_nue = TRUE)
for (field in c("fert_crop", "fert_pas", "manure_crop", "manure_pas",
                "crop_n", "residue_n", "pasture_n")) {
  expect_equal(nue[[field]], off[[field]])
}
for (field in c("gas_crop", "gas_pas", "gas_total", "leach_crop",
                "leach_pas", "leach_total")) {
  expect_equal(nue[[field]], off[[field]])
}
expect_equal(nue$gas_nox_crop, nox_crop)
expect_equal(nue$gas_nox_pas, nox_pas)
stopifnot(nue$gas_pas[2] > 0, nue$gas_crop[2] == 0,
          nue$leach_pas[2] < 0, nue$GA[5] == 0, nue$gas_total[5] == 0)
expect_equal(nue$input_total, nue$crop_n + nue$residue_n + nue$pasture_n +
               nue$gas_total + nue$leach_total)

# Active emission grids must never silently receive zero context. Missing,
# wrong-year and wrong-scenario pasture contexts all fail before NUE is run.
for (mode in c("absent", "wrong_year", "wrong_scenario")) {
  bad <- make_raw_env()
  if (mode == "absent") bad$SOC_Fertpas_P <- bad$SOC_Fertpas_P[-1, , drop = FALSE]
  if (mode == "wrong_year") bad$SOC_Fertpas_P$YEMF[1] <- 2050L
  if (mode == "wrong_scenario") bad$SOC_Fertpas_P$SCENARIO[1] <- "other"
  expect_error(build_scenario_grid(bad, target_year, target_scenario),
               paste("missing active pasture context:", mode))
}
for (field in c("SOC", "GA", "PREC")) {
  bad <- make_raw_env()
  bad$SOC_Fertpas_P[[field]] <- NULL
  expect_error(build_scenario_grid(bad, target_year, target_scenario),
               paste("missing required column", field))
  for (invalid in c(NA_real_, NaN, Inf, -Inf)) {
    bad <- make_raw_env()
    bad$SOC_Fertpas_P[[field]][1] <- invalid
    expect_error(build_scenario_grid(bad, target_year, target_scenario),
                 paste("nonfinite active context", field, invalid))
  }
}
for (area in c(0, -1)) {
  bad <- make_raw_env()
  bad$SOC_Fertpas_P$GA[1] <- area
  expect_error(build_scenario_grid(bad, target_year, target_scenario),
               "nonpositive agricultural area on an active pasture grid")
}

# Existing raw-model zero-area/zero-gas rows must stay unchanged even if they
# have nonzero nitrogen inputs. Fixing that upstream land classification is a
# distinct scientific change and is deliberately outside this context repair.
legacy_zero <- make_raw_env()
legacy_zero$SOC_Fertpas_P$GA[1] <- 0
legacy_zero$OUTgaspasdis$OUTgaspasgrid[2] <- 0
legacy_zero$OUTleapasdis$OUTleagrid[2] <- with(flows[2, ],
  fert_pas + manure_pas + dep_pas + fix_pas - pasture_n)
legacy_grid <- build_scenario_grid(legacy_zero, target_year, target_scenario)
legacy_nue <- apply_scenario_to_grid(legacy_grid, nue_mitigation = TRUE,
                                     nue_target = .70)
stopifnot(legacy_grid$GA[2] == 0, legacy_nue$gas_pas[2] == 0)
expect_equal(legacy_nue$fert_pas, legacy_grid$fert_pas)
expect_equal(legacy_nue$manure_pas, legacy_grid$manure_pas)
expect_equal(legacy_nue$leach_pas[2], legacy_zero$OUTleapasdis$OUTleagrid[2])

# Full finite coverage alone is insufficient: unchanged fertilizer/manure
# must reproduce the saved raw gas, so a hidden context mismatch fails early.
bad_gas <- make_raw_env()
bad_gas$OUTgaspasdis$OUTgaspasgrid[2] <- bad_gas$OUTgaspasdis$OUTgaspasgrid[2] + .001
expect_error(build_scenario_grid(bad_gas, target_year, target_scenario),
             "base pasture gas cannot be reproduced")
roundoff <- make_raw_env()
roundoff$OUTgaspasdis$OUTgaspasgrid[2] <- roundoff$OUTgaspasdis$OUTgaspasgrid[2] + 1e-12
invisible(build_scenario_grid(roundoff, target_year, target_scenario))

cat("All gas-context coverage, validation and no-policy-change tests passed.\n")
