script_path <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]])
scenario_dir <- dirname(dirname(normalizePath(script_path, winslash = "/")))
r_dir <- file.path(scenario_dir, "..", "R")
source(file.path(r_dir, "grid_emissions_csv.R"))
source(file.path(scenario_dir, "R", "scenario_registry.R"))

equal <- function(actual, expected) {
  stopifnot(isTRUE(all.equal(actual, expected, tolerance = 1e-12, check.attributes = FALSE)))
}
expect_error <- function(expression) {
  stopifnot(inherits(tryCatch({force(expression); NULL}, error = identity), "error"))
}
fixture <- function(raw = "SSP2_BaU_NoCC") {
  env <- new.env(parent = emptyenv())
  add <- function(object, column, grids, values) {
    x <- data.frame(G = grids, YEMF = 2030L, SCENARIO = raw, value = values,
                    stringsAsFactors = FALSE)
    names(x)[4L] <- column
    # Add deliberately large unrelated year and scenario rows.
    other <- x[1L, ]; other$YEMF <- 2050L; other[[column]] <- 999
    wrong <- x[1L, ]; wrong$SCENARIO <- "WRONG"; wrong[[column]] <- 777
    assign(object, rbind(x, other, wrong), envir = env)
  }
  add("OUTgasdis", "OUTgasgrid", c("1", "720"), c(10, 20))
  add("OUTgascropdis", "OUTgascropgrid", c("1", "720"), c(6, 12))
  add("OUTgaspasdis", "OUTgaspasgrid", c("1", "720"), c(4, 8))
  add("OUTgasdis_N2", "OUTgasgrid", c("1", "720"), c(3, 6))
  add("OUTgasdis_NH3", "OUTgasgrid", c("1", "720"), c(2, 4))
  # Duplicate contributions to a border grid must be summed once.
  add("OUTgasdis_NOx", "OUTgasgrid", c("1", "1", "720"), c(2, 3, 10))
  # Species land shares deliberately differ from aggregate soil-gas shares.
  add("OUTgascropdis_N2", "OUTgascropgrid", c("1", "720"), c(1, 2))
  add("OUTgaspasdis_N2", "OUTgaspasgrid", c("1", "720"), c(2, 4))
  add("OUTgascropdis_NH3", "OUTgascropgrid", c("1", "720"), c(1.5, 3))
  add("OUTgaspasdis_NH3", "OUTgaspasgrid", c("1", "720"), c(0.5, 1))
  add("OUTgascropdis_NOx", "OUTgascropgrid", c("1", "1", "720"), c(1.5, 2, 7))
  add("OUTgaspasdis_NOx", "OUTgaspasgrid", c("1", "720"), c(1.5, 3))
  # The livestock-only emitting grid is absent from every soil source.
  add("NH3_loss1dis", "NH3_MMS_TgN_yr", c("1", "259200"), c(1, 7))
  add("NH3_loss2dis", "NH3_direct_TgN_yr", c("720", "259200"), c(2, 8))
  env$NH3_loss1 <- data.frame(ManureStable = 8)
  env$NH3_loss2 <- data.frame(ManurePas = 10)
  env
}

env <- fixture()
table <- nflow_build_grid_emissions(env, 2030L, "SSP2_BaU_NoCC", "gmd_baseline")
equal(table$G, c("1", "720", "259200"))
equal(names(table), nflow_grid_emission_columns())
stopifnot(!any(c("longitude", "latitude", "scenario_id", "scenario_label",
                "negative_emission_flag") %in% names(table)))
equal(table$N2_cropland_TgN_yr, c(1, 2, 0))
equal(table$N2_pasture_TgN_yr, c(2, 4, 0))
equal(table$N2_total_TgN_yr, c(3, 6, 0))
equal(table$NOx_total_TgN_yr, c(5, 10, 0) * 5 / 14)
equal(table$N2O_total_TgN_yr, c(5, 10, 0) * 9 / 14)
equal(table$NOx_cropland_TgN_yr, c(3.5, 7, 0) * 5 / 14)
equal(table$N2O_pasture_TgN_yr, c(1.5, 3, 0) * 9 / 14)
equal(table$NH3_soil_cropland_TgN_yr, c(1.5, 3, 0))
equal(table$NH3_soil_pasture_TgN_yr, c(0.5, 1, 0))
equal(table$NH3_others_TgN_yr, c(1, 2, 15))
equal(table$NH3_total_TgN_yr, c(3, 6, 15))
for (species in c("N2", "NOx", "N2O")) {
  equal(table[[paste0(species, "_total_TgN_yr")]],
        table[[paste0(species, "_cropland_TgN_yr")]] + table[[paste0(species, "_pasture_TgN_yr")]])
}
equal(sum(table$N2_total_TgN_yr + table$NOx_total_TgN_yr +
          table$N2O_total_TgN_yr + table$NH3_total_TgN_yr), 48)
signed <- fixture()
signed$OUTgasdis_N2$OUTgasgrid[1L] <- -3
signed$OUTgasdis$OUTgasgrid[1L] <- 4
signed$OUTgascropdis_N2$OUTgascropgrid[1L] <- -5
signed$OUTgascropdis$OUTgascropgrid[1L] <- 0
signed_table <- nflow_build_grid_emissions(signed, 2030L, "SSP2_BaU_NoCC", "gmd_baseline")
equal(signed_table$N2_total_TgN_yr, c(-3, 6, 0))
equal(signed_table$N2_cropland_TgN_yr, c(-5, 2, 0))
for (bad in c(NA, Inf, -1)) {
  broken <- fixture(); broken$NH3_loss1dis$NH3_MMS_TgN_yr[1L] <- bad
  expect_error(nflow_build_grid_emissions(broken, 2030L, "SSP2_BaU_NoCC", "gmd_baseline"))
}
broken <- fixture(); rm("NH3_loss2dis", envir = broken)
expect_error(nflow_build_grid_emissions(broken, 2030L, "SSP2_BaU_NoCC", "gmd_baseline"))
broken <- fixture(); rm("OUTgascropdis_N2", envir = broken)
expect_error(nflow_build_grid_emissions(broken, 2030L, "SSP2_BaU_NoCC", "gmd_baseline"))
broken <- fixture(); broken$OUTgascropdis_N2$OUTgascropgrid[1L] <- 5
expect_error(nflow_build_grid_emissions(broken, 2030L, "SSP2_BaU_NoCC", "gmd_baseline"))
broken <- fixture(); broken$NH3_loss1$ManureStable <- 80
expect_error(nflow_build_grid_emissions(broken, 2030L, "SSP2_BaU_NoCC", "gmd_baseline"))
broken <- fixture(); broken$OUTgasdis_NH3$OUTgasgrid[1L] <- 3
expect_error(nflow_build_grid_emissions(broken, 2030L, "SSP2_BaU_NoCC", "gmd_baseline"))
expect_error(nflow_build_grid_emissions(env, 2020L, "SSP2_BaU_NoCC", "gmd_baseline"))

work <- tempfile("grid-export-test-")
dir.create(work)
on.exit_cleanup <- function() unlink(work, recursive = TRUE)
output <- file.path(work, "exports", "gmd_baseline")
nflow_export_grid_emissions(env, 2030L, "SSP2_BaU_NoCC", "gmd_baseline", output, save_source = TRUE)
csv <- read.csv(file.path(output, "grid_emissions_2030.csv"), colClasses = c(G = "character"))
equal(csv, table)
cache <- new.env(parent = emptyenv())
load(file.path(output, "grid_emissions_source_2030.rdat"), envir = cache)
equal(nflow_build_grid_emissions(cache, 2030L, "SSP2_BaU_NoCC", "gmd_baseline"), table)
equal(cache$grid_emissions_metadata$schema_version, 2L)
definitions <- read.csv(file.path(output, "grid_emissions_columns.csv"))
equal(definitions$column, names(table))
nflow_export_grid_emissions(signed, 2030L, "SSP2_BaU_NoCC", "gmd_baseline",
                            file.path(work, "signed"))
negative <- read.csv(file.path(work, "signed", "grid_emissions_negative_2030.csv"), colClasses = c(G = "character"))
equal(negative, signed_table[1L, ])

# Exercise the actual scenario save hook for all ten supported combinations
# and both GMD cases; policy scenario IDs must survive the raw-scenario mapping.
expressions <- parse(file.path(scenario_dir, "run_nflow_scenarios.R"))
for (expression in expressions) {
  if (is.call(expression) && identical(expression[[1L]], as.name("<-")) &&
      is.call(expression[[3L]]) && identical(expression[[3L]][[1L]], as.name("function"))) {
    eval(expression, envir = globalenv())
  }
}
master <- list(run_all_combinations = TRUE, scenario_gmd_inputs = FALSE)
cases <- expand_scenario_cases(master)
master$scenario_gmd_inputs <- TRUE
cases <- rbind(cases, expand_scenario_cases(master))
stopifnot(nrow(cases) == 12L)
for (index in seq_len(nrow(cases))) {
  row <- cases[index, ]
  sample <- fixture(row$raw_scenario)
  sample$scenario_audit <- data.frame(G = c("1", "720"), YEMF = 2030L,
                                     longitude = c(-179.75, 179.75), latitude = 89.75)
  sample$scenario_summary <- data.frame(year = 2030L)
  sample$OUTleadis <- data.frame(G = "1", YEMF = 2030L, OUTleagrid = 0)
  cfg <- list(base_scenario = row$raw_scenario, scenario_name = row$scenario_id,
              scenario_label = row$scenario_label, export_visitorn = FALSE)
  destination <- file.path(work, "cases", row$scenario_id)
  dir.create(destination, recursive = TRUE)
  save_case_outputs(sample, cfg, 2030L, destination)
  exported <- read.csv(file.path(destination, "grid_emissions_2030.csv"))
  stopifnot(identical(names(exported), nflow_grid_emission_columns()),
            all(exported$raw_scenario == row$raw_scenario))
  saved <- new.env(parent = emptyenv())
  load(file.path(destination, "nflow_2030.rdat"), envir = saved)
  stopifnot(all(nflow_emission_sources()$object %in% ls(saved)))
  write.csv(data.frame(raw_scenario = row$raw_scenario, scenario_name = row$scenario_id,
                        scenario_label = row$scenario_label), file.path(destination, "manifest.csv"), row.names = FALSE)
  write.csv(data.frame(status = "completed"), file.path(destination, "completion.csv"), row.names = FALSE)
}

# Exercise the real GMD Make_Result_csv hook without running the expensive model.
if (file.exists(file.path(r_dir, "Make_Result_csv.r"))) {
make_expressions <- parse(file.path(r_dir, "Make_Result_csv.r"))
definition <- Filter(function(expression) {
  is.call(expression) && identical(expression[[1L]], as.name("<-")) &&
    identical(expression[[2L]], as.name("run_rawdataprocess_once"))
}, as.list(make_expressions))
eval(definition[[1L]], envir = globalenv())
save_manure_diagnostics <- save_outleadis_for_visitorn <- copy_plot_objects <- function(...) invisible(NULL)
tibble <- tibble::tibble
BASE_SCENARIOset <- "SSP2_BaU_NoCC"
MIT_SCENARIOset <- "SSP2_500C_CACN_NoCC"
fake_r <- file.path(work, "gmd_model", "R"); dir.create(fake_r, recursive = TRUE)
file.copy(file.path(r_dir, "grid_emissions_csv.R"), fake_r)
writeLines("list2env(.GlobalEnv$raw_fixture, envir = environment())", file.path(fake_r, "RawDataProcess.R"))
previous <- getwd(); setwd(fake_r)
for (raw in c(BASE_SCENARIOset, MIT_SCENARIOset)) {
  raw_fixture <- as.list(fixture(raw))
  run_rawdataprocess_once("test", raw, 2030L, make_result = FALSE)
}
raw_fixture <- as.list(fixture(BASE_SCENARIOset))
for (object in nflow_emission_sources()$object) {
  raw_fixture[[object]]$YEMF[raw_fixture[[object]]$YEMF == 2030L] <- 2020L
}
run_rawdataprocess_once("historical figure", BASE_SCENARIOset, 2020L, make_result = FALSE)
setwd(previous)
stopifnot(file.exists(file.path(work, "gmd_model/output/grid_emissions/gmd_baseline/grid_emissions_2030.csv")),
          file.exists(file.path(work, "gmd_model/output/grid_emissions/gmd_500c/grid_emissions_2030.csv")),
          !file.exists(file.path(work, "gmd_model/output/grid_emissions/gmd_baseline/grid_emissions_2020.csv")))

} else {
  message("Newer single-run GMD export hook is not shipped or tested here; ",
          "the root GMD scripts are preserved separately.")
}

# CLI end-to-end: all native cases and compact GMD caches; no raw model needed.
rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
status <- system2(rscript, c(shQuote(file.path(scenario_dir, "export_grid_emissions.R")),
                            "--input-dir", shQuote(file.path(work, "cases")),
                            "--output-dir", shQuote(file.path(work, "cli_exports")),
                            "--years", "2030"))
stopifnot(status == 0L)
missing_year_status <- suppressWarnings(system2(
  rscript, c(shQuote(file.path(scenario_dir, "export_grid_emissions.R")),
             "--input-dir", shQuote(file.path(work, "exports")),
             "--output-dir", shQuote(file.path(work, "missing_year")),
             "--years", "2030,2050"),
  stdout = file.path(work, "missing_year.log"), stderr = file.path(work, "missing_year_error.log")
))
stopifnot(missing_year_status != 0L, !dir.exists(file.path(work, "missing_year")))
status <- system2(rscript, c(shQuote(file.path(scenario_dir, "export_grid_emissions.R")),
                            "--input-dir", shQuote(file.path(work, "exports")),
                            "--output-dir", shQuote(file.path(work, "cli_cache"))))
stopifnot(status == 0L)
on.exit_cleanup()
cat("Grid-emissions CSV tests passed: selection, species/land sources, schema, mass checks, signed values, cache, GMD and all 10 scenarios.\n")
