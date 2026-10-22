path <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
scenario_dir <- normalizePath(file.path(dirname(path), ".."), winslash = "/")
source(file.path(scenario_dir, "R", "scenario_registry.R"))
source(file.path(scenario_dir, "R", "config.R"))
cfg <- read_scenario_config(file.path(scenario_dir, "eight_cases.env"))
cases <- expand_scenario_cases(cfg)
stopifnot(nrow(cases) == 8, !any(cases$air_pollution))
cfg$run_scenarios <- c("baseline", "500c_nue")
stopifnot(identical(expand_scenario_cases(cfg)$scenario_id, cfg$run_scenarios))
for (bad in list("typo", c("all", "baseline"), c("baseline", "baseline"))) {
  cfg$run_scenarios <- bad
  stopifnot(inherits(tryCatch(expand_scenario_cases(cfg), error = identity), "error"))
}
cfg$run_scenarios <- "all"
cfg$run_all_combinations <- FALSE
cfg$axis_switches$"500c" <- TRUE
stopifnot(identical(expand_scenario_cases(cfg)$scenario_id, "500c"))
cat("Case selection tests passed.\n")
