script_argument <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(script_argument) > 0L) {
  sub("^--file=", "", script_argument[[1]])
} else {
  "run_nflow_plots.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
script_dir <- dirname(script_path)

source(file.path(script_dir, "R", "scenario_registry.R"), local = .GlobalEnv)
source(file.path(script_dir, "R", "config.R"), local = .GlobalEnv)

parse_args <- function(args) {
  result <- list(config = file.path(script_dir, "scenario_config.env"))
  index <- 1L
  while (index <= length(args)) {
    argument <- args[[index]]
    if (startsWith(argument, "--config=")) {
      result$config <- sub("^--config=", "", argument)
    } else if (identical(argument, "--config")) {
      index <- index + 1L
      if (index > length(args)) stop("--config requires a path.")
      result$config <- args[[index]]
    } else {
      stop("Unknown argument: ", argument)
    }
    index <- index + 1L
  }
  result
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
cfg <- read_scenario_config(args$config, validate_calculation_files = FALSE)
if (!dir.exists(cfg$output_dir)) {
  stop(
    "Scenario calculation output directory does not exist: ", cfg$output_dir,
    ". Run the calculation job before plotting."
  )
}

scenario_argument <- if (identical(cfg$plot_scenarios, "all")) {
  "all"
} else {
  paste(cfg$plot_scenarios, collapse = ",")
}
year_argument <- if (identical(cfg$plot_years, "all")) {
  "all"
} else {
  paste(cfg$plot_years, collapse = ",")
}

rscript <- file.path(
  R.home("bin"),
  if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
)
plot_script <- file.path(script_dir, "plot_nflow_scenarios.R")
plot_args <- c(
  shQuote(plot_script),
  "--scenario-root", shQuote(cfg$output_dir),
  "--scenarios", shQuote(scenario_argument),
  "--years", shQuote(year_argument),
  "--maps", if (cfg$plot_maps) "on" else "off"
)
if (nzchar(cfg$plot_output_dir)) {
  plot_args <- c(plot_args, "--output-dir", shQuote(cfg$plot_output_dir))
}

message("Plot-only scenario selection: ", scenario_argument)
message("Plot-only years: ", year_argument)
message("Reading saved calculations from: ", cfg$output_dir)
status <- system2(rscript, plot_args)
if (!identical(status, 0L)) {
  stop("Scenario plotting failed with exit status ", status, ".")
}
