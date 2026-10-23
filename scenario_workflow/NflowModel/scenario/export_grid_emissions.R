# Export completed, saved grid results without running RawDataProcess or GAMS.
script_argument <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_argument[[1L]]),
                              winslash = "/", mustWork = TRUE)
scenario_dir <- dirname(script_path)
source(file.path(scenario_dir, "..", "R", "grid_emissions_csv.R"))

parse_export_args <- function(args) {
  result <- list(input_dirs = character(),
                 output_dir = file.path(scenario_dir, "..", "output", "grid_emissions"),
                 scenarios = "all", years = "all", help = FALSE)
  index <- 1L
  while (index <= length(args)) {
    argument <- args[[index]]
    if (argument == "--help") {
      result$help <- TRUE
    } else {
      key <- sub("=.*$", "", argument)
      if (!key %in% c("--input-dir", "--output-dir", "--scenarios", "--years")) {
        stop("Unknown argument: ", argument)
      }
      if (grepl("=", argument, fixed = TRUE)) {
        value <- sub("^[^=]*=", "", argument)
      } else {
        index <- index + 1L
        if (index > length(args)) stop(key, " requires a value.")
        value <- args[[index]]
      }
      if (!nzchar(value)) stop(key, " requires a nonempty value.")
      if (key == "--input-dir") result$input_dirs <- c(result$input_dirs, value)
      if (key == "--output-dir") result$output_dir <- value
      if (key == "--scenarios") result$scenarios <- trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
      if (key == "--years") {
        result$years <- if (value == "all") "all" else {
          years <- suppressWarnings(as.integer(strsplit(value, ",", fixed = TRUE)[[1L]]))
          if (anyNA(years)) stop("--years requires 'all' or comma-separated integer years.")
          years
        }
      }
    }
    index <- index + 1L
  }
  if (!length(result$input_dirs)) {
    result$input_dirs <- c(file.path(scenario_dir, "..", "output", "scenarios"),
                           file.path(scenario_dir, "..", "output", "grid_emissions", "gmd_baseline"),
                           file.path(scenario_dir, "..", "output", "grid_emissions", "gmd_500c"))
    result$input_dirs <- result$input_dirs[dir.exists(result$input_dirs)]
  }
  result
}

export_saved_cases <- function(opts) {
  source_pattern <- "^(nflow_|grid_emissions_source_)[0-9]+[.]rdat$"
  candidates <- unlist(lapply(opts$input_dirs, function(root) {
    if (!dir.exists(root)) stop("Input directory not found: ", root)
    if (length(list.files(root, pattern = source_pattern))) return(root)
    list.dirs(root, recursive = FALSE, full.names = TRUE)
  }), use.names = FALSE)
  candidates <- unique(normalizePath(candidates, winslash = "/", mustWork = TRUE))
  candidates <- candidates[vapply(candidates, function(path) {
    length(list.files(path, pattern = source_pattern)) > 0L
  }, logical(1))]
  if (!identical(opts$scenarios, "all")) {
    missing <- setdiff(opts$scenarios, basename(candidates))
    if (length(missing)) stop("Saved scenario(s) not found: ", paste(missing, collapse = ", "))
    candidates <- candidates[basename(candidates) %in% opts$scenarios]
  }
  if (!length(candidates)) stop("No saved grid results found in the input directories.")
  manifest <- list()
  # Refuse collisions between result families (e.g. old and revised Baseline).
  keys <- character()
  jobs <- list()
  for (directory in candidates) {
    files <- list.files(directory, pattern = source_pattern, full.names = TRUE)
    years <- as.integer(sub(".*_([0-9]+)[.]rdat$", "\\1", files))
    # Prefer a compact export cache if both formats exist for a case/year.
    selected <- order(years, !grepl("^grid_emissions_source_", basename(files)))
    files <- files[selected]; years <- years[selected]
    keep <- !duplicated(years)
    if (!identical(opts$years, "all")) keep <- keep & years %in% opts$years
    for (index in which(keep)) {
      key <- paste(basename(directory), years[index], sep = "/")
      if (key %in% keys) stop("Multiple saved result families for ", key, ". Select one --input-dir.")
      keys <- c(keys, key)
      jobs[[length(jobs) + 1L]] <- list(directory = directory, file = files[index], year = years[index])
    }
  }
  if (!length(jobs)) stop("No saved result files match the requested years.")
  if (!identical(opts$years, "all")) {
    missing_years <- setdiff(opts$years, vapply(jobs, `[[`, integer(1), "year"))
    if (length(missing_years)) {
      stop("No saved results for requested year(s): ", paste(missing_years, collapse = ", "))
    }
  }
  for (job in jobs) {
    message("Reading saved grid result: ", job$file)
    env <- new.env(parent = emptyenv())
    load(job$file, envir = env)
    if (exists("grid_emissions_metadata", env, inherits = FALSE)) {
      metadata <- env$grid_emissions_metadata
    } else {
      path <- file.path(job$directory, "manifest.csv")
      if (!file.exists(path)) stop("Saved scenario metadata is missing: ", path)
      completion_path <- file.path(job$directory, "completion.csv")
      if (!file.exists(completion_path)) stop("Saved scenario has no completion marker: ", job$directory)
      completion <- utils::read.csv(completion_path, stringsAsFactors = FALSE)
      if (nrow(completion) != 1L || !"status" %in% names(completion) ||
          completion$status != "completed") stop("Saved scenario is incomplete: ", job$directory)
      row <- utils::read.csv(path, stringsAsFactors = FALSE)
      if (nrow(row) != 1L || !all(c("raw_scenario", "scenario_name", "scenario_label") %in% names(row))) {
        stop("Invalid saved scenario manifest: ", path)
      }
      metadata <- list(raw_scenario = row$raw_scenario, scenario_id = row$scenario_name,
                        scenario_label = row$scenario_label, year = job$year)
    }
    if (metadata$year != job$year) stop("Source filename and metadata years disagree: ", job$file)
    if (!grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", metadata$scenario_id)) stop("Invalid scenario ID.")
    output <- file.path(opts$output_dir, metadata$scenario_id)
    totals <- nflow_export_grid_emissions(env, job$year, metadata$raw_scenario,
                                          metadata$scenario_id, output,
                                          metadata$scenario_label)
    manifest[[length(manifest) + 1L]] <- data.frame(
      scenario_id = metadata$scenario_id, year = job$year,
      source_file = normalizePath(job$file, winslash = "/"),
      csv_file = normalizePath(file.path(output, paste0("grid_emissions_", job$year, ".csv")), winslash = "/"),
      source_size_bytes = file.info(job$file)$size,
      source_modified_at = format(file.info(job$file)$mtime, "%Y-%m-%dT%H:%M:%S%z"),
      exported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      stringsAsFactors = FALSE
    )
    rm(env, totals)
    invisible(gc(verbose = FALSE))
  }
  manifest <- do.call(rbind, manifest)
  # Keep a receipt for each invocation, including partial scenario selections.
  receipt <- file.path(opts$output_dir, paste0("export_manifest_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
  nflow_write_export_atomic(receipt, function(path) utils::write.csv(manifest, path, row.names = FALSE))
  message("Export completed: ", nrow(manifest), " case-years. Receipt: ", receipt)
  invisible(manifest)
}

opts <- parse_export_args(commandArgs(trailingOnly = TRUE))
if (opts$help) {
  cat("Usage: Rscript NflowModel/scenario/export_grid_emissions.R\n",
      "  [--input-dir DIR] [--output-dir DIR] [--scenarios all|ID,ID] [--years all|2030,2050]\n",
      "Default: saved output/scenarios plus GMD export caches; output/grid_emissions.\n",
      "Repeat --input-dir to select separate, nonoverlapping result families.\n")
} else {
  export_saved_cases(opts)
}
