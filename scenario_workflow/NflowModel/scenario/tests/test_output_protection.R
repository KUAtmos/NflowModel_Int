path <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
scenario_dir <- normalizePath(file.path(dirname(path), ".."), winslash = "/")
# Load the runner function definitions only; do not execute a scientific model.
for (expression in parse(file.path(scenario_dir, "run_nflow_scenarios.R"))) {
  if (is.call(expression) && identical(expression[[1]], as.name("<-")) &&
      is.call(expression[[3]]) &&
      identical(expression[[3]][[1]], as.name("function"))) {
    eval(expression, envir = globalenv())
  }
}
validate_case_inputs <- function(cfg) data.frame()
report_case <- function(...) invisible(NULL)
work <- tempfile("output_protection_")
dir.create(file.path(work, "baseline"), recursive = TRUE)
sentinel <- file.path(work, "baseline", "completion.csv")
writeLines("keep existing results", sentinel)
cfg <- list(nue_mitigation = FALSE, input_set = "current", gams_dir = "",
            output_dir = work, scenario_name = "baseline")
error <- tryCatch(run_one_case(cfg), error = identity)
stopifnot(inherits(error, "error"),
          grepl("Output directory is not empty", conditionMessage(error), fixed = TRUE),
          identical(readLines(sentinel), "keep existing results"))
unlink(work, recursive = TRUE)
cat("Existing-result protection test passed.\n")
