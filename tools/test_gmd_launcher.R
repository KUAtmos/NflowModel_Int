# Small orchestration tests; no model inputs, GAMS or global calculations.
args <- commandArgs(TRUE)
driver <- normalizePath(if (length(args)) args[1] else "tools/run_gmd.R",
                        winslash = "/", mustWork = TRUE)
scope <- new.env(parent = .GlobalEnv)
sys.source(driver, envir = scope)
fixture <- tempfile("gmd_launcher_")
dir.create(file.path(fixture, "NflowModel", "R"), recursive = TRUE)
dir.create(file.path(fixture, "VISIToRN"))
writeLines(c('stopifnot(basename(getwd()) == "R")',
             '.gmd_test_trace <- "calculate"'),
           file.path(fixture, "NflowModel/R/Make_Result_csv.r"))
writeLines(c('stopifnot(identical(.gmd_test_trace, "calculate"))',
             '.gmd_test_trace <- c(.gmd_test_trace, "plot")'),
           file.path(fixture, "NflowModel/R/Visualization_Nmodel.r"))
writeLines(c('stopifnot(length(.gmd_test_trace) == 2L)',
             'setwd("../../VISIToRN")',
             '.gmd_test_trace <- c(.gmd_test_trace, "route")'),
           file.path(fixture, "NflowModel/R/VISIToRN_input.r"))
writeLines(c('stopifnot(basename(getwd()) == "VISIToRN")',
             'stopifnot(length(.gmd_test_trace) == 3L)',
             '.gmd_test_trace <- c(.gmd_test_trace, "decompose")'),
           file.path(fixture, "NflowModel/R/Deco.R"))
Sys.setenv(GAMS_DIR = fixture)
wd <- getwd()
scope$run_gmd_workflow(fixture)
stopifnot(identical(.gmd_test_trace, c("calculate", "plot", "route", "decompose")),
          identical(getwd(), wd))
writeLines('stop("intentional test failure")',
           file.path(fixture, "NflowModel/R/Make_Result_csv.r"))
.gmd_test_trace <- character()
failure <- tryCatch(scope$run_gmd_workflow(fixture), error = identity)
stopifnot(inherits(failure, "error"), length(.gmd_test_trace) == 0L,
          identical(getwd(), wd))
Sys.unsetenv("GAMS_DIR")
failure <- tryCatch(scope$run_gmd_workflow(fixture), error = identity)
stopifnot(inherits(failure, "error"))
message("GMD launcher tests passed: shared state, order, directories, fail-fast.")
