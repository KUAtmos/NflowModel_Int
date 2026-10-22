# Orchestrate the preserved scientific scripts without changing their contents.
# All stages share .GlobalEnv because figures/decomposition use prior objects.
run_gmd_workflow <- function(root) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  if (!nzchar(Sys.getenv("GAMS_DIR"))) {
    stop("Set GAMS_DIR to the installed GAMS directory before running.")
  }
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  nflow_dir <- file.path(root, "NflowModel", "R")
  river_dir <- file.path(root, "VISIToRN")
  stages <- c("Make_Result_csv.r", "Visualization_Nmodel.r",
              "VISIToRN_input.r", "Deco.R")
  for (stage in stages) {
    setwd(if (stage == "Deco.R") river_dir else nflow_dir)
    message("GMD stage: ", stage)
    source(file.path(nflow_dir, stage), local = .GlobalEnv)
  }
  message("GMD workflow completed. Check outputs and scientific diagnostics.")
  invisible(NULL)
}

if (sys.nframe() == 0L) {
  script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(script_arg) != 1L) stop("Cannot identify the launcher path.")
  script_path <- normalizePath(sub("^--file=", "", script_arg),
                               winslash = "/", mustWork = TRUE)
  run_gmd_workflow(dirname(dirname(script_path)))
}
