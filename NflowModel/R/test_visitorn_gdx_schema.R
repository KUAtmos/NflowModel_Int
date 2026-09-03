args <- commandArgs(trailingOnly = TRUE)
project_root <- if (length(args) >= 1L) {
  normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
} else {
  normalizePath(file.path(getwd(), "../.."), winslash = "/", mustWork = TRUE)
}
visitorn_dir <- file.path(project_root, "VISIToRN")

suppressPackageStartupMessages({
  library(dplyr)
  library(gdxrrw)
})

gams_dir <- if (.Platform$OS.type == "windows") {
  "C:/GAMS/53"
} else {
  normalizePath(path.expand("~/gams39.3_linux_x64_64_sfx"), mustWork = TRUE)
}
gdxrrw::igdx(gams_dir)

builtin_vy <- gdxrrw::rgdx.param(
  file.path(visitorn_dir, "results_SSP2_BaU_NoCC.gdx"),
  "VY_load"
)
builtin_ga <- gdxrrw::rgdx.param(file.path(visitorn_dir, "data_prep.gdx"), "GA")
builtin_map <- gdxrrw::rgdx.set(file.path(visitorn_dir, "data_prep.gdx"), "MAP_GIJ")
cat("Built-in VY_load names:", paste(names(builtin_vy), collapse = ", "), "\n")
cat("Built-in GA names:", paste(names(builtin_ga), collapse = ", "), "\n")
cat("Built-in MAP_GIJ names:", paste(names(builtin_map), collapse = ", "), "\n")
cat("Built-in VY_load first row:", paste(as.character(builtin_vy[1L, ]), collapse = ", "), "\n")
cat("Built-in GA first row:", paste(as.character(builtin_ga[1L, ]), collapse = ", "), "\n")
cat("Built-in MAP_GIJ first row:", paste(as.character(builtin_map[1L, ]), collapse = ", "), "\n")

map_raw <- gdxrrw::rgdx(
  file.path(visitorn_dir, "data_prep.gdx"),
  list(name = "MAP_GIJ", form = "sparse")
)
cat("MAP_GIJ domains:", paste(map_raw$domains, collapse = ", "), "\n")
cat("MAP_GIJ val dimensions:", paste(dim(map_raw$val), collapse = " x "), "\n")
cat("MAP_GIJ UEL lengths:", paste(vapply(map_raw$uels, length, integer(1)), collapse = ", "), "\n")
cat(
  "MAP_GIJ decoded first row:",
  paste(vapply(seq_len(ncol(map_raw$val)), function(k) map_raw$uels[[k]][map_raw$val[1L, k]], character(1)), collapse = ", "),
  "\n"
)

read_parameter <- function(path, symbol_name) {
  x <- gdxrrw::rgdx(path, list(name = symbol_name, form = "sparse"))
  if (identical(symbol_name, "VY_load")) {
    cat("VY_load domains:", paste(x$domains, collapse = ", "), "\n")
    cat("VY_load UEL dimensions:", length(x$uels), "\n")
    cat("VY_load UEL lengths:", paste(vapply(x$uels, length, integer(1)), collapse = ", "), "\n")
    cat("VY_load fourth-dimension decoded head:", paste(x$uels[[4L]][utils::head(unique(x$val[, 4L]), 10L)], collapse = ", "), "\n")
  }
  if (identical(symbol_name, "GA")) {
    cat("GA domains:", paste(x$domains, collapse = ", "), "\n")
    cat("GA UEL dimensions:", length(x$uels), "\n")
    cat("GA UEL lengths:", paste(vapply(x$uels, length, integer(1)), collapse = ", "), "\n")
    cat("GA decoded head:", paste(x$uels[[1L]][utils::head(unique(x$val[, 1L]), 10L)], collapse = ", "), "\n")
  }
  out <- as.data.frame(x$val, stringsAsFactors = FALSE)
  n_dim <- ncol(out) - 1L
  if (length(x$uels) != n_dim) stop("Unexpected UEL dimensions for ", symbol_name)
  for (dimension in seq_len(n_dim)) {
    index <- as.integer(out[[dimension]])
    if (anyNA(index) || any(index < 1L | index > length(x$uels[[dimension]]))) {
      stop("Invalid UEL index for ", symbol_name)
    }
    out[[dimension]] <- x$uels[[dimension]][index]
  }
  valid_domains <-
    !is.null(x$domains) && length(x$domains) == n_dim &&
    all(nzchar(x$domains)) && all(x$domains != "*") && !anyDuplicated(x$domains)
  names(out)[seq_len(n_dim)] <- if (valid_domains) x$domains else paste0("i", seq_len(n_dim))
  names(out)[ncol(out)] <- "value"
  out[[symbol_name]] <- out$value
  out
}

vy <- read_parameter(
  file.path(visitorn_dir, "results_SSP2_BaU_NoCC.gdx"),
  "VY_load"
) %>%
  rename(G = i4)
ga <- read_parameter(file.path(visitorn_dir, "data_prep.gdx"), "GA")
joined <- left_join(vy, ga, by = "G") %>%
  mutate(LUAREA = VY_load * GA * 1000)

needed <- c("G", "VY_load", "GA", "LUAREA")
if (!all(needed %in% names(joined))) {
  stop("Missing columns after VY_load/GA join: ", paste(setdiff(needed, names(joined)), collapse = ", "))
}
cat("VY_load rows:", nrow(vy), "\n")
cat("GA rows:", nrow(ga), "\n")
cat("Joined rows:", nrow(joined), "\n")
cat("Missing VY_load:", sum(!is.finite(joined$VY_load)), "\n")
cat("Missing GA after join:", sum(!is.finite(joined$GA)), "\n")
cat("Unique VY_load G:", length(unique(vy$G)), "\n")
cat("Unique GA G:", length(unique(ga$G)), "\n")
cat("VY_load G range:", paste(range(vy$G), collapse = " to "), "\n")
cat("GA G range:", paste(range(ga$G), collapse = " to "), "\n")
cat("VY_load G head:", paste(utils::head(unique(vy$G), 10L), collapse = ", "), "\n")
cat("GA G head:", paste(utils::head(unique(ga$G), 10L), collapse = ", "), "\n")
if (any(!is.finite(joined$GA))) {
  cat(
    "Example unmatched G values:",
    paste(utils::head(unique(joined$G[!is.finite(joined$GA)]), 10L), collapse = ", "),
    "\n"
  )
}
if (!is.numeric(joined$LUAREA) || any(!is.finite(joined$LUAREA))) {
  stop("LUAREA contains a non-finite value.")
}

cat("GDX schema and LUAREA calculation passed.\n")
