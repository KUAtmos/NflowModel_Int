script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(script_arg) > 0L) {
  sub("^--file=", "", script_arg[[1]])
} else {
  "run_visitorn_scenarios.R"
}
script_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
visitorn_dir_default <- normalizePath(
  file.path(script_dir, ".."),
  winslash = "/",
  mustWork = TRUE
)
repository_dir_default <- normalizePath(
  file.path(visitorn_dir_default, ".."),
  winslash = "/",
  mustWork = TRUE
)

# Scenario jobs run on headless batch nodes.  Force the Cairo bitmap backend
# so every ggsave() call works without an X11 display.
if (isTRUE(capabilities("cairo"))) {
  options(bitmapType = "cairo")
} else if (!nzchar(Sys.getenv("DISPLAY"))) {
  stop(
    "PNG plotting requires Cairo on a headless node, but this R build has ",
    "no Cairo support. Use a Cairo-enabled R build or run under Xvfb."
  )
}

required_packages <- c(
  "dplyr", "tidyr", "readr", "ggplot2", "maps", "ncdf4",
  "inline", "RcppArmadillo"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing packages required for VISIToRN scenarios: ",
    paste(missing_packages, collapse = ", ")
  )
}

scenario_name_order <- c(
  "baseline",
  "500c",
  "demfwr",
  "nue",
  "air_pollution",
  "500c_demfwr",
  "500c_nue",
  "demfwr_nue",
  "nue_air_pollution",
  "500c_demfwr_nue"
)

scenario_display_labels <- c(
  baseline = "REF",
  `500c` = "CM",
  demfwr = "DFW",
  nue = "NUE",
  air_pollution = "AP",
  `500c_demfwr` = "CM + DFW",
  `500c_nue` = "CM + NUE",
  demfwr_nue = "DFW + NUE",
  nue_air_pollution = "NUE + AP",
  `500c_demfwr_nue` = "CM + DFW + NUE"
)

ordered_scenario_names <- function(values) {
  values <- unique(as.character(values))
  c(
    scenario_name_order[scenario_name_order %in% values],
    sort(setdiff(values, scenario_name_order))
  )
}

source(file.path(
  script_dir,
  "R",
  "visitorn_scenario_functions.R"
), local = .GlobalEnv)

parse_on_off <- function(value, key) {
  value <- tolower(trimws(as.character(value)))
  if (!value %in% c("on", "off")) {
    stop(key, " must be on or off.")
  }
  identical(value, "on")
}

split_csv <- function(value) {
  result <- trimws(strsplit(as.character(value), ",", fixed = TRUE)[[1]])
  result[nzchar(result)]
}

parse_args <- function(arguments) {
  result <- list(
    scenario_root = file.path(
      repository_dir_default,
      "NflowModel",
      "output",
      "scenarios"
    ),
    output_root = file.path(
      visitorn_dir_default,
      "output",
      "scenarios"
    ),
    comparison_root = file.path(
      visitorn_dir_default,
      "output",
      "comparisons"
    ),
    visitorn_dir = visitorn_dir_default,
    scenarios = "all",
    years = "all",
    nmip3 = "on",
    reference_year = "2010",
    reference_river_file = "",
    prepare_only = "off",
    comparison_only = "off",
    resume = "on"
  )
  valid <- names(result)

  index <- 1L
  while (index <= length(arguments)) {
    argument <- arguments[[index]]
    if (grepl("^--[^=]+=", argument)) {
      key <- sub("^--([^=]+)=.*$", "\\1", argument)
      value <- sub("^--[^=]+=", "", argument)
    } else if (startsWith(argument, "--")) {
      key <- sub("^--", "", argument)
      index <- index + 1L
      if (index > length(arguments)) {
        stop(argument, " requires a value.")
      }
      value <- arguments[[index]]
    } else {
      stop("Unknown argument: ", argument)
    }
    key <- gsub("-", "_", key)
    if (!key %in% valid) {
      stop("Unknown argument key: ", key)
    }
    result[[key]] <- value
    index <- index + 1L
  }

  for (key in c(
    "scenario_root", "output_root", "comparison_root", "visitorn_dir"
  )) {
    result[[key]] <- normalizePath(
      result[[key]],
      winslash = "/",
      mustWork = key %in% c("scenario_root", "visitorn_dir")
    )
  }
  result$nmip3 <- parse_on_off(result$nmip3, "--nmip3")
  result$reference_year <- suppressWarnings(as.integer(result$reference_year))
  if (length(result$reference_year) != 1L ||
      is.na(result$reference_year)) {
    stop("--reference-year must be one integer year.")
  }
  if (!nzchar(result$reference_river_file)) {
    result$reference_river_file <- file.path(
      result$output_root,
      "baseline",
      "tables",
      paste0("river_load_", result$reference_year, ".csv")
    )
  }
  result$reference_river_file <- normalizePath(
    result$reference_river_file,
    winslash = "/",
    mustWork = FALSE
  )
  result$prepare_only <- parse_on_off(
    result$prepare_only,
    "--prepare-only"
  )
  result$comparison_only <- parse_on_off(
    result$comparison_only,
    "--comparison-only"
  )
  if (result$prepare_only && result$comparison_only) {
    stop("--prepare-only and --comparison-only cannot both be on.")
  }
  result$resume <- parse_on_off(result$resume, "--resume")
  result
}

as_flag <- function(value) {
  tolower(as.character(value)) %in% c("true", "t", "1", "on", "yes")
}

scenario_label <- function(manifest) {
  name <- as.character(manifest$scenario_name[[1]])
  display_label <- unname(scenario_display_labels[name])
  if (length(display_label) == 1L &&
      !is.na(display_label) &&
      nzchar(display_label)) {
    return(display_label)
  }
  if ("scenario_label" %in% names(manifest)) {
    supplied <- trimws(as.character(manifest$scenario_label[[1]]))
    if (!is.na(supplied) && nzchar(supplied)) {
      return(supplied)
    }
  }
  name
}

discover_manifests <- function(root) {
  directories <- list.dirs(root, recursive = FALSE, full.names = TRUE)
  directories <- directories[file.exists(file.path(directories, "manifest.csv"))]
  if (length(directories) == 0L) {
    stop("No completed nitrogen-flow scenarios found under: ", root)
  }
  result <- lapply(directories, function(directory) {
    manifest <- utils::read.csv(
      file.path(directory, "manifest.csv"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    # Accept the former EAT-Lancet manifest names, but normalize every run to
    # the current 10-scenario schema used by NflowModel_Int_0.
    if (!"raw_scenario" %in% names(manifest) &&
        "base_scenario" %in% names(manifest)) {
      manifest$raw_scenario <- manifest$base_scenario
    }
    if (!"scenario_demfwr" %in% names(manifest) &&
        "eat_lancet" %in% names(manifest)) {
      manifest$scenario_demfwr <- manifest$eat_lancet
    }
    if (!"scenario_nue" %in% names(manifest) &&
        "nue_mitigation" %in% names(manifest)) {
      manifest$scenario_nue <- manifest$nue_mitigation
    }
    if (!"scenario_air_pollution" %in% names(manifest)) {
      manifest$scenario_air_pollution <- FALSE
    }
    required <- c(
      "scenario_name", "raw_scenario", "scenario_500c", "scenario_demfwr",
      "scenario_nue", "scenario_air_pollution", "years"
    )
    missing <- setdiff(required, names(manifest))
    if (length(missing) > 0L) {
      stop(
        "Manifest in ", directory, " is missing: ",
        paste(missing, collapse = ", "),
        ". Re-run the nitrogen-flow scenario."
      )
    }
    manifest$scenario_dir <- directory
    manifest$scenario_label <- scenario_label(manifest)
    manifest[1, , drop = FALSE]
  })
  manifests <- do.call(rbind, result)
  rownames(manifests) <- NULL
  if (anyDuplicated(manifests$scenario_name)) {
    stop("Duplicate scenario_name values found in nitrogen-flow outputs.")
  }
  ordered_names <- ordered_scenario_names(manifests$scenario_name)
  manifests <- manifests[
    match(ordered_names, manifests$scenario_name),
    ,
    drop = FALSE
  ]
  rownames(manifests) <- NULL
  manifests
}

comparison_identifier <- function(names, all_requested) {
  prefix <- if (all_requested) {
    paste0("all_", length(names), "__")
  } else {
    ""
  }
  raw <- paste0(prefix, paste(names, collapse = "__"))
  safe <- gsub("[^A-Za-z0-9_.-]+", "_", raw)
  if (nchar(safe) > 120L) {
    checksum <- sprintf("%06d", sum(utf8ToInt(safe)) %% 1000000L)
    safe <- paste0(substr(safe, 1L, 110L), "__", checksum)
  }
  safe
}

write_csv <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(value, path)
  invisible(path)
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
manifests <- discover_manifests(args$scenario_root)
all_requested <- identical(args$scenarios, "all")
if (!all_requested) {
  requested <- split_csv(args$scenarios)
  missing <- setdiff(requested, manifests$scenario_name)
  if (length(missing) > 0L) {
    stop(
      "Requested scenarios have no nitrogen-flow outputs: ",
      paste(missing, collapse = ", ")
    )
  }
  manifests <- manifests[
    match(requested, manifests$scenario_name),
    ,
    drop = FALSE
  ]
}

requested_years <- if (identical(args$years, "all")) {
  NULL
} else {
  years <- suppressWarnings(as.integer(split_csv(args$years)))
  if (length(years) == 0L || anyNA(years)) {
    stop("--years must be all or comma-separated integer years.")
  }
  unique(years)
}

dir.create(args$output_root, recursive = TRUE, showWarnings = FALSE)
dir.create(args$comparison_root, recursive = TRUE, showWarnings = FALSE)
static <- if (args$prepare_only || args$comparison_only) {
  NULL
} else {
  message("Loading VISIToRN static inputs...")
  load_visitorn_static_inputs(args$visitorn_dir)
}

all_river_results <- list()
scenario_run_manifests <- list()
for (scenario_index in seq_len(nrow(manifests))) {
  manifest <- manifests[scenario_index, , drop = FALSE]
  name <- manifest$scenario_name[[1]]
  label <- manifest$scenario_label[[1]]
  natural_land_scenario <- normalize_natural_land_scenario(
    manifest$raw_scenario[[1]]
  )
  available_years <- suppressWarnings(as.integer(split_csv(manifest$years[[1]])))
  years <- if (is.null(requested_years)) {
    available_years
  } else {
    requested_years
  }
  unavailable <- setdiff(years, available_years)
  if (length(unavailable) > 0L) {
    stop(
      "Scenario ", name, " does not contain requested years: ",
      paste(unavailable, collapse = ", ")
    )
  }

  scenario_output <- file.path(args$output_root, name)
  if (args$comparison_only) {
    river_path <- file.path(
      scenario_output,
      "tables",
      "river_load_all_years.csv"
    )
    if (!file.exists(river_path)) {
      stop(
        "Completed VISIToRN river output is missing for comparison-only mode: ",
        river_path
      )
    }
    scenario_river <- readr::read_csv(
      river_path,
      show_col_types = FALSE,
      progress = FALSE
    )
    required_river <- c(
      "rivnum", "river_basin_name", "n_load_kg", "n_load_gg", "year"
    )
    missing_river <- setdiff(required_river, names(scenario_river))
    if (length(missing_river) > 0L) {
      stop(
        river_path,
        " is missing: ",
        paste(missing_river, collapse = ", ")
      )
    }
    scenario_river <- scenario_river |>
      dplyr::filter(year %in% years)
    missing_years <- setdiff(years, unique(scenario_river$year))
    if (length(missing_years) > 0L) {
      stop(
        river_path,
        " does not contain requested years: ",
        paste(missing_years, collapse = ", ")
      )
    }
    scenario_river$scenario_name <- name
    scenario_river$scenario_label <- label
    all_river_results[[scenario_index]] <- scenario_river
    scenario_run_manifests[[scenario_index]] <- manifest
    next
  }
  input_dir <- file.path(scenario_output, "inputs")
  table_dir <- file.path(scenario_output, "tables")
  figure_dir <- file.path(scenario_output, "figures")
  raw_dir <- file.path(scenario_output, "raw")
  for (directory in c(input_dir, table_dir, figure_dir, raw_dir)) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }

  river_years <- list()
  concentration_years <- list()
  input_summaries <- list()
  for (year_index in seq_along(years)) {
    year <- years[[year_index]]
    message("Preparing VISIToRN input: ", name, " / ", year)
    natural_path <- natural_array_path(
      args$visitorn_dir,
      year,
      natural_land_scenario,
      args$nmip3
    )
    if (!file.exists(natural_path)) {
      stop(
        "Scenario-specific natural nitrogen input is missing: ",
        natural_path,
        "\nPrepare it with NflowModel/shell/",
        "prepare_visitorn_natural_inputs.sh before running VISIToRN."
      )
    }
    natural <- load_single_array(natural_path)
    outlea <- load_outleaching_with_coordinates(
      manifest$scenario_dir[[1]],
      year
    )
    nflow_source_paths <- c(
      file.path(
        manifest$scenario_dir[[1]],
        paste0("OUTleadis", year, ".rdat")
      ),
      file.path(
        manifest$scenario_dir[[1]],
        paste0("grid_audit_", year, ".csv")
      )
    )
    input_source_signature <- file_content_signature(c(
      natural_path,
      nflow_source_paths
    ))
    prepared <- make_visitorn_input(natural, outlea)
    input_metadata <- data.frame(
      scenario_name = name,
      scenario_label = label,
      year = year,
      scenario_500c = as_flag(manifest$scenario_500c[[1]]),
      scenario_demfwr = as_flag(manifest$scenario_demfwr[[1]]),
      scenario_nue = as_flag(manifest$scenario_nue[[1]]),
      scenario_air_pollution = as_flag(
        manifest$scenario_air_pollution[[1]]
      ),
      nmip3 = args$nmip3,
      natural_land_scenario = natural_land_scenario,
      natural_input_file = natural_path,
      nitrogen_flow_output = manifest$scenario_dir[[1]],
      input_source_signature = input_source_signature,
      created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      stringsAsFactors = FALSE
    )
    input_path <- file.path(
      input_dir,
      paste0("VISITnl_", name, "_", year, ".rdat")
    )
    save_visitorn_input(prepared$combined, input_metadata, input_path)
    input_summary <- cbind(input_metadata, prepared$summary)
    input_summaries[[year_index]] <- input_summary
    write_csv(
      input_summary,
      file.path(table_dir, paste0("input_summary_", year, ".csv"))
    )

    if (args$prepare_only) {
      rm(natural, outlea, prepared)
      invisible(gc())
      next
    }

    river_path <- file.path(table_dir, paste0("river_load_", year, ".csv"))
    concentration_path <- file.path(
      table_dir,
      paste0("concentration_summary_", year, ".csv")
    )
    routing_metadata_path <- file.path(
      table_dir,
      paste0("routing_metadata_", year, ".csv")
    )
    year_figure_dir <- file.path(figure_dir, as.character(year))
    year_raw_dir <- file.path(raw_dir, as.character(year))
    routing_raw_path <- file.path(year_raw_dir, "routing_output.rdat")
    routing_figure_paths <- file.path(
      year_figure_dir,
      c(
        "cumulative_n_outflow.png",
        "concentration_threshold_ratio.png"
      )
    )
    routing_source_signature <- file_content_signature(c(
      natural_path,
      nflow_source_paths,
      static$source_paths
    ))
    existing_metadata <- if (file.exists(routing_metadata_path)) {
      readr::read_csv(
        routing_metadata_path,
        show_col_types = FALSE,
        progress = FALSE
      )
    } else {
      NULL
    }
    signature_matches <- (
      !is.null(existing_metadata) &&
        nrow(existing_metadata) == 1L &&
        identical(
          existing_metadata$routing_source_signature[[1]],
          routing_source_signature
        )
    )
    can_resume <- (
      args$resume &&
        signature_matches &&
        all(file.exists(c(
          river_path,
          concentration_path,
          routing_raw_path,
          routing_figure_paths
        )))
    )
    if (
      can_resume
    ) {
      message("Using existing VISIToRN routing output: ", name, " / ", year)
      river <- readr::read_csv(
        river_path,
        show_col_types = FALSE,
        progress = FALSE
      )
      concentration_summary <- readr::read_csv(
        concentration_path,
        show_col_types = FALSE,
        progress = FALSE
      )
    } else {
      if (
        args$resume &&
          any(file.exists(c(
            river_path,
            concentration_path,
            routing_metadata_path
          )))
      ) {
        message(
          "Existing routing output is incomplete or its input checksum ",
          "changed; recalculating: ",
          name,
          " / ",
          year
        )
      }
      message("Running VISIToRN routing: ", name, " / ", year)
      routing <- run_visitorn_routing(
        prepared$combined,
        year,
        static,
        args$visitorn_dir
      )
      river <- aggregate_river_mouth_scenario(routing$qout_n, static)
      concentration_result <- make_concentration_summary(
        routing$qout_n,
        routing$qout_water
      )
      concentration_summary <- concentration_result$summary
      save_routing_maps(
        routing$qout_n,
        concentration_result$concentration,
        label,
        year,
        year_figure_dir
      )
      dir.create(year_raw_dir, recursive = TRUE, showWarnings = FALSE)
      QoutNint <- routing$qout_n
      Qout_int <- routing$qout_water
      C_fw_mgL <- concentration_result$concentration
      save(
        QoutNint,
        Qout_int,
        C_fw_mgL,
        file = routing_raw_path,
        compress = TRUE
      )
      routing_metadata <- data.frame(
        scenario_name = name,
        year = year,
        routing_source_signature = routing_source_signature,
        completed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        stringsAsFactors = FALSE
      )
      write_csv(routing_metadata, routing_metadata_path)
      rm(
        routing,
        concentration_result,
        QoutNint,
        Qout_int,
        C_fw_mgL
      )
      invisible(gc())
    }

    river$scenario_name <- name
    river$scenario_label <- label
    river$year <- year
    concentration_summary$scenario_name <- name
    concentration_summary$scenario_label <- label
    concentration_summary$year <- year
    write_csv(river, river_path)
    write_csv(concentration_summary, concentration_path)
    river_years[[year_index]] <- river
    concentration_years[[year_index]] <- concentration_summary
    rm(natural, outlea, prepared)
    invisible(gc())
  }

  write_csv(
    dplyr::bind_rows(input_summaries),
    file.path(table_dir, "input_summary_all_years.csv")
  )
  scenario_manifest <- manifest
  scenario_manifest$visitorn_nmip3 <- args$nmip3
  scenario_manifest$visitorn_prepare_only <- args$prepare_only
  scenario_manifest$visitorn_years <- paste(years, collapse = ",")
  scenario_manifest$visitorn_output <- scenario_output
  scenario_manifest$visitorn_completed_at <- format(
    Sys.time(),
    "%Y-%m-%dT%H:%M:%S%z"
  )
  write_csv(
    scenario_manifest,
    file.path(scenario_output, "manifest.csv")
  )
  scenario_run_manifests[[scenario_index]] <- scenario_manifest

  if (!args$prepare_only) {
    scenario_river <- dplyr::bind_rows(river_years)
    scenario_concentration <- dplyr::bind_rows(concentration_years)
    write_csv(
      scenario_river,
      file.path(table_dir, "river_load_all_years.csv")
    )
    write_csv(
      scenario_concentration,
      file.path(table_dir, "concentration_summary_all_years.csv")
    )
    save_scenario_river_plots(
      scenario_river,
      label,
      figure_dir
    )
    all_river_results[[scenario_index]] <- scenario_river
  }
}

if (!args$prepare_only) {
  reference_river <- load_reference_river_year(
    args$reference_river_file,
    args$reference_year
  )
  comparison_name <- comparison_identifier(
    manifests$scenario_name,
    all_requested
  )
  comparison_output <- file.path(args$comparison_root, comparison_name)
  save_comparison_outputs(
    dplyr::bind_rows(all_river_results),
    dplyr::bind_rows(scenario_run_manifests),
    comparison_output,
    reference_scenario = manifests$scenario_name[[1]],
    reference_river = reference_river,
    reference_year = args$reference_year
  )
  message(
    "VISIToRN comparison output: ",
    normalizePath(comparison_output, winslash = "/", mustWork = TRUE)
  )
}
message(
  "VISIToRN scenario outputs: ",
  normalizePath(args$output_root, winslash = "/", mustWork = TRUE)
)
