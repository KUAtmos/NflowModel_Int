script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(script_arg) > 0L) {
  sub("^--file=", "", script_arg[[1]])
} else {
  "plot_nflow_scenarios.R"
}
script_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))

# Batch nodes normally have no X11 display.  R may nevertheless default to
# the Xlib PNG device, which makes ggsave() fail after all scenario runs have
# completed.  Prefer Cairo explicitly whenever it is available.
if (isTRUE(capabilities("cairo"))) {
  options(bitmapType = "cairo")
} else if (!nzchar(Sys.getenv("DISPLAY"))) {
  stop(
    "PNG plotting requires Cairo on a headless node, but this R build has ",
    "no Cairo support. Use a Cairo-enabled R build or run under Xvfb."
  )
}

required_packages <- c("dplyr", "tidyr", "readr", "ggplot2", "scales", "maps")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing packages required for scenario plotting: ",
    paste(missing_packages, collapse = ", ")
  )
}

scenario_name_order <- c(
  "baseline",
  "500c",
  "gmd_baseline",
  "gmd_500c",
  "demfwr",
  "nue",
  "air_pollution",
  "500c_demfwr",
  "500c_nue",
  "demfwr_nue",
  "nue_air_pollution",
  "500c_demfwr_nue"
)

# Compact labels used consistently across the multi-scenario figures.  Keep
# the machine-readable scenario_name unchanged in tables and manifests.
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

plot_text_scale <- 2
plot_base_size <- 18 * plot_text_scale

parse_on_off_plot <- function(value, key) {
  value <- tolower(trimws(value))
  if (!value %in% c("on", "off")) {
    stop(key, " must be on or off.")
  }
  identical(value, "on")
}

split_list_argument <- function(value) {
  values <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  values[nzchar(values)]
}

parse_plot_args <- function(args) {
  result <- list(
    scenario_root = normalizePath(
      file.path(script_dir, "..", "output", "scenarios"),
      winslash = "/",
      mustWork = FALSE
    ),
    output_dir = "",
    scenarios = "all",
    years = "all",
    maps = "on"
  )

  i <- 1L
  while (i <= length(args)) {
    argument <- args[[i]]
    if (argument %in% c(
      "--scenario-root", "--output-dir", "--scenarios", "--years", "--maps"
    )) {
      i <- i + 1L
      if (i > length(args)) {
        stop(argument, " requires a value.")
      }
      key <- sub("^--", "", argument)
      key <- gsub("-", "_", key)
      result[[key]] <- args[[i]]
    } else if (grepl("^--[^=]+=", argument)) {
      key <- sub("^--([^=]+)=.*$", "\\1", argument)
      value <- sub("^--[^=]+=", "", argument)
      key <- gsub("-", "_", key)
      if (!key %in% names(result)) {
        stop("Unknown argument: ", argument)
      }
      result[[key]] <- value
    } else {
      stop("Unknown argument: ", argument)
    }
    i <- i + 1L
  }

  result$scenario_root <- normalizePath(
    result$scenario_root,
    winslash = "/",
    mustWork = TRUE
  )
  result$output_dir <- if (nzchar(result$output_dir)) {
    normalizePath(result$output_dir, winslash = "/", mustWork = FALSE)
  } else {
    ""
  }
  result$maps <- parse_on_off_plot(as.character(result$maps), "--maps")
  result
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

as_manifest_flag <- function(value) {
  tolower(as.character(value)) %in% c("true", "t", "1", "on", "yes")
}

make_scenario_label <- function(manifest) {
  scenario_name <- as.character(manifest$scenario_name[[1]])
  display_label <- unname(scenario_display_labels[scenario_name])
  if (length(display_label) == 1L &&
      !is.na(display_label) &&
      nzchar(display_label)) {
    return(display_label)
  }
  label <- as.character(manifest$scenario_label[[1]])
  if (!nzchar(label)) scenario_name else label
}

discover_scenarios <- function(root) {
  directories <- list.dirs(root, recursive = FALSE, full.names = TRUE)
  directories <- directories[
    file.exists(file.path(directories, "manifest.csv")) &
      file.exists(file.path(directories, "completion.csv"))
  ]
  if (length(directories) == 0L) {
    stop(
      "No completed scenario outputs were found under: ", root,
      ". Run the calculation job first. Plotting never starts a calculation."
    )
  }

  manifests <- lapply(directories, function(directory) {
    manifest <- utils::read.csv(
      file.path(directory, "manifest.csv"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    required <- c(
      "scenario_name", "scenario_label", "scenario_500c",
      "scenario_demfwr", "scenario_nue", "scenario_air_pollution", "years"
    )
    missing <- setdiff(required, names(manifest))
    if (length(missing) > 0L) {
      stop(
        "Manifest is missing columns in ", directory, ": ",
        paste(missing, collapse = ", "),
        ". Re-run this scenario with the current runner."
      )
    }
    completion <- utils::read.csv(
      file.path(directory, "completion.csv"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    completion_required <- c("status", "scenario_name", "years")
    completion_missing <- setdiff(completion_required, names(completion))
    if (length(completion_missing) > 0L) {
      stop(
        "Completion marker is missing columns in ", directory, ": ",
        paste(completion_missing, collapse = ", ")
      )
    }
    if (nrow(completion) != 1L ||
        !identical(as.character(completion$status[[1]]), "completed") ||
        !identical(
          as.character(completion$scenario_name[[1]]),
          as.character(manifest$scenario_name[[1]])
        ) ||
        !identical(
          as.character(completion$years[[1]]),
          as.character(manifest$years[[1]])
        )) {
      stop("Invalid completion marker in: ", directory)
    }
    manifest$scenario_dir <- directory
    manifest$scenario_label <- make_scenario_label(manifest)
    manifest[1, , drop = FALSE]
  })
  result <- do.call(rbind, manifests)
  rownames(result) <- NULL
  if (anyDuplicated(result$scenario_name)) {
    stop("Duplicate scenario_name values were found in output manifests.")
  }
  ordered_names <- ordered_scenario_names(result$scenario_name)
  result <- result[
    match(ordered_names, result$scenario_name),
    ,
    drop = FALSE
  ]
  rownames(result) <- NULL
  result
}

read_scenario_audits <- function(manifest_row,
                                 requested_years,
                                 reference_audit = NULL,
                                 reference_year = 2010L) {
  files <- list.files(
    manifest_row$scenario_dir[[1]],
    pattern = "^grid_audit_[0-9]+\\.csv$",
    full.names = TRUE
  )
  if (length(files) == 0L) {
    stop("No grid_audit files found for ", manifest_row$scenario_name[[1]], ".")
  }

  file_years <- as.integer(sub(
    "^grid_audit_([0-9]+)\\.csv$",
    "\\1",
    basename(files)
  ))
  manifest_years <- suppressWarnings(as.integer(split_list_argument(
    as.character(manifest_row$years[[1]])
  )))
  if (length(manifest_years) == 0L || anyNA(manifest_years)) {
    stop(
      "Manifest has an invalid years field for ",
      manifest_row$scenario_name[[1]], "."
    )
  }
  manifest_years <- unique(manifest_years)
  selected_years <- if (identical(requested_years, "all")) {
    manifest_years
  } else {
    allowed_years <- manifest_years
    if (!is.null(reference_audit)) {
      allowed_years <- unique(c(allowed_years, as.integer(reference_year)))
    }
    outside_run <- setdiff(requested_years, allowed_years)
    if (length(outside_run) > 0L) {
      stop(
        "Scenario ", manifest_row$scenario_name[[1]],
        " was not completed for requested years: ",
        paste(outside_run, collapse = ", ")
      )
    }
    requested_years
  }
  missing_years <- setdiff(selected_years, file_years)
  if (!is.null(reference_audit)) {
    missing_years <- setdiff(missing_years, as.integer(reference_year))
  }
  if (length(missing_years) > 0L) {
    stop(
      "Scenario ", manifest_row$scenario_name[[1]],
      " is missing completed grid audits for: ",
      paste(missing_years, collapse = ", ")
    )
  }
  keep <- file_years %in% selected_years
  files <- files[keep]
  file_years <- file_years[keep]
  result <- lapply(seq_along(files), function(index) {
    audit <- readr::read_csv(
      files[[index]],
      show_col_types = FALSE,
      progress = FALSE
    )
    audit$scenario_name <- manifest_row$scenario_name[[1]]
    audit$scenario_label <- manifest_row$scenario_label[[1]]
    audit$plot_year <- file_years[[index]]
    audit
  })
  if (!is.null(reference_audit) &&
      as.integer(reference_year) %in% selected_years &&
      !as.integer(reference_year) %in% file_years) {
    common_reference <- reference_audit
    common_reference$scenario_name <- manifest_row$scenario_name[[1]]
    common_reference$scenario_label <- manifest_row$scenario_label[[1]]
    common_reference$plot_year <- as.integer(reference_year)
    result[[length(result) + 1L]] <- common_reference
  }
  if (length(result) == 0L) {
    return(NULL)
  }
  dplyr::bind_rows(result)
}

validate_audit_columns <- function(audit) {
  required <- c(
    "G", "YEMF", "RegionCode",
    "fert_crop", "manure_crop", "dep_crop", "fix_crop",
    "fert_pas", "manure_pas", "dep_pas", "fix_pas",
    "crop_n", "pasture_n", "residue_n",
    "gas_total", "leach_total", "input_crop"
  )
  missing <- setdiff(required, names(audit))
  if (length(missing) > 0L) {
    stop("Grid audit is missing columns: ", paste(missing, collapse = ", "))
  }

  if (!"nue_output" %in% names(audit)) {
    audit$nue_output <- audit$crop_n + audit$residue_n
  }
  if (!"nue_target" %in% names(audit)) {
    audit$nue_target <- NA_real_
  }
  if (!"nue_infeasible" %in% names(audit)) {
    audit$nue_infeasible <- FALSE
  }
  audit
}

make_component_tables <- function(audit) {
  inputs <- audit |>
    dplyr::transmute(
      scenario_name,
      scenario_label,
      year = as.integer(YEMF),
      region = as.character(RegionCode),
      Fertilizer = fert_crop + fert_pas,
      Manure = manure_crop + manure_pas,
      Deposition = dep_crop + dep_pas,
      Fixation = fix_crop + fix_pas
    ) |>
    tidyr::pivot_longer(
      cols = c("Fertilizer", "Manure", "Deposition", "Fixation"),
      names_to = "component",
      values_to = "value"
    ) |>
    dplyr::mutate(flow_type = "Input")

  outputs <- audit |>
    dplyr::transmute(
      scenario_name,
      scenario_label,
      year = as.integer(YEMF),
      region = as.character(RegionCode),
      Uptake = crop_n + pasture_n,
      Residues = residue_n,
      `Gaseous loss` = gas_total,
      Leaching = leach_total
    ) |>
    tidyr::pivot_longer(
      cols = c("Uptake", "Residues", "Gaseous loss", "Leaching"),
      names_to = "component",
      values_to = "value"
    ) |>
    dplyr::mutate(flow_type = "Output")

  regional <- dplyr::bind_rows(inputs, outputs) |>
    dplyr::group_by(
      scenario_name, scenario_label, year, region, flow_type, component
    ) |>
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  global <- regional |>
    dplyr::group_by(
      scenario_name, scenario_label, year, flow_type, component
    ) |>
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  list(regional = regional, global = global)
}

make_nue_table <- function(audit) {
  finite_target <- function(values) {
    values <- values[is.finite(values)]
    if (length(values) == 0L) NA_real_ else values[[1]]
  }

  audit |>
    dplyr::group_by(scenario_name, scenario_label, YEMF) |>
    dplyr::summarise(
      nue = if (sum(input_crop, na.rm = TRUE) > 0) {
        sum(nue_output, na.rm = TRUE) / sum(input_crop, na.rm = TRUE)
      } else {
        NA_real_
      },
      nue_target = finite_target(nue_target),
      infeasible_grid_share = mean(as.logical(nue_infeasible), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(year = YEMF)
}

component_palette <- c(
  Fertilizer = "#0072B2",
  Manure = "#009E73",
  Deposition = "#56B4E9",
  Fixation = "#CCBB44",
  Uptake = "#CC79A7",
  Residues = "#666666",
  `Gaseous loss` = "#E69F00",
  Leaching = "#D55E00"
)

region_labels <- c(
  BRA = "Brazil",
  CAN = "Canada",
  CHN = "China",
  CIS = "Former\nSoviet Union",
  IND = "India",
  JPN = "Japan",
  TUR = "Turkey",
  USA = "United States",
  XAF = "Sub-Saharan\nAfrica",
  XER = "Non-EU Europe",
  XE25 = "EU",
  XLM = "Latin America",
  XME = "Middle East",
  XNF = "North Africa",
  XOC = "Australia and\nNew Zealand",
  XSA = "Other Asia",
  XSE = "Other East and\nSoutheast Asia"
)

label_region <- function(values) {
  labels <- unname(region_labels[as.character(values)])
  labels[is.na(labels)] <- as.character(values)[is.na(labels)]
  labels
}

save_component_plot <- function(table, selected_flow_type, regional, path) {
  plot_data <- table |>
    dplyr::filter(.data$flow_type == .env$selected_flow_type)
  # The common 2010 reference remains in the saved component tables for
  # diagnostics, but the global scenario-comparison bars show future years
  # only. Regional products retain the years requested in the configuration.
  if (!regional) {
    plot_data <- plot_data |>
      dplyr::filter(.data$year != 2010L)
  }
  if (nrow(plot_data) == 0L) {
    stop("No component data found for flow type: ", selected_flow_type)
  }
  if (
    anyNA(plot_data$component) ||
      anyNA(plot_data$value) ||
      any(!is.finite(plot_data$value)) ||
      !all(plot_data$flow_type == selected_flow_type)
  ) {
    stop("Invalid or mixed component data for flow type: ", selected_flow_type)
  }

  scenario_levels <- levels(plot_data$scenario_label)
  if (is.null(scenario_levels)) {
    scenario_levels <- unique(as.character(plot_data$scenario_label))
  }
  scenario_levels <- scenario_levels[
    scenario_levels %in% unique(as.character(plot_data$scenario_label))
  ]
  year_levels <- sort(unique(as.integer(plot_data$year)))
  plot_data <- plot_data |>
    dplyr::mutate(
      year = factor(as.integer(year), levels = year_levels),
      scenario_label = factor(
        as.character(scenario_label),
        levels = scenario_levels
      )
    )

  if (regional) {
    case_levels <- unlist(lapply(year_levels, function(selected_year) {
      paste(selected_year, scenario_levels)
    }), use.names = FALSE)
    plot_data <- plot_data |>
      dplyr::mutate(
        plot_x = factor(
          paste(as.character(year), as.character(scenario_label)),
          levels = case_levels
        )
      )
  } else {
    plot_data$plot_x <- plot_data$scenario_label
  }

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = plot_x, y = value, fill = component)
  ) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::scale_fill_manual(values = component_palette, drop = FALSE) +
    ggplot2::labs(
      x = NULL,
      y = paste0("Nitrogen ", tolower(selected_flow_type), " (Tg N yr-1)"),
      fill = "Component"
    ) +
    ggplot2::theme_minimal(base_size = plot_base_size) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5,
        size = 15 * plot_text_scale
      ),
      axis.text.y = ggplot2::element_text(size = 15 * plot_text_scale),
      axis.title.y = ggplot2::element_text(size = 18 * plot_text_scale),
      strip.text = ggplot2::element_text(
        size = 18 * plot_text_scale,
        face = "bold"
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 17 * plot_text_scale),
      legend.text = ggplot2::element_text(size = 16 * plot_text_scale),
      plot.margin = ggplot2::margin(15, 18, 22, 18)
    )

  if (regional) {
    plot <- plot + ggplot2::facet_wrap(
      ggplot2::vars(region),
      scales = "free_y",
      ncol = 3
    )
  } else {
    plot <- plot + ggplot2::facet_grid(
      cols = ggplot2::vars(year),
      scales = "free_x",
      space = "free_x"
    )
  }
  ggplot2::ggsave(
    path,
    plot = plot,
    width = if (regional) 36 else max(32, 10 * length(year_levels)),
    height = if (regional) 42 else 20,
    dpi = 260,
    limitsize = FALSE,
    bg = "white"
  )
}

save_gmd_style_component_plot <- function(table, selected_flow_type, path) {
  plot_data <- table |>
    dplyr::filter(.data$flow_type == .env$selected_flow_type) |>
    dplyr::mutate(
      year = factor(year, levels = sort(unique(year))),
      region = factor(
        region,
        levels = c(names(region_labels), setdiff(unique(region), names(region_labels)))
      ),
      component = dplyr::recode(
        component,
        Uptake = "Crop uptake",
        Leaching = "Leaching loss"
      )
    )

  colors <- c(
    component_palette,
    `Crop uptake` = unname(component_palette[["Uptake"]]),
    `Leaching loss` = unname(component_palette[["Leaching"]])
  )
  component_order <- if (identical(selected_flow_type, "Input")) {
    c("Fertilizer", "Manure", "Deposition", "Fixation")
  } else {
    c("Crop uptake", "Residues", "Gaseous loss", "Leaching loss")
  }
  plot_data$component <- factor(plot_data$component, levels = component_order)
  if (
    nrow(plot_data) == 0L ||
      anyNA(plot_data$component) ||
      anyNA(plot_data$value) ||
      any(!is.finite(plot_data$value)) ||
      !all(plot_data$flow_type == selected_flow_type)
  ) {
    stop(
      "Invalid, missing, or mixed GMD component data for flow type: ",
      selected_flow_type
    )
  }

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = year, y = value, fill = component)
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::facet_wrap(
      ggplot2::vars(region),
      scales = "free_y",
      ncol = 3,
      labeller = ggplot2::labeller(region = label_region)
    ) +
    ggplot2::scale_fill_manual(values = colors, drop = FALSE) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::labs(
      x = NULL,
      y = paste0("Nitrogen ", tolower(selected_flow_type), " (Tg N yr-1)"),
      fill = "Component"
    ) +
    ggplot2::theme_bw(base_size = plot_base_size) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "grey95"),
      strip.text = ggplot2::element_text(
        size = 18 * plot_text_scale,
        face = "bold"
      ),
      axis.text = ggplot2::element_text(size = 15 * plot_text_scale),
      axis.title.y = ggplot2::element_text(size = 18 * plot_text_scale),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 17 * plot_text_scale),
      legend.text = ggplot2::element_text(size = 16 * plot_text_scale)
    )

  ggplot2::ggsave(
    path,
    plot = plot,
    width = 24,
    height = 38,
    dpi = 300,
    bg = "white"
  )
}

save_gmd_style_scenario_plots <- function(regional_components, output_dir) {
  scenario_names <- unique(regional_components$scenario_name)
  for (selected_scenario in scenario_names) {
    scenario_table <- regional_components |>
      dplyr::filter(.data$scenario_name == .env$selected_scenario)
    if (
      nrow(scenario_table) == 0L ||
        !identical(unique(scenario_table$scenario_name), selected_scenario)
    ) {
      stop("Scenario plot data filtering failed for: ", selected_scenario)
    }
    scenario_dir <- file.path(output_dir, "by_scenario", selected_scenario)
    dir.create(scenario_dir, recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(
      scenario_table,
      file.path(scenario_dir, "regional_components.csv")
    )
    save_gmd_style_component_plot(
      scenario_table,
      "Input",
      file.path(scenario_dir, "Input_17_stacked_components.png")
    )
    save_gmd_style_component_plot(
      scenario_table,
      "Output",
      file.path(scenario_dir, "Output_17_finalrep.png")
    )
  }
  invisible(scenario_names)
}

save_nue_plot <- function(nue, path) {
  nue <- nue |>
    dplyr::mutate(year = factor(year, levels = sort(unique(year))))

  plot <- ggplot2::ggplot(
    nue,
    ggplot2::aes(x = year, y = nue, fill = scenario_label)
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge(width = 0.8),
      width = 0.72
    ) +
    ggplot2::geom_point(
      data = nue[is.finite(nue$nue_target), , drop = FALSE],
      ggplot2::aes(y = nue_target, color = scenario_label),
      position = ggplot2::position_dodge(width = 0.8),
      shape = 4,
      stroke = 1.2,
      size = 4,
      show.legend = FALSE
    ) +
    ggplot2::labs(
      x = "Year",
      y = "Cropland NUE",
      fill = "Scenario",
      caption = "Crosses show configured minimum NUE targets."
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::theme_minimal(base_size = plot_base_size) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 16 * plot_text_scale),
      axis.title = ggplot2::element_text(size = 18 * plot_text_scale),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 17 * plot_text_scale),
      legend.text = ggplot2::element_text(size = 16 * plot_text_scale)
    )

  ggplot2::ggsave(
    path,
    plot = plot,
    width = 24,
    height = 16,
    dpi = 260,
    bg = "white"
  )
}

make_map_table <- function(audit) {
  required <- c("longitude", "latitude", "GA")
  if (!all(required %in% names(audit))) {
    warning(
      "Map coordinates are absent from grid audits. Re-run scenarios with ",
      "the current runner to enable maps."
    )
    return(NULL)
  }

  audit |>
    dplyr::transmute(
      scenario_name,
      scenario_label,
      year = as.integer(YEMF),
      longitude = as.numeric(longitude),
      latitude = as.numeric(latitude),
      area_ha = as.numeric(GA),
      `Total input` = (
        fert_crop + manure_crop + dep_crop + fix_crop +
          fert_pas + manure_pas + dep_pas + fix_pas
      ),
      `N uptake` = crop_n + pasture_n + residue_n,
      `Gaseous loss` = gas_total,
      Leaching = leach_total
    ) |>
    tidyr::pivot_longer(
      cols = c("Total input", "N uptake", "Gaseous loss", "Leaching"),
      names_to = "metric",
      values_to = "value"
    ) |>
    dplyr::mutate(
      intensity = dplyr::if_else(
        is.finite(area_ha) & area_ha > 0,
        pmax(value, 0) * 1e9 / area_ha,
        NA_real_
      )
    ) |>
    dplyr::filter(
      is.finite(longitude),
      is.finite(latitude),
      is.finite(intensity),
      latitude >= -60,
      latitude <= 90
    )
}

save_map_plots <- function(map_table, output_dir) {
  if (is.null(map_table) || nrow(map_table) == 0L) {
    warning("No valid coordinate/intensity rows were available for maps.")
    return(invisible(character()))
  }

  world <- ggplot2::map_data("world")
  scenario_count <- length(unique(map_table$scenario_label))
  year_count <- length(unique(map_table$year))
  paths <- character()

  for (metric_name in unique(map_table$metric)) {
    plot_data <- map_table |>
      dplyr::filter(metric == metric_name)
    upper <- as.numeric(stats::quantile(
      plot_data$intensity,
      probs = 0.99,
      na.rm = TRUE,
      names = FALSE
    ))
    if (!is.finite(upper) || upper <= 0) {
      upper <- 1
    }

    plot <- ggplot2::ggplot(plot_data) +
      ggplot2::geom_raster(
        ggplot2::aes(x = longitude, y = latitude, fill = intensity)
      ) +
      ggplot2::geom_polygon(
        data = world,
        ggplot2::aes(x = long, y = lat, group = group),
        fill = NA,
        color = "grey25",
        linewidth = 0.15,
        inherit.aes = FALSE
      ) +
      ggplot2::facet_grid(
        ggplot2::vars(scenario_label),
        ggplot2::vars(year)
      ) +
      ggplot2::scale_fill_viridis_c(
        option = "C",
        trans = "sqrt",
        limits = c(0, upper),
        oob = scales::squish,
        na.value = "transparent"
      ) +
      ggplot2::coord_quickmap(
        xlim = c(-180, 180),
        ylim = c(-60, 90),
        expand = FALSE
      ) +
      ggplot2::labs(
        x = NULL,
        y = NULL,
        fill = "kg N ha-1 yr-1",
        title = metric_name,
        subtitle = "Common scale; values above the 99th percentile are capped."
      ) +
      ggplot2::theme_minimal(base_size = 16 * plot_text_scale) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(size = 15 * plot_text_scale),
        strip.text.y = ggplot2::element_text(
          angle = 0,
          size = 15 * plot_text_scale
        ),
        plot.title = ggplot2::element_text(size = 20 * plot_text_scale),
        plot.subtitle = ggplot2::element_text(size = 16 * plot_text_scale),
        legend.position = "bottom",
        legend.title = ggplot2::element_text(size = 16 * plot_text_scale),
        legend.text = ggplot2::element_text(size = 15 * plot_text_scale)
      )

    file_stem <- tolower(gsub("[^A-Za-z0-9]+", "_", metric_name))
    path <- file.path(output_dir, paste0("map_", file_stem, ".png"))
    ggplot2::ggsave(
      path,
      plot = plot,
      width = max(24, 8 * year_count),
      height = max(14, 5 * scenario_count),
      dpi = 240,
      limitsize = FALSE
    )
    paths <- c(paths, path)
  }
  invisible(paths)
}

args <- parse_plot_args(commandArgs(trailingOnly = TRUE))
manifests <- discover_scenarios(args$scenario_root)
all_requested <- identical(tolower(args$scenarios), "all")

if (!all_requested) {
  requested_scenarios <- split_list_argument(args$scenarios)
  if (length(requested_scenarios) == 0L || anyDuplicated(requested_scenarios)) {
    stop("--scenarios must contain one or more unique scenario IDs.")
  }
  missing_scenarios <- setdiff(requested_scenarios, manifests$scenario_name)
  if (length(missing_scenarios) > 0L) {
    stop(
      "Requested scenarios have no completed outputs: ",
      paste(missing_scenarios, collapse = ", ")
    )
  }
  manifests <- manifests[
    match(requested_scenarios, manifests$scenario_name),
    ,
    drop = FALSE
  ]
}

if (!nzchar(args$output_dir)) {
  args$output_dir <- file.path(
    dirname(args$scenario_root),
    "comparisons",
    comparison_identifier(manifests$scenario_name, all_requested)
  )
}

requested_years <- if (identical(args$years, "all")) {
  "all"
} else {
  values <- suppressWarnings(as.integer(split_list_argument(args$years)))
  if (length(values) == 0L || anyNA(values)) {
    stop("--years must be all or a comma-separated list of integer years.")
  }
  unique(values)
}

reference_year <- 2010L
reference_audit <- NULL
if (!identical(requested_years, "all") &&
    reference_year %in% requested_years) {
  baseline_index <- match("baseline", manifests$scenario_name)
  if (is.na(baseline_index)) {
    stop(
      "The common 2010 reference requires the completed *_No Baseline ",
      "scenario output."
    )
  }
  reference_path <- file.path(
    manifests$scenario_dir[[baseline_index]],
    paste0("grid_audit_", reference_year, ".csv")
  )
  if (!file.exists(reference_path)) {
    stop(
      "The common *_No reference grid audit is missing: ", reference_path,
      ". Re-run the nitrogen-flow scenario calculation."
    )
  }
  reference_audit <- readr::read_csv(
    reference_path,
    show_col_types = FALSE,
    progress = FALSE
  )
}

audits <- lapply(seq_len(nrow(manifests)), function(index) {
  read_scenario_audits(
    manifests[index, , drop = FALSE],
    requested_years,
    reference_audit = reference_audit,
    reference_year = reference_year
  )
})
audit <- dplyr::bind_rows(audits)
if (nrow(audit) == 0L) {
  stop("No grid audits matched the selected scenarios and years.")
}
audit <- validate_audit_columns(audit)

scenario_levels <- manifests$scenario_label
audit$scenario_label <- factor(audit$scenario_label, levels = scenario_levels)
dir.create(args$output_dir, recursive = TRUE, showWarnings = FALSE)

components <- make_component_tables(audit)
nue <- make_nue_table(audit)
readr::write_csv(
  components$regional,
  file.path(args$output_dir, "regional_components.csv")
)
readr::write_csv(
  components$global,
  file.path(args$output_dir, "global_components.csv")
)
readr::write_csv(nue, file.path(args$output_dir, "global_nue.csv"))
readr::write_csv(manifests, file.path(args$output_dir, "plot_manifest.csv"))

save_component_plot(
  components$global,
  "Input",
  FALSE,
  file.path(args$output_dir, "global_inputs.png")
)
save_component_plot(
  components$global,
  "Output",
  FALSE,
  file.path(args$output_dir, "global_outputs.png")
)
save_component_plot(
  components$regional,
  "Input",
  TRUE,
  file.path(args$output_dir, "regional_inputs.png")
)
save_component_plot(
  components$regional,
  "Output",
  TRUE,
  file.path(args$output_dir, "regional_outputs.png")
)
save_nue_plot(nue, file.path(args$output_dir, "nue_comparison.png"))
save_gmd_style_scenario_plots(components$regional, args$output_dir)

if (args$maps) {
  save_map_plots(make_map_table(audit), args$output_dir)
}

message(
  "Scenario plots: ",
  normalizePath(args$output_dir, winslash = "/", mustWork = TRUE)
)
