trim_config_value <- function(x) {
  x <- trimws(x)
  if (nchar(x) >= 2L) {
    first <- substr(x, 1L, 1L)
    last <- substr(x, nchar(x), nchar(x))
    if ((first == "\"" && last == "\"") ||
        (first == "'" && last == "'")) {
      x <- substr(x, 2L, nchar(x) - 1L)
    }
  }
  x
}

read_key_value_config <- function(path) {
  config_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  lines <- readLines(config_path, warn = FALSE, encoding = "UTF-8")
  lines <- trimws(lines)
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]

  entries <- lapply(lines, function(line) {
    position <- regexpr("=", line, fixed = TRUE)[1]
    if (position < 2L) {
      stop("Invalid configuration line (expected KEY=VALUE): ", line)
    }
    list(
      key = trimws(substr(line, 1L, position - 1L)),
      value = trim_config_value(substr(line, position + 1L, nchar(line)))
    )
  })
  keys <- vapply(entries, `[[`, character(1), "key")
  if (anyDuplicated(keys)) {
    stop(
      "Duplicate configuration keys: ",
      paste(unique(keys[duplicated(keys)]), collapse = ", ")
    )
  }
  setNames(lapply(entries, `[[`, "value"), keys)
}

parse_on_off <- function(x, key) {
  value <- tolower(trimws(as.character(x)[1]))
  if (!value %in% c("on", "off")) {
    stop(key, " must be 'on' or 'off' (received: ", x, ").")
  }
  identical(value, "on")
}

parse_input_set <- function(x) {
  value <- tolower(trimws(as.character(x)[1]))
  aliases <- c(current = "current", default = "current", latest = "current")
  normalized <- unname(aliases[value])
  if (length(normalized) == 0L || is.na(normalized)) {
    stop("INPUT_SET must be 'current' (received: ", x, ").")
  }
  normalized
}

split_csv_value <- function(x) {
  values <- trimws(strsplit(as.character(x), ",", fixed = TRUE)[[1]])
  values[nzchar(values)]
}

scenario_seed <- function(base_seed, scenario, year) {
  key <- paste(as.character(scenario), as.integer(year), sep = "|")
  codes <- utf8ToInt(enc2utf8(key))
  weighted <- sum(as.double(codes) * seq_along(codes))
  as.integer((as.double(base_seed) + weighted) %% 2147483646 + 1)
}

resolve_config_path <- function(path, config_dir) {
  if (is.null(path) || !nzchar(path)) {
    return("")
  }
  expanded <- path.expand(path)
  absolute <- grepl("^([A-Za-z]:[/\\\\]|/)", expanded)
  candidate <- if (absolute) expanded else file.path(config_dir, expanded)
  normalizePath(candidate, winslash = "/", mustWork = FALSE)
}

read_scenario_config <- function(path, validate_calculation_files = TRUE) {
  config_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  config_dir <- dirname(config_path)
  raw <- read_key_value_config(config_path)
  get_value <- function(key, default = NULL) {
    value <- raw[[key]]
    if (is.null(value) || !nzchar(value)) default else value
  }

  axes <- scenario_axis_registry()
  axis_switches <- setNames(
    lapply(seq_len(nrow(axes)), function(index) {
      parse_on_off(
        get_value(axes$config_key[[index]], "off"),
        axes$config_key[[index]]
      )
    }),
    axes$id
  )
  unsupported <- axes$id[!axes$implemented & vapply(
    axes$id,
    function(id) isTRUE(axis_switches[[id]]),
    logical(1)
  )]
  if (length(unsupported) > 0L) {
    stop(
      "The following scenario axes are reserved but not implemented: ",
      paste(unsupported, collapse = ", ")
    )
  }

  years <- suppressWarnings(as.integer(split_csv_value(
    get_value("YEARS", "2030,2050,2100")
  )))
  if (length(years) == 0L || anyNA(years)) {
    stop("YEARS must be a comma-separated list of integer years.")
  }
  reference_year <- suppressWarnings(as.integer(
    get_value("SUMMARY_REFERENCE_YEAR", "2010")
  ))
  if (length(reference_year) != 1L || is.na(reference_year)) {
    stop("SUMMARY_REFERENCE_YEAR must be one integer year.")
  }
  random_seed_base <- suppressWarnings(as.integer(
    get_value("RANDOM_SEED_BASE", "20260918")
  ))
  if (length(random_seed_base) != 1L || is.na(random_seed_base) ||
      random_seed_base <= 0L) {
    stop("RANDOM_SEED_BASE must be one positive integer.")
  }

  plot_scenarios <- split_csv_value(get_value("PLOT_SCENARIOS", "all"))
  if (length(plot_scenarios) == 0L ||
      ("all" %in% tolower(plot_scenarios) && length(plot_scenarios) > 1L)) {
    stop("PLOT_SCENARIOS must be 'all' or a comma-separated scenario-ID list.")
  }
  if (identical(tolower(plot_scenarios), "all")) {
    plot_scenarios <- "all"
  }
  plot_year_text <- split_csv_value(get_value("PLOT_YEARS", "2030,2050,2100"))
  if (identical(tolower(plot_year_text), "all")) {
    plot_years <- "all"
  } else {
    plot_years <- suppressWarnings(as.integer(plot_year_text))
    if (length(plot_years) == 0L || anyNA(plot_years)) {
      stop("PLOT_YEARS must be 'all' or a comma-separated integer-year list.")
    }
    plot_years <- unique(plot_years)
  }

  default_gams_dir <- Sys.getenv("GAMS_DIR", unset = "")
  if (!nzchar(default_gams_dir) &&
      .Platform$OS.type == "windows" &&
      dir.exists("C:/GAMS/53")) {
    default_gams_dir <- "C:/GAMS/53"
  }

  cfg <- list(
    config_path = config_path,
    config_dir = config_dir,
    run_all_combinations = parse_on_off(
      get_value("RUN_ALL_COMBINATIONS", "off"),
      "RUN_ALL_COMBINATIONS"
    ),
    scenario_gmd_inputs = parse_on_off(
      get_value("SCENARIO_GMD_INPUTS", "off"),
      "SCENARIO_GMD_INPUTS"
    ),
    axis_switches = axis_switches,
    run_scenarios = split_csv_value(get_value("RUN_SCENARIOS", "all")),
    years = unique(years),
    summary_reference_year = reference_year,
    summary_reference_scenario = get_value(
      "SUMMARY_REFERENCE_SCENARIO",
      "SSP2_BaU_NoCC_No"
    ),
    random_seed_base = random_seed_base,
    input_set = parse_input_set(get_value("INPUT_SET", "current")),
    # Optional state-update hook used by the isolated Nflow--GEOS feedback
    # workflow.  The empty default preserves the scenario registry's normal
    # deposition mapping for every existing calculation.
    deposition_prefix_override = get_value(
      "DEPOSITION_PREFIX_OVERRIDE",
      ""
    ),
    include_residue_in_nue = parse_on_off(
      get_value("INCLUDE_RESIDUE_IN_NUE", "on"),
      "INCLUDE_RESIDUE_IN_NUE"
    ),
    nue_adjustment_mode = "fertilizer_input",
    manure_recycling_mitigation = FALSE,
    baseline_manure_recycling_rate = as.numeric(
      get_value("BASELINE_MANURE_RECYCLING_RATE", "0.4")
    ),
    export_visitorn = parse_on_off(
      get_value("EXPORT_VISITORN", "on"),
      "EXPORT_VISITORN"
    ),
    fail_fast = parse_on_off(
      get_value("FAIL_FAST", "on"),
      "FAIL_FAST"
    ),
    gams_dir = get_value("GAMS_DIR", default_gams_dir),
    nue_target_file = resolve_config_path(
      get_value("NUE_TARGET_FILE", "data/nue_targets.csv"),
      config_dir
    ),
    output_dir = resolve_config_path(
      get_value("OUTPUT_DIR", "../output/scenarios"),
      config_dir
    ),
    plot_scenarios = plot_scenarios,
    plot_years = plot_years,
    plot_maps = parse_on_off(
      get_value("PLOT_MAPS", "on"),
      "PLOT_MAPS"
    ),
    plot_output_dir = resolve_config_path(
      get_value("PLOT_OUTPUT_DIR", ""),
      config_dir
    ),
    visitorn_output_dir = resolve_config_path(
      get_value(
        "VISITORN_OUTPUT_DIR",
        "../../VISIToRN/Data_pd/scenarios"
      ),
      config_dir
    ),
    raw_model_script = resolve_config_path(
      get_value("RAW_MODEL_SCRIPT", "../R/RawDataProcess.R"),
      config_dir
    )
  )

  if (!is.finite(cfg$baseline_manure_recycling_rate) ||
      cfg$baseline_manure_recycling_rate <= 0 ||
      cfg$baseline_manure_recycling_rate > 1) {
    stop("BASELINE_MANURE_RECYCLING_RATE must be in (0,1].")
  }
  if (nzchar(cfg$deposition_prefix_override) &&
      !grepl("^[A-Za-z0-9_.-]+$", cfg$deposition_prefix_override)) {
    stop(
      "DEPOSITION_PREFIX_OVERRIDE may contain only letters, numbers, ",
      "'.', '_' and '-'."
    )
  }
  if (isTRUE(validate_calculation_files)) {
    if (!file.exists(cfg$nue_target_file)) {
      stop("NUE target file does not exist: ", cfg$nue_target_file)
    }
    if (!file.exists(cfg$raw_model_script)) {
      stop("Raw model script does not exist: ", cfg$raw_model_script)
    }
  }
  cfg
}

read_nue_schedule <- function(path) {
  schedule <- utils::read.csv(path, stringsAsFactors = FALSE)
  required <- c("year", "nue_target")
  missing <- setdiff(required, names(schedule))
  if (length(missing) > 0L) {
    stop("NUE target file is missing columns: ", paste(missing, collapse = ", "))
  }
  schedule$year <- as.integer(schedule$year)
  schedule$nue_target <- as.numeric(schedule$nue_target)
  if (!"manure_recycling_target" %in% names(schedule)) {
    schedule$manure_recycling_target <- NA_real_
  }
  schedule$manure_recycling_target <- as.numeric(
    schedule$manure_recycling_target
  )
  if (anyNA(schedule$year) || anyNA(schedule$nue_target) ||
      any(schedule$nue_target <= 0 | schedule$nue_target > 1)) {
    stop("NUE targets must be finite values in (0,1].")
  }
  schedule <- schedule[order(schedule$year), , drop = FALSE]
  if (anyDuplicated(schedule$year)) {
    stop("NUE target years must be unique.")
  }
  schedule
}

interpolate_schedule <- function(schedule, year) {
  interpolate <- function(values) {
    if (all(is.na(values))) return(NA_real_)
    stats::approx(
      x = schedule$year,
      y = values,
      xout = as.numeric(year),
      method = "linear",
      rule = 2,
      ties = "ordered"
    )$y
  }
  list(
    nue_target = interpolate(schedule$nue_target),
    manure_recycling_target = interpolate(schedule$manure_recycling_target)
  )
}
