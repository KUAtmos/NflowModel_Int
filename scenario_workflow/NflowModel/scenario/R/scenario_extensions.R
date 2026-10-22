as_grid_key <- function(x) {
  as.character(x)
}

extract_grid_value <- function(env,
                               object_name,
                               value_column,
                               output_column,
                               year,
                               scenario = NULL,
                               required = TRUE) {
  if (!exists(object_name, envir = env, inherits = FALSE)) {
    if (required) {
      stop("Required model object not found: ", object_name)
    }
    return(data.frame(
      G = character(),
      YEMF = integer(),
      value = numeric(),
      stringsAsFactors = FALSE
    )[FALSE, ])
  }

  x <- get(object_name, envir = env, inherits = FALSE)
  required_columns <- c("G", "YEMF", value_column)
  missing <- setdiff(required_columns, names(x))
  if (length(missing) > 0L) {
    stop(
      object_name, " is missing columns: ",
      paste(missing, collapse = ", ")
    )
  }

  x <- x[as.character(x$YEMF) == as.character(year), , drop = FALSE]
  if (!is.null(scenario) && "SCENARIO" %in% names(x)) {
    x <- x[as.character(x$SCENARIO) == as.character(scenario), , drop = FALSE]
  }

  if (nrow(x) == 0L) {
    if (required) {
      stop(object_name, " has no data for year ", year, ".")
    }
    out <- data.frame(G = character(), YEMF = integer(), value = numeric())
    names(out)[3] <- output_column
    return(out)
  }

  out <- data.frame(
    G = as_grid_key(x$G),
    YEMF = as.integer(as.character(x$YEMF)),
    value = as.numeric(x[[value_column]]),
    stringsAsFactors = FALSE
  )
  out$value[!is.finite(out$value)] <- 0
  out <- stats::aggregate(
    out$value,
    by = list(G = out$G, YEMF = out$YEMF),
    FUN = sum,
    na.rm = TRUE
  )
  names(out)[3] <- output_column
  out
}

merge_grid_parts <- function(parts) {
  Reduce(
    function(left, right) merge(left, right, by = c("G", "YEMF"), all = TRUE),
    parts
  )
}

build_region_map <- function(env, year) {
  collapse_mapping <- function(x) {
    required <- c("G", "RegionCode")
    missing <- setdiff(required, names(x))
    if (length(missing) > 0L) {
      stop("Region mapping is missing columns: ", paste(missing, collapse = ", "))
    }

    weight <- if ("RegionWeight" %in% names(x)) {
      suppressWarnings(as.numeric(x$RegionWeight))
    } else {
      rep(1, nrow(x))
    }
    weight[!is.finite(weight) | weight < 0] <- 0
    x <- data.frame(
      G = as_grid_key(x$G),
      RegionCode = as.character(x$RegionCode),
      RegionWeight = weight,
      stringsAsFactors = FALSE
    )
    x <- x[
      !is.na(x$G) & nzchar(x$G) &
        !is.na(x$RegionCode) & nzchar(x$RegionCode),
      ,
      drop = FALSE
    ]
    if (nrow(x) == 0L) {
      stop("Region mapping contains no valid grid-region rows.")
    }

    weighted <- stats::aggregate(
      x$RegionWeight,
      by = list(G = x$G, RegionCode = x$RegionCode),
      FUN = sum,
      na.rm = TRUE
    )
    names(weighted)[3] <- "RegionWeight"
    totals <- stats::aggregate(
      weighted$RegionWeight,
      by = list(G = weighted$G),
      FUN = sum,
      na.rm = TRUE
    )
    names(totals)[2] <- "TotalRegionWeight"
    counts <- stats::aggregate(
      weighted$RegionCode,
      by = list(G = weighted$G),
      FUN = length
    )
    names(counts)[2] <- "RegionCandidateCount"
    weighted <- merge(weighted, totals, by = "G", all.x = TRUE)
    weighted <- merge(weighted, counts, by = "G", all.x = TRUE)
    weighted$RegionAllocationShare <- ifelse(
      weighted$TotalRegionWeight > 0,
      weighted$RegionWeight / weighted$TotalRegionWeight,
      1 / weighted$RegionCandidateCount
    )

    # Assign each border grid once to the region containing the largest
    # AIM-PLUM land-use area. RegionCode provides a deterministic tie-break.
    weighted <- weighted[order(
      weighted$G,
      -weighted$RegionAllocationShare,
      weighted$RegionCode
    ), , drop = FALSE]
    selected <- weighted[!duplicated(weighted$G), c(
      "G", "RegionCode", "RegionAllocationShare", "RegionCandidateCount"
    )]
    rownames(selected) <- NULL
    selected
  }

  if (exists("grid_region_map", envir = env, inherits = FALSE)) {
    x <- get("grid_region_map", envir = env, inherits = FALSE)
    if (!all(c("G", "RegionCode") %in% names(x))) {
      stop("grid_region_map must contain G and RegionCode.")
    }
    weight_column <- c("RegionWeight", "LUAREA", "weight", "Weight")
    weight_column <- weight_column[weight_column %in% names(x)][1]
    if (!is.na(weight_column)) {
      x$RegionWeight <- x[[weight_column]]
    }
    return(collapse_mapping(x))
  }

  if (!exists("VY_load2", envir = env, inherits = FALSE)) {
    stop("VY_load2 is required to map grids to AIM-Hub regions.")
  }

  x <- get("VY_load2", envir = env, inherits = FALSE)
  required <- c("G", "i1")
  if (!all(required %in% names(x))) {
    stop("VY_load2 must contain G and i1.")
  }
  if ("i2" %in% names(x)) {
    year_rows <- as.character(x$i2) == as.character(year)
    if (any(year_rows)) {
      x <- x[year_rows, , drop = FALSE]
    }
  }

  mapping <- data.frame(
    G = as_grid_key(x$G),
    RegionCode = as.character(x$i1),
    RegionWeight = if ("LUAREA" %in% names(x)) x$LUAREA else 1,
    stringsAsFactors = FALSE
  )
  collapse_mapping(mapping)
}

build_grid_coordinate_map <- function(env) {
  if (!exists("MAP_GIJ", envir = env, inherits = FALSE)) {
    return(NULL)
  }

  x <- get("MAP_GIJ", envir = env, inherits = FALSE)
  grid_column <- c("G")[c("G") %in% names(x)][1]
  i_column <- c("lat", "I")[c("lat", "I") %in% names(x)][1]
  j_column <- c("lon", "J")[c("lon", "J") %in% names(x)][1]
  if (anyNA(c(grid_column, i_column, j_column))) {
    warning("MAP_GIJ does not contain G and lat/lon (or I/J); maps are disabled.")
    return(NULL)
  }

  out <- data.frame(
    G = as_grid_key(x[[grid_column]]),
    grid_i = suppressWarnings(as.numeric(as.character(x[[i_column]]))),
    grid_j = suppressWarnings(as.numeric(as.character(x[[j_column]]))),
    stringsAsFactors = FALSE
  )
  out <- out[stats::complete.cases(out), , drop = FALSE]
  out <- unique(out)
  if (anyDuplicated(out$G)) {
    stop("MAP_GIJ maps some grid identifiers to multiple coordinates.")
  }

  # MAP_GIJ stores 0.5-degree grid indices rather than geographic degrees.
  out$longitude <- 0.5 * out$grid_j - 180.25
  out$latitude <- -0.5 * out$grid_i + 90.25
  out
}

extract_gas_context <- function(env, year, scenario) {
  # GA is the same cropland + pasture area in all three raw-model tables.
  # Crop-only extraction loses the context for pasture-only grids. Prefer
  # the combined table and retain the union of the land-specific tables.
  candidates <- c("SOC_Fert_P", "SOC_Fertcrop_P", "SOC_Fertpas_P")
  candidates <- candidates[
    vapply(candidates, exists, logical(1), envir = env, inherits = FALSE)
  ]
  if (length(candidates) == 0L) {
    stop("A SOC_Fert*_P table is required to recalculate gas losses.")
  }

  required <- c("G", "YEMF", "SOC", "GA", "PREC")
  parts <- lapply(candidates, function(object_name) {
    x <- get(object_name, envir = env, inherits = FALSE)
    missing <- setdiff(required, names(x))
    if (length(missing) > 0L) {
      stop(object_name, " is missing columns: ", paste(missing, collapse = ", "))
    }
    keep <- as.character(x$YEMF) %in% as.character(year)
    if ("SCENARIO" %in% names(x)) {
      keep <- keep & as.character(x$SCENARIO) %in% as.character(scenario)
    }
    x <- x[keep, , drop = FALSE]
    data.frame(
      G = as_grid_key(x$G),
      YEMF = as.integer(as.character(x$YEMF)),
      SOC = as.numeric(as.character(x$SOC)),
      GA = as.numeric(as.character(x$GA)),
      PREC = as.numeric(as.character(x$PREC)),
      stringsAsFactors = FALSE
    )
  })
  x <- do.call(rbind, parts)
  if (nrow(x) == 0L) return(x)
  if (anyNA(x$G) || any(!nzchar(x$G)) || anyNA(x$YEMF)) {
    stop("Gas context contains an invalid grid or year key.")
  }

  finite <- is.finite(x$GA) & is.finite(x$SOC) & is.finite(x$PREC)
  positive <- finite & x$GA > 0
  # A manure-only row can have placeholder GA=0 in one of the raw tables.
  # Use an available positive-area context instead. Explicit zero-area rows
  # with no positive counterpart retain the legacy behavior, not an imputed area.
  priority <- ifelse(positive, 0L, ifelse(finite & x$GA == 0, 1L, 2L))
  x <- x[order(priority, seq_len(nrow(x))), , drop = FALSE]
  positive <- is.finite(x$GA) & x$GA > 0 &
    is.finite(x$SOC) & is.finite(x$PREC)
  key <- paste(x$G, x$YEMF, sep = "\r")
  first <- match(key, key)
  for (column in c("GA", "SOC", "PREC")) {
    reference <- x[[column]][first]
    conflict <- positive &
      abs(x[[column]] - reference) > 1e-10 * pmax(1, abs(reference))
    if (any(conflict, na.rm = TRUE)) {
      stop(
        "Conflicting gas context for ", column, " at grids: ",
        paste(utils::head(x$G[which(conflict)], 10), collapse = ", ")
      )
    }
  }
  x <- x[!duplicated(key), , drop = FALSE]
  rownames(x) <- NULL
  x
}

validate_gas_context <- function(grid) {
  activity_columns <- c(
    "fert_crop", "manure_crop", "fert_pas", "manure_pas",
    "base_gas_crop", "base_gas_pas"
  )
  active <- Reduce(`|`, lapply(activity_columns, function(column) {
    is.finite(grid[[column]]) & grid[[column]] != 0
  }))
  valid <- is.finite(grid$GA) & grid$GA >= 0 &
    is.finite(grid$SOC) & is.finite(grid$PREC)
  invalid <- active & !valid
  if (any(invalid)) {
    stop(
      "Missing or invalid gas context on active agricultural grids: ",
      paste(utils::head(grid$G[invalid], 10), collapse = ", "),
      ". Do not replace missing area, SOC, or precipitation with zero."
    )
  }
  zero_area <- active & valid & grid$GA == 0
  nonzero_gas <- Reduce(`|`, lapply(c("base_gas_crop", "base_gas_pas"), function(column) {
    is.finite(grid[[column]]) & grid[[column]] != 0
  }))
  if (any(zero_area & nonzero_gas)) {
    stop("Zero-area gas context cannot reproduce nonzero base gas emissions.")
  }
  if (any(zero_area)) {
    warning(
      sum(zero_area), " active grids already have explicit GA=0 and zero base gas. ",
      "Their legacy area treatment is preserved; no area is inferred.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

build_scenario_grid <- function(env, year, scenario) {
  specifications <- list(
    c("INfercropdis", "Fert_Use_Grid", "fert_crop"),
    c("INmancropdis", "ManureCropGrid", "manure_crop"),
    c("INdepcropdis", "N_deposition_Crop", "dep_crop"),
    c("INfixcropdis", "N_fix_crop", "fix_crop"),
    c("INferpasdis", "Fert_Use_Grid", "fert_pas"),
    c("INmanpasdis", "ManurePasGrid", "manure_pas"),
    c("INdeppasdis", "N_deposition_Pas", "dep_pas"),
    c("INfixpasdis", "N_fix_pas", "fix_pas"),
    c("OUTcropcropdis", "CropN", "crop_n"),
    c("OUTcroppasdis", "OUTcroppas", "pasture_n"),
    c("OUTresdis", "OUTresgrid", "residue_n"),
    c("OUTgascropdis", "OUTgascropgrid", "base_gas_crop"),
    c("OUTgaspasdis", "OUTgaspasgrid", "base_gas_pas"),
    c("OUTleacropdis", "OUTleacrop", "base_leach_crop"),
    c("OUTleapasdis", "OUTleagrid", "base_leach_pas")
  )

  parts <- lapply(specifications, function(spec) {
    extract_grid_value(
      env = env,
      object_name = spec[[1]],
      value_column = spec[[2]],
      output_column = spec[[3]],
      year = year,
      scenario = scenario,
      required = TRUE
    )
  })

  grid <- merge_grid_parts(parts)
  if (exists("residue_reuse_grid", envir = env, inherits = FALSE)) {
    residue_reuse <- extract_grid_value(
      env = env,
      object_name = "residue_reuse_grid",
      value_column = "INfer_EX",
      output_column = "residue_recycled_model",
      year = year,
      scenario = scenario,
      required = FALSE
    )
    grid <- merge(grid, residue_reuse, by = c("G", "YEMF"), all.x = TRUE)
  }
  if (!"residue_recycled_model" %in% names(grid)) {
    grid$residue_recycled_model <- 0
  }
  grid <- merge(grid, build_region_map(env, year), by = "G", all.x = TRUE)
  if (anyNA(grid$RegionCode)) {
    stop(
      "No AIM-Hub region mapping for grids: ",
      paste(utils::head(grid$G[is.na(grid$RegionCode)], 10), collapse = ", ")
    )
  }

  coordinates <- build_grid_coordinate_map(env)
  if (!is.null(coordinates)) {
    grid <- merge(grid, coordinates, by = "G", all.x = TRUE)
  } else {
    grid$grid_i <- NA_real_
    grid$grid_j <- NA_real_
    grid$longitude <- NA_real_
    grid$latitude <- NA_real_
  }

  context <- extract_gas_context(env, year, scenario)
  grid <- merge(grid, context, by = c("G", "YEMF"), all.x = TRUE)
  # Check before the general flow-table NA fill so missing context cannot
  # silently turn a positive pasture emission into zero during NUE evaluation.
  validate_gas_context(grid)
  coordinate_columns <- c("grid_i", "grid_j", "longitude", "latitude")
  numeric_columns <- setdiff(
    names(grid),
    c("G", "RegionCode", coordinate_columns)
  )
  for (column in numeric_columns) {
    grid[[column]][!is.finite(grid[[column]]) | is.na(grid[[column]])] <- 0
  }
  validate_base_gas_reproduction(grid)
  grid
}

calculate_signed_leaching <- function(grid) {
  # Match the legacy (non-NUE) N balance. Preserve negative residuals here;
  # the common VISIToRN input builder applies its own combined-input boundary.
  grid$leach_crop <- grid$input_crop - (grid$crop_n + grid$residue_n) - grid$gas_crop
  grid$leach_pas <- grid$input_pas - grid$pasture_n - grid$gas_pas
  grid$leach_total <- grid$leach_crop + grid$leach_pas
  grid
}

calculate_gas_losses <- function(fert_crop,
                                 manure_crop,
                                 fert_pas,
                                 manure_pas,
                                 soc,
                                 ga,
                                 prec) {
  safe_ga <- ifelse(is.finite(ga) & ga > 0, ga, 0)
  crop_rate <- ifelse(
    safe_ga > 0,
    pmax(fert_crop + manure_crop, 0) * 1e9 / safe_ga,
    0
  )
  pasture_rate <- ifelse(
    safe_ga > 0,
    pmax(fert_pas + manure_pas, 0) * 1e9 / safe_ga,
    0
  )
  total_rate <- crop_rate + pasture_rate
  crop_share <- ifelse(total_rate > 0, crop_rate / total_rate, 0)
  pasture_share <- ifelse(total_rate > 0, pasture_rate / total_rate, 0)
  background <- pmax(0.025 + 0.000855 * prec + 0.117 * soc, 0)

  nox_crop <- (
    background * crop_share + 0.01725 * crop_rate
  ) * safe_ga / 1e9
  nox_pas <- (
    background * pasture_share + 0.01725 * pasture_rate
  ) * safe_ga / 1e9
  nh3_crop <- 0.113 * crop_rate * safe_ga / 1e9
  nh3_pas <- 0.113 * pasture_rate * safe_ga / 1e9
  n2_crop <- nox_crop * 14 / 8
  n2_pas <- nox_pas * 14 / 8

  data.frame(
    gas_nox_crop = nox_crop,
    gas_nox_pas = nox_pas,
    gas_nh3_crop = nh3_crop,
    gas_nh3_pas = nh3_pas,
    gas_n2_crop = n2_crop,
    gas_n2_pas = n2_pas,
    gas_crop = nox_crop + nh3_crop + n2_crop,
    gas_pas = nox_pas + nh3_pas + n2_pas
  )
}

validate_base_gas_reproduction <- function(grid,
                                           absolute_tolerance = 1e-10,
                                           relative_tolerance = 1e-8) {
  # An unchanged-fertilizer calculation must reproduce both original land
  # components before a policy is allowed to change fertilizer or recycling.
  gas <- calculate_gas_losses(
    grid$fert_crop, grid$manure_crop, grid$fert_pas, grid$manure_pas,
    grid$SOC, grid$GA, grid$PREC
  )
  max_difference <- 0
  for (component in c("crop", "pas")) {
    actual <- gas[[paste0("gas_", component)]]
    expected <- grid[[paste0("base_gas_", component)]]
    difference <- abs(actual - expected)
    invalid <- !is.finite(difference) |
      difference > absolute_tolerance + relative_tolerance * abs(expected)
    if (any(invalid)) {
      stop(
        "Unchanged-fertilizer gas reproduction failed for ", component,
        " at grids: ",
        paste(utils::head(grid$G[invalid], 10), collapse = ", "),
        ". Check raw gas context and rates before applying NUE."
      )
    }
    max_difference <- max(c(max_difference, difference))
  }
  message(sprintf(
    "Gas context check passed: %d grids; max base-gas difference %.3g Tg N/yr.",
    nrow(grid), max_difference
  ))
  invisible(TRUE)
}

apply_scenario_to_grid <- function(grid,
                                   nue_mitigation,
                                   nue_target = NA_real_,
                                   manure_recycling_target = NA_real_,
                                   baseline_manure_recycling_rate = 0.4,
                                   include_residue_in_nue = TRUE,
                                   nue_adjustment_mode = "fertilizer_input",
                                   manure_recycling_mitigation = FALSE) {
  required <- c(
    "G", "YEMF", "RegionCode",
    "fert_crop", "manure_crop", "dep_crop", "fix_crop",
    "fert_pas", "manure_pas", "dep_pas", "fix_pas",
    "crop_n", "pasture_n", "residue_n",
    "SOC", "GA", "PREC",
    "base_gas_crop", "base_gas_pas",
    "base_leach_crop", "base_leach_pas"
  )
  missing <- setdiff(required, names(grid))
  if (length(missing) > 0L) {
    stop("Scenario grid is missing columns: ", paste(missing, collapse = ", "))
  }

  out <- grid
  base_numeric <- setdiff(required, c("G", "RegionCode", "YEMF"))
  for (column in base_numeric) {
    out[[column]] <- as.numeric(out[[column]])
    out[[paste0(column, "_base")]] <- out[[column]]
  }

  # DEMFWR is represented by its own AIM-Hub, AIM-PLUM, livestock, body-weight
  # and deposition inputs. No regional multiplier is applied here. In this
  # version of the raw model, INfer contains both synthetic fertilizer and
  # recycled crop residue. Keep residue recycling fixed and adjust only the
  # synthetic component when the NUE policy is enabled.
  out$residue_recycled_base <- pmax(out$residue_recycled_model, 0)
  out$residue_recycled_base <- pmin(
    out$residue_recycled_base,
    pmax(out$fert_crop_base, 0)
  )
  out$synthetic_fert_crop_base <- pmax(
    out$fert_crop_base - out$residue_recycled_base,
    0
  )
  out$residue_recycling_rate <- ifelse(
    out$residue_n > 0,
    out$residue_recycled_base / out$residue_n,
    0
  )
  out$residue_recycled <- out$residue_recycled_base
  out$synthetic_fert_crop <- out$synthetic_fert_crop_base
  out$fert_crop <- out$synthetic_fert_crop + out$residue_recycled

  out$crop_n_before_policy <- out$crop_n
  out$residue_n_before_policy <- out$residue_n
  out$residue_recycled_before_policy <- out$residue_recycled
  out$synthetic_fert_crop_before_policy <- out$synthetic_fert_crop
  out$manure_crop_before_policy <- out$manure_crop

  output_for_nue_before_policy <- out$crop_n
  if (isTRUE(include_residue_in_nue)) {
    output_for_nue_before_policy <- (
      output_for_nue_before_policy + out$residue_n
    )
  }
  # The NUE definition can optionally use harvest N only, but crop-residue N
  # remains a physical output in the soil N balance in either mode.
  physical_crop_output_before_policy <- out$crop_n + out$residue_n
  out$input_crop_before_policy <- (
    out$fert_crop + out$manure_crop +
      out$dep_crop + out$fix_crop
  )
  out$nue_output_before_policy <- output_for_nue_before_policy
  out$nue_before_policy <- ifelse(
    out$input_crop_before_policy > 0,
    output_for_nue_before_policy / out$input_crop_before_policy,
    NA_real_
  )
  out$nue_target <- if (isTRUE(nue_mitigation)) nue_target else NA_real_
  out$manure_recycling_target <- if (
    isTRUE(nue_mitigation) && isTRUE(manure_recycling_mitigation)
  ) {
    manure_recycling_target
  } else {
    NA_real_
  }
  out$nue_adjustment_mode <- if (isTRUE(nue_mitigation)) {
    nue_adjustment_mode
  } else {
    "none"
  }
  out$nue_infeasible <- FALSE
  out$nue_required_total_input <- NA_real_
  out$nue_required_infer_crop <- NA_real_
  out$nue_required_synthetic_fert_crop <- NA_real_
  out$nue_fertilizer_reduction <- 0
  out$nue_fertilizer_floor_limited <- FALSE
  out$nue_mass_balance_limited <- FALSE
  out$nue_target_input_excess <- 0

  if (isTRUE(nue_mitigation)) {
    if (!identical(nue_adjustment_mode, "fertilizer_input")) {
      stop("Unsupported NUE adjustment mode: ", nue_adjustment_mode)
    }
    if (!is.finite(nue_target) || nue_target <= 0 || nue_target > 1) {
      stop("nue_target must be in (0,1] when NUE mitigation is enabled.")
    }
    if (isTRUE(manure_recycling_mitigation)) {
      if (!is.finite(manure_recycling_target) ||
          manure_recycling_target < 0 ||
          manure_recycling_target > 1) {
        stop("manure_recycling_target must be in [0,1].")
      }

      manure_available <- if (baseline_manure_recycling_rate > 0) {
        pmax(out$manure_crop, 0) / baseline_manure_recycling_rate
      } else {
        rep(0, nrow(out))
      }
      out$manure_crop <- manure_available * manure_recycling_target
    }

    # Keep crop and crop-residue N recovery fixed.  For a target NUE T,
    # solve the soil-system definition directly for cropland fertilizer input:
    #
    #   INfer_target = (CropN + ResidueN) / T
    #                  - (INman + INdep + INfix)
    #
    # INfer includes fixed crop-residue return plus synthetic fertilizer.
    # A minimum target is never used to increase fertilizer when a grid already
    # meets T.
    fixed_non_infer_input <- pmax(
      out$manure_crop + out$dep_crop + out$fix_crop,
      0
    )
    out$nue_required_total_input <- output_for_nue_before_policy / nue_target
    out$nue_required_infer_crop <- pmax(
      out$nue_required_total_input - fixed_non_infer_input,
      0
    )
    out$nue_required_synthetic_fert_crop <- pmax(
      out$nue_required_infer_crop - out$residue_recycled,
      0
    )
    out$nue_fertilizer_floor_limited <- (
      out$nue_required_infer_crop + 1e-12 < out$residue_recycled
    )

    current_input <- (
      out$synthetic_fert_crop + out$residue_recycled + fixed_non_infer_input
    )
    nue_at_current_input <- ifelse(
      current_input > 0,
      output_for_nue_before_policy / current_input,
      NA_real_
    )
    target_already_met <- is.finite(nue_at_current_input) &
      nue_at_current_input + 1e-10 >= nue_target
    target_synthetic_fert <- ifelse(
      target_already_met,
      out$synthetic_fert_crop,
      pmin(
        out$synthetic_fert_crop,
        out$nue_required_synthetic_fert_crop
      )
    )
    gas_at_synthetic_fert <- function(synthetic_fert) {
      trial_gas <- calculate_gas_losses(
        fert_crop = synthetic_fert + out$residue_recycled,
        manure_crop = out$manure_crop,
        fert_pas = out$fert_pas,
        manure_pas = out$manure_pas,
        soc = out$SOC,
        ga = out$GA,
        prec = out$PREC
      )
      trial_gas$gas_crop
    }
    balance_at_synthetic_fert <- function(synthetic_fert) {
      trial_input <- (
        synthetic_fert + out$residue_recycled + fixed_non_infer_input
      )
      trial_input - physical_crop_output_before_policy -
        gas_at_synthetic_fert(synthetic_fert)
    }

    balance_at_current <- balance_at_synthetic_fert(out$synthetic_fert_crop)
    balance_at_target <- balance_at_synthetic_fert(target_synthetic_fert)
    balance_tolerance <- 64 * .Machine$double.eps * pmax(
      abs(current_input),
      abs(output_for_nue_before_policy),
      abs(balance_at_current),
      1e-30
    )
    needs_mass_balance_floor <- !target_already_met &
      target_synthetic_fert < out$synthetic_fert_crop &
      balance_at_target < -balance_tolerance &
      balance_at_current >= -balance_tolerance
    no_feasible_reduction <- !target_already_met &
      balance_at_current < -balance_tolerance

    # If the exact NUE solution would make leaching negative, find the smallest
    # synthetic-fertilizer input that preserves the grid N mass balance.  Such
    # grids are reported as target-infeasible instead of changing crop uptake.
    lower <- target_synthetic_fert
    upper <- out$synthetic_fert_crop
    if (any(needs_mass_balance_floor)) {
      for (iteration in seq_len(80L)) {
        middle <- (lower + upper) / 2
        middle_balance <- balance_at_synthetic_fert(middle)
        feasible_middle <- middle_balance >= -balance_tolerance
        update_upper <- needs_mass_balance_floor & feasible_middle
        update_lower <- needs_mass_balance_floor & !feasible_middle
        upper[update_upper] <- middle[update_upper]
        lower[update_lower] <- middle[update_lower]
      }
    }
    final_synthetic_fert <- target_synthetic_fert
    final_synthetic_fert[needs_mass_balance_floor] <- upper[
      needs_mass_balance_floor
    ]
    final_synthetic_fert[no_feasible_reduction] <- out$synthetic_fert_crop[
      no_feasible_reduction
    ]
    final_synthetic_fert[!is.finite(final_synthetic_fert)] <- 0
    out$nue_mass_balance_limited <- (
      needs_mass_balance_floor | no_feasible_reduction
    )
    out$nue_fertilizer_reduction <- pmax(
      out$synthetic_fert_crop - final_synthetic_fert,
      0
    )
    out$synthetic_fert_crop <- final_synthetic_fert
    out$fert_crop <- out$synthetic_fert_crop + out$residue_recycled
  }

  out$input_crop <- (
    out$fert_crop + out$manure_crop + out$dep_crop + out$fix_crop
  )
  out$input_pas <- (
    out$fert_pas + out$manure_pas + out$dep_pas + out$fix_pas
  )
  out$input_total <- out$input_crop + out$input_pas
  output_for_nue <- out$crop_n
  if (isTRUE(include_residue_in_nue)) {
    output_for_nue <- output_for_nue + out$residue_n
  }
  physical_crop_output <- out$crop_n + out$residue_n
  out$nue_output <- output_for_nue
  out$nue_fixed_input <- out$dep_crop + out$fix_crop
  out$nue_achieved <- ifelse(
    out$input_crop > 0,
    output_for_nue / out$input_crop,
    NA_real_
  )
  applicable_nue <- out$input_crop > 0 | output_for_nue > 0
  raw_target_met <- is.na(out$nue_achieved) |
    out$nue_achieved + 1e-10 >= nue_target
  if (isTRUE(nue_mitigation)) {
    out$nue_infeasible <- applicable_nue & !raw_target_met
    out$nue_target_input_excess <- pmax(
      out$input_crop - output_for_nue / nue_target,
      0
    )
  }
  out$nue_target_met <- if (!isTRUE(nue_mitigation)) {
    TRUE
  } else {
    !out$nue_infeasible & raw_target_met
  }
  manure_available_final <- pmax(
    out$manure_crop_base,
    0
  ) / baseline_manure_recycling_rate
  out$manure_recycling_achieved <- ifelse(
    manure_available_final > 0,
    out$manure_crop / manure_available_final,
    NA_real_
  )
  out$manure_recycling_target_met <- if (
    !isTRUE(nue_mitigation) || !isTRUE(manure_recycling_mitigation)
  ) {
    TRUE
  } else {
    is.na(out$manure_recycling_achieved) |
      abs(out$manure_recycling_achieved - manure_recycling_target) <= 1e-10
  }
  recovery_tolerance <- 64 * .Machine$double.eps * pmax(
    abs(out$nue_output_before_policy),
    1e-30
  )
  out$crop_n_preserved <- (
    abs(out$crop_n - out$crop_n_before_policy) <= recovery_tolerance
  )
  out$residue_n_preserved <- (
    abs(out$residue_n - out$residue_n_before_policy) <= recovery_tolerance
  )

  changed <- isTRUE(nue_mitigation)
  if (changed) {
    gas <- calculate_gas_losses(
      fert_crop = out$fert_crop,
      manure_crop = out$manure_crop,
      fert_pas = out$fert_pas,
      manure_pas = out$manure_pas,
      soc = out$SOC,
      ga = out$GA,
      prec = out$PREC
    )
    out <- cbind(out, gas)
    out <- calculate_signed_leaching(out)
  } else {
    out$gas_crop <- out$base_gas_crop
    out$gas_pas <- out$base_gas_pas
    out$gas_nox_crop <- NA_real_
    out$gas_nox_pas <- NA_real_
    out$gas_nh3_crop <- NA_real_
    out$gas_nh3_pas <- NA_real_
    out$gas_n2_crop <- NA_real_
    out$gas_n2_pas <- NA_real_
    out$leach_crop <- out$base_leach_crop
    out$leach_pas <- out$base_leach_pas
  }
  out$gas_total <- out$gas_crop + out$gas_pas
  out$leach_total <- out$leach_crop + out$leach_pas
  out
}

make_scenario_summary <- function(grid, cfg, year) {
  safe_ratio <- function(numerator, denominator) {
    if (sum(denominator, na.rm = TRUE) > 0) {
      sum(numerator, na.rm = TRUE) / sum(denominator, na.rm = TRUE)
    } else {
      NA_real_
    }
  }
  adjustment_mode <- if (!is.null(cfg$nue_adjustment_mode)) {
    cfg$nue_adjustment_mode
  } else {
    "fertilizer_input"
  }
  baseline_manure_rate <- if (!is.null(cfg$baseline_manure_recycling_rate)) {
    cfg$baseline_manure_recycling_rate
  } else {
    0.4
  }
  manure_recycling_mitigation <- isTRUE(cfg$manure_recycling_mitigation)

  output_final <- grid$crop_n
  if (isTRUE(cfg$include_residue_in_nue)) {
    output_final <- output_final + grid$residue_n
  }
  feasible <- !grid$nue_infeasible
  applicable <- grid$input_crop_before_policy > 0 | grid$nue_output > 0
  feasible_applicable <- feasible & applicable
  share_or_na <- function(values) {
    if (length(values) == 0L) NA_real_ else mean(values, na.rm = TRUE)
  }

  data.frame(
    scenario_name = cfg$scenario_name,
    year = as.integer(year),
    flow_unit = "Tg N yr-1",
    scenario_500c = cfg$scenario_500c,
    scenario_demfwr = cfg$scenario_demfwr,
    nue_mitigation = cfg$nue_mitigation,
    scenario_air_pollution = isTRUE(cfg$scenario_air_pollution),
    nue_adjustment_mode = adjustment_mode,
    manure_recycling_mitigation = manure_recycling_mitigation,
    nue_target = if (cfg$nue_mitigation) unique(grid$nue_target)[1] else NA_real_,
    nue = safe_ratio(output_final, grid$input_crop),
    crop_n_preserved = all(grid$crop_n_preserved, na.rm = TRUE),
    residue_n_preserved = all(grid$residue_n_preserved, na.rm = TRUE),
    target_met_grid_share = mean(grid$nue_target_met, na.rm = TRUE),
    target_met_applicable_grid_share = share_or_na(
      grid$nue_target_met[applicable]
    ),
    target_met_feasible_grid_share = share_or_na(
      grid$nue_target_met[feasible_applicable]
    ),
    infeasible_grid_count = sum(grid$nue_infeasible, na.rm = TRUE),
    mass_balance_limited_grid_count = sum(
      grid$nue_mass_balance_limited,
      na.rm = TRUE
    ),
    fertilizer_floor_limited_grid_count = sum(
      grid$nue_fertilizer_floor_limited,
      na.rm = TRUE
    ),
    nue_target_input_excess = sum(
      grid$nue_target_input_excess,
      na.rm = TRUE
    ),
    infeasible_nue_output_share = safe_ratio(
      grid$nue_output[grid$nue_infeasible],
      grid$nue_output
    ),
    manure_recycling_target = if (
      cfg$nue_mitigation && manure_recycling_mitigation
    ) {
      unique(grid$manure_recycling_target)[1]
    } else {
      NA_real_
    },
    manure_recycling_achieved = safe_ratio(
      grid$manure_crop,
      grid$manure_crop_before_policy /
        baseline_manure_rate
    ),
    manure_recycling_target_met = all(
      grid$manure_recycling_target_met,
      na.rm = TRUE
    ),
    fertilizer_crop = sum(grid$fert_crop, na.rm = TRUE),
    fertilizer_pasture = sum(grid$fert_pas, na.rm = TRUE),
    fertilizer_total = sum(
      grid$fert_crop + grid$fert_pas,
      na.rm = TRUE
    ),
    synthetic_fertilizer_crop = sum(
      grid$synthetic_fert_crop,
      na.rm = TRUE
    ),
    synthetic_fertilizer_pasture = sum(
      grid$fert_pas,
      na.rm = TRUE
    ),
    synthetic_fertilizer_total = sum(
      grid$synthetic_fert_crop + grid$fert_pas,
      na.rm = TRUE
    ),
    recycled_residue_crop = sum(grid$residue_recycled, na.rm = TRUE),
    manure_crop = sum(grid$manure_crop, na.rm = TRUE),
    manure_pasture = sum(grid$manure_pas, na.rm = TRUE),
    manure_total = sum(
      grid$manure_crop + grid$manure_pas,
      na.rm = TRUE
    ),
    deposition_crop = sum(grid$dep_crop, na.rm = TRUE),
    deposition_pasture = sum(grid$dep_pas, na.rm = TRUE),
    deposition_total = sum(
      grid$dep_crop + grid$dep_pas,
      na.rm = TRUE
    ),
    biological_fixation_crop = sum(grid$fix_crop, na.rm = TRUE),
    biological_fixation_pasture = sum(grid$fix_pas, na.rm = TRUE),
    biological_fixation_total = sum(
      grid$fix_crop + grid$fix_pas,
      na.rm = TRUE
    ),
    nitrogen_input_crop = sum(grid$input_crop, na.rm = TRUE),
    nitrogen_input_pasture = sum(grid$input_pas, na.rm = TRUE),
    nitrogen_input_total = sum(grid$input_total, na.rm = TRUE),
    crop_n = sum(grid$crop_n, na.rm = TRUE),
    pasture_n = sum(grid$pasture_n, na.rm = TRUE),
    residue_n = sum(grid$residue_n, na.rm = TRUE),
    recovered_n_total = sum(
      grid$crop_n + grid$pasture_n + grid$residue_n,
      na.rm = TRUE
    ),
    gaseous_loss_crop = sum(grid$gas_crop, na.rm = TRUE),
    gaseous_loss_pasture = sum(grid$gas_pas, na.rm = TRUE),
    gaseous_loss_total = sum(grid$gas_total, na.rm = TRUE),
    leaching_crop = sum(grid$leach_crop, na.rm = TRUE),
    leaching_pasture = sum(grid$leach_pas, na.rm = TRUE),
    leaching_total = sum(grid$leach_total, na.rm = TRUE),
    nitrogen_output_total = sum(
      grid$crop_n + grid$pasture_n + grid$residue_n +
        grid$gas_total + grid$leach_total,
      na.rm = TRUE
    ),
    nitrogen_balance_gap = sum(
      grid$input_total - grid$crop_n - grid$pasture_n - grid$residue_n -
        grid$gas_total - grid$leach_total,
      na.rm = TRUE
    ),
    stringsAsFactors = FALSE
  )
}

aggregate_with_scenario <- function(grid, value, result_name, scenario) {
  out <- stats::aggregate(
    grid[[value]],
    by = list(YEMF = grid$YEMF),
    FUN = sum,
    na.rm = TRUE
  )
  names(out)[2] <- result_name
  out$SCENARIO <- scenario
  out[, c("SCENARIO", "YEMF", result_name), drop = FALSE]
}

grid_output <- function(grid, value, result_name, scenario = NULL) {
  out <- data.frame(
    G = grid$G,
    YEMF = grid$YEMF,
    value = grid[[value]],
    stringsAsFactors = FALSE
  )
  names(out)[3] <- result_name
  if (!is.null(scenario)) {
    out$SCENARIO <- scenario
    out <- out[, c("G", "YEMF", "SCENARIO", result_name), drop = FALSE]
  }
  out
}

assign_grid_to_environment <- function(env, grid, scenario) {
  assign(
    "INfercropdis",
    grid_output(grid, "fert_crop", "Fert_Use_Grid"),
    envir = env
  )
  assign(
    "INferpasdis",
    grid_output(grid, "fert_pas", "Fert_Use_Grid"),
    envir = env
  )
  total_fert <- grid$fert_crop + grid$fert_pas
  grid$fert_total <- total_fert
  assign(
    "INferdis",
    grid_output(grid, "fert_total", "Fert_Use_Grid"),
    envir = env
  )
  assign(
    "INfercrop",
    aggregate_with_scenario(grid, "fert_crop", "INfercrop", scenario),
    envir = env
  )
  assign(
    "INferpas",
    aggregate_with_scenario(grid, "fert_pas", "INferpas", scenario),
    envir = env
  )
  assign(
    "INfer",
    aggregate_with_scenario(grid, "fert_total", "INfer", scenario),
    envir = env
  )

  assign(
    "INmancropdis",
    grid_output(grid, "manure_crop", "ManureCropGrid"),
    envir = env
  )
  assign(
    "INmanpasdis",
    grid_output(grid, "manure_pas", "ManurePasGrid"),
    envir = env
  )
  grid$manure_total <- grid$manure_crop + grid$manure_pas
  assign(
    "INmandis",
    grid_output(grid, "manure_total", "ManureGrid"),
    envir = env
  )
  assign(
    "INmancrop",
    aggregate_with_scenario(grid, "manure_crop", "INman", scenario),
    envir = env
  )
  assign(
    "INmanpas",
    aggregate_with_scenario(grid, "manure_pas", "INman", scenario),
    envir = env
  )
  assign(
    "INman",
    aggregate_with_scenario(grid, "manure_total", "INman", scenario),
    envir = env
  )

  assign("Inputdis", grid_output(grid, "input_total", "Input"), envir = env)
  assign("Inputpasdis", grid_output(grid, "input_pas", "Input"), envir = env)
  assign(
    "OUTcropcropdis",
    grid_output(grid, "crop_n", "CropN"),
    envir = env
  )
  assign(
    "OUTcroppasdis",
    grid_output(grid, "pasture_n", "OUTcroppas"),
    envir = env
  )
  grid$crop_total <- grid$crop_n + grid$pasture_n
  assign("OUTcropdis", grid_output(grid, "crop_total", "CropN"), envir = env)
  assign(
    "OUTresdis",
    grid_output(grid, "residue_n", "OUTresgrid"),
    envir = env
  )
  assign("OUTcrop", aggregate_with_scenario(
    grid, "crop_total", "OUTcrop", scenario
  )[, c("YEMF", "OUTcrop"), drop = FALSE], envir = env)
  assign("OUTres", aggregate_with_scenario(
    grid, "residue_n", "OUTres", scenario
  )[, c("YEMF", "OUTres"), drop = FALSE], envir = env)

  gas_specs <- list(
    OUTgascropdis = list("gas_crop", "OUTgascropgrid", FALSE),
    OUTgaspasdis = list("gas_pas", "OUTgaspasgrid", FALSE),
    OUTgasdis = list("gas_total", "OUTgasgrid", FALSE),
    OUTgascropdis_NOx = list("gas_nox_crop", "OUTgascropgrid", TRUE),
    OUTgaspasdis_NOx = list("gas_nox_pas", "OUTgaspasgrid", TRUE),
    OUTgasdis_NOx = list("gas_nox_total", "OUTgasgrid", TRUE),
    OUTgascropdis_NH3 = list("gas_nh3_crop", "OUTgascropgrid", TRUE),
    OUTgaspasdis_NH3 = list("gas_nh3_pas", "OUTgaspasgrid", TRUE),
    OUTgasdis_NH3 = list("gas_nh3_total", "OUTgasgrid", TRUE),
    OUTgascropdis_N2 = list("gas_n2_crop", "OUTgascropgrid", TRUE),
    OUTgaspasdis_N2 = list("gas_n2_pas", "OUTgaspasgrid", TRUE),
    OUTgasdis_N2 = list("gas_n2_total", "OUTgasgrid", TRUE)
  )
  grid$gas_nox_total <- grid$gas_nox_crop + grid$gas_nox_pas
  grid$gas_nh3_total <- grid$gas_nh3_crop + grid$gas_nh3_pas
  grid$gas_n2_total <- grid$gas_n2_crop + grid$gas_n2_pas
  for (object_name in names(gas_specs)) {
    spec <- gas_specs[[object_name]]
    assign(
      object_name,
      grid_output(
        grid,
        spec[[1]],
        spec[[2]],
        if (isTRUE(spec[[3]])) scenario else NULL
      ),
      envir = env
    )
  }

  aggregate_gas <- function(value, total_name, crop_name, pasture_name) {
    assign(
      total_name,
      aggregate_with_scenario(grid, paste0(value, "_total"), "OUTgas", scenario),
      envir = env
    )
    assign(
      crop_name,
      aggregate_with_scenario(grid, paste0(value, "_crop"), "OUTgascrop", scenario),
      envir = env
    )
    assign(
      pasture_name,
      aggregate_with_scenario(grid, paste0(value, "_pas"), "OUTgaspas", scenario),
      envir = env
    )
  }
  aggregate_gas("gas_nox", "OUTgas_NOx", "OUTgascrop_NOx", "OUTgaspas_NOx")
  aggregate_gas("gas_nh3", "OUTgas_NH3", "OUTgascrop_NH3", "OUTgaspas_NH3")
  aggregate_gas("gas_n2", "OUTgas_N2", "OUTgascrop_N2", "OUTgaspas_N2")
  assign(
    "OUTgas",
    aggregate_with_scenario(grid, "gas_total", "OUTgas", scenario),
    envir = env
  )
  assign(
    "OUTgascrop",
    aggregate_with_scenario(grid, "gas_crop", "OUTgascrop", scenario),
    envir = env
  )
  assign(
    "OUTgaspas",
    aggregate_with_scenario(grid, "gas_pas", "OUTgaspas", scenario),
    envir = env
  )

  assign("OUTleadis", grid_output(grid, "leach_total", "OUTleagrid"), envir = env)
  assign(
    "OUTleacropdis",
    grid_output(grid, "leach_crop", "OUTleacrop"),
    envir = env
  )
  assign(
    "OUTleapasdis",
    grid_output(grid, "leach_pas", "OUTleagrid"),
    envir = env
  )
  outlea <- stats::aggregate(
    grid$leach_total,
    by = list(YEMF = grid$YEMF),
    FUN = sum,
    na.rm = TRUE
  )
  names(outlea)[2] <- "OUTlea"
  assign("OUTlea", outlea, envir = env)
  invisible(env)
}

rebuild_agricultural_emission_grids <- function(env, tolerance = 1e-10) {
  normalise_component <- function(object_name, value_column) {
    if (!exists(object_name, envir = env, inherits = FALSE)) {
      stop("Required model object not found: ", object_name)
    }
    x <- get(object_name, envir = env, inherits = FALSE)
    required <- c("G", "YEMF", "SCENARIO", value_column)
    missing <- setdiff(required, names(x))
    if (length(missing) > 0L) {
      stop(
        object_name, " is missing columns: ",
        paste(missing, collapse = ", ")
      )
    }

    out <- data.frame(
      G = as.character(x$G),
      YEMF = suppressWarnings(as.integer(as.character(x$YEMF))),
      SCENARIO = as.character(x$SCENARIO),
      value = suppressWarnings(as.numeric(x[[value_column]])),
      stringsAsFactors = FALSE
    )
    invalid_key <- is.na(out$G) | !nzchar(out$G) |
      is.na(out$YEMF) | is.na(out$SCENARIO) | !nzchar(out$SCENARIO)
    if (any(invalid_key)) {
      stop(object_name, " contains an invalid grid, year, or scenario key.")
    }
    if (any(!is.finite(out$value) | out$value < 0)) {
      stop(object_name, " contains non-finite or negative emissions.")
    }

    out <- stats::aggregate(
      out$value,
      by = list(G = out$G, YEMF = out$YEMF, SCENARIO = out$SCENARIO),
      FUN = sum
    )
    names(out)[4] <- "value"
    out
  }

  check_total <- function(actual, expected, label) {
    scale <- max(1, abs(expected))
    if (!is.finite(actual) || !is.finite(expected) ||
        abs(actual - expected) > tolerance * scale) {
      stop(
        label, " failed its mass-balance check: expected ", expected,
        ", obtained ", actual, "."
      )
    }
    invisible(TRUE)
  }

  scalar_total <- function(object_name, value_column) {
    if (!exists(object_name, envir = env, inherits = FALSE)) {
      stop("Required model object not found: ", object_name)
    }
    x <- get(object_name, envir = env, inherits = FALSE)
    if (!is.data.frame(x) || !value_column %in% names(x)) {
      stop(object_name, " must contain column ", value_column, ".")
    }
    value <- suppressWarnings(as.numeric(x[[value_column]]))
    if (length(value) == 0L || any(!is.finite(value) | value < 0)) {
      stop(object_name, " contains invalid emissions.")
    }
    sum(value)
  }

  nox <- normalise_component("OUTgasdis_NOx", "OUTgasgrid")
  no_agriculture <- nox[, c("G", "YEMF", "SCENARIO"), drop = FALSE]
  no_agriculture$OUTgasgrid <- nox$value * 5 / 14

  nh3_soil <- normalise_component("OUTgasdis_NH3", "OUTgasgrid")
  nh3_mms <- normalise_component("NH3_loss1dis", "NH3_MMS_TgN_yr")
  nh3_direct <- normalise_component("NH3_loss2dis", "NH3_direct_TgN_yr")
  nh3_components <- rbind(nh3_soil, nh3_mms, nh3_direct)
  nh3_agriculture <- stats::aggregate(
    nh3_components$value,
    by = list(
      G = nh3_components$G,
      YEMF = nh3_components$YEMF,
      SCENARIO = nh3_components$SCENARIO
    ),
    FUN = sum
  )
  names(nh3_agriculture)[4] <- "OUTgasgrid"

  check_total(
    sum(no_agriculture$OUTgasgrid),
    sum(nox$value) * 5 / 14,
    "Agricultural NO grid"
  )
  check_total(
    sum(nh3_mms$value),
    scalar_total("NH3_loss1", "ManureStable"),
    "Manure-management NH3 grid"
  )
  check_total(
    sum(nh3_direct$value),
    scalar_total("NH3_loss2", "ManurePas"),
    "Direct-excretion NH3 grid"
  )
  check_total(
    sum(nh3_agriculture$OUTgasgrid),
    sum(nh3_soil$value) + sum(nh3_mms$value) + sum(nh3_direct$value),
    "Agricultural NH3 grid"
  )

  assign("OUTgasdis_NO_agriculture", no_agriculture, envir = env)
  assign("OUTgasdis_NH3_agriculture", nh3_agriculture, envir = env)
  invisible(env)
}

apply_nflow_scenario <- function(env,
                                 cfg,
                                 year,
                                 nue_schedule = NULL) {
  grid <- build_scenario_grid(
    env = env,
    year = year,
    scenario = cfg$base_scenario
  )

  schedule_values <- if (cfg$nue_mitigation) {
    interpolate_schedule(nue_schedule, year)
  } else {
    list(nue_target = NA_real_, manure_recycling_target = NA_real_)
  }

  adjusted <- apply_scenario_to_grid(
    grid = grid,
    nue_mitigation = cfg$nue_mitigation,
    nue_target = schedule_values$nue_target,
    manure_recycling_target = schedule_values$manure_recycling_target,
    baseline_manure_recycling_rate = cfg$baseline_manure_recycling_rate,
    include_residue_in_nue = cfg$include_residue_in_nue,
    nue_adjustment_mode = cfg$nue_adjustment_mode,
    manure_recycling_mitigation = cfg$manure_recycling_mitigation
  )

  if (cfg$nue_mitigation) {
    assign_grid_to_environment(env, adjusted, cfg$base_scenario)
  }
  rebuild_agricultural_emission_grids(env)
  assign("scenario_audit", adjusted, envir = env)
  assign(
    "scenario_summary",
    make_scenario_summary(adjusted, cfg, year),
    envir = env
  )
  invisible(env)
}
