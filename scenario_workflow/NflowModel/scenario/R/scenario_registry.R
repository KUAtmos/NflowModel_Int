scenario_axis_registry <- function() {
  data.frame(
    id = c("500c", "demfwr", "nue", "air_pollution"),
    config_key = c(
      "SCENARIO_500C",
      "SCENARIO_DEMFWR",
      "SCENARIO_NUE",
      "SCENARIO_AIR_POLLUTION"
    ),
    label = c("500C", "DEMFWR", "NUE", "Air pollution control"),
    implemented = c(TRUE, TRUE, TRUE, TRUE),
    # NUE is applied after the raw model. The other three axes are matched
    # against the registered raw/deposition input combinations below.
    affects_raw_input = c(TRUE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
}

raw_input_scenario_registry <- function() {
  data.frame(
    scenario_500c = c(FALSE, TRUE, FALSE, TRUE, FALSE),
    scenario_demfwr = c(FALSE, FALSE, TRUE, TRUE, FALSE),
    scenario_air_pollution = c(FALSE, FALSE, FALSE, FALSE, TRUE),
    raw_scenario = c(
      "SSP2_BaU_NoCC_No",
      "SSP2_500C_CACN_NoCC_No",
      "SSP2_BaU_DEMFWR_No",
      "SSP2_500C_CACN_DEMFWR_No",
      # APMTFR changes GEOS-Chem deposition only. Agricultural inputs remain
      # the Baseline inputs because this is the only completed APMTFR run.
      "SSP2_BaU_NoCC_No"
    ),
    deposition_prefix = c(
      "", "", "", "",
      "SSP2_BaU_NoCC_No_APMTFR"
    ),
    stringsAsFactors = FALSE
  )
}

scenario_id_from_switches <- function(switches) {
  axes <- scenario_axis_registry()
  active <- axes$id[vapply(
    axes$id,
    function(id) isTRUE(switches[[id]]),
    logical(1)
  )]
  if (length(active) == 0L) "baseline" else paste(active, collapse = "_")
}

scenario_label_from_switches <- function(switches) {
  axes <- scenario_axis_registry()
  active <- axes$label[vapply(
    axes$id,
    function(id) isTRUE(switches[[id]]),
    logical(1)
  )]
  if (length(active) == 0L) "Baseline" else paste(active, collapse = " + ")
}

match_raw_input_registration <- function(switches) {
  registry <- raw_input_scenario_registry()
  raw_axes <- scenario_axis_registry()
  raw_axes <- raw_axes[raw_axes$affects_raw_input, , drop = FALSE]
  selected <- rep(TRUE, nrow(registry))
  for (id in raw_axes$id) {
    selected <- selected &
      registry[[paste0("scenario_", id)]] == isTRUE(switches[[id]])
  }
  registry[selected, , drop = FALSE]
}

scenario_combination_supported <- function(switches) {
  nrow(match_raw_input_registration(switches)) == 1L
}

assert_supported_scenario_combination <- function(switches) {
  if (!scenario_combination_supported(switches)) {
    stop(
      "Air-pollution control currently has GEOS-Chem deposition only for ",
      "the Baseline (500C=off, DEMFWR=off) input. Complete and register ",
      "the corresponding APMTFR GEOS-Chem runs before combining it with ",
      "500C or DEMFWR."
    )
  }
  invisible(TRUE)
}

resolve_raw_scenario <- function(switches) {
  raw_axes <- scenario_axis_registry()
  raw_axes <- raw_axes[raw_axes$affects_raw_input, , drop = FALSE]
  selected <- match_raw_input_registration(switches)
  if (nrow(selected) != 1L) {
    active <- raw_axes$id[vapply(
      raw_axes$id,
      function(id) isTRUE(switches[[id]]),
      logical(1)
    )]
    stop(
      "No unique raw-input scenario matches the selected input switches: ",
      if (length(active) == 0L) "none" else paste(active, collapse = ", ")
    )
  }
  selected$raw_scenario[[1]]
}

resolve_deposition_prefix <- function(switches) {
  selected <- match_raw_input_registration(switches)
  if (nrow(selected) != 1L) {
    stop("No unique deposition input matches the selected scenario switches.")
  }
  as.character(selected$deposition_prefix[[1]])
}

expand_gmd_scenario_cases <- function(master_cfg) {
  axes <- scenario_axis_registry()
  unsupported_axes <- c("demfwr", "nue", "air_pollution")

  if (!isTRUE(master_cfg$run_all_combinations)) {
    active_unsupported <- unsupported_axes[vapply(
      unsupported_axes,
      function(id) isTRUE(master_cfg$axis_switches[[id]]),
      logical(1)
    )]
    if (length(active_unsupported) > 0L) {
      stop(
        "SCENARIO_GMD_INPUTS=on selects the two historical input cases only. ",
        "Set SCENARIO_DEMFWR, SCENARIO_NUE, and ",
        "SCENARIO_AIR_POLLUTION to off. Active unsupported switches: ",
        paste(active_unsupported, collapse = ", ")
      )
    }
  }

  selected_500c <- if (isTRUE(master_cfg$run_all_combinations)) {
    c(FALSE, TRUE)
  } else {
    isTRUE(master_cfg$axis_switches[["500c"]])
  }

  combinations <- data.frame(
    `500c` = selected_500c,
    demfwr = FALSE,
    nue = FALSE,
    air_pollution = FALSE,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  combinations$scenario_id <- ifelse(
    combinations$`500c`,
    "gmd_500c",
    "gmd_baseline"
  )
  combinations$scenario_label <- ifelse(
    combinations$`500c`,
    "GMD 500C",
    "GMD Baseline"
  )
  combinations$raw_scenario <- ifelse(
    combinations$`500c`,
    "SSP2_500C_CACN_NoCC",
    "SSP2_BaU_NoCC"
  )
  combinations$deposition_prefix <- ""
  combinations[, c(axes$id, "scenario_id", "scenario_label",
                    "raw_scenario", "deposition_prefix"), drop = FALSE]
}

filter_selected_cases <- function(cases, master_cfg) {
  selected <- master_cfg$run_scenarios
  if (is.null(selected) || identical(selected, "all")) return(cases)
  if (!length(selected) || anyDuplicated(selected) ||
      any(!selected %in% cases$scenario_id)) {
    stop("RUN_SCENARIOS must be 'all' or unique IDs selected by the switches: ",
         paste(cases$scenario_id, collapse = ", "))
  }
  cases[cases$scenario_id %in% selected, , drop = FALSE]
}

expand_scenario_cases <- function(master_cfg) {
  if (isTRUE(master_cfg$scenario_gmd_inputs)) {
    return(filter_selected_cases(expand_gmd_scenario_cases(master_cfg), master_cfg))
  }

  axes <- scenario_axis_registry()
  implemented <- axes[axes$implemented, , drop = FALSE]

  if (isTRUE(master_cfg$run_all_combinations)) {
    combinations <- expand.grid(
      setNames(
        rep(list(c(FALSE, TRUE)), nrow(implemented)),
        implemented$id
      ),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    combinations <- as.data.frame(
      setNames(
        lapply(implemented$id, function(id) master_cfg$axis_switches[[id]]),
        implemented$id
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  supported <- vapply(seq_len(nrow(combinations)), function(index) {
    scenario_combination_supported(as.list(combinations[index, , drop = FALSE]))
  }, logical(1))
  if (isTRUE(master_cfg$run_all_combinations)) {
    combinations <- combinations[supported, , drop = FALSE]
  } else if (!all(supported)) {
    assert_supported_scenario_combination(
      as.list(combinations[which(!supported)[1], , drop = FALSE])
    )
  }

  for (id in axes$id[!axes$implemented]) {
    combinations[[id]] <- FALSE
  }
  combinations <- combinations[, axes$id, drop = FALSE]

  active_count <- rowSums(combinations[, implemented$id, drop = FALSE])
  binary_rank <- rowSums(sweep(
    combinations[, implemented$id, drop = FALSE] * 1L,
    2,
    2 ^ (seq_len(nrow(implemented)) - 1L),
    `*`
  ))
  combinations <- combinations[order(active_count, binary_rank), , drop = FALSE]
  rownames(combinations) <- NULL

  switch_rows <- lapply(seq_len(nrow(combinations)), function(index) {
    as.list(combinations[index, , drop = FALSE])
  })
  combinations$scenario_id <- vapply(
    switch_rows,
    scenario_id_from_switches,
    character(1)
  )
  combinations$scenario_label <- vapply(
    switch_rows,
    scenario_label_from_switches,
    character(1)
  )
  combinations$raw_scenario <- vapply(
    switch_rows,
    resolve_raw_scenario,
    character(1)
  )
  combinations$deposition_prefix <- vapply(
    switch_rows,
    resolve_deposition_prefix,
    character(1)
  )

  if (anyDuplicated(combinations$scenario_id)) {
    stop("The scenario registry generated duplicate scenario IDs.")
  }
  filter_selected_cases(combinations, master_cfg)
}

make_case_config <- function(master_cfg, case_row) {
  case_row <- as.list(case_row[1, , drop = FALSE])
  cfg <- master_cfg
  cfg$scenario_name <- as.character(case_row$scenario_id)
  cfg$scenario_label <- as.character(case_row$scenario_label)
  cfg$base_scenario <- as.character(case_row$raw_scenario)
  cfg$scenario_500c <- isTRUE(case_row$`500c`)
  cfg$scenario_demfwr <- isTRUE(case_row$demfwr)
  cfg$nue_mitigation <- isTRUE(case_row$nue)
  cfg$scenario_air_pollution <- isTRUE(case_row$air_pollution)
  registry_prefix <- as.character(case_row$deposition_prefix)
  forced_prefix <- as.character(master_cfg$deposition_prefix_override)[1]
  cfg$deposition_prefix_override <- if (
    !is.na(forced_prefix) && nzchar(forced_prefix)
  ) {
    forced_prefix
  } else {
    registry_prefix
  }
  cfg$scenario_gmd_inputs <- isTRUE(master_cfg$scenario_gmd_inputs)
  if (cfg$scenario_gmd_inputs) {
    cfg$summary_reference_scenario <- "SSP2_BaU_NoCC"
  }
  cfg
}
