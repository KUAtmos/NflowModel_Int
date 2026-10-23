# Helpers used only by the future-livestock-density manure calculation.

nflow_impute_body_weight <- function(target_heads,
                                     reference_heads,
                                     body_weight,
                                     region_map,
                                     year) {
  required_heads <- c("I", "J", "slnum", "Livestock_number")
  required_bw <- c("I", "J", "slnum", "BodyWeight")
  required_map <- c("I", "J", "R")
  if (!all(required_heads %in% names(target_heads)) ||
      !all(required_heads %in% names(reference_heads)) ||
      !all(required_bw %in% names(body_weight)) ||
      !all(required_map %in% names(region_map))) {
    stop("Body-weight imputation received an input with missing columns.")
  }

  normalize_keys <- function(x) {
    x$I <- as.integer(as.character(x$I))
    x$J <- as.integer(as.character(x$J))
    if ("slnum" %in% names(x)) {
      x$slnum <- as.integer(as.character(x$slnum))
    }
    x
  }
  target_heads <- normalize_keys(target_heads)
  reference_heads <- normalize_keys(reference_heads)
  body_weight <- normalize_keys(body_weight)
  region_map <- normalize_keys(region_map)
  region_map <- unique(region_map[required_map])
  if (anyDuplicated(target_heads[c("I", "J", "slnum")]) ||
      anyDuplicated(reference_heads[c("I", "J", "slnum")]) ||
      anyDuplicated(body_weight[c("I", "J", "slnum")])) {
    stop("Livestock heads and body weight must be unique by I/J/slnum.")
  }
  if (anyDuplicated(region_map[c("I", "J")])) {
    stop("MAP_RIJ must contain one region for each I/J grid cell.")
  }

  body_weight$BodyWeight <- suppressWarnings(as.numeric(body_weight$BodyWeight))
  body_weight$BodyWeight[
    !is.finite(body_weight$BodyWeight) | body_weight$BodyWeight <= 0
  ] <- NA_real_

  add_context <- function(heads) {
    heads$Livestock_number <- suppressWarnings(
      as.numeric(heads$Livestock_number)
    )
    heads$Livestock_number[
      !is.finite(heads$Livestock_number) | heads$Livestock_number < 0
    ] <- 0
    heads |>
      dplyr::left_join(body_weight, by = c("I", "J", "slnum")) |>
      dplyr::left_join(region_map, by = c("I", "J"))
  }

  reference <- add_context(reference_heads)

  regional_fill <- reference |>
    dplyr::filter(!is.na(R), is.finite(BodyWeight), BodyWeight > 0) |>
    dplyr::group_by(R, slnum) |>
    dplyr::summarise(
      weighted_sum = sum(BodyWeight * Livestock_number, na.rm = TRUE),
      head_sum = sum(Livestock_number, na.rm = TRUE),
      simple_mean = mean(BodyWeight, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      regional_bw = dplyr::if_else(
        is.finite(head_sum) & head_sum > 0,
        weighted_sum / head_sum,
        simple_mean
      )
    ) |>
    dplyr::select(R, slnum, regional_bw)

  global_fill <- reference |>
    dplyr::filter(!is.na(R), is.finite(BodyWeight), BodyWeight > 0) |>
    dplyr::group_by(slnum) |>
    dplyr::summarise(
      weighted_sum = sum(BodyWeight * Livestock_number, na.rm = TRUE),
      head_sum = sum(Livestock_number, na.rm = TRUE),
      simple_mean = mean(BodyWeight, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      global_bw = dplyr::if_else(
        is.finite(head_sum) & head_sum > 0,
        weighted_sum / head_sum,
        simple_mean
      )
    ) |>
    dplyr::select(slnum, global_bw)

  target <- add_context(target_heads)
  completed <- target |>
    dplyr::filter(Livestock_number > 0, !is.na(R)) |>
    dplyr::left_join(regional_fill, by = c("R", "slnum")) |>
    dplyr::left_join(global_fill, by = "slnum") |>
    dplyr::mutate(
      BodyWeight_original = BodyWeight,
      bw_fill_source = dplyr::case_when(
        is.finite(BodyWeight) & BodyWeight > 0 ~ "grid_2010",
        is.finite(regional_bw) & regional_bw > 0 ~ "region_species_2010",
        is.finite(global_bw) & global_bw > 0 ~ "global_species_2010",
        TRUE ~ "unresolved"
      ),
      BodyWeight = dplyr::case_when(
        is.finite(BodyWeight) & BodyWeight > 0 ~ BodyWeight,
        is.finite(regional_bw) & regional_bw > 0 ~ regional_bw,
        is.finite(global_bw) & global_bw > 0 ~ global_bw,
        TRUE ~ NA_real_
      )
    )

  unresolved <- completed |>
    dplyr::filter(!is.finite(BodyWeight) | BodyWeight <= 0)
  if (nrow(unresolved) > 0L) {
    examples <- utils::head(unresolved[c("R", "I", "J", "slnum")], 5)
    stop(
      "Could not impute a positive 2010 body weight for future livestock cells: ",
      paste(apply(examples, 1, paste, collapse = "/"), collapse = ", ")
    )
  }

  target_diagnostics <- completed |>
    dplyr::mutate(
      year = as.integer(year),
      imputed = bw_fill_source != "grid_2010"
    ) |>
    dplyr::group_by(year, slnum, bw_fill_source) |>
    dplyr::summarise(
      cells = dplyr::n(),
      livestock_heads = sum(Livestock_number, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(imputed = bw_fill_source != "grid_2010")
  domain_diagnostics <- dplyr::bind_rows(
    reference |>
      dplyr::filter(as.integer(year) != 2010L, Livestock_number > 0) |>
      dplyr::transmute(
        year = 2010L,
        slnum,
        bw_fill_source = dplyr::if_else(
          is.na(R),
          "outside_model_domain",
          "reference_model_domain"
        ),
        Livestock_number
      ),
    target |>
      dplyr::filter(Livestock_number > 0, is.na(R)) |>
      dplyr::transmute(
        year = as.integer(year),
        slnum,
        bw_fill_source = "outside_model_domain",
        Livestock_number
      )
  ) |>
    dplyr::group_by(year, slnum, bw_fill_source) |>
    dplyr::summarise(
      cells = dplyr::n(),
      livestock_heads = sum(Livestock_number, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(imputed = FALSE)
  diagnostics <- dplyr::bind_rows(target_diagnostics, domain_diagnostics) |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      positive_heads_in_diagnostics = sum(livestock_heads, na.rm = TRUE),
      livestock_head_share = dplyr::if_else(
        positive_heads_in_diagnostics > 0,
        livestock_heads/positive_heads_in_diagnostics,
        0
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(year, slnum, bw_fill_source)

  list(
    data = completed |>
      dplyr::mutate(Weight = Livestock_number * BodyWeight) |>
      dplyr::select(
        I, J, slnum, Livestock_number, BodyWeight, Weight, R,
        bw_fill_source
      ),
    diagnostics = diagnostics,
    regional_fill = regional_fill,
    global_fill = global_fill
  )
}

nflow_read_body_weight_table <- function(path,
                                         n_species,
                                         n_latitude,
                                         n_longitude) {
  dimensions <- suppressWarnings(
    as.integer(c(n_species, n_latitude, n_longitude))
  )
  if (length(dimensions) != 3L || any(!is.finite(dimensions)) ||
      any(dimensions <= 0L)) {
    stop("Body-weight table dimensions must be positive integers.")
  }
  if (!file.exists(path)) {
    stop("Body-weight table does not exist: ", path)
  }

  expected_rows <- dimensions[[1]]*dimensions[[2]]
  values_per_row <- dimensions[[3]]
  values <- numeric(expected_rows*values_per_row)
  connection <- file(path, open = "rt")
  on.exit(close(connection), add = TRUE)
  header <- readLines(connection, n = 1L, warn = FALSE)
  if (length(header) != 1L) {
    stop("Body-weight table is empty: ", path)
  }

  for (row_index in seq_len(expected_rows)) {
    row <- readLines(connection, n = 1L, warn = FALSE)
    if (length(row) != 1L) {
      stop(
        "Unexpected body-weight row count in ", path,
        ": expected ", expected_rows,
        ", found ", row_index - 1L
      )
    }
    row <- sub("[,;][[:space:]]*$", "", row)
    tokens <- trimws(strsplit(row, ",", fixed = TRUE)[[1]])
    if (length(tokens) != values_per_row || any(!nzchar(tokens))) {
      stop(
        "Body-weight row ", row_index, " in ", path,
        " must contain exactly ", values_per_row, " values; found ",
        length(tokens), "."
      )
    }
    parsed <- suppressWarnings(as.numeric(tokens))
    if (any(!is.finite(parsed))) {
      bad_token <- tokens[which(!is.finite(parsed))[[1]]]
      stop(
        "Non-numeric body-weight value in row ", row_index,
        " of ", path, ": ", bad_token
      )
    }
    offset <- (row_index - 1L)*values_per_row
    values[offset + seq_len(values_per_row)] <- parsed
  }

  remaining <- readLines(connection, warn = FALSE)
  remaining <- remaining[nzchar(trimws(remaining))]
  if (length(remaining) > 0L) {
    stop(
      "Unexpected extra body-weight rows in ", path,
      ": ", length(remaining)
    )
  }
  values
}

nflow_allocate_group_loss_to_grid <- function(livestock,
                                                group_loss,
                                                grid_map,
                                                loss_column,
                                                output_column,
                                                year,
                                                scenario,
                                                tolerance = 1e-10) {
  required_livestock <- c("I", "J", "R", "slnum", "Weight")
  required_loss <- c("R", "slnum", loss_column)
  required_map <- c("I", "J", "G")
  if (!all(required_livestock %in% names(livestock)) ||
      !all(required_loss %in% names(group_loss)) ||
      !all(required_map %in% names(grid_map))) {
    stop("NH3 grid allocation received an input with missing columns.")
  }

  livestock <- livestock[required_livestock]
  group_loss <- group_loss[required_loss]
  grid_map <- unique(grid_map[required_map])
  livestock$I <- as.integer(as.character(livestock$I))
  livestock$J <- as.integer(as.character(livestock$J))
  livestock$slnum <- as.integer(as.character(livestock$slnum))
  livestock$R <- as.character(livestock$R)
  livestock$Weight <- suppressWarnings(as.numeric(livestock$Weight))
  group_loss$slnum <- as.integer(as.character(group_loss$slnum))
  group_loss$R <- as.character(group_loss$R)
  group_loss$.group_loss_kg_n <- suppressWarnings(
    as.numeric(group_loss[[loss_column]])
  )
  grid_map$I <- as.integer(as.character(grid_map$I))
  grid_map$J <- as.integer(as.character(grid_map$J))
  grid_map$G <- as.character(grid_map$G)

  if (anyDuplicated(livestock[c("I", "J", "slnum")])) {
    stop("Livestock rows must be unique by I/J/slnum for NH3 allocation.")
  }
  if (anyDuplicated(group_loss[c("R", "slnum")])) {
    stop("NH3 group losses must be unique by R/slnum.")
  }
  if (anyDuplicated(grid_map[c("I", "J")])) {
    stop("NH3 grid map must be unique by I/J.")
  }
  if (any(!is.finite(livestock$Weight) | livestock$Weight < 0) ||
      any(!is.finite(group_loss$.group_loss_kg_n) |
          group_loss$.group_loss_kg_n < 0)) {
    stop("NH3 allocation weights and group losses must be finite and non-negative.")
  }

  group_weights <- livestock |>
    dplyr::group_by(R, slnum) |>
    dplyr::summarise(.group_weight = sum(Weight), .groups = "drop")
  allocation <- livestock |>
    dplyr::left_join(group_weights, by = c("R", "slnum")) |>
    dplyr::left_join(
      group_loss[c("R", "slnum", ".group_loss_kg_n")],
      by = c("R", "slnum")
    )
  if (any(!is.finite(allocation$.group_loss_kg_n))) {
    stop("Every livestock R/slnum group must have an NH3 group loss.")
  }
  invalid_weight <- allocation$.group_loss_kg_n > 0 &
    (!is.finite(allocation$.group_weight) | allocation$.group_weight <= 0)
  if (any(invalid_weight)) {
    stop("A positive NH3 group loss has no positive livestock weight.")
  }
  allocation$.loss_kg_n <- ifelse(
    allocation$.group_loss_kg_n > 0,
    allocation$.group_loss_kg_n * allocation$Weight / allocation$.group_weight,
    0
  )
  allocation <- allocation |>
    dplyr::left_join(grid_map, by = c("I", "J"))
  if (any(allocation$.loss_kg_n > 0 & is.na(allocation$G))) {
    stop("A positive NH3 grid loss could not be mapped to an N-flow grid.")
  }

  output <- allocation |>
    dplyr::filter(!is.na(G)) |>
    dplyr::group_by(G) |>
    dplyr::summarise(.value = sum(.loss_kg_n) / 1e9, .groups = "drop")
  expected <- sum(group_loss$.group_loss_kg_n) / 1e9
  recovered <- sum(output$.value)
  scale <- max(1, abs(expected))
  if (!is.finite(recovered) || abs(recovered - expected) > tolerance * scale) {
    stop("Grid-resolved NH3 loss failed its mass-balance check.")
  }
  output$YEMF <- as.integer(year)
  output$SCENARIO <- as.character(scenario)
  output <- output[c("G", "YEMF", "SCENARIO", ".value")]
  names(output)[[4L]] <- output_column
  output
}

nflow_allocate_direct_excretion <- function(source,
                                             regional_total,
                                             pasture_allocation,
                                             tolerance = 1e-10) {
  required_columns <- list(
    source = c("RegionCode", "G", "tN"),
    regional_total = c("RegionCode", "YEMF", "SCENARIO", "tN"),
    pasture_allocation = c("RegionCode", "YEMF", "G", "LURATIO")
  )
  inputs <- list(
    source = source,
    regional_total = regional_total,
    pasture_allocation = pasture_allocation
  )
  for (label in names(inputs)) {
    missing_columns <- setdiff(required_columns[[label]], names(inputs[[label]]))
    if (length(missing_columns) > 0L) {
      stop(
        label, " is missing required column(s): ",
        paste(missing_columns, collapse = ", ")
      )
    }
  }

  tolerance <- suppressWarnings(as.numeric(tolerance)[1])
  if (!is.finite(tolerance) || tolerance <= 0) {
    stop("tolerance must be a finite positive number.")
  }

  normalize_region <- function(x) {
    x$RegionCode <- as.character(x$RegionCode)
    x
  }
  source <- normalize_region(as.data.frame(source))
  regional_total <- normalize_region(as.data.frame(regional_total))
  pasture_allocation <- normalize_region(as.data.frame(pasture_allocation))
  source$G <- as.character(source$G)
  pasture_allocation$G <- as.character(pasture_allocation$G)
  regional_total$SCENARIO <- as.character(regional_total$SCENARIO)
  regional_total$YEMF <- suppressWarnings(
    as.integer(as.character(regional_total$YEMF))
  )
  pasture_allocation$YEMF <- suppressWarnings(
    as.integer(as.character(pasture_allocation$YEMF))
  )
  source$tN <- suppressWarnings(as.numeric(source$tN))
  regional_total$tN <- suppressWarnings(as.numeric(regional_total$tN))
  pasture_allocation$LURATIO <- suppressWarnings(
    as.numeric(pasture_allocation$LURATIO)
  )

  if (any(is.na(source$RegionCode)) || any(!nzchar(source$RegionCode)) ||
      any(is.na(regional_total$RegionCode)) ||
      any(!nzchar(regional_total$RegionCode)) ||
      any(is.na(pasture_allocation$RegionCode)) ||
      any(!nzchar(pasture_allocation$RegionCode))) {
    stop("RegionCode must be non-missing and non-empty.")
  }
  if (any(!is.finite(regional_total$YEMF)) ||
      any(!is.finite(pasture_allocation$YEMF))) {
    stop("YEMF must be a finite integer year.")
  }
  if (any(is.na(regional_total$SCENARIO)) ||
      any(!nzchar(regional_total$SCENARIO))) {
    stop("SCENARIO must be non-missing and non-empty.")
  }
  if (any(!is.finite(source$tN)) || any(source$tN < 0) ||
      any(!is.finite(regional_total$tN)) || any(regional_total$tN < 0) ||
      any(!is.finite(pasture_allocation$LURATIO)) ||
      any(pasture_allocation$LURATIO < 0)) {
    stop("tN and LURATIO values must be finite and nonnegative.")
  }
  if (any(is.na(pasture_allocation$G)) ||
      any(!nzchar(pasture_allocation$G))) {
    stop("Pasture allocation G values must be non-missing and non-empty.")
  }

  if (anyDuplicated(source[c("RegionCode", "G")])) {
    stop("source must be unique by RegionCode/G.")
  }
  if (anyDuplicated(
    regional_total[c("RegionCode", "YEMF", "SCENARIO")]
  )) {
    stop("regional_total must be unique by RegionCode/YEMF/SCENARIO.")
  }
  if (anyDuplicated(
    pasture_allocation[c("RegionCode", "YEMF", "G")]
  )) {
    stop("pasture_allocation must be unique by RegionCode/YEMF/G.")
  }

  scenario_years <- unique(regional_total[c("YEMF", "SCENARIO")])
  if (nrow(scenario_years) != 1L) {
    stop(
      "regional_total must contain exactly one YEMF/SCENARIO combination; ",
      "call this function once per model run."
    )
  }

  unexpected_source_regions <- source |>
    dplyr::distinct(RegionCode) |>
    dplyr::anti_join(
      regional_total |> dplyr::distinct(RegionCode),
      by = "RegionCode"
    )
  if (nrow(unexpected_source_regions) > 0L) {
    stop("source contains RegionCode values absent from regional_total.")
  }

  target_pasture <- pasture_allocation |>
    dplyr::semi_join(
      regional_total |> dplyr::distinct(RegionCode, YEMF),
      by = c("RegionCode", "YEMF")
    ) |>
    dplyr::filter(LURATIO > 0)

  pasture_check <- regional_total |>
    dplyr::select(RegionCode, YEMF) |>
    dplyr::distinct() |>
    dplyr::left_join(
      target_pasture |>
        dplyr::group_by(RegionCode, YEMF) |>
        dplyr::summarise(
          pasture_cells = dplyr::n(),
          ratio_sum = sum(LURATIO),
          .groups = "drop"
        ),
      by = c("RegionCode", "YEMF")
    )
  if (any(is.na(pasture_check$pasture_cells)) ||
      any(pasture_check$pasture_cells < 1L)) {
    stop("Every regional total must have at least one positive-LURATIO pasture grid.")
  }
  ratio_scale <- pmax(1, abs(pasture_check$ratio_sum))
  if (any(abs(pasture_check$ratio_sum - 1) > tolerance * ratio_scale)) {
    stop("Pasture LURATIO must sum to one within each RegionCode/YEMF.")
  }
  target_pasture <- target_pasture |>
    dplyr::group_by(RegionCode, YEMF) |>
    dplyr::mutate(allocation_ratio = LURATIO / sum(LURATIO)) |>
    dplyr::ungroup()

  source_totals <- source |>
    dplyr::group_by(RegionCode) |>
    dplyr::summarise(source_total_tN = sum(tN), .groups = "drop")
  regional_budget <- regional_total |>
    dplyr::left_join(source_totals, by = "RegionCode") |>
    dplyr::mutate(source_total_tN = dplyr::coalesce(source_total_tN, 0))
  budget_scale <- pmax(1, abs(regional_budget$tN))
  if (any(
    regional_budget$source_total_tN - regional_budget$tN >
      tolerance * budget_scale
  )) {
    stop("source tN exceeds regional_total tN for at least one region.")
  }

  pasture_grids <- target_pasture |>
    dplyr::select(RegionCode, G) |>
    dplyr::distinct() |>
    dplyr::mutate(has_pasture = TRUE)
  retained_source <- source |>
    dplyr::left_join(pasture_grids, by = c("RegionCode", "G")) |>
    dplyr::filter(!is.na(G), dplyr::coalesce(has_pasture, FALSE)) |>
    dplyr::select(RegionCode, G, source_tN = tN)
  retained_totals <- retained_source |>
    dplyr::group_by(RegionCode) |>
    dplyr::summarise(retained_source_tN = sum(source_tN), .groups = "drop")

  regional_budget <- regional_budget |>
    dplyr::left_join(retained_totals, by = "RegionCode") |>
    dplyr::mutate(
      retained_source_tN = dplyr::coalesce(retained_source_tN, 0),
      redistributed_pool_tN = pmax(tN - retained_source_tN, 0)
    )

  allocation <- target_pasture |>
    dplyr::inner_join(
      regional_budget |>
        dplyr::select(
          RegionCode, YEMF, SCENARIO,
          regional_total_tN = tN,
          redistributed_pool_tN
        ),
      by = c("RegionCode", "YEMF")
    ) |>
    dplyr::left_join(retained_source, by = c("RegionCode", "G")) |>
    dplyr::mutate(
      source_tN = dplyr::coalesce(source_tN, 0),
      redistributed_tN = redistributed_pool_tN * allocation_ratio,
      tN = source_tN + redistributed_tN
    ) |>
    dplyr::group_by(RegionCode, YEMF, SCENARIO) |>
    dplyr::mutate(
      mass_residual_tN = dplyr::first(regional_total_tN) - sum(tN),
      # A regional source total can exceed its independently calculated target
      # by a very small amount that is still inside the relative tolerance
      # checked above. When the redistribution pool is zero, assigning the
      # whole negative round-off residual to redistributed_tN would create a
      # physically meaningless negative component. Apply a negative residual
      # to redistribution only up to the amount available there, then absorb
      # any remainder in the largest allocated row's retained source.
      correction_row = dplyr::row_number() == which.max(tN),
      redistributed_correction_tN = dplyr::if_else(
        correction_row,
        pmax(mass_residual_tN, -redistributed_tN),
        0
      ),
      source_correction_tN = dplyr::if_else(
        correction_row,
        mass_residual_tN - redistributed_correction_tN,
        0
      ),
      redistributed_tN = redistributed_tN + redistributed_correction_tN,
      source_tN = source_tN + source_correction_tN,
      tN = source_tN + redistributed_tN
    ) |>
    dplyr::ungroup()

  allocated_totals <- allocation |>
    dplyr::group_by(RegionCode, YEMF, SCENARIO) |>
    dplyr::summarise(allocated_tN = sum(tN), .groups = "drop") |>
    dplyr::left_join(
      regional_total |>
        dplyr::rename(expected_tN = tN),
      by = c("RegionCode", "YEMF", "SCENARIO")
    )
  mass_scale <- pmax(1, abs(allocated_totals$expected_tN))
  if (any(
    abs(allocated_totals$allocated_tN - allocated_totals$expected_tN) >
      tolerance * mass_scale
  )) {
    stop("Direct-excretion allocation failed the regional mass-balance check.")
  }
  negative_rows <- allocation |>
    dplyr::filter(
      source_tN < -tolerance |
        redistributed_tN < -tolerance |
        tN < -tolerance
    )
  if (nrow(negative_rows) > 0L) {
    negative_row <- negative_rows |>
      dplyr::mutate(min_component_tN = pmin(source_tN, redistributed_tN, tN)) |>
      dplyr::arrange(min_component_tN) |>
      dplyr::slice(1L)
    stop(
      "Direct-excretion allocation produced a negative value: ",
      "RegionCode=", negative_row$RegionCode,
      ", YEMF=", negative_row$YEMF,
      ", G=", negative_row$G,
      ", source_tN=", format(negative_row$source_tN, digits = 16),
      ", redistributed_tN=",
      format(negative_row$redistributed_tN, digits = 16),
      ", tN=", format(negative_row$tN, digits = 16),
      "."
    )
  }

  allocation |>
    dplyr::select(
      RegionCode, YEMF, SCENARIO, G,
      source_tN, redistributed_tN, tN
    ) |>
    dplyr::arrange(RegionCode, YEMF, SCENARIO, G)
}
