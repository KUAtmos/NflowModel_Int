# Shared, base-R export for both the GMD and scenario-matrix pipelines.
# Values are nitrogen mass per grid per year, not mass of the gas molecule.
nflow_emission_sources <- function() {
  data.frame(
    object = c("OUTgasdis", "OUTgascropdis", "OUTgaspasdis",
               "OUTgasdis_N2", "OUTgasdis_NH3", "OUTgasdis_NOx",
               "NH3_loss1dis", "NH3_loss2dis",
               "OUTgascropdis_N2", "OUTgaspasdis_N2",
               "OUTgascropdis_NH3", "OUTgaspasdis_NH3",
               "OUTgascropdis_NOx", "OUTgaspasdis_NOx"),
    value = c("OUTgasgrid", "OUTgascropgrid", "OUTgaspasgrid",
              "OUTgasgrid", "OUTgasgrid", "OUTgasgrid",
              "NH3_MMS_TgN_yr", "NH3_direct_TgN_yr",
              rep(c("OUTgascropgrid", "OUTgaspasgrid"), 3L)),
    column = c("soil_gas_total", "soil_gas_cropland", "soil_gas_pasture",
               "N2_total", "NH3_soil_total", "NOx_N2O_pool_total",
               "NH3_MMS", "NH3_direct_excretion",
               "N2_cropland", "N2_pasture",
               "NH3_soil_cropland", "NH3_soil_pasture",
               "NOx_N2O_pool_cropland", "NOx_N2O_pool_pasture"),
    stringsAsFactors = FALSE
  )
}

nflow_emission_component <- function(env, object, value, year, raw_scenario) {
  if (!exists(object, envir = env, inherits = FALSE)) {
    stop("Grid-emission export requires model object: ", object,
         ". Recalculate the model if the saved result predates the land-source export.")
  }
  x <- get(object, envir = env, inherits = FALSE)
  if (!is.data.frame(x) || !all(c("G", "YEMF", value) %in% names(x))) {
    stop(object, " must contain G, YEMF and ", value, ".")
  }
  selected <- !is.na(x$YEMF) & as.character(x$YEMF) == as.character(year)
  if ("SCENARIO" %in% names(x)) {
    selected <- selected & !is.na(x$SCENARIO) &
      as.character(x$SCENARIO) == raw_scenario
  }
  x <- x[selected, , drop = FALSE]
  if (!nrow(x)) stop(object, " has no rows for ", raw_scenario, " / ", year)
  out <- data.frame(G = as.character(x$G),
                    value = suppressWarnings(as.numeric(as.character(x[[value]]))),
                    stringsAsFactors = FALSE)
  if (anyNA(out$G) || any(!nzchar(out$G)) || any(!is.finite(out$value))) {
    stop(object, " contains invalid grid identifiers or emission values.")
  }
  # Retain signed soil-model values; clipping changes the model's budget.
  if (object %in% c("NH3_loss1dis", "NH3_loss2dis") && any(out$value < 0)) {
    stop(object, " contains negative livestock volatilisation values.")
  }
  stats::aggregate(value ~ G, out, sum)
}

nflow_assert_emission_equal <- function(actual, expected, label) {
  if (any(!is.finite(actual)) || any(!is.finite(expected)) ||
      any(abs(actual - expected) > 1e-8 * pmax(1, abs(expected)))) {
    stop("Grid-emission mass check failed: ", label)
  }
}

nflow_grid_emission_columns <- function() {
  c("G", "year", "raw_scenario", "unit",
    "N2_cropland_TgN_yr", "N2_pasture_TgN_yr", "N2_total_TgN_yr",
    "NOx_cropland_TgN_yr", "NOx_pasture_TgN_yr", "NOx_total_TgN_yr",
    "N2O_cropland_TgN_yr", "N2O_pasture_TgN_yr", "N2O_total_TgN_yr",
    "NH3_soil_cropland_TgN_yr", "NH3_soil_pasture_TgN_yr",
    "NH3_others_TgN_yr", "NH3_total_TgN_yr")
}

nflow_build_grid_emissions <- function(env, year, raw_scenario, scenario_id,
                                       scenario_label = scenario_id) {
  if (length(year) != 1L || is.na(suppressWarnings(as.integer(year))) ||
      length(raw_scenario) != 1L || is.na(raw_scenario) || !nzchar(raw_scenario) ||
      length(scenario_id) != 1L || is.na(scenario_id) || !nzchar(scenario_id)) {
    stop("Export requires one year, raw scenario and scenario ID.")
  }
  sources <- nflow_emission_sources()
  parts <- lapply(seq_len(nrow(sources)), function(index) {
    nflow_emission_component(env, sources$object[index], sources$value[index], year, raw_scenario)
  })
  grids <- unique(unlist(lapply(parts, function(x) x$G), use.names = FALSE))
  grids <- grids[order(suppressWarnings(as.numeric(grids)), grids, na.last = TRUE)]
  components <- setNames(lapply(seq_along(parts), function(index) {
    component <- parts[[index]]
    values <- component$value[match(grids, component$G)]
    values[is.na(values)] <- 0
    nflow_assert_emission_equal(sum(values), sum(component$value), sources$object[index])
    values
  }), sources$column)
  # Use the model's chemical-species land arrays directly, including policies.
  for (species in c("N2", "NH3_soil", "NOx_N2O_pool")) {
    nflow_assert_emission_equal(
      components[[paste0(species, "_cropland")]] + components[[paste0(species, "_pasture")]],
      components[[paste0(species, "_total")]], paste(species, "land-source sum")
    )
  }
  for (land in c("total", "cropland", "pasture")) {
    nflow_assert_emission_equal(
      components[[paste0("soil_gas_", land)]],
      components[[paste0("N2_", land)]] + components[[paste0("NH3_soil_", land)]] +
        components[[paste0("NOx_N2O_pool_", land)]], paste(land, "soil species sum")
    )
  }
  out <- data.frame(G = grids, year = as.integer(year), raw_scenario = raw_scenario,
                    unit = "Tg N grid^-1 yr^-1", stringsAsFactors = FALSE)
  for (species in c("N2", "NOx", "N2O")) {
    for (land in c("cropland", "pasture", "total")) {
      values <- if (species == "N2") components[[paste0("N2_", land)]] else
        components[[paste0("NOx_N2O_pool_", land)]] * if (species == "NOx") 5 / 14 else 9 / 14
      out[[paste0(species, "_", land, "_TgN_yr")]] <- values
    }
  }
  out$NH3_soil_cropland_TgN_yr <- components$NH3_soil_cropland
  out$NH3_soil_pasture_TgN_yr <- components$NH3_soil_pasture
  out$NH3_others_TgN_yr <- components$NH3_MMS + components$NH3_direct_excretion
  out$NH3_total_TgN_yr <- components$NH3_soil_total + out$NH3_others_TgN_yr
  nflow_assert_emission_equal(out$NH3_total_TgN_yr,
    out$NH3_soil_cropland_TgN_yr + out$NH3_soil_pasture_TgN_yr + out$NH3_others_TgN_yr,
    "NH3 land-source and others sum")
  for (spec in list(c("NH3_loss1", "ManureStable", "NH3_MMS"),
                    c("NH3_loss2", "ManurePas", "NH3_direct_excretion"))) {
    if (exists(spec[1L], envir = env, inherits = FALSE)) {
      scalar <- get(spec[1L], envir = env, inherits = FALSE)
      nflow_assert_emission_equal(sum(components[[spec[3L]]]), sum(scalar[[spec[2L]]]), spec[1L])
    }
  }
  for (spec in list(c("OUTgasdis_NO_agriculture", "NOx_total_TgN_yr"),
                    c("OUTgasdis_NH3_agriculture", "NH3_total_TgN_yr"))) {
    if (exists(spec[1L], envir = env, inherits = FALSE)) {
      component <- nflow_emission_component(env, spec[1L], "OUTgasgrid", year, raw_scenario)
      values <- component$value[match(grids, component$G)]
      values[is.na(values)] <- 0
      nflow_assert_emission_equal(out[[spec[2L]]], values, spec[1L])
    }
  }
  out[nflow_grid_emission_columns()]
}

nflow_write_export_atomic <- function(path, writer) {
  # Mapped Windows shares may deny traversal at the drive root.
  create_directory <- function(directory) {
    if (dir.exists(directory)) return(invisible(NULL))
    parent <- dirname(directory)
    if (identical(directory, parent)) stop("Cannot create output directory: ", directory)
    create_directory(parent)
    if (!dir.create(directory, showWarnings = FALSE) && !dir.exists(directory)) {
      stop("Cannot create output directory: ", directory)
    }
  }
  create_directory(dirname(path))
  temporary <- tempfile(".grid-emissions-", tmpdir = dirname(path))
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  writer(temporary)
  if (!file.rename(temporary, path)) {
    if (!file.copy(temporary, path, overwrite = TRUE)) stop("Could not write: ", path)
    unlink(temporary)
  }
  invisible(path)
}

nflow_export_grid_emissions <- function(env, year, raw_scenario, scenario_id,
                                        output_dir, scenario_label = scenario_id,
                                        save_source = FALSE) {
  table <- nflow_build_grid_emissions(env, year, raw_scenario, scenario_id, scenario_label)
  descriptions <- c(
    "One-based cell ID on the 720 x 360 model grid; only modeled source cells are listed",
    "Model year", "Raw input scenario; policy scenario is identified by the containing directory",
    "Nitrogen mass per grid per year; not gas molecular mass",
    "OUTgascropdis_N2$OUTgascropgrid", "OUTgaspasdis_N2$OUTgaspasgrid", "OUTgasdis_N2$OUTgasgrid",
    "5/14 of OUTgascropdis_NOx$OUTgascropgrid", "5/14 of OUTgaspasdis_NOx$OUTgaspasgrid",
    "5/14 of OUTgasdis_NOx$OUTgasgrid",
    "9/14 of OUTgascropdis_NOx$OUTgascropgrid", "9/14 of OUTgaspasdis_NOx$OUTgaspasgrid",
    "9/14 of OUTgasdis_NOx$OUTgasgrid",
    "OUTgascropdis_NH3$OUTgascropgrid; soil NH3 from cropland",
    "OUTgaspasdis_NH3$OUTgaspasgrid; soil NH3 from pasture",
    "NH3_loss1dis$NH3_MMS_TgN_yr + NH3_loss2dis$NH3_direct_TgN_yr; manure management plus direct excretion",
    "OUTgasdis_NH3$OUTgasgrid + NH3_others_TgN_yr; all agricultural NH3 sources"
  )
  definitions <- data.frame(column = names(table), description = descriptions,
    unit = c("index", "year", "text", "text", rep("Tg N grid^-1 yr^-1", 13L)),
    stringsAsFactors = FALSE)
  nflow_write_export_atomic(file.path(output_dir, "grid_emissions_columns.csv"), function(path) {
    utils::write.csv(definitions, path, row.names = FALSE)
  })
  csv_path <- file.path(output_dir, paste0("grid_emissions_", year, ".csv"))
  nflow_write_export_atomic(csv_path, function(path) {
    utils::write.csv(table, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  })
  columns <- grep("_TgN_yr$", names(table), value = TRUE)
  # The diagnostic file keeps the same schema, without adding a flag column.
  negative <- rowSums(table[columns] < 0) > 0L
  nflow_write_export_atomic(
    file.path(output_dir, paste0("grid_emissions_negative_", year, ".csv")),
    function(path) utils::write.csv(table[negative, ], path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  )
  totals <- data.frame(raw_scenario = raw_scenario, year = as.integer(year), column = columns,
                       total = vapply(table[columns], sum, numeric(1)),
                       unit = "Tg N yr^-1", row.names = NULL)
  nflow_write_export_atomic(
    file.path(output_dir, paste0("grid_emissions_totals_", year, ".csv")),
    function(path) utils::write.csv(totals, path, row.names = FALSE)
  )
  if (save_source) {
    cache <- new.env(parent = emptyenv())
    sources <- nflow_emission_sources()
    for (index in seq_len(nrow(sources))) {
      component <- nflow_emission_component(env, sources$object[index], sources$value[index], year, raw_scenario)
      names(component)[2L] <- sources$value[index]
      component$YEMF <- as.integer(year)
      component$SCENARIO <- raw_scenario
      assign(sources$object[index], component, envir = cache)
    }
    cache$grid_emissions_metadata <- list(schema_version = 2L, year = as.integer(year),
      raw_scenario = raw_scenario, scenario_id = scenario_id, scenario_label = scenario_label)
    nflow_write_export_atomic(
      file.path(output_dir, paste0("grid_emissions_source_", year, ".rdat")),
      function(path) save(list = ls(cache), envir = cache, file = path)
    )
  }
  message("Grid emissions CSV: ", csv_path, " (", nrow(table), " grids)")
  invisible(totals)
}
