script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- sub("^--file=", "", script_arg[[1]])
test_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
scenario_dir <- normalizePath(file.path(test_dir, ".."), winslash = "/")
visitorn_dir <- normalizePath(file.path(scenario_dir, ".."), winslash = "/")
runner <- file.path(scenario_dir, "run_visitorn_scenarios.R")
functions_file <- file.path(
  scenario_dir,
  "R",
  "visitorn_scenario_functions.R"
)
functions_source <- paste(readLines(functions_file, warn = FALSE), collapse = "\n")
stopifnot(
  grepl("plot_text_scale <- 2", functions_source, fixed = TRUE),
  grepl("n_load_tg <- totals$n_load_gg / 1000", functions_source, fixed = TRUE),
  grepl("n_load_tg <- top$n_load_gg / 1000", functions_source, fixed = TRUE),
  !grepl("N load (Gg N yr-1)", functions_source, fixed = TRUE)
)
source(functions_file, local = .GlobalEnv)

# Comparison styles must not change when AP is omitted or scenarios are
# reordered. NUE counterparts share a policy color, not a line/marker style.
style_manifest <- data.frame(
  scenario_name = c(
    "baseline", "500c", "demfwr", "nue", "air_pollution",
    "500c_demfwr", "500c_nue", "demfwr_nue", "nue_air_pollution",
    "500c_demfwr_nue"
  ),
  scenario_label = c(
    "REF", "CM", "DFW", "NUE", "AP", "CM + DFW", "CM + NUE",
    "DFW + NUE", "NUE + AP", "CM + DFW + NUE"
  ),
  stringsAsFactors = FALSE
)
styles_ten <- river_scenario_styles(style_manifest)
style_columns <- c("colour", "linetype", "shape")
for (selection in list(
  c(1:4, 6:8, 10),
  c(10, 3, 1, 8),
  rev(seq_len(nrow(style_manifest)))
)) {
  selected_styles <- river_scenario_styles(style_manifest[selection, ])
  expected_styles <- styles_ten[selection, ]
  rownames(expected_styles) <- NULL
  stopifnot(identical(selected_styles, expected_styles))
}
stopifnot(
  nrow(unique(styles_ten[style_columns])) == 10L,
  nrow(unique(styles_ten[c(1:4, 6:8, 10), style_columns])) == 8L,
  identical(styles_ten$colour[c(1, 2, 3, 6, 5)],
            c("#333333", "#D55E00", "#0072B2", "#009E73", "#CC79A7"))
)
for (pair in list(c(1, 4), c(2, 7), c(3, 8), c(6, 10), c(5, 9))) {
  stopifnot(
    styles_ten$colour[pair[1]] == styles_ten$colour[pair[2]],
    identical(styles_ten$linetype[pair], c("solid", "longdash")),
    identical(styles_ten$shape[pair], c(16L, 17L))
  )
}
alias_manifest <- style_manifest[c(3, 6, 8, 10), ]
alias_manifest$scenario_name <- gsub(
  "demfwr", "eatlancet", alias_manifest$scenario_name, fixed = TRUE
)
alias_styles <- river_scenario_styles(alias_manifest)
stopifnot(identical(
  unname(as.matrix(alias_styles[style_columns])),
  unname(as.matrix(styles_ten[c(3, 6, 8, 10), style_columns]))
))

# Exercise scale training and guide merging without writing any image.
style_plot_data <- expand.grid(
  year = c(2030L, 2100L),
  scenario_label = style_manifest$scenario_label,
  stringsAsFactors = FALSE
)
style_plot_data$scenario_label <- factor(
  style_plot_data$scenario_label, levels = style_manifest$scenario_label
)
style_plot_data$value <- seq_len(nrow(style_plot_data))
style_plot <- ggplot2::ggplot(
  style_plot_data,
  ggplot2::aes(
    year, value, colour = scenario_label, linetype = scenario_label,
    shape = scenario_label, group = scenario_label
  )
) +
  ggplot2::geom_line(linewidth = 1.2) +
  ggplot2::geom_point(size = 3.8) +
  river_comparison_scales(styles_ten) +
  ggplot2::theme(legend.position = "bottom")
style_built <- ggplot2::ggplot_build(style_plot)
line_groups <- split(style_built$data[[1]], style_built$data[[1]]$group)
point_groups <- split(style_built$data[[2]], style_built$data[[2]]$group)
stopifnot(
  identical(unname(vapply(line_groups, function(x) x$colour[1], "")),
            styles_ten$colour),
  identical(unname(vapply(line_groups, function(x) x$linetype[1], "")),
            styles_ten$linetype),
  identical(unname(vapply(point_groups, function(x) as.integer(x$shape[1]), 0L)),
            styles_ten$shape),
  all(style_built$data[[1]]$linewidth == 1.2)
)
style_grob <- ggplot2::ggplotGrob(style_plot)
guide_boxes <- style_grob$grobs[grepl("^guide-box", style_grob$layout$name)]
guide_boxes <- Filter(function(x) inherits(x, "gtable"), guide_boxes)
stopifnot(
  length(guide_boxes) == 1L,
  sum(guide_boxes[[1]]$layout$name == "guides") == 1L
)

fixture_root <- tempfile("visitorn_scenario_fixture_")
nflow_root <- file.path(fixture_root, "nflow")
fixture_visitorn <- file.path(fixture_root, "VISIToRN")
output_root <- file.path(fixture_root, "output", "scenarios")
comparison_root <- file.path(fixture_root, "output", "comparisons")
dir.create(nflow_root, recursive = TRUE)
dir.create(file.path(fixture_visitorn, "Data_pd"), recursive = TRUE)
on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

write_scenario <- function(name, raw_scenario, scenario_500c, scenario_demfwr,
                           scenario_nue, scenario_air_pollution, scale) {
  scenario_path <- file.path(nflow_root, name)
  dir.create(scenario_path)
  manifest <- data.frame(
    scenario_name = name,
    scenario_label = if (name == "demfwr") "DEMFWR" else "Baseline",
    raw_scenario = raw_scenario,
    scenario_500c = scenario_500c,
    scenario_demfwr = scenario_demfwr,
    scenario_nue = scenario_nue,
    scenario_air_pollution = scenario_air_pollution,
    years = "2030",
    aimhub_input = "",
    created_at = "2026-01-01T00:00:00+0000",
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    manifest,
    file.path(scenario_path, "manifest.csv"),
    row.names = FALSE
  )

  OUTleadis <- data.frame(
    G = c("1", "1", "722"),
    YEMF = c("2030", "2030", "2030"),
    OUTleagrid = c(0.001, 0.002, -0.5) * scale,
    stringsAsFactors = FALSE
  )
  save(OUTleadis, file = file.path(scenario_path, "OUTleadis2030.rdat"))
  audit <- data.frame(
    G = c("1", "1", "722"),
    longitude = c(NA, NA, -179.25),
    latitude = c(NA, NA, 89.25),
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    audit,
    file.path(scenario_path, "grid_audit_2030.csv"),
    row.names = FALSE
  )
}

write_scenario(
  "baseline",
  "SSP2_BaU_NoCC_No",
  FALSE,
  FALSE,
  FALSE,
  FALSE,
  1
)
write_scenario(
  "demfwr",
  "SSP2_BaU_DEMFWR_No",
  FALSE,
  TRUE,
  FALSE,
  FALSE,
  0.8
)

write_natural_input <- function(land_scenario, value) {
  VISITnl_nat <- array(0, dim = c(720, 360, 12))
  VISITnl_nat[1, 1, ] <- value
  VISITnl_nat[2, 2, ] <- 2e6
  path <- natural_array_path(
    fixture_visitorn,
    2030,
    land_scenario,
    TRUE
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  save(VISITnl_nat, file = path, compress = TRUE)
  path
}

baseline_natural_path <- write_natural_input("SSP2_BaU_NoCC_No", 10)
eat_natural_path <- write_natural_input("SSP2_BaU_DEMFWR_No", 20)
stopifnot(
  baseline_natural_path != eat_natural_path,
  normalize_natural_land_scenario("nue") == "SSP2_BaU_NoCC_No",
  normalize_natural_land_scenario("demfwr_nue") ==
    "SSP2_BaU_DEMFWR_No",
  normalize_natural_land_scenario("500c_demfwr_nue") ==
    "SSP2_500C_CACN_DEMFWR_No",
  normalize_natural_land_scenario("air_pollution") ==
    "SSP2_BaU_NoCC_No"
)

rscript <- file.path(
  R.home("bin"),
  if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
)
command_output <- system2(
  rscript,
  args = c(
    shQuote(runner),
    "--scenario-root", shQuote(nflow_root),
    "--output-root", shQuote(output_root),
    "--comparison-root", shQuote(comparison_root),
    "--visitorn-dir", shQuote(fixture_visitorn),
    "--scenarios", "baseline,demfwr",
    "--years", "2030",
    "--nmip3", "on",
    "--prepare-only", "on",
    "--resume", "off"
  ),
  stdout = TRUE,
  stderr = TRUE
)
status <- attr(command_output, "status")
if (is.null(status)) {
  status <- 0L
}
if (status != 0L) {
  cat(paste(command_output, collapse = "\n"), "\n")
  stop("VISIToRN prepare-only command failed with status ", status, ".")
}

expected <- c(
  file.path(
    output_root,
    "baseline",
    "inputs",
    "VISITnl_baseline_2030.rdat"
  ),
  file.path(
    output_root,
    "baseline",
    "tables",
    "input_summary_all_years.csv"
  ),
  file.path(output_root, "baseline", "manifest.csv"),
  file.path(
    output_root,
    "demfwr",
    "inputs",
    "VISITnl_demfwr_2030.rdat"
  )
)
missing <- expected[!file.exists(expected)]
if (length(missing) > 0L) {
  stop("Prepare-only test did not create: ", paste(missing, collapse = ", "))
}

prepared <- new.env()
load(expected[[1]], envir = prepared)
stopifnot(
  identical(dim(prepared$VISITnl_input), c(720L, 360L, 12L)),
  prepared$input_metadata$scenario_name[[1]] == "baseline",
  prepared$input_metadata$scenario_label[[1]] == "REF",
  prepared$input_metadata$natural_land_scenario[[1]] ==
    "SSP2_BaU_NoCC_No"
)
expected_daily <- (0.001 + 0.002) * 1e9 / 365.25
stopifnot(all(
  abs(prepared$VISITnl_input[1, 1, ] - (10 + expected_daily)) < 1e-6
))
expected_signed_combination <- 2e6 - 0.5 * 1e9 / 365.25
stopifnot(all(
  abs(
    prepared$VISITnl_input[2, 2, ] - expected_signed_combination
  ) < 1e-6
))
prepared_summary <- readr::read_csv(
  file.path(
    output_root,
    "baseline",
    "tables",
    "input_summary_2030.csv"
  ),
  show_col_types = FALSE
)
stopifnot(
  prepared_summary$negative_nflow_cells[[1]] == 1L,
  prepared_summary$negative_combined_cells_clamped[[1]] == 0L
)

prepared_eat <- new.env()
load(expected[[4]], envir = prepared_eat)
expected_daily_eat <- expected_daily * 0.8
stopifnot(
  prepared_eat$input_metadata$natural_land_scenario[[1]] ==
    "SSP2_BaU_DEMFWR_No",
  all(
    abs(
      prepared_eat$VISITnl_input[1, 1, ] -
        (20 + expected_daily_eat)
    ) < 1e-6
  )
)

plot_dir <- file.path(fixture_root, "plot_test")
river <- expand.grid(
  rivnum = 1:5,
  year = c(2030L, 2050L),
  scenario_name = c("baseline", "demfwr"),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
river$river_basin_name <- ifelse(
  river$rivnum <= 3,
  paste0("River ", river$rivnum),
  "Unknown-------------------------"
)
river$scenario_label <- river$scenario_name
river$n_load_kg <- (
  1e6 * river$rivnum *
    ifelse(river$scenario_name == "baseline", 1, 0.8)
)
river$n_load_gg <- river$n_load_kg / 1e6

save_scenario_river_plots(
  river[river$scenario_name == "baseline", ],
  "baseline",
  file.path(plot_dir, "scenario")
)
comparison_manifest <- data.frame(
  scenario_name = c("baseline", "demfwr"),
  scenario_label = c("baseline", "DEMFWR"),
  stringsAsFactors = FALSE
)
df_river_all <- data.frame(
  rivnum = factor(1:5),
  river_basin_name = c(
    "River 1", "River 2", "River 3",
    "Unknown-------------------------",
    "Unknown-------------------------"
  ),
  Nload = c(30, 20, 10, 5, 1),
  year = 2010L,
  stringsAsFactors = FALSE
)
reference_file <- file.path(
  fixture_visitorn,
  "Data_pd",
  "df_river_all_NMIP3.rdat"
)
save(df_river_all, file = reference_file)
reference_river <- load_reference_river_year(reference_file, 2010L)
stopifnot(
  nrow(reference_river) == 5L,
  all(reference_river$year == 2010L),
  all(reference_river$n_load_kg == reference_river$n_load_gg * 1e6)
)
reference_csv <- file.path(fixture_visitorn, "reference_2010.csv")
readr::write_csv(reference_river, reference_csv)
reference_river_csv <- load_reference_river_year(reference_csv, 2010L)
stopifnot(
  identical(reference_river_csv$rivnum, reference_river$rivnum),
  all(reference_river_csv$n_load_gg == reference_river$n_load_gg)
)
save_comparison_outputs(
  river,
  comparison_manifest,
  file.path(plot_dir, "comparison"),
  reference_scenario = "baseline",
  reference_river = reference_river,
  reference_year = 2010L
)

plot_expected <- c(
  file.path(plot_dir, "scenario", "total_river_load.png"),
  file.path(plot_dir, "scenario", "river_load_top20.png"),
  file.path(
    plot_dir,
    "comparison",
    "figures",
    "total_river_load_comparison.png"
  ),
  file.path(
    plot_dir,
    "comparison",
    "figures",
    "top14_river_load_comparison.png"
  ),
  file.path(
    plot_dir,
    "comparison",
    "tables",
    "river_basin_ranking.csv"
  ),
  file.path(
    plot_dir,
    "comparison",
    "tables",
    "river_load_change_from_reference.csv"
  )
)
missing_plots <- plot_expected[!file.exists(plot_expected)]
if (length(missing_plots) > 0L) {
  stop(
    "River plotting test did not create: ",
    paste(missing_plots, collapse = ", ")
  )
}

changes <- readr::read_csv(
  utils::tail(plot_expected, 1),
  show_col_types = FALSE
)
eat_changes <- changes[changes$scenario_name == "demfwr", ]
stopifnot(
  all(abs(
    eat_changes$ratio_to_reference[eat_changes$year > 2010] - 0.8
  ) < 1e-12),
  all(eat_changes$ratio_to_reference[eat_changes$year == 2010] == 1)
)

comparison_river <- readr::read_csv(
  file.path(
    plot_dir,
    "comparison",
    "tables",
    "river_load_comparison.csv"
  ),
  show_col_types = FALSE
)
stopifnot(
  all(c(2010L, 2030L, 2050L) %in% comparison_river$year),
  nrow(comparison_river[comparison_river$year == 2010L, ]) == 10L,
  all(comparison_river$is_common_reference_year[
    comparison_river$year == 2010L
  ])
)

ranking <- readr::read_csv(
  plot_expected[[length(plot_expected) - 1L]],
  show_col_types = FALSE
)
stopifnot(
  nrow(ranking) == 5L,
  !anyDuplicated(ranking$rivnum),
  all(ranking$selected_for_figure[ranking$rivnum <= 3]),
  !any(ranking$selected_for_figure[ranking$rivnum > 3]),
  all(!ranking$river_name_available[ranking$rivnum > 3]),
  length(unique(ranking$river_plot_label[ranking$rivnum > 3])) == 2L
)

selection_check <- select_top_river_records(
  river,
  2L,
  ranking_source = reference_river
)
stopifnot(setequal(
  unique(selection_check$records$rivnum),
  c(1L, 2L)
))

# Comparison-only mode must consume completed river CSVs without requiring
# VISIToRN static inputs or rerunning river routing.
for (scenario in c("baseline", "demfwr")) {
  existing_table_dir <- file.path(output_root, scenario, "tables")
  dir.create(existing_table_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(
    river[river$scenario_name == scenario, ],
    file.path(existing_table_dir, "river_load_all_years.csv")
  )
}
# The default comparison reference is the freshly routed *_No Baseline 2010
# table, not the legacy non-*_No GMD RData file.
readr::write_csv(
  reference_river,
  file.path(
    output_root,
    "baseline",
    "tables",
    "river_load_2010.csv"
  )
)
comparison_only_root <- file.path(
  fixture_root,
  "output",
  "comparison_only"
)
comparison_only_output <- system2(
  rscript,
  args = c(
    shQuote(runner),
    "--scenario-root", shQuote(nflow_root),
    "--output-root", shQuote(output_root),
    "--comparison-root", shQuote(comparison_only_root),
    "--visitorn-dir", shQuote(fixture_visitorn),
    "--scenarios", "baseline,demfwr",
    "--years", "2030",
    "--nmip3", "on",
    "--prepare-only", "off",
    "--comparison-only", "on",
    "--resume", "on"
  ),
  stdout = TRUE,
  stderr = TRUE
)
comparison_only_status <- attr(comparison_only_output, "status")
if (is.null(comparison_only_status)) {
  comparison_only_status <- 0L
}
if (comparison_only_status != 0L) {
  cat(paste(comparison_only_output, collapse = "\n"), "\n")
  stop(
    "VISIToRN comparison-only command failed with status ",
    comparison_only_status,
    "."
  )
}
comparison_only_table <- readr::read_csv(
  file.path(
    comparison_only_root,
    "baseline__demfwr",
    "tables",
    "river_load_comparison.csv"
  ),
  show_col_types = FALSE
)
stopifnot(
  setequal(unique(comparison_only_table$year), c(2010L, 2030L)),
  nrow(comparison_only_table[
    comparison_only_table$year == 2010L,
  ]) == 10L
)

cat("VISIToRN scenario pipeline test passed.\n")
