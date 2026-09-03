# =========================================================
# Data
# =========================================================
load(file.path(data_dir, "LandMask_Loop.rdat"), envir = .GlobalEnv)
load(file.path(data_dir, "DistMat.rdat"), envir = .GlobalEnv)
load(file.path(data_dir, "VISITro_nat_ag.rdat"), envir = .GlobalEnv)
load(file.path(data_dir, "RivArea.rdat"), envir = .GlobalEnv)
load(file.path(data_dir, "HumanN_future.rdat"), envir = .GlobalEnv)
load(file.path(data_dir, "dat_tas.rdat"), envir = .GlobalEnv)
load(file.path(data_dir, "dat_dir.rdat"), envir = .GlobalEnv)

rivnum_values <- scan("rivnum05.asc", what = numeric(), quiet = TRUE)
rivnum_matrix <- matrix(rivnum_values, ncol = 720, nrow = 360, byrow = TRUE)

nc <- nc_open("trip_0.5x0.5.nc")
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
rpaths <- ncvar_get(nc, "rpaths")
nc_close(nc)

grid_df <- expand.grid(lon = lon, lat = lat) %>%
  mutate(rpaths = as.vector(rpaths)) %>%
  mutate(
    lon_idx = round((lon + 179.75) / 0.5) + 1,
    lat_idx = round((89.75 - lat) / 0.5) + 1
  ) %>%
  select(rpaths, lon_idx, lat_idx)

riv_table <- read.table("rivnum05.txt", header = TRUE, stringsAsFactors = FALSE) %>%
  select(rivnum, river_basin_name)

build_case_label <- function(year, is_mit) {
  if (is_mit) {
    paste0(year, "mit")
  } else {
    as.character(year)
  }
}

build_visitnl_name <- function(year, is_mit, nmip3 = FALSE) {
  suffix_mit <- if (is_mit) "mit" else ""
  suffix_nmip3 <- if (nmip3) "_NMIP3" else ""

  paste0("VISITnl_", year, suffix_mit, suffix_nmip3)
}

aggregate_river_mouth <- function(test_sim, rivnum_matrix, grid_df, riv_table) {
  df <- data.frame(
    lon_idx = rep(1:720, each = 360),
    lat_idx = rep(1:360, times = 720),
    Nout = as.vector(t(test_sim$QoutNint)),
    rivnum = as.vector(rivnum_matrix)
  )



  df <- df %>%
    left_join(grid_df, by = c("lon_idx", "lat_idx"))

  df_mouth <- df %>%
    filter(rivnum > 0, rpaths %in% c(9, 12))

  df_river_mouth <- df_mouth %>%
    group_by(rivnum) %>%
    summarise(Nload = sum(Nout, na.rm = TRUE), .groups = "drop")

  df_river_named_mouth <- left_join(df_river_mouth, riv_table, by = "rivnum")

  df_river_named_mouth
}

analyze_concentration <- function(test_sim,
                                  case_label,
                                  nmip3 = FALSE,
                                  out_dir = "output",
                                  thr = 2.5) {

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  suffix_save <- if (nmip3) "_NMIP3" else ""
  file_suffix <- paste0(case_label, suffix_save)

  # =========================================================
  # Extract VISIToRN outputs
  # =========================================================
  QoutNint <- test_sim$QoutNint   # cumulative N outflow: ∑ QoutN(t)
  Qout     <- test_sim$Qout       # water outflow [lon, lat, time]

  if (is.null(QoutNint)) {
    stop("test_sim$QoutNint が見つかりません。")
  }
  if (is.null(Qout)) {
    stop("test_sim$Qout が見つかりません。")
  }

  # =========================================================
  # Cumulative water outflow: ∑ Qout(t)
  # =========================================================
  Qout_int <- apply(Qout, c(1, 2), sum, na.rm = TRUE)

  # Safety check
  if (!all(dim(QoutNint) == dim(Qout_int))) {
    stop(
      "QoutNint と Qout_int の次元が一致しません: ",
      "dim(QoutNint) = ", paste(dim(QoutNint), collapse = " x "),
      ", dim(Qout_int) = ", paste(dim(Qout_int), collapse = " x ")
    )
  }

  # =========================================================
  # Freshwater concentration
  # =========================================================
  C_fw_ratio <- QoutNint / Qout_int
  C_fw_ratio[Qout_int <= 0] <- NA
  C_fw_ratio[!is.finite(C_fw_ratio)] <- NA

  # mg N/L
  # 以前のコードと同じ換算を維持
  C_fw_mgL <- C_fw_ratio * 1e6

  # =========================================================
  # Ratio to threshold
  # =========================================================
  R <- C_fw_mgL / thr
  R[C_fw_mgL <= 0] <- NA
  R[!is.finite(R)] <- NA

  breaks_ratio <- c(0, 0.3, 0.6, 1.0, 2, 3, Inf)

  classes_vec <- cut(
    as.vector(R),
    breaks = breaks_ratio,
    include.lowest = TRUE,
    labels = FALSE
  )

  Z <- matrix(
    classes_vec,
    nrow = nrow(C_fw_mgL),
    ncol = ncol(C_fw_mgL),
    byrow = FALSE
  )

  Z[is.na(R)] <- NA

  cols <- c(
    "#08306B", "#4292C6", "#C6DBEF",
    "#FEE0D2", "#FB6A4A", "#CB181D"
  )

  class_labels <- c(
    "<= 0.3",
    "0.3–0.6",
    "0.6–1.0",
    "1.0–2.0",
    "2.0–3.0",
    "> 3.0"
  )

  lon_seq <- seq(-179.75, 179.75, by = 0.5)
  lat_seq <- seq( -89.75,  89.75, by = 0.5)

  # =========================================================
  # 1) Concentration relative to threshold: 6 classes
  # =========================================================
  png(
    filename = file.path(out_dir, paste0("C_fw_ratio_6class_", file_suffix, ".png")),
    width = 1200,
    height = 800,
    res = 150
  )

  fields::image.plot(
    x = lon_seq,
    y = lat_seq,
    z = Z[, ncol(Z):1],
    breaks = seq(0.5, 6.5, by = 1),
    col = cols,
    na.col = "grey80",
    main = paste0(
      "N concentration relative to threshold (",
      thr,
      " mg N/L): ",
      case_label
    ),
    xlab = "lon",
    ylab = "lat",
    axis.args = list(
      at = 1:6,
      labels = class_labels
    )
  )

  maps::map("world", add = TRUE, col = "black", lwd = 1)

  dev.off()

  # =========================================================
  # 2) Cumulative N outflow map
  # =========================================================
  QoutNint_clipped <- pmin(pmax(QoutNint, 0), 5.0e+08)

  png(
    filename = file.path(out_dir, paste0("QoutNint_geos_Nbud_", file_suffix, ".png")),
    width = 1200,
    height = 800,
    res = 150
  )

  fields::image.plot(
    x = lon_seq,
    y = lat_seq,
    z = QoutNint_clipped[, ncol(QoutNint_clipped):1],
    zlim = c(0, 5.0e+08),
    col = fields::tim.colors(64),
    xlab = "lon",
    ylab = "lat",
    main = paste0("Cumulative N outflow: ", case_label)
  )

  maps::map("world", add = TRUE, col = "black", lwd = 1)

  dev.off()

  # =========================================================
  # 3) Log10 cumulative water outflow map
  # =========================================================
  Q_out <- Qout_int
  Q_out[Q_out <= 0] <- NA

  Q_log <- log10(Q_out)

  zmin_Q <- min(Q_log, na.rm = TRUE)
  zmax_Q <- max(Q_log, na.rm = TRUE)

  png(
    filename = file.path(out_dir, paste0("Q_out_log_map_", file_suffix, ".png")),
    width = 1200,
    height = 800,
    res = 150
  )

  fields::image.plot(
    x = lon_seq,
    y = lat_seq,
    z = Q_log[, ncol(Q_log):1],
    zlim = c(zmin_Q, zmax_Q),
    col = fields::tim.colors(64),
    xlab = "lon",
    ylab = "lat",
    main = paste0("Log10 of cumulative water outflow: ", case_label)
  )

  maps::map("world", add = TRUE, col = "black", lwd = 1)

  dev.off()

  # =========================================================
  # Summary table
  # =========================================================
  conc_summary <- tibble::tibble(
    case_label = case_label,
    threshold_mgN_L = thr,
    n_valid = sum(!is.na(C_fw_mgL)),
    mean_mgN_L = mean(C_fw_mgL, na.rm = TRUE),
    median_mgN_L = median(C_fw_mgL, na.rm = TRUE),
    p90_mgN_L = as.numeric(stats::quantile(C_fw_mgL, 0.90, na.rm = TRUE)),
    p95_mgN_L = as.numeric(stats::quantile(C_fw_mgL, 0.95, na.rm = TRUE)),
    max_mgN_L = max(C_fw_mgL, na.rm = TRUE),
    area_over_threshold_cells = sum(C_fw_mgL > thr, na.rm = TRUE)
  )

  save(
    QoutNint,
    Qout_int,
    C_fw_ratio,
    C_fw_mgL,
    R,
    Z,
    conc_summary,
    file = file.path(out_dir, paste0("C_fw_analysis_", file_suffix, ".rdat"))
  )

  readr::write_csv(
    conc_summary,
    file.path(out_dir, paste0("C_fw_summary_", file_suffix, ".csv"))
  )

  invisible(list(
    Qout_int = Qout_int,
    C_fw_ratio = C_fw_ratio,
    C_fw_mgL = C_fw_mgL,
    R = R,
    Z = Z,
    summary = conc_summary
  ))
}

run_one_case <- function(year, is_mit, nmip3, demo_file,
                         rivnum_matrix, grid_df, riv_table) {

  case_label <- build_case_label(year, is_mit)

  visitnl_object_name <- build_visitnl_name(year, is_mit, nmip3)
  humanN_year <- as.character(year)

  if (!exists(visitnl_object_name, inherits = TRUE)) {
    stop("No object found: ", visitnl_object_name)
  }

  e <- new.env(parent = .GlobalEnv)
  e$visitnl_object_name <- visitnl_object_name
  e$humanN_year <- humanN_year
  e$dat_dir <- get("dat_dir", envir = .GlobalEnv)

  message("Running case: ", visitnl_object_name, " / HumanN year = ", humanN_year)

  source(demo_file, local = e)

  if (!exists("test_sim", envir = e, inherits = FALSE)) {
    stop("No object found: test_sim")
  }

  # =========================================================
  # Concentration analysis
  # =========================================================
  conc_result <- analyze_concentration(
    test_sim = e$test_sim,
    case_label = case_label,
    nmip3 = nmip3,
    out_dir = "output",
    thr = 2.5
  )

  assign(
    paste0("conc_result_", case_label, if (nmip3) "_NMIP3" else ""),
    conc_result,
    envir = .GlobalEnv
  )

  # =========================================================
  # River-mouth aggregation
  # =========================================================
  aggregate_river_mouth(
    test_sim = e$test_sim,
    rivnum_matrix = rivnum_matrix,
    grid_df = grid_df,
    riv_table = riv_table
  )
}

make_obs_compare_plot <- function(model_df, nmip3 = FALSE) {
  obs_raw <- readr::read_csv(
    "River_Nitrogen_Load_Data.csv",
    show_col_types = FALSE,
    guess_max = 100000
  )

  names(obs_raw) <- stringr::str_replace_all(names(obs_raw), "\\s+", "_")

  name_col <- names(obs_raw)[stringr::str_detect(names(obs_raw), stringr::regex("^name$", ignore_case = TRUE))]
  if (length(name_col) == 0) name_col <- names(obs_raw)[1]

  din_col <- names(obs_raw)[stringr::str_detect(names(obs_raw), stringr::regex("DIN", ignore_case = TRUE))]
  if (length(din_col) == 0) {
    stop("観測CSVに DIN を含む列が見つかりません。列名をご確認ください。")
  }

  obs <- obs_raw %>%
    dplyr::transmute(
      obs_name = trimws(.data[[name_col]]),
      DIN_load = suppressWarnings(as.numeric(.data[[din_col[1]]]))
    )

  lines <- readLines("River_obs_analysis.txt", encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  lines <- lines[!stringr::str_starts(trimws(lines), "#")]

  split2 <- strsplit(lines, ",")
  map <- tibble::tibble(
    csv_name = trimws(vapply(split2, `[`, character(1), 1)),
    df_name  = trimws(vapply(split2, `[`, character(1), 2))
  ) %>%
    dplyr::filter(csv_name != "", df_name != "")

  model <- model_df %>%
    dplyr::transmute(
      df_name = trimws(river_basin_name),
      Nload   = as.numeric(Nload / 1000000)
    )

  dat_raw <- obs %>%
    dplyr::rename(csv_name = obs_name) %>%
    dplyr::inner_join(map, by = "csv_name") %>%
    dplyr::inner_join(model, by = "df_name") %>%
    dplyr::transmute(
      csv_name,
      df_name,
      DIN_load_kt = as.numeric(DIN_load),
      Nload_kt    = as.numeric(Nload)
    ) %>%
    dplyr::filter(!is.na(DIN_load_kt), !is.na(Nload_kt))

  dat <- dat_raw %>%
    dplyr::group_by(csv_name) %>%
    dplyr::summarise(
      DIN_load_kt = dplyr::first(DIN_load_kt),
      Nload_kt    = sum(Nload_kt, na.rm = TRUE),
      n_sources   = dplyr::n(),
      merged_from = paste(unique(df_name), collapse = " + "),
      .groups = "drop"
    )

  dat_pos <- dat %>%
    dplyr::filter(DIN_load_kt > 0, Nload_kt > 0)

  label_top <- dat_pos %>%
    dplyr::mutate(
      ratio = Nload_kt / DIN_load_kt,
      dev = abs(log10(ratio))
    ) %>%
    dplyr::slice_max(dev, n = 20)

  lbl_num <- if (exists("cut_short_scale", where = asNamespace("scales"))) {
    scales::label_number(scale_cut = scales::cut_short_scale())
  } else {
    scales::label_number()
  }

  all_vals <- c(dat_pos$DIN_load_kt, dat_pos$Nload_kt)
  all_vals <- all_vals[is.finite(all_vals) & all_vals > 0]
  lo <- 10^floor(log10(min(all_vals)))
  hi <- 10^ceiling(log10(max(all_vals)))

  fit <- lm(log10(Nload_kt) ~ log10(DIN_load_kt), data = dat_pos)

  xnew <- data.frame(DIN_load_kt = 10^seq(log10(lo), log10(hi), length.out = 200))
  pr <- predict(fit, newdata = xnew, se.fit = TRUE)

  df_line <- transform(
    xnew,
    y_fit = 10^pr$fit,
    y_lo  = 10^(pr$fit - 1.96 * pr$se.fit),
    y_hi  = 10^(pr$fit + 1.96 * pr$se.fit)
  )

  p <- ggplot2::ggplot(dat_pos, ggplot2::aes(x = DIN_load_kt, y = Nload_kt)) +
    ggplot2::geom_point(alpha = 0.85, size = 2) +
    ggplot2::geom_function(fun = function(x) x) +
    ggplot2::geom_function(fun = function(x) 2 * x, linetype = "dashed") +
    ggplot2::geom_function(fun = function(x) 0.5 * x, linetype = "dashed") +
    ggrepel::geom_text_repel(
      data = label_top,
      ggplot2::aes(label = csv_name),
      size = 3,
      max.overlaps = 100
    ) +
    ggplot2::geom_ribbon(
      data = df_line,
      ggplot2::aes(x = DIN_load_kt, ymin = y_lo, ymax = y_hi),
      alpha = 0.12,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_line(
      data = df_line,
      ggplot2::aes(x = DIN_load_kt, y = y_fit),
      linewidth = 1,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_x_log10(
      limits = c(lo, hi),
      labels = lbl_num,
      name = "Observed N load (kt N yr-1)",
      expand = ggplot2::expansion(mult = 0.02)
    ) +
    ggplot2::scale_y_log10(
      limits = c(lo, hi),
      labels = lbl_num,
      name = "Simulated N load (kt N yr-1)",
      expand = ggplot2::expansion(mult = 0.02)
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::ggtitle(
      "河川ごとの窒素負荷の本研究における推計値および観測値（He et al., 2011）（kt-N）",
      subtitle = "実線: 1:1, 破線: ×2 / ×0.5"
    )

  calc_metrics <- function(df, obs_col = "DIN_load_kt", mod_col = "Nload_kt") {
    dat2 <- df %>%
      dplyr::transmute(
        O = as.numeric(.data[[obs_col]]),
        M = as.numeric(.data[[mod_col]])
      ) %>%
      dplyr::filter(is.finite(O), is.finite(M), O > 0, M > 0)

    e  <- dat2$M - dat2$O
    pe <- 100 * e / dat2$O
    ape <- abs(pe)

    fit_lin <- lm(M ~ O, data = dat2)
    fit_log <- lm(log10(M) ~ log10(O), data = dat2)

    tibble::tibble(
      n = nrow(dat2),
      R2 = summary(fit_lin)$r.squared,
      RMSE = sqrt(mean(e^2)),
      MAE = mean(abs(e)),
      NRMSE_meanO = sqrt(mean(e^2)) / mean(dat2$O),
      NRMSE_meanO_pct = 100 * sqrt(mean(e^2)) / mean(dat2$O),
      MAPE = mean(ape),
      MdAPE = median(ape),
      MPE = mean(pe),
      PBIAS = 100 * sum(e) / sum(dat2$O),
      R2_log10 = summary(fit_log)$r.squared,
      RMSE_log10 = sqrt(mean((log10(dat2$M) - log10(dat2$O))^2)),
      typical_factor_from_RMSElog10 = 10^sqrt(mean((log10(dat2$M) - log10(dat2$O))^2))
    )
  }

  metrics <- calc_metrics(dat_pos)

  suffix_save <- if (nmip3) "_NMIP3" else ""

  print(p)

  out_dir <- "output"
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  ggplot2::ggsave(
    filename = file.path(out_dir, paste0("Obs_compare", suffix_save, ".png")),
    plot = p,
    width = 6,
    height = 6,
    dpi = 300,
    bg = "white"
  )
  save(
    metrics,
    file = paste0("obs_compare_metrics", suffix_save, ".rdat")
  )

  invisible(list(
    plot = p,
    metrics = metrics,
    matched_data = dat
  ))
}

# =========================================================
# scenarios
# =========================================================
run_plan <- data.frame(
  year = c(2010, 2030, 2050, 2100, 2030, 2050, 2100),
  is_mit = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
)

# =========================================================
# execute
# =========================================================
results <- list()

for (i in seq_len(nrow(run_plan))) {
  year_i <- run_plan$year[i]
  is_mit_i <- run_plan$is_mit[i]

  case_label <- build_case_label(year_i, is_mit_i)

  results[[case_label]] <- run_one_case(
    year = year_i,
    is_mit = is_mit_i,
    nmip3 = NMIP3,
    demo_file = demo_file,
    rivnum_matrix = rivnum_matrix,
    grid_df = grid_df,
    riv_table = riv_table
  )

  assign(
    paste0("df_river_named_mouth_", case_label),
    results[[case_label]],
    envir = .GlobalEnv
  )
}

obs_compare <- make_obs_compare_plot(
  model_df = results[["2010"]],
  nmip3 = NMIP3
)

# =========================================================
# aggregate and prepare for plotting
# =========================================================
bau_labels <- c("2010", "2030", "2050", "2100")
mit_labels <- c("2030mit", "2050mit", "2100mit")

df_river_all <- bind_rows(
  lapply(bau_labels, function(lbl) {
    results[[lbl]] %>%
      mutate(year = as.integer(lbl))
  })
) %>%
  mutate(
    year = as.integer(year),
    rivnum = as.factor(rivnum),
    Nload = Nload / 1000000
  )

df_river_all_mit <- bind_rows(
  lapply(mit_labels, function(lbl) {
    yr <- as.integer(sub("mit", "", lbl))
    results[[lbl]] %>%
      mutate(year = yr)
  })
) %>%
  mutate(
    year = as.integer(year),
    rivnum = as.factor(rivnum),
    Nload = Nload / 1000000
  )

# =========================================================
# save
# =========================================================
suffix_save <- if (NMIP3) "_NMIP3" else ""

save(df_river_all,
     file = paste0("Data_pd/df_river_all", suffix_save, ".rdat"))

save(df_river_all_mit,
     file = paste0("Data_pd/df_river_all_mit", suffix_save, ".rdat"))