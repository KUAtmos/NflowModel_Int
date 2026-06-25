# =========================================================
# Run_VISIToRN_MCMC2.R
# Reconstructed script for VISIToRN MCMC calibration
# =========================================================
# Purpose:
#   1. Run VISIToRN repeatedly using MCMC
#   2. Execute 3 independent chains with different initial values
#   3. Extract the best parameter set from each chain and average them
#   4. Diagnose convergence using R-hat, ESS, trace plots, and histograms
#
# Notes:
#   - This version uses raw Vmax, not log10(Vmax), following the later version
#     of the MCMC script.
#   - VISIToRN itself receives ParFit = c(Vmax, Kno3, Q10).
#   - sigma is used only for likelihood evaluation.
# =========================================================

# =========================================================
# User settings
# =========================================================

script_dir <- getwd()
data_dir  <- file.path(script_dir, "Data_pd")
demo_file <- file.path(script_dir, "Demo_VISIToRN.R")
visiTorn_func_file <- file.path(script_dir, "VISIToRN_func.R")

NMIP3 <- TRUE

# Main switches
RUN_MCMC <- TRUE          # TRUE: run 3 chains; FALSE: analyze existing RDS files only
ANALYZE_RESULTS <- TRUE   # TRUE: calculate R-hat, histograms, trace plots, summaries

# MCMC settings
OUT_PREFIX <- "260601MCMC_logdiff"
ITER <- 10000
BURNIN <- 5000
THIN <- 1
SAVE_EVERY <- 100
YEAR <- 2010
IS_MIT <- FALSE

# If TRUE, include Hastings correction for multiplicative proposals.
# Set FALSE if you want to reproduce the previous uncorrected Metropolis runs exactly.
USE_HASTINGS_CORRECTION <- FALSE

# Figure output
fig_dir <- "MCMC_figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# Safer PNG output on Linux/HPC
options(bitmapType = "cairo")

# =========================================================
# Packages
# =========================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(ncdf4)
  library(coda)
})

# =========================================================
# Data loading
# =========================================================

load(file.path(data_dir, "LandMask_Loop.rdat"), envir = .GlobalEnv)
load(file.path(data_dir, "DistMat.rdat"), envir = .GlobalEnv)
load(file.path(data_dir, "VISITro_nat_ag.rdat"), envir = .GlobalEnv)
load(file.path(data_dir, "RivArea.rdat"), envir = .GlobalEnv)
load(file.path(data_dir, "HumanN_future.rdat"), envir = .GlobalEnv)
load(file.path(data_dir, "dat_tas.rdat"), envir = .GlobalEnv)
load(file.path(data_dir, "dat_dir.rdat"), envir = .GlobalEnv)

# VISIT nitrogen leaching inputs
load(file.path(data_dir, "VISITnl_2010_NMIP3.rdat"), envir = .GlobalEnv)
load(file.path(data_dir, "VISITnl_2030_NMIP3.rdat"), envir = .GlobalEnv)
load(file.path(data_dir, "VISITnl_2050_NMIP3.rdat"), envir = .GlobalEnv)
load(file.path(data_dir, "VISITnl_2100_NMIP3.rdat"), envir = .GlobalEnv)
load(file.path(data_dir, "VISITnl_2030mit_NMIP3.rdat"), envir = .GlobalEnv)
load(file.path(data_dir, "VISITnl_2050mit_NMIP3.rdat"), envir = .GlobalEnv)
load(file.path(data_dir, "VISITnl_2100mit_NMIP3.rdat"), envir = .GlobalEnv)

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

# =========================================================
# Helper functions
# =========================================================

build_visitnl_name <- function(year, is_mit, nmip3 = FALSE) {
  suffix_mit   <- if (is_mit) "mit" else ""
  suffix_nmip3 <- if (nmip3) "_NMIP3" else ""
  paste0("VISITnl_", year, suffix_mit, suffix_nmip3)
}

aggregate_river_mouth <- function(test_sim, rivnum_matrix, grid_df, riv_table) {
  df <- data.frame(
    lon_idx = rep(1:720, each = 360),
    lat_idx = rep(1:360, times = 720),
    Nout    = as.vector(t(test_sim$QoutNint)),
    rivnum  = as.vector(rivnum_matrix)
  )

  df <- df %>%
    left_join(grid_df, by = c("lon_idx", "lat_idx"))

  df_mouth <- df %>%
    filter(rivnum > 0, rpaths %in% c(9, 12))

  df_river_mouth <- df_mouth %>%
    group_by(rivnum) %>%
    summarise(Nload = sum(Nout, na.rm = TRUE), .groups = "drop")

  left_join(df_river_mouth, riv_table, by = "rivnum")
}

prepare_obs_lookup <- function() {
  obs_raw <- readr::read_csv(
    "River_Nitrogen_Load_Data.csv",
    show_col_types = FALSE,
    guess_max = 100000
  )

  names(obs_raw) <- stringr::str_replace_all(names(obs_raw), "\\s+", "_")

  name_col <- names(obs_raw)[stringr::str_detect(
    names(obs_raw), stringr::regex("^name$", ignore_case = TRUE)
  )]
  if (length(name_col) == 0) name_col <- names(obs_raw)[1]

  din_col <- names(obs_raw)[stringr::str_detect(
    names(obs_raw), stringr::regex("DIN", ignore_case = TRUE)
  )]
  if (length(din_col) == 0) {
    stop("観測CSVに DIN を含む列が見つかりません。")
  }

  obs <- obs_raw %>%
    transmute(
      csv_name = trimws(.data[[name_col]]),
      DIN_load_kt = suppressWarnings(as.numeric(.data[[din_col[1]]]))
    )

  lines <- readLines("River_obs_analysis.txt", encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  lines <- lines[!stringr::str_starts(trimws(lines), "#")]

  split2 <- strsplit(lines, ",")
  map <- tibble(
    csv_name = trimws(vapply(split2, `[`, character(1), 1)),
    df_name  = trimws(vapply(split2, `[`, character(1), 2))
  ) %>%
    filter(csv_name != "", df_name != "")

  list(obs = obs, map = map)
}

match_model_obs <- function(model_df, obs_lookup) {
  model <- model_df %>%
    transmute(
      df_name  = trimws(river_basin_name),
      Nload_kt = as.numeric(Nload / 1000000)
    )

  dat_raw <- obs_lookup$obs %>%
    inner_join(obs_lookup$map, by = "csv_name") %>%
    inner_join(model, by = "df_name") %>%
    filter(!is.na(DIN_load_kt), !is.na(Nload_kt))

  dat_raw %>%
    group_by(csv_name) %>%
    summarise(
      DIN_load_kt = first(DIN_load_kt),
      Nload_kt    = sum(Nload_kt, na.rm = TRUE),
      n_sources   = n(),
      merged_from = paste(unique(df_name), collapse = " + "),
      .groups = "drop"
    ) %>%
    filter(DIN_load_kt > 0, Nload_kt > 0)
}

run_one_case_with_parfit <- function(
  year = 2010,
  is_mit = FALSE,
  nmip3 = NMIP3,
  demo_file = demo_file,
  parfit
) {
  visitnl_object_name <- build_visitnl_name(year, is_mit, nmip3)
  humanN_year <- as.character(year)

  if (!exists(visitnl_object_name, inherits = TRUE)) {
    stop("No object found: ", visitnl_object_name)
  }

  e <- new.env(parent = .GlobalEnv)
  e$visitnl_object_name <- visitnl_object_name
  e$humanN_year <- humanN_year
  e$ParFit_input <- parfit
  e$visiTorn_func_file <- visiTorn_func_file
  e$dat_dir <- dat_dir

  source(demo_file, local = e)

  if (!exists("test_sim", envir = e, inherits = FALSE)) {
    stop("No object found: test_sim")
  }

  aggregate_river_mouth(
    test_sim = e$test_sim,
    rivnum_matrix = rivnum_matrix,
    grid_df = grid_df,
    riv_table = riv_table
  )
}

# =========================================================
# Log-difference MCMC, raw Vmax version
# =========================================================

calc_loglik_logdiff <- function(dat, sigma) {
  sum(
    dnorm(
      log(dat$DIN_load_kt) - log(dat$Nload_kt),
      mean = 0,
      sd = sigma,
      log = TRUE
    ),
    na.rm = TRUE
  )
}

calc_logprior <- function(par) {
  dunif(par$Vmax,  min = 10,   max = 1000, log = TRUE) +
    dunif(par$Kno3, min = 0.01, max = 1000, log = TRUE) +
    dunif(par$Q10,  min = 1,    max = 3,    log = TRUE) +
    dunif(par$sigma,min = 0,    max = 4,    log = TRUE)
}

evaluate_logpost <- function(
  par,
  obs_lookup,
  year = 2010,
  is_mit = FALSE,
  nmip3 = NMIP3
) {
  # Prior support check
  if (par$Vmax < 10 || par$Vmax > 1000) return(list(logpost = -Inf))
  if (par$Kno3 < 0.01 || par$Kno3 > 1000) return(list(logpost = -Inf))
  if (par$Q10 < 1 || par$Q10 > 3) return(list(logpost = -Inf))
  if (par$sigma <= 0 || par$sigma > 4) return(list(logpost = -Inf))

  model_df <- tryCatch(
    run_one_case_with_parfit(
      year = year,
      is_mit = is_mit,
      nmip3 = nmip3,
      demo_file = demo_file,
      parfit = c(par$Vmax, par$Kno3, par$Q10)
    ),
    error = function(e) {
      message("run_one_case_with_parfit() failed: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(model_df)) return(list(logpost = -Inf))

  dat <- match_model_obs(model_df, obs_lookup)

  if (nrow(dat) == 0) {
    message("No matched observations after join/filter.")
    return(list(logpost = -Inf))
  }

  ll <- calc_loglik_logdiff(dat, sigma = par$sigma)
  lp <- calc_logprior(par)

  list(
    logpost = ll + lp,
    loglik = ll,
    logprior = lp,
    par = par,
    matched = dat,
    model_df = model_df
  )
}

run_mcmc_visiTorn_logdiff_rawVmax <- function(
  iter = 10000,
  init = list(
    Vmax = 96.3,
    Kno3 = 0.422,
    Q10 = 1.4,
    sigma = 2.5
  ),
  year = 2010,
  is_mit = FALSE,
  nmip3 = NMIP3,
  burnin = 3000,
  thin = 1,
  save_every = 100,
  out_rds = "mcmc_visiTorn_2010_logdiff_rawVmax.rds",
  use_hastings_correction = FALSE
) {
  obs_lookup <- prepare_obs_lookup()

  cur_par <- init
  cur <- evaluate_logpost(
    par = cur_par,
    obs_lookup = obs_lookup,
    year = year,
    is_mit = is_mit,
    nmip3 = nmip3
  )

  if (!is.finite(cur$logpost)) {
    stop("初期値で posterior が有限になりません。初期値またはVmaxスケールを確認してください。")
  }

  chain <- data.frame(
    iter = seq_len(iter),
    Vmax = NA_real_,
    Kno3 = NA_real_,
    Q10 = NA_real_,
    sigma = NA_real_,
    loglik = NA_real_,
    logpost = NA_real_,
    accept = FALSE
  )

  chain$Vmax[1]    <- cur$par$Vmax
  chain$Kno3[1]    <- cur$par$Kno3
  chain$Q10[1]     <- cur$par$Q10
  chain$sigma[1]   <- cur$par$sigma
  chain$loglik[1]  <- cur$loglik
  chain$logpost[1] <- cur$logpost
  chain$accept[1]  <- TRUE

  best <- cur
  t0 <- Sys.time()

  for (i in 2:iter) {
    # Multiplicative proposals for positive parameters
    Vmax_can  <- cur$par$Vmax  * exp(runif(1, -0.2, 0.2))
    Kno3_can  <- cur$par$Kno3  * exp(runif(1, -0.5, 0.5))
    sigma_can <- cur$par$sigma * exp(runif(1, -0.1, 0.1))

    # Additive proposal for Q10
    Q10_can <- cur$par$Q10 + runif(1, -0.2, 0.2)

    can_par <- list(
      Vmax = Vmax_can,
      Kno3 = Kno3_can,
      Q10 = Q10_can,
      sigma = sigma_can
    )

    can <- evaluate_logpost(
      par = can_par,
      obs_lookup = obs_lookup,
      year = year,
      is_mit = is_mit,
      nmip3 = nmip3
    )

    accepted <- FALSE
    if (is.finite(can$logpost)) {
      log_alpha <- can$logpost - cur$logpost

      # Optional Hastings correction for log-scale random walk proposal.
      # This was not in the earlier simple implementation; keep FALSE to reproduce old runs.
      if (use_hastings_correction) {
        log_hastings <-
          log(can_par$Vmax / cur$par$Vmax) +
          log(can_par$Kno3 / cur$par$Kno3) +
          log(can_par$sigma / cur$par$sigma)
        log_alpha <- log_alpha + log_hastings
      }

      if (log_alpha > log(runif(1))) {
        cur <- can
        accepted <- TRUE
        if (cur$logpost > best$logpost) best <- cur
      }
    }

    chain$Vmax[i]    <- cur$par$Vmax
    chain$Kno3[i]    <- cur$par$Kno3
    chain$Q10[i]     <- cur$par$Q10
    chain$sigma[i]   <- cur$par$sigma
    chain$loglik[i]  <- cur$loglik
    chain$logpost[i] <- cur$logpost
    chain$accept[i]  <- accepted

    if (i %% 10 == 0) {
      cat(sprintf(
        "[%d/%d] elapsed=%s accept_rate=%.3f logpost=%.3f loglik=%.3f Vmax=%.3f Kno3=%.4f Q10=%.4f sigma=%.4f\n",
        i, iter,
        format(Sys.time() - t0),
        mean(chain$accept[2:i], na.rm = TRUE),
        cur$logpost,
        cur$loglik,
        cur$par$Vmax,
        cur$par$Kno3,
        cur$par$Q10,
        cur$par$sigma
      ))
    }

    if (i %% save_every == 0) {
      saveRDS(
        list(
          chain = chain[1:i, ],
          best_par = best$par,
          best_loglik = best$loglik,
          best_logpost = best$logpost,
          matched_best = best$matched,
          model_best = best$model_df,
          accept_rate = mean(chain$accept[2:i], na.rm = TRUE)
        ),
        out_rds
      )
    }
  }

  posterior_idx <- seq.int(from = burnin + 1, to = iter, by = thin)
  posterior_idx <- posterior_idx[posterior_idx >= 1 & posterior_idx <= iter]
  posterior <- chain[posterior_idx, , drop = FALSE]

  out <- list(
    chain = chain,
    posterior = posterior,
    best_par = best$par,
    best_loglik = best$loglik,
    best_logpost = best$logpost,
    matched_best = best$matched,
    model_best = best$model_df,
    accept_rate = mean(chain$accept[-1], na.rm = TRUE),
    settings = list(
      iter = iter,
      burnin = burnin,
      thin = thin,
      year = year,
      is_mit = is_mit,
      nmip3 = nmip3,
      use_hastings_correction = use_hastings_correction
    )
  )

  saveRDS(out, out_rds)
  out
}

# =========================================================
# Run 3 independent chains
# =========================================================

run_three_chains <- function() {
  init_list <- list(
    list(Vmax = 50,  Kno3 = 0.05, Q10 = 1.2, sigma = 1.0),
    list(Vmax = 300, Kno3 = 1.0,  Q10 = 1.8, sigma = 2.0),
    list(Vmax = 850, Kno3 = 20,   Q10 = 2.6, sigma = 3.0)
  )

  fits <- vector("list", length(init_list))

  for (k in seq_along(init_list)) {
    set.seed(1000 + k)

    out_rds <- sprintf("%s_chain%d.rds", OUT_PREFIX, k)

    fits[[k]] <- run_mcmc_visiTorn_logdiff_rawVmax(
      iter = ITER,
      init = init_list[[k]],
      year = YEAR,
      is_mit = IS_MIT,
      nmip3 = NMIP3,
      burnin = BURNIN,
      thin = THIN,
      save_every = SAVE_EVERY,
      out_rds = out_rds,
      use_hastings_correction = USE_HASTINGS_CORRECTION
    )
  }

  fits
}

# =========================================================
# Diagnostics and summaries
# =========================================================

safe_png <- function(filename, width = 1200, height = 800, res = 120, expr) {
  outfile <- file.path(fig_dir, filename)
  grDevices::png(outfile, width = width, height = height, res = res, type = "cairo")
  tryCatch(
    force(expr),
    finally = {
      if (dev.cur() != 1) dev.off()
    }
  )
}

safe_pdf <- function(filename, width = 10, height = 7, expr) {
  outfile <- file.path(fig_dir, filename)
  grDevices::pdf(outfile, width = width, height = height)
  tryCatch(
    force(expr),
    finally = {
      if (dev.cur() != 1) dev.off()
    }
  )
}

plot_hist_by_chain <- function(post_each_chain, param, filename_png, filename_pdf = NULL) {
  plot_body <- function() {
    par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

    for (k in sort(unique(post_each_chain$chain))) {
      x <- post_each_chain[post_each_chain$chain == k, param]
      x <- x[is.finite(x)]

      if (length(x) == 0) {
        plot.new()
        title(main = paste0(param, " - chain ", k))
        text(0.5, 0.5, "No finite values")
      } else {
        hist(
          x,
          breaks = 30,
          main = paste0(param, " - chain ", k),
          xlab = param,
          ylab = "Frequency"
        )
      }
    }
  }

  safe_png(filename_png, width = 1200, height = 400, res = 120, expr = plot_body())

  if (!is.null(filename_pdf)) {
    safe_pdf(filename_pdf, width = 12, height = 4, expr = plot_body())
  }
}

analyze_mcmc_fits <- function(fits) {
  for (k in seq_along(fits)) {
    if (is.null(fits[[k]]$posterior)) stop("chain ", k, " に posterior がありません。")
    if (is.null(fits[[k]]$best_par)) stop("chain ", k, " に best_par がありません。")
  }

  # ---------------------------------------------------------
  # Best parameter set from each chain
  # ---------------------------------------------------------
  best_each_chain <- bind_rows(
    lapply(seq_along(fits), function(k) {
      data.frame(
        chain = k,
        Vmax  = fits[[k]]$best_par$Vmax,
        Kno3  = fits[[k]]$best_par$Kno3,
        Q10   = fits[[k]]$best_par$Q10,
        sigma = fits[[k]]$best_par$sigma,
        best_loglik  = fits[[k]]$best_loglik,
        best_logpost = fits[[k]]$best_logpost,
        accept_rate  = fits[[k]]$accept_rate
      )
    })
  )

  print(best_each_chain)
  write.csv(best_each_chain, "MCMC_best_each_chain.csv", row.names = FALSE)

  best_mean <- best_each_chain %>%
    summarise(
      Vmax  = mean(Vmax, na.rm = TRUE),
      Kno3  = mean(Kno3, na.rm = TRUE),
      Q10   = mean(Q10, na.rm = TRUE),
      sigma = mean(sigma, na.rm = TRUE)
    )

  print(best_mean)
  write.csv(best_mean, "MCMC_best_mean.csv", row.names = FALSE)

  ParFit_final <- c(
    Vmax = best_mean$Vmax,
    Kno3 = best_mean$Kno3,
    Q10  = best_mean$Q10
  )

  print(ParFit_final)

  # ---------------------------------------------------------
  # Combine posterior samples
  # ---------------------------------------------------------
  post_each_chain <- bind_rows(
    lapply(seq_along(fits), function(k) {
      fits[[k]]$posterior %>%
        mutate(chain = k)
    })
  )

  post_each_chain <- post_each_chain %>%
    mutate(
      logVmax  = log(Vmax),
      logKno3  = log(Kno3),
      logSigma = log(sigma)
    )

  write.csv(post_each_chain, "MCMC_posterior_all_chains.csv", row.names = FALSE)

  posterior_summary_each_chain <- post_each_chain %>%
    group_by(chain) %>%
    summarise(
      Vmax_mean  = mean(Vmax, na.rm = TRUE),
      Kno3_mean  = mean(Kno3, na.rm = TRUE),
      Q10_mean   = mean(Q10, na.rm = TRUE),
      sigma_mean = mean(sigma, na.rm = TRUE),
      Vmax_median  = median(Vmax, na.rm = TRUE),
      Kno3_median  = median(Kno3, na.rm = TRUE),
      Q10_median   = median(Q10, na.rm = TRUE),
      sigma_median = median(sigma, na.rm = TRUE),
      Vmax_sd  = sd(Vmax, na.rm = TRUE),
      Kno3_sd  = sd(Kno3, na.rm = TRUE),
      Q10_sd   = sd(Q10, na.rm = TRUE),
      sigma_sd = sd(sigma, na.rm = TRUE),
      .groups = "drop"
    )

  print(posterior_summary_each_chain)
  write.csv(posterior_summary_each_chain, "MCMC_posterior_summary_each_chain.csv", row.names = FALSE)

  posterior_summary_all <- post_each_chain %>%
    summarise(
      Vmax_mean  = mean(Vmax, na.rm = TRUE),
      Kno3_mean  = mean(Kno3, na.rm = TRUE),
      Q10_mean   = mean(Q10, na.rm = TRUE),
      sigma_mean = mean(sigma, na.rm = TRUE),
      Vmax_median  = median(Vmax, na.rm = TRUE),
      Kno3_median  = median(Kno3, na.rm = TRUE),
      Q10_median   = median(Q10, na.rm = TRUE),
      sigma_median = median(sigma, na.rm = TRUE),
      Vmax_sd  = sd(Vmax, na.rm = TRUE),
      Kno3_sd  = sd(Kno3, na.rm = TRUE),
      Q10_sd   = sd(Q10, na.rm = TRUE),
      sigma_sd = sd(sigma, na.rm = TRUE)
    )

  print(posterior_summary_all)
  write.csv(posterior_summary_all, "MCMC_posterior_summary_all.csv", row.names = FALSE)

  # ---------------------------------------------------------
  # R-hat and ESS
  # ---------------------------------------------------------
  mcmc_chains <- coda::mcmc.list(
    lapply(fits, function(x) {
      p <- x$posterior
      p$logVmax  <- log(p$Vmax)
      p$logKno3  <- log(p$Kno3)
      p$logSigma <- log(p$sigma)

      coda::mcmc(
        as.matrix(
          p[, c("logVmax", "logKno3", "Q10", "logSigma", "logpost")]
        )
      )
    })
  )

  rhat <- coda::gelman.diag(
    mcmc_chains,
    autoburnin = FALSE,
    multivariate = FALSE
  )

  print(rhat)

  rhat_table <- data.frame(
    parameter = rownames(rhat$psrf),
    Rhat = rhat$psrf[, "Point est."],
    Rhat_upper = rhat$psrf[, "Upper C.I."]
  )

  print(rhat_table)
  write.csv(rhat_table, "MCMC_Rhat.csv", row.names = FALSE)

  ess <- coda::effectiveSize(mcmc_chains)
  ess_table <- data.frame(
    parameter = names(ess),
    ESS = as.numeric(ess)
  )

  print(ess_table)
  write.csv(ess_table, "MCMC_ESS.csv", row.names = FALSE)

  accept_table <- data.frame(
    chain = seq_along(fits),
    accept_rate = sapply(fits, function(x) x$accept_rate)
  )

  print(accept_table)
  write.csv(accept_table, "MCMC_accept_rate.csv", row.names = FALSE)

  # ---------------------------------------------------------
  # Histograms
  # ---------------------------------------------------------
  plot_hist_by_chain(post_each_chain, "Vmax",    "hist_Vmax_by_chain.png",    "hist_Vmax_by_chain.pdf")
  plot_hist_by_chain(post_each_chain, "Kno3",    "hist_Kno3_by_chain.png",    "hist_Kno3_by_chain.pdf")
  plot_hist_by_chain(post_each_chain, "Q10",     "hist_Q10_by_chain.png",     "hist_Q10_by_chain.pdf")
  plot_hist_by_chain(post_each_chain, "sigma",   "hist_sigma_by_chain.png",   "hist_sigma_by_chain.pdf")
  plot_hist_by_chain(post_each_chain, "logpost", "hist_logpost_by_chain.png", "hist_logpost_by_chain.pdf")
  plot_hist_by_chain(post_each_chain, "logVmax", "hist_logVmax_by_chain.png", "hist_logVmax_by_chain.pdf")
  plot_hist_by_chain(post_each_chain, "logKno3", "hist_logKno3_by_chain.png", "hist_logKno3_by_chain.pdf")
  plot_hist_by_chain(post_each_chain, "logSigma", "hist_logSigma_by_chain.png", "hist_logSigma_by_chain.pdf")

  # ---------------------------------------------------------
  # Trace and density plots
  # ---------------------------------------------------------
  safe_png("traceplot_MCMC_parameters.png", width = 1200, height = 800, res = 120,
           expr = coda::traceplot(mcmc_chains))
  safe_pdf("traceplot_MCMC_parameters.pdf", width = 10, height = 7,
           expr = coda::traceplot(mcmc_chains))

  safe_png("densityplot_MCMC_parameters.png", width = 1200, height = 800, res = 120,
           expr = coda::densplot(mcmc_chains))
  safe_pdf("densityplot_MCMC_parameters.pdf", width = 10, height = 7,
           expr = coda::densplot(mcmc_chains))

  # ---------------------------------------------------------
  # Simple convergence judgement
  # ---------------------------------------------------------
  rhat_ok <- all(rhat_table$Rhat < 1.05, na.rm = TRUE)
  ess_ok <- all(ess_table$ESS > 100, na.rm = TRUE)
  accept_ok <- all(
    accept_table$accept_rate > 0.1 &
      accept_table$accept_rate < 0.7,
    na.rm = TRUE
  )

  cat("\n=============================\n")
  cat("Convergence summary\n")
  cat("=============================\n")
  cat("Rhat < 1.05:", rhat_ok, "\n")
  cat("ESS > 100:", ess_ok, "\n")
  cat("0.1 < accept_rate < 0.7:", accept_ok, "\n")

  if (rhat_ok && ess_ok && accept_ok) {
    cat("MCMCは概ね収束していると判断できます。\n")
  } else {
    cat("MCMCの収束は不十分な可能性があります。trace plot, histogram, R-hatを確認してください。\n")
  }

  out <- list(
    fits = fits,
    best_each_chain = best_each_chain,
    best_mean = best_mean,
    ParFit_final = ParFit_final,
    post_each_chain = post_each_chain,
    posterior_summary_each_chain = posterior_summary_each_chain,
    posterior_summary_all = posterior_summary_all,
    rhat = rhat,
    rhat_table = rhat_table,
    ess_table = ess_table,
    accept_table = accept_table,
    convergence = list(
      rhat_ok = rhat_ok,
      ess_ok = ess_ok,
      accept_ok = accept_ok
    )
  )

  saveRDS(out, "MCMC_3chains_diagnostics.rds")
  out
}

# =========================================================
# Main execution
# =========================================================

if (RUN_MCMC) {
  fits <- run_three_chains()
} else {
  rds_files <- sprintf("%s_chain%d.rds", OUT_PREFIX, 1:3)

  if (!all(file.exists(rds_files))) {
    stop(
      "以下のRDSファイルが見つかりません:\n",
      paste(rds_files[!file.exists(rds_files)], collapse = "\n")
    )
  }

  fits <- lapply(rds_files, readRDS)
}

if (ANALYZE_RESULTS) {
  diagnostics <- analyze_mcmc_fits(fits)
}

cat("\nFinished Run_VISIToRN_MCMC2.R\n")
