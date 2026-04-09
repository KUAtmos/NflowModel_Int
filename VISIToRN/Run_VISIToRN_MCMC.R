# =========================================================
# Settings
# =========================================================
script_dir <- getwd()

data_dir  <- file.path(script_dir, "Data_pd")
demo_file <- file.path(script_dir, "Demo_VISIToRN.R")
visiTorn_func_file <- file.path(script_dir, "VISIToRN_func.R")
NMIP3 <- TRUE

# =========================================================
# Packages
# =========================================================
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(ncdf4)
})

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
# Helpers
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
# Log-posterior
# =========================================================
# parameterization:
# z1 = log(Vmax)
# z2 = log(Kno3)
# z3 = qlogis((Q10 - q10_lo)/(q10_hi - q10_lo))
# z4 = log(sigma)
# proposal is symmetric in z-space

bounds <- list(
  Vmax  = c(1, 5000),
  Kno3  = c(0.01, 1000),
  Q10   = c(1, 4),
  sigma = c(0.05, 4)
)

z_to_par <- function(z, bounds) {
  q10_lo <- bounds$Q10[1]
  q10_hi <- bounds$Q10[2]

  p_q10 <- plogis(z[3])

  list(
    Vmax  = exp(z[1]),
    Kno3  = exp(z[2]),
    Q10   = q10_lo + (q10_hi - q10_lo) * p_q10,
    sigma = exp(z[4]),
    p_q10 = p_q10
  )
}

par_to_z <- function(par, bounds) {
  eps <- 1e-8
  q10_lo <- bounds$Q10[1]
  q10_hi <- bounds$Q10[2]

  p_q10 <- (par$Q10 - q10_lo) / (q10_hi - q10_lo)
  p_q10 <- min(max(p_q10, eps), 1 - eps)

  c(
    log(par$Vmax),
    log(par$Kno3),
    qlogis(p_q10),
    log(par$sigma)
  )
}

log_jacobian <- function(par, bounds) {
  q10_range <- bounds$Q10[2] - bounds$Q10[1]
  p_q10 <- par$p_q10

  log(par$Vmax) +
    log(par$Kno3) +
    log(q10_range) + log(p_q10) + log(1 - p_q10) +
    log(par$sigma)
}

loglik_river <- function(dat, sigma) {
  sum(dnorm(log(dat$DIN_load_kt) - log(dat$Nload_kt),
            mean = 0, sd = sigma, log = TRUE))
}

logpost_z <- function(z, obs_lookup, year = 2010, is_mit = FALSE, nmip3 = NMIP3) {
  par <- z_to_par(z, bounds)

  # uniform prior in original parameter space
  if (par$Vmax  < bounds$Vmax[1]  || par$Vmax  > bounds$Vmax[2])  return(list(logpost = -Inf))
  if (par$Kno3  < bounds$Kno3[1]  || par$Kno3  > bounds$Kno3[2])  return(list(logpost = -Inf))
  if (par$Q10   < bounds$Q10[1]   || par$Q10   > bounds$Q10[2])   return(list(logpost = -Inf))
  if (par$sigma < bounds$sigma[1] || par$sigma > bounds$sigma[2]) return(list(logpost = -Inf))

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

if (is.null(model_df)) {
  message("model_df is NULL")
  return(list(logpost = -Inf))
}

message("model_df rows: ", nrow(model_df))
print(utils::head(model_df))

  dat <- match_model_obs(model_df, obs_lookup)
message("matched rows: ", nrow(dat))
print(utils::head(dat))

if (nrow(dat) == 0) {
  message("No matched observations after join/filter.")
  return(list(logpost = -Inf))
}

  if (nrow(dat) == 0) return(list(logpost = -Inf))

  ll <- loglik_river(dat, sigma = par$sigma)

  # posterior in transformed space = likelihood + prior + Jacobian
  lp <- ll + log_jacobian(par, bounds)

  list(
    logpost = lp,
    par = par,
    matched = dat,
    model_df = model_df
  )
}

# =========================================================
# MCMC
# =========================================================
run_mcmc_visiTorn <- function(
  iter = 5,
  init = list(Vmax = 867.78451, Kno3 = 2.14321, Q10 = 2.36906, sigma = 0.5),
  proposal_sd = c(logVmax = 0.08, logKno3 = 0.08, logitQ10 = 0.08, logSigma = 0.05),
  burnin = 500,
  thin = 1,
  year = 2010,
  is_mit = FALSE,
  nmip3 = NMIP3,
  save_every = 50,
  out_rds = "mcmc_visiTorn_2010.rds"
) {
  obs_lookup <- prepare_obs_lookup()

  z_cur <- par_to_z(init, bounds)
  cur <- logpost_z(z_cur, obs_lookup = obs_lookup, year = year, is_mit = is_mit, nmip3 = nmip3)

  if (!is.finite(cur$logpost)) {
    stop("初期値で posterior が有限になりません。初期値を見直してください。")
  }

  chain <- data.frame(
    iter = seq_len(iter),
    Vmax = NA_real_,
    Kno3 = NA_real_,
    Q10 = NA_real_,
    sigma = NA_real_,
    logpost = NA_real_,
    accept = FALSE
  )

  chain$Vmax[1]   <- cur$par$Vmax
  chain$Kno3[1]   <- cur$par$Kno3
  chain$Q10[1]    <- cur$par$Q10
  chain$sigma[1]  <- cur$par$sigma
  chain$logpost[1] <- cur$logpost
  chain$accept[1] <- TRUE

  best <- cur
  t0 <- Sys.time()

  for (i in 2:iter) {
    z_can <- z_cur + c(
      rnorm(1, 0, proposal_sd["logVmax"]),
      rnorm(1, 0, proposal_sd["logKno3"]),
      rnorm(1, 0, proposal_sd["logitQ10"]),
      rnorm(1, 0, proposal_sd["logSigma"])
    )

    can <- logpost_z(z_can, obs_lookup = obs_lookup, year = year, is_mit = is_mit, nmip3 = nmip3)

    log_alpha <- can$logpost - cur$logpost
    accepted <- is.finite(log_alpha) && (log(runif(1)) < log_alpha)

    if (accepted) {
      z_cur <- z_can
      cur <- can
      if (cur$logpost > best$logpost) best <- cur
    }

    chain$Vmax[i]   <- cur$par$Vmax
    chain$Kno3[i]   <- cur$par$Kno3
    chain$Q10[i]    <- cur$par$Q10
    chain$sigma[i]  <- cur$par$sigma
    chain$logpost[i] <- cur$logpost
    chain$accept[i] <- accepted

    if (i %% 10 == 0) {
      cat(
        sprintf(
          "[%d/%d] elapsed=%s accept_rate=%.3f logpost=%.2f Vmax=%.3f Kno3=%.3f Q10=%.3f sigma=%.3f\n",
          i, iter,
          format(Sys.time() - t0),
          mean(chain$accept[2:i]),
          cur$logpost,
          cur$par$Vmax, cur$par$Kno3, cur$par$Q10, cur$par$sigma
        )
      )
    }

    if (i %% save_every == 0) {
      saveRDS(
        list(
          chain = chain[1:i, ],
          best_par = c(Vmax = best$par$Vmax, Kno3 = best$par$Kno3, Q10 = best$par$Q10, sigma = best$par$sigma),
          best_logpost = best$logpost,
          matched_best = best$matched
        ),
        out_rds
      )
    }
  }

  posterior <- chain %>%
    slice(seq.int(from = burnin + 1, to = n(), by = thin))

  summary_tbl <- posterior %>%
    summarise(
      Vmax_mean = mean(Vmax),
      Vmax_q05 = quantile(Vmax, 0.05),
      Vmax_q50 = quantile(Vmax, 0.50),
      Vmax_q95 = quantile(Vmax, 0.95),

      Kno3_mean = mean(Kno3),
      Kno3_q05 = quantile(Kno3, 0.05),
      Kno3_q50 = quantile(Kno3, 0.50),
      Kno3_q95 = quantile(Kno3, 0.95),

      Q10_mean = mean(Q10),
      Q10_q05 = quantile(Q10, 0.05),
      Q10_q50 = quantile(Q10, 0.50),
      Q10_q95 = quantile(Q10, 0.95),

      sigma_mean = mean(sigma),
      sigma_q05 = quantile(sigma, 0.05),
      sigma_q50 = quantile(sigma, 0.50),
      sigma_q95 = quantile(sigma, 0.95),

      accept_rate = mean(chain$accept[-1], na.rm = TRUE)
    )

  out <- list(
    chain = chain,
    posterior = posterior,
    summary = summary_tbl,
    best_par = c(Vmax = best$par$Vmax, Kno3 = best$par$Kno3, Q10 = best$par$Q10, sigma = best$par$sigma),
    best_logpost = best$logpost,
    matched_best = best$matched,
    model_best = best$model_df
  )

  saveRDS(out, out_rds)
  out
}

# =========================================================
# Run
# =========================================================
mcmc_fit <- run_mcmc_visiTorn(
  iter = 10000,
  init = list(Vmax = 867.78451, Kno3 = 2.14321, Q10 = 2.36906, sigma = 0.5),
  proposal_sd = c(logVmax = 0.06, logKno3 = 0.08, logitQ10 = 0.08, logSigma = 0.05),
  burnin = 3000,
  thin = 2,
  year = 2010,
  is_mit = FALSE,
  nmip3 = NMIP3,
  out_rds = "mcmc_visiTorn_2010.rds"
)

print(mcmc_fit$summary)
print(mcmc_fit$best_par)