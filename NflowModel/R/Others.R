#観測値との比較(2010年, 自作)
load("Nload.rdat")
df_river_named_mouth <- df_river_named_mouth %>%
  mutate(Nload = Nload/1000000) #kg -> kt

norm_river <- function(x) {
  x %>%
    toupper() %>%
    str_replace_all("_", " ") %>%
    str_replace_all("\\(.*?\\)", "") %>%
    str_replace_all("[^A-Z ]", " ") %>%
    str_replace_all("\\b(ST)\\.?\\b", "SAINT") %>%
    str_replace_all("\\b(RIO|RIVER|RV|RIVIERE)\\b", " ") %>%
    str_squish()
}

mouth_keyed <- df_river_named_mouth %>%
  mutate(key = norm_river(.data$river_basin_name)) %>%
  group_by(key) %>%
  summarise(
    river_basin_name = first(.data$river_basin_name),
    Nload_mouth      = mean(.data$Nload, na.rm = TRUE),
    .groups = "drop"
  )

nload_keyed <- Nload %>%
  mutate(key = norm_river(.data$river_name)) %>%
  group_by(key) %>%
  summarise(
    river_name_nload = first(.data$river_name),  
    Nload_obs        = mean(.data$Nload, na.rm = TRUE),
    .groups = "drop"
  )

comparison <- inner_join(nload_keyed, mouth_keyed, by = "key") %>%
  transmute(
    river_name   = .data$river_name_nload,  
    Nload_obs    = .data$Nload_obs,         
    Nload_mouth  = .data$Nload_mouth        
  )

miss_in_mouth <- anti_join(nload_keyed, mouth_keyed, by = "key") %>% pull(river_name_nload)
miss_in_nload <- anti_join(mouth_keyed, nload_keyed, by = "key") %>% pull(river_basin_name)

comparison <- comparison %>%
  mutate(diff = Nload_mouth - Nload_obs,
         rel_error = (Nload_mouth - Nload_obs)/Nload_obs)

stopifnot(all(c("river_name","Nload_obs","Nload_mouth") %in% names(comparison)))


dat0 <- comparison %>%
  transmute(
    label    = trimws(river_name),
    obs_kt   = as.numeric(Nload_obs),
    model_kt = as.numeric(Nload_mouth)
  )


dat_agg <- dat0 %>%
  group_by(label) %>%
  summarise(
    obs_kt   = first(obs_kt[is.finite(obs_kt)]),
    model_kt = sum(model_kt, na.rm = TRUE),
    n_rows   = n(),
    .groups = "drop"
  )


dat_pos <- dat_agg %>%
  filter(is.finite(obs_kt), is.finite(model_kt), obs_kt > 0, model_kt > 0)


lbl_num <- if (exists("cut_short_scale", where = asNamespace("scales"))) {
  scales::label_number(scale_cut = scales::cut_short_scale())
} else {
  scales::label_number()
}


all_vals <- c(dat_pos$obs_kt, dat_pos$model_kt)
lo <- 10^floor(log10(min(all_vals)))
hi <- 10^ceiling(log10(max(all_vals)))

fit <- lm(log10(model_kt) ~ log10(obs_kt), data = dat_pos)
co  <- coef(fit); a <- unname(co[1]); b <- unname(co[2])
r2  <- summary(fit)$r.squared
eq_txt <- sprintf("Regression: y = 10^{%.2f} x^{%.2f}  (R^2 = %.2f)", a, b, r2)

xnew <- data.frame(obs_kt = 10^seq(log10(lo), log10(hi), length.out = 200))
pr   <- predict(fit, newdata = xnew, se.fit = TRUE)
df_line <- transform(
  xnew,
  y_fit = 10^pr$fit,
  y_lo  = 10^(pr$fit - 1.96*pr$se.fit),
  y_hi  = 10^(pr$fit + 1.96*pr$se.fit)
)


label_top <- dat_pos %>%
  mutate(ratio = model_kt / obs_kt,
         dev   = abs(log10(ratio))) %>%
  slice_max(dev, n = 20)


p <- ggplot(dat_pos, aes(x = obs_kt, y = model_kt)) +
  geom_point(alpha = 0.85, size = 2) +
  geom_function(fun = function(x) x) +
  geom_function(fun = function(x) 2 * x, linetype = "dashed") +
  geom_function(fun = function(x) 0.5 * x, linetype = "dashed") +
  ggrepel::geom_text_repel(
    data = label_top, aes(label = label),
    size = 3, max.overlaps = 100
  ) +
  geom_ribbon(data = df_line,
              aes(x = obs_kt, ymin = y_lo, ymax = y_hi),
              alpha = 0.12, inherit.aes = FALSE) +
  geom_line(data = df_line,
            aes(x = obs_kt, y = y_fit),
            linewidth = 1, inherit.aes = FALSE) +
  scale_x_log10(limits = c(lo, hi), labels = lbl_num, name = "Observation (kt N / yr)",
                expand = ggplot2::expansion(mult = 0.02)) +
  scale_y_log10(limits = c(lo, hi), labels = lbl_num, name = "This study (kt N / yr)",
                expand = ggplot2::expansion(mult = 0.02)) +
  coord_equal() +
  theme_bw(base_size = 12) +
  ggtitle("River Nloads：Observation vs This study（kt-N）",
          subtitle = "Solid line: 1:1, Dashed line: ×2 / ×0.5") +
  annotate("text", x = lo*1.1, y = hi/1.5, label = eq_txt,
           hjust = 0, vjust = 0, size = 3)

print(p)

ggsave(file.path("output/comparison_obs_vs_modeled_by_river_name.png"),
       p, width = 6, height = 6, dpi = 300)


#Natural and agricultural visitlu
nc_visitLU <- nc_open("visit_20250625_atmosclus_TEST_fNleach.nc") #kg/m2/s

fNleach <- ncvar_get(nc_visitLU, "n_leach")

time_vals <- ncvar_get(nc_visitLU, "time")
time_units <- ncatt_get(nc_visitLU, "time", "units")$value

origin_year <- 1850
years <- origin_year + (time_vals / 12)
target_idx <- which(years >= 2010 & years < 2011) #Institute Variables

fNleach_slice <- ncvar_get(nc_visitLU, "n_leach",
                           start = c(1, 1, 1, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fNleach_slice[,,ti], responseName = "Nleach")
  colnames(df) <- c("lon_idx", "lat_idx", "Nleach")
  df <- df %>%
    mutate(
      lon = lon[as.integer(lon_idx)],
      lat = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, Nleach)
  return(df)
})

fNleach_df5 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITnat_VISITLU <- fNleach_df5 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(Nleach)) %>%
  mutate(Nleach = Nleach * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, Nleach)

fNleach_slice <- ncvar_get(nc_visitLU, "n_leach",
                           start = c(1, 1, 2, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fNleach_slice[,,ti], responseName = "Nleach")
  colnames(df) <- c("lon_idx", "lat_idx", "Nleach")
  df <- df %>%
    mutate(
      lon = lon[as.integer(lon_idx)],
      lat = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, Nleach)
  return(df)
})

fNleach_df6 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITagr_VISITLU <- fNleach_df6 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(Nleach)) %>%
  mutate(Nleach = Nleach * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, Nleach)

VISIT_VISITLU <- VISITnat_VISITLU %>%
  left_join(VISITagr_VISITLU, by = c("year", "lon", "lat")) %>%
  mutate(Nleach = Nleach.x + Nleach.y) %>%
  select(year, lon, lat, Nleach)

VISITnl_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITnl_nat_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITnl_agr_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
year_vals <- sort(unique(VISIT_VISITLU$year))

VISIT_VISITLU <- VISIT_VISITLU %>%
  mutate(
    lon_idx = round((lon + 179.75) / 0.5) + 1,
    lat_idx = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_VISITLU, 1, function(row) {
  VISITnl_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Nleach"]
})

save(VISITnl_VISITLU, file = "Data_pd/VISITnl_VISITLU.rdat")

VISITnat_VISITLU <- VISITnat_VISITLU %>%
  mutate(
    lon_idx = round((lon + 179.75) / 0.5) + 1,
    lat_idx = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

VISITagr_VISITLU <- VISITagr_VISITLU %>%
  mutate(
    lon_idx = round((lon + 179.75) / 0.5) + 1,
    lat_idx = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISITnat_VISITLU, 1, function(row) {
  VISITnl_nat_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Nleach"]
})

save(VISITnl_nat_VISITLU, file = "Data_pd/VISITnl_nat_VISITLU.rdat")

apply(VISITagr_VISITLU, 1, function(row) {
  VISITnl_agr_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Nleach"]
})

save(VISITnl_agr_VISITLU, file = "Data_pd/VISITnl_agr_VISITLU.rdat")


#fNleach_df, fNleach_df2 map
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

custom_colors <- c("#d4e6f1", "#7fb3d5", "#2e86c1", "#f5b041", "#d35400", "#c0392b")

fNleach_df_mean <- fNleach_df %>%
  group_by(lon, lat) %>%
  summarise(Nleach = mean(Nleach))

plot1 <- ggplot() +
  geom_tile(data = fNleach_df_mean, aes(x = lon, y = lat, fill = Nleach)) + 
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.075, 0.15, 0.25, 0.625, 1),
    limits = c(0, 0.0000000004), 
    breaks = c(0, 0.0000000001, 0.0000000002, 0.0000000003, 0.0000000004),  
    labels = c("0", "1.0e-10", "2.0e-10", "3.0e-10", "4.0e-10+"), 
    oob = scales::squish,
    name = "Nleach flux\n(kg N m^-2 natural land s^-1)",  
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Nleach flux"
  ) +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  
    panel.grid = element_blank()
  )

ggsave(
  filename = "output/nat_flux_visit_2010.png",  
  plot = plot1,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)



world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

custom_colors <- c("#d4e6f1", "#7fb3d5", "#2e86c1", "#f5b041", "#d35400", "#c0392b")

fNleach_df2_mean <- fNleach_df2 %>%
  group_by(lon, lat) %>%
  summarise(Nleach = mean(Nleach))

plot1 <- ggplot() +
  geom_tile(data = fNleach_df2_mean, aes(x = lon, y = lat, fill = Nleach)) + 
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.075, 0.15, 0.25, 0.625, 1),
    limits = c(0, 0.0000000004), 
    breaks = c(0, 0.0000000001, 0.0000000002, 0.0000000003, 0.0000000004),  
    labels = c("0", "1.0e-10", "2.0e-10", "3.0e-10", "4.0e-10+"), 
    oob = scales::squish,
    name = "Nleach flux\n(kg N m^-2 agricultural land s^-1)",  
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Nleach flux"
  ) +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  
    panel.grid = element_blank()
  )

ggsave(
  filename = "output/x_visit_2010.png",  
  plot = plot1,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)

#TRIP data read memo
bin_file <- "rivseq.05.bin"

con <- file(bin_file, "rb")
river_seq <- readBin(con, what = "integer", size = 4, n = 360*720, endian = "big")
close(con)

river_seq_mat <- matrix(river_seq, nrow = 360, ncol = 720, byrow = TRUE)

line <- readLines("rivnum05.asc", n = 10)[7]
length(unlist(strsplit(line, "\\s+")))


df_plot <- df %>%
  filter(rivnum >= 1, rivnum <= 10) %>%
  mutate(
    lon = -179.75 + (lon_idx - 1) * 0.5,
    lat =  89.75 - (lat_idx - 1) * 0.5
  )

world <- ne_countries(scale = "medium", returnclass = "sf")

p <- ggplot() +
  geom_tile(data = df_plot, aes(x = lon, y = lat, fill = factor(rivnum))) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.3) +
  scale_fill_viridis_d(name = "River Number") +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Grids with rivnum 1–10",
    x = "Longitude",
    y = "Latitude"
  )

df_plot_nout <- df %>%
  filter(Nout > 0) %>%
  mutate(
    lon = -179.75 + (lon_idx - 1) * 0.5,
    lat =  89.75 - (lat_idx - 1) * 0.5
  )

world <- ne_countries(scale = "medium", returnclass = "sf")

p <- ggplot() +
  geom_tile(data = df_plot_nout, aes(x = lon, y = lat, fill = Nout)) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.3) +
  scale_fill_viridis_c(name = "N out") +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Grids with Nout > 0",
    x = "Longitude",
    y = "Latitude"
  )

print(p)

#Country Grid Map
nc <- nc_open("countrymasks.nc")
v  <- names(nc$var)
v_iso <- v[grepl("^m_[A-Z]{3}$", v)]

lon <- ncvar_get(nc, nc$dim[[1]]$name)
lat <- ncvar_get(nc, nc$dim[[2]]$name)
nx <- length(lon); ny <- length(lat)

lst <- vector("list", length(v_iso))
for (k in seq_along(v_iso)) {
  m <- ncvar_get(nc, v_iso[k])              # [lon, lat]
  idx <- which(m > 0, arr.ind = TRUE)
  if (nrow(idx) == 0) next
  lst[[k]] <- tibble(
    lon  = lon[idx[,1]],
    lat  = lat[idx[,2]],
    iso3 = sub("^m_", "", v_iso[k]),
    hit  = 1
  )
}
nc_close(nc)

df_bin <- bind_rows(lst)

df_fractional <- df_bin %>%
  group_by(lon, lat) %>%
  mutate(nhit = sum(hit)) %>%
  ungroup() %>%
  mutate(frac = 1 / nhit) %>%
  select(lon, lat, iso3, frac)
  group_by(lon, lat) %>%
  summarise(sum_frac = sum(frac), .groups = "drop")
summary(chk$sum_frac)
#VISITnl_VISITLUとVISITnl_nat_ag_Nbud_PLUMLUの比較
load("Data_pd/VISITnl_VISITLU.rdat")
load("Data_pd/VISITnl_Nbud_geos_PLUMLU2010.rdat")
arr_ll <- aperm(VISITnl_VISITLU, c(2, 1, 3))  # [lat, lon, month]


# 2) 直接 SpatRaster に（ここでは引数は arr_ll のみ）
r <- rast(arr_ll)              # dims = 360(rows) x 720(cols) x 12(layers)

# 3) extent と CRS を後から設定
ext(r) <- ext(-180, 180, -90, 90)
crs(r) <- "EPSG:4326"
names(r) <- month.abb

# 2) 年合計（または平均）を作る
r_sum  <- sum(r, na.rm = TRUE)*30*365/360     #kgN/grid/year
# r_mean <- mean(r, na.rm = TRUE)  # 年平均にしたい場合はこちら

# 3) ggplot 用にデータフレーム化
df_plot <- as.data.frame(r_sum, xy = TRUE, na.rm = FALSE)
names(df_plot)[3] <- "Nleach"

coast <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")

p <- ggplot() +
  geom_raster(data = df_plot, aes(x = x, y = y, fill = Nleach)) +
  geom_sf(data = coast, fill = NA, color = "grey20", linewidth = 0.2) +
  coord_sf(expand = FALSE, xlim = c(-180, 180), ylim = c(-90, 90)) +  
  scale_fill_gradientn(
    colours = c("blue", "cyan", "yellow", "red", "darkred"),
    limits  = c(0, 2e7),             # グラデーション範囲を 0～2e7 に固定
    oob     = scales::squish,        # 範囲外の値は端に丸める
    na.value = "transparent"
  ) +
  labs(title = "Annual Nitrogen Leaching (VISIT)",
       fill  = "N leaching\n(annual total)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        legend.position = "right")

ggsave("output/VISITnl_VISITLU2.png", p, width = 10, height = 6, dpi = 300)


arr_ll <- aperm(VISITnl_nat_ag_Nbud_PLUMLU2010, c(2, 1, 3))  # [lat, lon, month]


r <- rast(arr_ll)              


ext(r) <- ext(-180, 180, -90, 90)
crs(r) <- "EPSG:4326"
names(r) <- month.abb


r_sum  <- sum(r, na.rm = TRUE)*30*365/360     #kgN/grid/year



df_plot <- as.data.frame(r_sum, xy = TRUE, na.rm = FALSE)
names(df_plot)[3] <- "Nleach"

coast <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")

p <- ggplot() +
  geom_raster(data = df_plot, aes(x = x, y = y, fill = Nleach)) +
  geom_sf(data = coast, fill = NA, color = "grey20", linewidth = 0.2) +
  coord_sf(expand = FALSE, xlim = c(-180, 180), ylim = c(-90, 90)) +  
  scale_fill_gradientn(
    colours = c("blue", "cyan", "yellow", "red", "darkred"),
    limits  = c(0, 2e7),             
    oob     = scales::squish,        
    na.value = "transparent"
  ) +
  labs(title = "Annual Nitrogen Leaching (VISIT)",
       fill  = "N leaching\n(annual total)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        legend.position = "right")

ggsave("output/VISITnl_nat_ag_Nbud_PLUMLU.png", p, width = 10, height = 6, dpi = 300)

to_yearsum <- function(arr3d) {
  r <- rast(aperm(arr3d, c(2, 1, 3)))      # [lat, lon, month]
  ext(r) <- ext(-180, 180, -90, 90)
  crs(r) <- "EPSG:4326"
  names(r) <- month.abb

  r_year <- sum(r, na.rm = TRUE) * 30 * 365 / 360
  return(r_year)
}

r_base <- to_yearsum(VISITnl_VISITLU)
r_nat  <- to_yearsum(VISITnl_nat_ag_Nbud_PLUMLU2010)

r_diff <- r_nat - r_base

v  <- values(r_diff, na.rm = TRUE)
lim <- as.numeric(quantile(abs(v), 0.995, na.rm = TRUE))
if (!is.finite(lim) || lim == 0) lim <- max(abs(v), na.rm = TRUE)

df_diff <- as.data.frame(r_diff, xy = TRUE, na.rm = FALSE)
names(df_diff)[3] <- "Nleach_diff"
coast <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")

p_diff <- ggplot() +
  geom_raster(data = df_diff, aes(x = x, y = y, fill = Nleach_diff)) +
  geom_sf(data = coast, fill = NA, color = "grey20", linewidth = 0.2) +
  coord_sf(expand = FALSE, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0,
    limits = c(-lim, lim),
    oob = squish,
    name = "ΔN leaching\n(kg N / grid / yr)"
  ) +
  labs(title = "Difference in Annual Nitrogen Leaching\n(nat_ag_PLUMLU2010 − VISITLU)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        legend.position = "right")

ggsave("output/VISITnl_diff_abs.png", p_diff, width = 10, height = 6, dpi = 300)


#VISIT単、統合版の比較（流域単位）
top20_VISIT <- df_river_named_mouth_VISITonly %>%
  arrange(desc(Nload)) %>% slice(1:20) %>% pull(rivnum)

top20_INT <- df_river_named_mouth_INT %>%
  arrange(desc(Nload)) %>% slice(1:20) %>% pull(rivnum)

top20_union <- union(top20_VISIT, top20_INT)

df_compare <- df_river_named_mouth_VISITonly %>%
  select(rivnum, river_basin_name, Nload_VISIT = Nload) %>%
  inner_join(
    df_river_named_mouth_INT %>% select(rivnum, Nload_INT = Nload),
    by = "rivnum"
  ) %>%
  filter(rivnum %in% top20_union)

p <- ggplot(df_compare, aes(x = Nload_VISIT, y = Nload_INT)) +
  geom_point(size = 3, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_x_log10(labels = scales::label_scientific(),
                expand = expansion(mult = c(0.10, 0.05))) +
  scale_y_log10(labels = scales::label_scientific(),
                expand = expansion(mult = c(0.05, 0.05))) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(plot.margin = margin(10, 40, 10, 40),
        panel.grid.minor = element_blank()) +
  labs(title = "N load comparison (Top 20 in either dataset)",
       x = "N load (VISIT only)", y = "N load (INT)")

lab_df <- df_compare %>%
  filter(is.finite(Nload_VISIT), is.finite(Nload_INT),
         Nload_VISIT > 0, Nload_INT > 0)

p_lab <- p +
  geom_text_repel(
    data = lab_df,
    aes(label = as.character(river_basin_name)),
    size = 3, max.overlaps = Inf,
    box.padding = 0.25, point.padding = 0.2,
    min.segment.length = 0, seed = 42
  )

ggsave("output/Basin_compare2.png", p_lab, width = 6, height = 6, dpi = 300)


#Fertilizer grid compare (国別集約して比較)
        #VISITのインプットとして使っているNMIP
        con <- gzfile("Fertilizer_grid/NMIP2_ninput_2010_fert.tsv.gz", "rt") #kgN/ha/yr
        df  <- read.table(con, header = FALSE,
                          col.names = c("lat_idx","lon_idx","Nfert_NHx","Nfert_NOy"))
        close(con)

        df$lon  <- -179.75 + 0.5*df$lon_idx
        df$lat  <-   89.75 - 0.5*df$lat_idx
        df$year <- 2010
        df$Nfert_total <- pmax(df$Nfert_NHx, 0) + pmax(df$Nfert_NOy, 0)

        df <- df %>%
          select(lon, lat, Nfert_total)

        gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
          rename(lat = I, lon = J) %>%
          mutate(
            lon = 0.5 * as.numeric(lon) - 180.25,
            lat = -0.5 * as.numeric(lat) + 90.25
          )

        NMIP2_fert <- df %>%
          left_join(gridarea, by = c("lon", "lat")) %>%
          mutate(Nfert_kgN = Nfert_total*GAIJ/1000) %>% #GgN/yr
          select(lon, lat, Nfert_kgN) %>%
          left_join(df_fractional, by = c("lon", "lat"))%>%
          filter(!is.na(frac)) %>%
          mutate(Nfert_kgN = Nfert_kgN*frac) %>%
          group_by(iso3) %>%
          summarise(Nfert = sum(Nfert_kgN)) %>%
          rename(ISO3_code = iso3, Value = Nfert)

        #窒素収支モデルの出力
        load("Fertilizer_grid/INferdis.rdat") #TgN

        MAP_GIJ <- rgdx.set(data_prep, 'MAP_GIJ') %>%
          rename(lat = I) %>%
          rename(lon = J)

        Nbud_fert <- INferdis %>%
          left_join(MAP_GIJ, by = "G") %>%
          mutate(
            lon           = as.numeric(as.character(lon)),
            lat           = as.numeric(as.character(lat)),
            YEMF          = as.integer(as.character(YEMF)),
            Fert_Use_Grid = as.numeric(as.character(Fert_Use_Grid))
          ) %>%
          filter(YEMF == 2010) %>%
          mutate(
            lon = -179.75 + (lon - 1) * 0.5,
            lat =   89.75 - (lat - 1) * 0.5
          ) %>%
          select(lat, lon, YEMF, Fert_Use_Grid)

        Nbud_fert2 <- Nbud_fert %>%
          left_join(df_fractional, by = c("lon", "lat")) %>%
          mutate(Fert_Use_Grid = Fert_Use_Grid*frac*1000) %>% #GgN
          group_by(iso3) %>%
          summarise(Fert_Use_Grid = sum(Fert_Use_Grid)) %>%
          filter(!is.na(iso3)) %>%
          rename(ISO3_code = iso3, Value = Fert_Use_Grid)

        #FAOSTAT
        FAO_fert <- read_csv("Fertilizer_grid/FertUse2010.csv") #ton

        FAO_fert2 <- FAO_fert %>%
          select("Area Code (M49)", "Value")

        Areacode_3cha <- read.csv("data_pd/UNSD_Methodology.csv", stringsAsFactors = FALSE)
        a_column <- Areacode_3cha$Country_data
        split_data <- str_split(a_column, ";")
        extracted_data <- lapply(split_data, function(x) {
          three_letters <- x[length(x) - 3]
          number <- x[length(x) - 5]
          return(c(number, three_letters))
        })

        Areacode_ISO <- do.call(rbind, extracted_data) %>%
          as.data.frame()

        colnames(Areacode_ISO) <- c("Area Code (M49)", "ISO3_code")

        FAO_fert3 <- FAO_fert2 %>%
          left_join(Areacode_ISO, by = "Area Code (M49)") %>%
          select(Value, ISO3_code) %>%
          mutate(Value = Value/1000) #Gg

        #3つを比較
        n_top <- 20
        ranking_source <- "sum"   # "sum" / "NMIP2" / "Nbud" / "FAOSTAT"

        # 1) 各データフレームをクリーンに（NA/空文字の国コードを除去）
        nmip2 <- NMIP2_fert %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%
          group_by(ISO3_code) %>%
          summarise(nmip2 = sum(as.numeric(Value), na.rm = TRUE), .groups = "drop")

        nbud <- Nbud_fert2 %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%
          group_by(ISO3_code) %>%
          summarise(nbud = sum(as.numeric(Value), na.rm = TRUE), .groups = "drop")

        fao <- FAO_fert3 %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%
          group_by(ISO3_code) %>%
          summarise(fao = sum(as.numeric(Value), na.rm = TRUE), .groups = "drop")

        # 2) 結合＆順位づけ
        df_comb <- nmip2 %>%
          full_join(nbud, by = "ISO3_code") %>%
          full_join(fao,  by = "ISO3_code") %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%        # ← 保険
          mutate(across(c(nmip2, nbud, fao), ~replace_na(., 0))) %>%
          mutate(
            rank_sum   = nmip2 + nbud + fao,
            rank_nmip2 = nmip2,
            rank_nbud  = nbud,
            rank_fao   = fao
          )

        rank_col <- switch(ranking_source,
          "NMIP2"   = "rank_nmip2",
          "Nbud"    = "rank_nbud",
          "FAOSTAT" = "rank_fao",
          "sum"     = "rank_sum"
        )

        top_iso3 <- df_comb %>%
          filter(.data[[rank_col]] > 0) %>%
          slice_max(order_by = .data[[rank_col]], n = n_top, with_ties = FALSE) %>%
          pull(ISO3_code)

        # 3) ロング化（ここでも NA を落とす保険）
        df_long <- df_comb %>%
          filter(ISO3_code %in% top_iso3) %>%
          arrange(desc(.data[[rank_col]])) %>%
          mutate(ISO3_code = factor(ISO3_code, levels = ISO3_code)) %>%
          pivot_longer(cols = c(nmip2, nbud, fao),
                      names_to = "source", values_to = "value") %>%
          mutate(source = recode(source, nmip2 = "VISIT", nbud = "N budget model", fao = "FAOSTAT")) %>%
          drop_na(ISO3_code, value)                                  # ← ここで最終的に NA を除去

        # 4) 描画
        ggplot(df_long, aes(x = ISO3_code, y = value, fill = source)) +
          geom_col(position = position_dodge(width = 0.8), width = 0.72) +
          scale_y_continuous(labels = label_comma()) +
          labs(
            title = sprintf("施肥量 上位%dか国の比較（データ源別）", n_top),
            x = "国（ISO3コード）",
            y = "施肥量（元データの単位）",
            fill = "データ源"
          ) +
          theme_minimal(base_size = 12) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Fert 国別比較（他フローと同様の手法）
load("Data_pd/VISITnf_VISITLU.rdat")  # 期待: VISITnf_VISITLU が存在（kgN/grid/day）

annual_sum_n_fertin <- apply(VISITnf_VISITLU, c(1, 2), sum, na.rm = TRUE)
annual_sum_n_fertin <- annual_sum_n_fertin * 30 * 365 / 360  # kgN/grid/yr に近似換算

lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)
lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)

lon_col <- rep(lon_vec, times = length(lat_vec))
lat_col <- rep(lat_vec, each  = length(lon_vec))

df <- data.frame(
  lon   = lon_col,
  lat   = lat_col,
  Value = as.vector(annual_sum_n_fertin)
)

annual_n_fertin <- df %>%
  mutate(Value = Value / 1e6) %>%       # Gg/yr
  select(lon, lat, Value) %>%
  left_join(df_fractional, by = c("lon", "lat")) %>%
  filter(!is.na(frac)) %>%
  mutate(Value = Value * frac) %>%
  group_by(iso3) %>%
  summarise(Value = sum(Value), .groups = "drop") %>%
  rename(ISO3_code = iso3)

# --- 窒素収支モデルとの整合パート（NetCDF は n_fertin を読む） ---
library(ncdf4)

nc_nat <- nc_open("visit_20250820_atmosclus_geos_TEST_n_fertin_natonly.nc")  # kg/m2/s
nc_agr <- nc_open("visit_20250820_atmosclus_geos_TEST_n_fertin_agronly.nc")

# VISIT natural
n_fertin <- ncvar_get(nc_nat, "n_fertin")

time_vals   <- ncvar_get(nc_nat, "time")
time_units  <- ncatt_get(nc_nat, "time", "units")$value
origin_year <- 1850
years       <- origin_year + (time_vals / 12)
target_idx  <- which(years >= 2010 & years < 2011)

n_fertin_slice <- ncvar_get(
  nc_nat, "n_fertin",
  start = c(1, 1, 1, min(target_idx)),   # PFT 1: natural
  count = c(720, 360, 1, length(target_idx))
)

lon <- ncvar_get(nc_nat, "lon")
lat <- ncvar_get(nc_nat, "lat")

df_list <- lapply(seq_along(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  dd <- as.data.frame.table(n_fertin_slice[,,ti], responseName = "n_fertin")
  colnames(dd) <- c("lon_idx", "lat_idx", "n_fertin")
  dd %>%
    mutate(
      lon  = lon[as.integer(lon_idx)],
      lat  = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, n_fertin)
})
n_fertin_df <- dplyr::bind_rows(df_list)

# VISIT agricultural
n_fertin2   <- ncvar_get(nc_agr, "n_fertin")
time_vals2  <- ncvar_get(nc_agr, "time")
time_units2 <- ncatt_get(nc_agr, "time", "units")$value
years2      <- origin_year + (time_vals2 / 12)
target_idx2 <- which(years2 >= 2010 & years2 < 2011)

n_fertin_slice2 <- ncvar_get(
  nc_agr, "n_fertin",
  start = c(1, 1, 2, min(target_idx2)),  # PFT 2: agricultural
  count = c(720, 360, 1, length(target_idx2))
)

lon2 <- ncvar_get(nc_agr, "lon")
lat2 <- ncvar_get(nc_agr, "lat")

df_list2 <- lapply(seq_along(target_idx2), function(ti){
  year_val2 <- years2[target_idx2[ti]]   # ★修正：years2 / target_idx2
  dd2 <- as.data.frame.table(n_fertin_slice2[,,ti], responseName = "n_fertin")
  colnames(dd2) <- c("lon_idx", "lat_idx", "n_fertin")
  dd2 %>%
    mutate(
      lon  = lon2[as.integer(lon_idx)],
      lat  = lat2[as.integer(lat_idx)],
      year = year_val2
    ) %>%
    select(year, lon, lat, n_fertin)
})
n_fertin_df2 <- dplyr::bind_rows(df_list2)

# --- AIM-PLUM LU をグリッドへ ---
AIMPLUM_BaU      <- "results_SSP2_BaU_NoCC.gdx"
AIMPLUM_BaU_data <- gdxrrw::rgdx(AIMPLUM_BaU)
VY_load <- rgdx.param(AIMPLUM_BaU, 'VY_load') %>%
  rename(G = i4)

GA      <- rgdx.param(data_prep, 'GA')      # kha
MAP_GIJ <- rgdx.set(  data_prep, 'MAP_GIJ') %>%
  rename(lat = I, lon = J)

VY_load2 <- VY_load %>%
  left_join(GA, by = "G") %>%
  mutate(LUAREA = value * GA * 1000)  # ha

LUAREA_Region <- VY_load2 %>%
  group_by(i1, i2, i3) %>%
  summarise(sum_LUAREA = sum(LUAREA), .groups = "drop")

VY_load2 <- VY_load2 %>%
  left_join(LUAREA_Region, by = c("i1", "i2", "i3")) %>%
  mutate(LURATIO = LUAREA / sum_LUAREA) %>%
  left_join(MAP_GIJ, by = c("G"))

VY_load2$lat <- as.numeric(as.character(VY_load2$lat))
VY_load2$lon <- as.numeric(as.character(VY_load2$lon))
VY_load2$i2  <- as.numeric(as.character(VY_load2$i2))

VY_load3 <- VY_load2 %>%
  filter(i2 == "2010") %>%
  filter(i3 %in% c("AFR", "OL", "FRS", "OTH_AIR", "OTH_ARF")) %>%
  select(i3, LUAREA, lat, lon) %>%
  group_by(lat, lon) %>%
  summarise(LU_nat = sum(LUAREA), .groups = "drop") %>%
  mutate(lon = 0.5 * lon - 180.25, lat = -0.5 * lat + 90.25)

VY_load4 <- VY_load2 %>%
  filter(i2 == "2010") %>%
  filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR",
                   "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A", "PAS")) %>%  # 構造は踏襲
  select(i3, LUAREA, lat, lon) %>%
  group_by(lat, lon) %>%
  summarise(LU_agr = sum(LUAREA), .groups = "drop") %>%
  mutate(LU_agr = ifelse(LU_agr < 0, 0, LU_agr)) %>%
  mutate(lon = 0.5 * lon - 180.25, lat = -0.5 * lat + 90.25)

# --- 面積積分（kgN/grid/day） ---
VISIT_nat <- n_fertin_df %>%
  full_join(VY_load3, by = c("lon", "lat")) %>%
  mutate(
    n_fertin = replace_na(n_fertin, 0),
    LU_nat   = replace_na(LU_nat, 0)
  ) %>%
  mutate(n_fertin_grid = n_fertin * LU_nat * 10000 * 3600 * 24) %>%  # kgN/grid/day
  select(year, lon, lat, n_fertin_grid)

VISIT_agr <- n_fertin_df2 %>%
  full_join(VY_load4, by = c("lon", "lat")) %>%
  mutate(
    n_fertin = replace_na(n_fertin, 0),
    LU_agr   = replace_na(LU_agr, 0)
  ) %>%
  mutate(n_fertin_grid2 = n_fertin * LU_agr * 10000 * 3600 * 24) %>%  # kgN/grid/day
  select(year, lon, lat, n_fertin_grid2)

VISIT_nat_agr <- VISIT_nat %>%
  left_join(VISIT_agr, by = c("year", "lon", "lat")) %>%
  mutate(n_fertin = n_fertin_grid + n_fertin_grid2) %>%
  select(year, lon, lat, n_fertin)

VISITn_fertin_nat_ag_PLUMLU <- array(0, dim = c(720, 360, 12))
year_vals <- sort(unique(VISIT_nat_agr$year))

VISIT_nat_agr <- VISIT_nat_agr %>%
  mutate(
    lon_idx   = round((lon + 179.75) / 0.5) + 1,
    lat_idx   = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_nat_agr, 1, function(row) {
  VISITn_fertin_nat_ag_PLUMLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["n_fertin"]
})

save(VISITn_fertin_nat_ag_PLUMLU, file = "Data_pd/VISITn_fertin_Nbud_geos_PLUMLU.rdat")

# --- N 収支モデル入力（INfer）に対応（OUTlea -> INfer へリネーム） ---
load("Data_pd/INferdis.rdat")   # 期待: INferdis（G, YEMF, INfergrid など）
INfergrid_ag <- INferdis %>%
  mutate(INfergrid = Fert_Use_Grid * 1e9 / 365.25) %>%   # Gg/yr -> kg/day
  left_join(MAP_GIJ, by = "G") %>%
  mutate(
    lon = as.numeric(as.character(lon)),
    lat = as.numeric(as.character(lat))
  ) %>%
  mutate(lon = 0.5 * lon - 180.25, lat = -0.5 * lat + 90.25) %>%
  filter(YEMF == "2010") %>%
  select(INfergrid, lat, lon)

VISIT_Nbud <- VISIT_nat %>%
  left_join(INfergrid_ag, by = c("lat", "lon")) %>%
  replace_na(list(
    n_fertin_grid = 0,
    INfergrid     = 0
  )) %>%
  mutate(
    n_fertin_grid = n_fertin_grid + INfergrid,
    n_fertin_grid = ifelse(n_fertin_grid < 0, 0, n_fertin_grid)
  ) %>%
  select(year, lon, lat, n_fertin_grid)

VISITn_fertin_grid_nat_ag_Nbud_PLUMLU2010 <- array(0, dim = c(720, 360, 12))
year_vals <- sort(unique(VISIT_Nbud$year))

VISIT_Nbud <- VISIT_Nbud %>%
  mutate(
    lon_idx   = round((lon + 179.75) / 0.5) + 1,
    lat_idx   = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_Nbud, 1, function(row) {
  VISITn_fertin_grid_nat_ag_Nbud_PLUMLU2010[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["n_fertin_grid"]
})

save(VISITn_fertin_grid_nat_ag_Nbud_PLUMLU2010, file = "Data_pd/VISITn_fertin_grid_Nbud_geos_PLUMLU2010.rdat")

annual_sum <- apply(VISITn_fertin_grid_nat_ag_Nbud_PLUMLU2010, c(1, 2), sum, na.rm = TRUE)
annual_sum <- annual_sum * 30 * 365 / 360 / 1e6  # GgN/grid/yr

lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)
lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)

lon_col <- rep(lon_vec, times = length(lat_vec))
lat_col <- rep(lat_vec, each  = length(lon_vec))

df <- data.frame(
  lon = lon_col,
  lat = lat_col,
  Value = as.vector(annual_sum)
)

annual_n_fertin_grid2 <- df %>%
  select(lon, lat, Value) %>%
  left_join(df_fractional, by = c("lon", "lat")) %>%
  filter(!is.na(frac)) %>%
  mutate(Value = Value * frac) %>%
  group_by(iso3) %>%
  summarise(Value = sum(Value), .groups = "drop") %>%
  rename(ISO3_code = iso3)

# 2つを比較（VISIT 単独 vs VISIT + N budget）
n_top <- 20
ranking_source <- "sum"  # "sum" / "" / "vlt"

emi <- annual_n_fertin %>%
  transmute(ISO3_code = str_to_upper(str_trim(as.character(ISO3_code))),
            emi = as.numeric(Value)) %>%
  filter(!is.na(ISO3_code), ISO3_code != "") %>%
  group_by(ISO3_code) %>%
  summarise(emi = sum(emi, na.rm = TRUE), .groups = "drop")

vlt <- annual_n_fertin_grid2 %>%
  transmute(ISO3_code = str_to_upper(str_trim(as.character(ISO3_code))),
            vlt = as.numeric(Value)) %>%
  filter(!is.na(ISO3_code), ISO3_code != "") %>%
  group_by(ISO3_code) %>%
  summarise(vlt = sum(vlt, na.rm = TRUE), .groups = "drop")

dfc <- emi %>%
  full_join(vlt, by = "ISO3_code") %>%
  filter(!is.na(ISO3_code), ISO3_code != "") %>%
  mutate(across(c(emi, vlt), ~replace_na(., 0))) %>%
  mutate(
    rank_sum = emi + vlt,
    rank_emi = emi,
    rank_vlt = vlt
  )

rank_col <- switch(ranking_source,
  "emi" = "rank_",
  "vlt" = "rank_vlt",
  "sum" = "rank_sum"
)

top_iso3 <- dfc %>%
  filter(.data[[rank_col]] > 0) %>%
  slice_max(order_by = .data[[rank_col]], n = n_top, with_ties = FALSE) %>%
  pull(ISO3_code)

df_long <- dfc %>%
  filter(ISO3_code %in% top_iso3) %>%
  arrange(desc(.data[[rank_col]])) %>%
  mutate(ISO3_code = factor(ISO3_code, levels = ISO3_code)) %>%
  pivot_longer(cols = c(emi, vlt),
               names_to = "source", values_to = "value") %>%
  mutate(source = dplyr::recode(source,
                                emi = "VISIT",
                                vlt = "VISIT + N budget model")) %>%
  drop_na(ISO3_code, value)

ggplot(df_long, aes(x = ISO3_code, y = value, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    title = sprintf("上位%dか国の比較：n_fertin（合成肥料投入量）", n_top),
    x = "国（ISO3コード）",
    y = "Gg N / 年",
    fill = "系列"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Manure grid compare(国別集約して比較)
        #VISITのインプットとして使っているNMIP
        con <- gzfile("Manure_grid/NMIP2_ninput_2010_manure.tsv.gz", "rt") #kgN/ha/yr
        df  <- read.table(con, header = FALSE,
                          col.names = c("lat_idx","lon_idx","Nman"))
        close(con)

        df$lon  <- -179.75 + 0.5*df$lon_idx
        df$lat  <-   89.75 - 0.5*df$lat_idx
        df$year <- 2010
        df$Nman_total <- pmax(df$Nman, 0)

        df <- df %>%
          select(lon, lat, Nman_total)

        gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
          rename(lat = I, lon = J) %>%
          mutate(
            lon = 0.5 * as.numeric(lon) - 180.25,
            lat = -0.5 * as.numeric(lat) + 90.25
          )

        NMIP2_man <- df %>%
          left_join(gridarea, by = c("lon", "lat")) %>%
          mutate(Nman_kgN = Nman_total*GAIJ/1000) %>% #GgN/yr
          select(lon, lat, Nman_kgN) %>%
          left_join(df_fractional, by = c("lon", "lat"))%>%
          filter(!is.na(frac)) %>%
          mutate(Nman_kgN = Nman_kgN*frac) %>%
          group_by(iso3) %>%
          summarise(Nman = sum(Nman_kgN)) %>%
          rename(ISO3_code = iso3, Value = Nman)

        #窒素収支モデルの出力
        load("Manure_grid/INmandis.rdat") #TgN

        MAP_GIJ <- rgdx.set(data_prep, 'MAP_GIJ') %>%
          rename(lat = I) %>%
          rename(lon = J)

        Nbud_man <- INmandis %>%
          left_join(MAP_GIJ, by = "G") %>%
          mutate(
            lon           = as.numeric(as.character(lon)),
            lat           = as.numeric(as.character(lat)),
            YEMF          = as.integer(as.character(YEMF)),
            ManureGrid = as.numeric(as.character(ManureGrid))
          ) %>%
          filter(YEMF == 2010) %>%
          mutate(
            lon = -179.75 + (lon - 1) * 0.5,
            lat =   89.75 - (lat - 1) * 0.5
          ) %>%
          select(lat, lon, YEMF, ManureGrid)

        Nbud_man2 <- Nbud_man %>%
          left_join(df_fractional, by = c("lon", "lat")) %>%
          mutate(ManureGrid = ManureGrid*frac*1000) %>% #GgN
          group_by(iso3) %>%
          summarise(ManureGrid = sum(ManureGrid)) %>%
          filter(!is.na(iso3)) %>%
          rename(ISO3_code = iso3, Value = ManureGrid)

        #2つを比較
        n_top <- 20
        ranking_source <- "sum"   # "sum" / "NMIP2" / "Nbud"

        # 1) クレンジング＆国別集計
        nmip2m <- NMIP2_man %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%
          group_by(ISO3_code) %>%
          summarise(nmip2 = sum(as.numeric(Value), na.rm = TRUE), .groups = "drop")

        nbudm <- Nbud_man2 %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%
          group_by(ISO3_code) %>%
          summarise(nbud = sum(as.numeric(Value), na.rm = TRUE), .groups = "drop")

        # 2) 結合＆順位付け
        dfm <- nmip2m %>%
          full_join(nbudm, by = "ISO3_code") %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%     # 保険
          mutate(across(c(nmip2, nbud), ~replace_na(., 0))) %>%
          mutate(
            rank_sum   = nmip2 + nbud,
            rank_nmip2 = nmip2,
            rank_nbud  = nbud
          )

        rank_col <- switch(ranking_source,
          "NMIP2" = "rank_nmip2",
          "Nbud"  = "rank_nbud",
          "sum"   = "rank_sum"
        )

        top_iso3_m <- dfm %>%
          filter(.data[[rank_col]] > 0) %>%
          slice_max(order_by = .data[[rank_col]], n = n_top, with_ties = FALSE) %>%
          pull(ISO3_code)

        # 3) ロング化（最終NA除去）
        dfm_long <- dfm %>%
          filter(ISO3_code %in% top_iso3_m) %>%
          arrange(desc(.data[[rank_col]])) %>%
          mutate(ISO3_code = factor(ISO3_code, levels = ISO3_code)) %>%
          pivot_longer(cols = c(nmip2, nbud),
                      names_to = "source", values_to = "value") %>%
          mutate(source = recode(source, nmip2 = "VISIT", nbud = "N budget model")) %>%
          drop_na(ISO3_code, value)

        # 4) 描画
        ggplot(dfm_long, aes(x = ISO3_code, y = value, fill = source)) +
          geom_col(position = position_dodge(width = 0.8), width = 0.72) +
          scale_y_continuous(labels = label_comma()) +
          labs(
            title = sprintf("Manure 上位%dか国の比較（データ源別）", n_top),
            x = "国（ISO3コード）",
            y = "Manure量（元データの単位）",
            fill = "データ源"
          ) +
          theme_minimal(base_size = 12) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Man 国別比較（他フローと同様の手法）
load("Data_pd/INmandis.rdat")   # オブジェクト名 INmandis または INfixdis を想定

# どちらの名前で入っていても拾えるようにしておく
manure_df <- if (exists("INmandis")) {
  INmandis
} else if (exists("INfixdis")) {
  INfixdis
} else {
  stop("INmandis も INfixdis も見つかりませんでした。rdat内のオブジェクト名を確認してください。")
}

# 1) AIM グリッド G → lon/lat 対応
MAP_GIJ <- rgdx.set(data_prep, 'MAP_GIJ') %>%
  dplyr::rename(lat = I, lon = J)

# 2) グリッド別 Manure（Tg N/yr）→ lon/lat に付与 → Gg N/yr に変換
#    想定カラム:
#      G      : グリッド番号
#      YEMF   : 年（例: 2010 / "2010"）
#      ManureGrid : 家畜ふん投入量 [Tg N / yr / grid]
#    ※ ManureGrid の列名が異なる場合は下の 'ManureGrid' を置き換えてください。
INmanure_grid <- manure_df %>%
  dplyr::filter(YEMF %in% c(2010, "2010")) %>%     # 対象年（必要に応じて変更）
  dplyr::left_join(MAP_GIJ, by = "G") %>%
  dplyr::mutate(
    lon = as.numeric(as.character(lon)),
    lat = as.numeric(as.character(lat))
  ) %>%
  # AIM グリッドインデックス → 0.5°中心座標
  dplyr::mutate(
    lon = 0.5 * lon - 180.25,
    lat = -0.5 * lat + 90.25
  ) %>%
  # Tg → Gg（1 Tg = 1000 Gg）
  dplyr::mutate(Manure_Gg = ManureGrid * 1000) %>%   # ★N_fix → ManureGrid に置換
  dplyr::select(lon, lat, Manure_Gg)

# 3) 国境ポリゴンのフラクションで按分し、国別合計（Gg N/yr）
#    df_fractional: lon, lat, iso3, frac（既存を流用）
INmanure_country <- INmanure_grid %>%
  dplyr::left_join(df_fractional, by = c("lon", "lat")) %>%
  dplyr::filter(!is.na(frac), !is.na(iso3)) %>%
  dplyr::mutate(Value = Manure_Gg * frac) %>%
  dplyr::group_by(iso3) %>%
  dplyr::summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  dplyr::rename(ISO3_code = iso3)

# 4) 上位 n か国を可視化
n_top <- 20

plot_df <- INmanure_country %>%
  dplyr::filter(!is.na(ISO3_code), ISO3_code != "") %>%
  dplyr::arrange(dplyr::desc(Value)) %>%
  dplyr::slice_head(n = n_top) %>%
  dplyr::mutate(ISO3_code = factor(ISO3_code, levels = ISO3_code))

ggplot2::ggplot(plot_df, ggplot2::aes(x = ISO3_code, y = Value)) +
  ggplot2::geom_col() +
  ggplot2::scale_y_continuous(labels = scales::label_comma()) +
  ggplot2::labs(
    title = sprintf("窒素収支モデルによる家畜ふん投入量 上位%dか国", n_top),
    x     = "国（ISO3コード）",
    y     = "家畜ふん投入量 [Gg N / yr]"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))


#Crop 国別比較
        #VISIT
        load("Data_pd/VISITnuptake_no3_VISITLU.rdat") #kgN/grid/day
        load("Data_pd/VISITnuptake_nh4_VISITLU.rdat") #kgN/grid/day

        annual_sum_no3 <- apply(VISITnuptake_no3_VISITLU, c(1, 2), sum, na.rm = TRUE)
        annual_sum_nh4 <- apply(VISITnuptake_nh4_VISITLU, c(1, 2), sum, na.rm = TRUE)

        annual_sum_no3 <- annual_sum_no3*30*365/360 #kgN/grid
        annual_sum_nh4 <- annual_sum_nh4*30*365/360 #kgN/grid

        lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)   # 長さ 720
        lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)   # 長さ 360

        lon_col <- rep(lon_vec, times = length(lat_vec))         # 720 を 360 回
        lat_col <- rep(lat_vec, each  = length(lon_vec))         # 各latにつき 720 lon

        df <- data.frame(
          lon = lon_col,
          lat = lat_col,
          NO3 = as.vector(annual_sum_no3),   # 720×360 → 259,200
          NH4 = as.vector(annual_sum_nh4)
        )

        annual_uptake <- df %>%
          mutate(Value = (NO3 + NH4)/1000000) %>% #Gg/yr
          select(lon, lat, Value) %>%
          left_join(df_fractional, by = c("lon", "lat")) %>%
          filter(!is.na(frac)) %>%
          mutate(Value = Value*frac) %>%
          group_by(iso3) %>%
          summarise(Value = sum(Value)) %>%
          rename(ISO3_code = iso3)

        #窒素収支モデル
        nc_nat <- nc_open("visit_20250820_atmosclus_geos_TEST_uptake_no3_natonly.nc") #kg/m2/s
        nc_agr <- nc_open("visit_20250820_atmosclus_geos_TEST_uptake_no3_agronly.nc") 

        #VISIT natural
        fNleach <- ncvar_get(nc_nat, "uptake_no3")

        time_vals <- ncvar_get(nc_nat, "time")
        time_units <- ncatt_get(nc_nat, "time", "units")$value

        origin_year <- 1850
        years <- origin_year + (time_vals / 12)
        target_idx <- which(years >= 2010 & years < 2011) #Institute Variables

        fNleach_slice <- ncvar_get(nc_nat, "uptake_no3",
                                  start = c(1, 1, 1, min(target_idx)),  #PFT//natural or agricultural//
                                  count = c(720, 360, 1, length(target_idx)))

        lon <- ncvar_get(nc_nat, "lon")
        lat <- ncvar_get(nc_nat, "lat")

        df_list <- lapply(1:length(target_idx), function(ti){
          year_val <- years[target_idx[ti]]
          df <- as.data.frame.table(fNleach_slice[,,ti], responseName = "uptake_no3")
          colnames(df) <- c("lon_idx", "lat_idx", "uptake_no3")
          df <- df %>%
            mutate(
              lon = lon[as.integer(lon_idx)],
              lat = lat[as.integer(lat_idx)],
              year = year_val
            ) %>%
            select(year, lon, lat, uptake_no3)
          return(df)
        })

        fNleach_df <- bind_rows(df_list)

        #VISIT agricultural
        fNleach2 <- ncvar_get(nc_agr, "uptake_no3")

        time_vals2 <- ncvar_get(nc_agr, "time")
        time_units2 <- ncatt_get(nc_agr, "time", "units")$value

        years <- origin_year + (time_vals2 / 12)
        target_idx2 <- which(years >= 2010 & years < 2011) #Institute Variables

        fNleach_slice2 <- ncvar_get(nc_agr, "uptake_no3",
                                  start = c(1, 1, 2, min(target_idx2)),  #PFT//natural or agricultural//
                                  count = c(720, 360, 1, length(target_idx2)))

        lon2 <- ncvar_get(nc_agr, "lon")
        lat2 <- ncvar_get(nc_agr, "lat")

        df_list2 <- lapply(1:length(target_idx2), function(ti){
          year_val2 <- years[target_idx[ti]]
          df2 <- as.data.frame.table(fNleach_slice2[,,ti], responseName = "uptake_no3")
          colnames(df2) <- c("lon_idx", "lat_idx", "uptake_no3")
          df2 <- df2 %>%
            mutate(
              lon = lon2[as.integer(lon_idx)],
              lat = lat2[as.integer(lat_idx)],
              year = year_val2
            ) %>%
            select(year, lon, lat, uptake_no3)
          return(df2)
        })

        fNleach_df2 <- bind_rows(df_list2)

        #Natural only and agricultural only
        AIMPLUM_BaU <- "results_SSP2_BaU_NoCC.gdx" #input data scenario
        AIMPLUM_BaU_data <- gdxrrw::rgdx(AIMPLUM_BaU)
        VY_load <- rgdx.param(AIMPLUM_BaU, 'VY_load') %>%
          rename(G = i4)


        GA <- rgdx.param(data_prep, 'GA') #kha
        MAP_GIJ <- rgdx.set(data_prep, 'MAP_GIJ') %>%
          rename(lat = I) %>%
          rename(lon = J)

        VY_load2 <- VY_load %>%
          left_join(GA, by = "G") %>%
          mutate(LUAREA = value*GA*1000) #ha

        LUAREA_Region <- VY_load2 %>%
          group_by(i1, i2, i3) %>%
          summarise(sum_LUAREA = sum(LUAREA))

        VY_load2 <- VY_load2 %>%
          left_join(LUAREA_Region, by = c("i1", "i2", "i3")) %>%
          mutate(LURATIO = LUAREA/sum_LUAREA) %>%
          left_join(MAP_GIJ, by = c("G"))

        VY_load2$lat <- as.numeric(as.character(VY_load2$lat))
        VY_load2$lon <- as.numeric(as.character(VY_load2$lon))
        VY_load2$i2 <- as.numeric(as.character(VY_load2$i2))

        VY_load3 <- VY_load2 %>%
          filter(i2 == "2010") %>% #Year
          filter(i3 %in% c("AFR", "OL", "FRS", "OTH_AIR", "OTH_ARF")) %>%
          select(i3, LUAREA, lat, lon) %>% #ha
          group_by(lat, lon) %>%
          summarise(LU_nat = sum(LUAREA)) %>%
          mutate(lon = 0.5*lon - 180.25, lat = -0.5*lat + 90.25) 

        VY_load4 <- VY_load2 %>%
          filter(i2 == "2010") %>% #Year
          filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A", "PAS")) %>%
          select(i3, LUAREA, lat, lon) %>% #ha
          group_by(lat, lon) %>%
          summarise(LU_agr = sum(LUAREA)) %>%
          mutate(LU_agr = ifelse(LU_agr < 0, 0, LU_agr)) %>%
          mutate(lon = 0.5*lon - 180.25, lat = -0.5*lat + 90.25) 

        VISIT_nat1 <- fNleach_df %>%
          full_join(VY_load3, by = c("lon", "lat")) %>%
          mutate(
            uptake_no3 = replace_na(uptake_no3, 0),
            LU_nat = replace_na(LU_nat, 0)
          ) %>%
          mutate(uptake_no3_grid = uptake_no3*LU_nat*10000*3600*24) %>% #kgN/grid/day
          select(year, lon, lat, uptake_no3_grid)

        VISIT_agr <- fNleach_df2 %>%
          full_join(VY_load4, by = c("lon", "lat")) %>%
          mutate(
            uptake_no3 = replace_na(uptake_no3, 0),
            LU_agr = replace_na(LU_agr, 0)
          ) %>%
          mutate(uptake_no3_grid2 = uptake_no3*LU_agr*10000*3600*24) %>% #kgN/grid/day
          select(year, lon, lat, uptake_no3_grid2)

        VISIT_nat_agr <- VISIT_nat1 %>%
          left_join(VISIT_agr, by = c("year", "lon", "lat")) %>%
          mutate(uptake_no3 = uptake_no3_grid + uptake_no3_grid2) %>%
          select(year, lon, lat, uptake_no3)

        VISITuptakeno3_nat_ag_PLUMLU <- array(0, dim = c(720, 360, 12)) #array name
        year_vals <- sort(unique(VISIT_nat_agr$year))

        VISIT_nat_agr <- VISIT_nat_agr %>%
          mutate(
            lon_idx = round((lon + 179.75) / 0.5) + 1,
            lat_idx = round((89.75 - lat) / 0.5) + 1,
            month_idx = match(year, year_vals)
          )

        apply(VISIT_nat_agr, 1, function(row) {
          VISITuptakeno3_nat_ag_PLUMLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Nleach"]
        })

        save(VISITuptakeno3_nat_ag_PLUMLU, file = "Data_pd/VISITuptakeno3_Nbud_geos_PLUMLU.rdat")


        nc_nat <- nc_open("visit_20250820_atmosclus_geos_TEST_uptake_nh4_natonly.nc") #kg/m2/s
        nc_agr <- nc_open("visit_20250820_atmosclus_geos_TEST_uptake_nh4_agronly.nc") 

        #VISIT natural
        fNleach <- ncvar_get(nc_nat, "uptake_nh4")

        time_vals <- ncvar_get(nc_nat, "time")
        time_units <- ncatt_get(nc_nat, "time", "units")$value

        origin_year <- 1850
        years <- origin_year + (time_vals / 12)
        target_idx <- which(years >= 2010 & years < 2011) #Institute Variables

        fNleach_slice <- ncvar_get(nc_nat, "uptake_nh4",
                                  start = c(1, 1, 1, min(target_idx)),  #PFT//natural or agricultural//
                                  count = c(720, 360, 1, length(target_idx)))

        lon <- ncvar_get(nc_nat, "lon")
        lat <- ncvar_get(nc_nat, "lat")

        df_list <- lapply(1:length(target_idx), function(ti){
          year_val <- years[target_idx[ti]]
          df <- as.data.frame.table(fNleach_slice[,,ti], responseName = "uptake_nh4")
          colnames(df) <- c("lon_idx", "lat_idx", "uptake_nh4")
          df <- df %>%
            mutate(
              lon = lon[as.integer(lon_idx)],
              lat = lat[as.integer(lat_idx)],
              year = year_val
            ) %>%
            select(year, lon, lat, uptake_nh4)
          return(df)
        })

        fNleach_df <- bind_rows(df_list)

        #VISIT agricultural
        fNleach2 <- ncvar_get(nc_agr, "uptake_nh4")

        time_vals2 <- ncvar_get(nc_agr, "time")
        time_units2 <- ncatt_get(nc_agr, "time", "units")$value

        years <- origin_year + (time_vals2 / 12)
        target_idx2 <- which(years >= 2010 & years < 2011) #Institute Variables

        fNleach_slice2 <- ncvar_get(nc_agr, "uptake_nh4",
                                  start = c(1, 1, 2, min(target_idx2)),  #PFT//natural or agricultural//
                                  count = c(720, 360, 1, length(target_idx2)))

        lon2 <- ncvar_get(nc_agr, "lon")
        lat2 <- ncvar_get(nc_agr, "lat")

        df_list2 <- lapply(1:length(target_idx2), function(ti){
          year_val2 <- years[target_idx[ti]]
          df2 <- as.data.frame.table(fNleach_slice2[,,ti], responseName = "uptake_nh4")
          colnames(df2) <- c("lon_idx", "lat_idx", "uptake_nh4")
          df2 <- df2 %>%
            mutate(
              lon = lon2[as.integer(lon_idx)],
              lat = lat2[as.integer(lat_idx)],
              year = year_val2
            ) %>%
            select(year, lon, lat, uptake_nh4)
          return(df2)
        })

        fNleach_df2 <- bind_rows(df_list2)

        #Natural only and agricultural only
        AIMPLUM_BaU <- "results_SSP2_BaU_NoCC.gdx" #input data scenario
        AIMPLUM_BaU_data <- gdxrrw::rgdx(AIMPLUM_BaU)
        VY_load <- rgdx.param(AIMPLUM_BaU, 'VY_load') %>%
          rename(G = i4)


        GA <- rgdx.param(data_prep, 'GA') #kha
        MAP_GIJ <- rgdx.set(data_prep, 'MAP_GIJ') %>%
          rename(lat = I) %>%
          rename(lon = J)

        VY_load2 <- VY_load %>%
          left_join(GA, by = "G") %>%
          mutate(LUAREA = value*GA*1000) #ha

        LUAREA_Region <- VY_load2 %>%
          group_by(i1, i2, i3) %>%
          summarise(sum_LUAREA = sum(LUAREA))

        VY_load2 <- VY_load2 %>%
          left_join(LUAREA_Region, by = c("i1", "i2", "i3")) %>%
          mutate(LURATIO = LUAREA/sum_LUAREA) %>%
          left_join(MAP_GIJ, by = c("G"))

        VY_load2$lat <- as.numeric(as.character(VY_load2$lat))
        VY_load2$lon <- as.numeric(as.character(VY_load2$lon))
        VY_load2$i2 <- as.numeric(as.character(VY_load2$i2))

        VY_load3 <- VY_load2 %>%
          filter(i2 == "2010") %>% #Year
          filter(i3 %in% c("AFR", "OL", "FRS", "OTH_AIR", "OTH_ARF")) %>%
          select(i3, LUAREA, lat, lon) %>% #ha
          group_by(lat, lon) %>%
          summarise(LU_nat = sum(LUAREA)) %>%
          mutate(lon = 0.5*lon - 180.25, lat = -0.5*lat + 90.25) 

        VY_load4 <- VY_load2 %>%
          filter(i2 == "2010") %>% #Year
          filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A", "PAS")) %>%
          select(i3, LUAREA, lat, lon) %>% #ha
          group_by(lat, lon) %>%
          summarise(LU_agr = sum(LUAREA)) %>%
          mutate(LU_agr = ifelse(LU_agr < 0, 0, LU_agr)) %>%
          mutate(lon = 0.5*lon - 180.25, lat = -0.5*lat + 90.25) 

        VISIT_nat2 <- fNleach_df %>%
          full_join(VY_load3, by = c("lon", "lat")) %>%
          mutate(
            uptake_nh4 = replace_na(uptake_nh4, 0),
            LU_nat = replace_na(LU_nat, 0)
          ) %>%
          mutate(uptake_nh4_grid = uptake_nh4*LU_nat*10000*3600*24) %>% #kgN/grid/day
          select(year, lon, lat, uptake_nh4_grid)

        VISIT_agr <- fNleach_df2 %>%
          full_join(VY_load4, by = c("lon", "lat")) %>%
          mutate(
            uptake_nh4 = replace_na(uptake_nh4, 0),
            LU_agr = replace_na(LU_agr, 0)
          ) %>%
          mutate(uptake_nh4_grid2 = uptake_nh4*LU_agr*10000*3600*24) %>% #kgN/grid/day
          select(year, lon, lat, uptake_nh4_grid2)

        VISIT_nat_agr <- VISIT_nat2 %>%
          left_join(VISIT_agr, by = c("year", "lon", "lat")) %>%
          mutate(uptake_nh4 = uptake_nh4_grid + uptake_nh4_grid2) %>%
          select(year, lon, lat, uptake_nh4)

        VISITuptakenh4_nat_ag_PLUMLU <- array(0, dim = c(720, 360, 12)) #array name
        year_vals <- sort(unique(VISIT_nat_agr$year))

        VISIT_nat_agr <- VISIT_nat_agr %>%
          mutate(
            lon_idx = round((lon + 179.75) / 0.5) + 1,
            lat_idx = round((89.75 - lat) / 0.5) + 1,
            month_idx = match(year, year_vals)
          )

        apply(VISIT_nat_agr, 1, function(row) {
          VISITuptakenh4_nat_ag_PLUMLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Nleach"]
        })

        save(VISITuptakenh4_nat_ag_PLUMLU, file = "Data_pd/VISITuptakenh4_Nbud_geos_PLUMLU.rdat")

        #2つのVISIT_natを合わせる。
        VISIT_nat <- VISIT_nat1 %>%
          left_join(VISIT_nat2, by = c("year", "lon", "lat")) %>%
          mutate(uptake = uptake_nh4_grid + uptake_no3_grid) %>%
          select(year, lon, lat, uptake)

        #Make nitrogen budget model uptake array
        #Make VISIToRN input (uptake)
        load("Data_pd/OUTcropdis.rdat")
        load("Data_pd/OUTresdis.rdat")

        OUTcropdis <- OUTcropdis %>%
          left_join(OUTresdis, by = c("G", "YEMF")) %>%
          mutate(CropN = CropN + OUTresgrid) %>%
          select(-OUTresgrid)

        OUTcrop_ag <- OUTcropdis %>%
          mutate(CropN = CropN*1000000000/365.25) %>% #kgN/grid/day
          left_join(MAP_GIJ, by = "G") %>%
          mutate(
            lon = as.numeric(as.character(lon)),
            lat = as.numeric(as.character(lat))
          ) %>%
          mutate(lon = 0.5*lon - 180.25, lat = -0.5*lat + 90.25) %>%
          filter(YEMF == "2010") %>% #Year
          select(CropN, lat, lon)

        VISIT_Nbud <- VISIT_nat %>%
          left_join(OUTcrop_ag, by = c("lat", "lon")) %>%
          replace_na(list(
            uptake = 0,
            CropN = 0
          )) %>%
          mutate(
            uptake = uptake + CropN,
            uptake = ifelse(uptake < 0, 0, uptake)  
          ) %>%
          select(year, lon, lat, uptake)

        VISITuptake_nat_ag_Nbud_PLUMLU2010 <- array(0, dim = c(720, 360, 12)) #array name
        year_vals <- sort(unique(VISIT_Nbud$year))

        VISIT_Nbud <- VISIT_Nbud %>%
          mutate(
            lon_idx = round((lon + 179.75) / 0.5) + 1,
            lat_idx = round((89.75 - lat) / 0.5) + 1,
            month_idx = match(year, year_vals)
          )

        apply(VISIT_Nbud, 1, function(row) {
          VISITuptake_nat_ag_Nbud_PLUMLU2010[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["uptake"]
        })

        save(VISITuptake_nat_ag_Nbud_PLUMLU2010, file = "Data_pd/VISITuptake_Nbud_geos_PLUMLU2010.rdat")


        annual_sum <- apply(VISITuptake_nat_ag_Nbud_PLUMLU2010, c(1, 2), sum, na.rm = TRUE)

        annual_sum <- annual_sum*30*365/360/1000000 #GgN/grid

        lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)   # 長さ 720
        lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)   # 長さ 360

        lon_col <- rep(lon_vec, times = length(lat_vec))         # 720 を 360 回
        lat_col <- rep(lat_vec, each  = length(lon_vec))         # 各latにつき 720 lon

        df <- data.frame(
          lon = lon_col,
          lat = lat_col,
          Value = as.vector(annual_sum)
        )

        annual_uptake2 <- df %>%
          select(lon, lat, Value) %>%
          left_join(df_fractional, by = c("lon", "lat")) %>%
          filter(!is.na(frac)) %>%
          mutate(Value = Value*frac) %>%
          group_by(iso3) %>%
          summarise(Value = sum(Value)) %>%
          rename(ISO3_code = iso3)

        #2つを比較
        n_top <- 20
        ranking_source <- "sum"   # "sum" / "v1" / "v2"

        # --- 年列がある場合だけ年を固定（必要なら有効化） ---
        # if ("Year" %in% names(annual_uptake))  annual_uptake  <- filter(annual_uptake,  Year == 2010)
        # if ("Year" %in% names(annual_uptake2)) annual_uptake2 <- filter(annual_uptake2, Year == 2010)

        # 1) クレンジング＆国別集計
        u1 <- annual_uptake %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%
          group_by(ISO3_code) %>%
          summarise(v1 = sum(as.numeric(Value), na.rm = TRUE), .groups = "drop")

        u2 <- annual_uptake2 %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%
          group_by(ISO3_code) %>%
          summarise(v2 = sum(as.numeric(Value), na.rm = TRUE), .groups = "drop")

        # 2) 結合＆順位付け
        df_u <- u1 %>%
          full_join(u2, by = "ISO3_code") %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%                  # 保険
          mutate(across(c(v1, v2), ~replace_na(., 0))) %>%
          mutate(
            rank_sum = v1 + v2,
            rank_v1  = v1,
            rank_v2  = v2
          )

        rank_col <- switch(ranking_source,
          "v1"  = "rank_v1",
          "v2"  = "rank_v2",
          "sum" = "rank_sum"
        )

        top_iso3_u <- df_u %>%
          filter(.data[[rank_col]] > 0) %>%
          slice_max(order_by = .data[[rank_col]], n = n_top, with_ties = FALSE) %>%
          pull(ISO3_code)

        # 3) ロング化（NA除去）
        df_u_long <- df_u %>%
          filter(ISO3_code %in% top_iso3_u) %>%
          arrange(desc(.data[[rank_col]])) %>%
          mutate(ISO3_code = factor(ISO3_code, levels = ISO3_code)) %>%
          pivot_longer(cols = c(v1, v2), names_to = "source", values_to = "value") %>%
          mutate(source = recode(source,
                                v1 = "VISIT",
                                v2 = "VISIT+N budget model")) %>%
          drop_na(ISO3_code, value)

        # 4) 描画
        ggplot(df_u_long, aes(x = ISO3_code, y = value, fill = source)) +
          geom_col(position = position_dodge(width = 0.8), width = 0.72) +
          scale_y_continuous(labels = label_comma()) +
          labs(
            title = sprintf("作物窒素吸収量 上位%dか国の比較（annual_uptake vs annual_uptake2）", n_top),
            x = "国（ISO3コード）",
            y = "作物窒素吸収量（元データの単位）",
            fill = "データ源"
          ) +
          theme_minimal(base_size = 12) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))


#NH3 国別比較
        #VISIT
        load("Data_pd/VISITnnh3vlt_VISITLU.rdat") #kgN/grid/day

        annual_sum_nh3emi <- apply(VISITnnh3vlt_VISITLU, c(1, 2), sum, na.rm = TRUE)

        annual_sum_nh3emi <- annual_sum_nh3emi*30*365/360 #kgN/grid

        lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)   # 長さ 720
        lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)   # 長さ 360

        lon_col <- rep(lon_vec, times = length(lat_vec))         # 720 を 360 回
        lat_col <- rep(lat_vec, each  = length(lon_vec))         # 各latにつき 720 lon

        df <- data.frame(
          lon = lon_col,
          lat = lat_col,
          Value = as.vector(annual_sum_nh3emi)
        )

        annual_nh3emi <- df %>%
          mutate(Value = Value/1000000) %>% #Gg/yr
          select(lon, lat, Value) %>%
          left_join(df_fractional, by = c("lon", "lat")) %>%
          filter(!is.na(frac)) %>%
          mutate(Value = Value*frac) %>%
          group_by(iso3) %>%
          summarise(Value = sum(Value)) %>%
          rename(ISO3_code = iso3)

        #窒素収支モデル
        nc_nat <- nc_open("visit_20250820_atmosclus_geos_TEST_n_nh3vlt_natonly.nc") #kg/m2/s
        nc_agr <- nc_open("visit_20250820_atmosclus_geos_TEST_n_nh3vlt_agronly.nc") 

        #VISIT natural
        fNleach <- ncvar_get(nc_nat, "n_nh3vlt")

        time_vals <- ncvar_get(nc_nat, "time")
        time_units <- ncatt_get(nc_nat, "time", "units")$value

        origin_year <- 1850
        years <- origin_year + (time_vals / 12)
        target_idx <- which(years >= 2010 & years < 2011) #Institute Variables

        fNleach_slice <- ncvar_get(nc_nat, "n_nh3vlt",
                                  start = c(1, 1, 1, min(target_idx)),  #PFT//natural or agricultural//
                                  count = c(720, 360, 1, length(target_idx)))

        lon <- ncvar_get(nc_nat, "lon")
        lat <- ncvar_get(nc_nat, "lat")

        df_list <- lapply(1:length(target_idx), function(ti){
          year_val <- years[target_idx[ti]]
          df <- as.data.frame.table(fNleach_slice[,,ti], responseName = "n_nh3vlt")
          colnames(df) <- c("lon_idx", "lat_idx", "n_nh3vlt")
          df <- df %>%
            mutate(
              lon = lon[as.integer(lon_idx)],
              lat = lat[as.integer(lat_idx)],
              year = year_val
            ) %>%
            select(year, lon, lat, n_nh3vlt)
          return(df)
        })

        fNleach_df <- bind_rows(df_list)

        #VISIT agricultural
        fNleach2 <- ncvar_get(nc_agr, "n_nh3vlt")

        time_vals2 <- ncvar_get(nc_agr, "time")
        time_units2 <- ncatt_get(nc_agr, "time", "units")$value

        years <- origin_year + (time_vals2 / 12)
        target_idx2 <- which(years >= 2010 & years < 2011) #Institute Variables

        fNleach_slice2 <- ncvar_get(nc_agr, "n_nh3vlt",
                                  start = c(1, 1, 2, min(target_idx2)),  #PFT//natural or agricultural//
                                  count = c(720, 360, 1, length(target_idx2)))

        lon2 <- ncvar_get(nc_agr, "lon")
        lat2 <- ncvar_get(nc_agr, "lat")

        df_list2 <- lapply(1:length(target_idx2), function(ti){
          year_val2 <- years[target_idx[ti]]
          df2 <- as.data.frame.table(fNleach_slice2[,,ti], responseName = "n_nh3vlt")
          colnames(df2) <- c("lon_idx", "lat_idx", "n_nh3vlt")
          df2 <- df2 %>%
            mutate(
              lon = lon2[as.integer(lon_idx)],
              lat = lat2[as.integer(lat_idx)],
              year = year_val2
            ) %>%
            select(year, lon, lat, n_nh3vlt)
          return(df2)
        })

        fNleach_df2 <- bind_rows(df_list2)

        #Natural only and agricultural only
        AIMPLUM_BaU <- "results_SSP2_BaU_NoCC.gdx" #input data scenario
        AIMPLUM_BaU_data <- gdxrrw::rgdx(AIMPLUM_BaU)
        VY_load <- rgdx.param(AIMPLUM_BaU, 'VY_load') %>%
          rename(G = i4)


        GA <- rgdx.param(data_prep, 'GA') #kha
        MAP_GIJ <- rgdx.set(data_prep, 'MAP_GIJ') %>%
          rename(lat = I) %>%
          rename(lon = J)

        VY_load2 <- VY_load %>%
          left_join(GA, by = "G") %>%
          mutate(LUAREA = value*GA*1000) #ha

        LUAREA_Region <- VY_load2 %>%
          group_by(i1, i2, i3) %>%
          summarise(sum_LUAREA = sum(LUAREA))

        VY_load2 <- VY_load2 %>%
          left_join(LUAREA_Region, by = c("i1", "i2", "i3")) %>%
          mutate(LURATIO = LUAREA/sum_LUAREA) %>%
          left_join(MAP_GIJ, by = c("G"))

        VY_load2$lat <- as.numeric(as.character(VY_load2$lat))
        VY_load2$lon <- as.numeric(as.character(VY_load2$lon))
        VY_load2$i2 <- as.numeric(as.character(VY_load2$i2))

        VY_load3 <- VY_load2 %>%
          filter(i2 == "2010") %>% #Year
          filter(i3 %in% c("AFR", "OL", "FRS", "OTH_AIR", "OTH_ARF")) %>%
          select(i3, LUAREA, lat, lon) %>% #ha
          group_by(lat, lon) %>%
          summarise(LU_nat = sum(LUAREA)) %>%
          mutate(lon = 0.5*lon - 180.25, lat = -0.5*lat + 90.25) 

        VY_load4 <- VY_load2 %>%
          filter(i2 == "2010") %>% #Year
          filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A", "PAS")) %>%
          select(i3, LUAREA, lat, lon) %>% #ha
          group_by(lat, lon) %>%
          summarise(LU_agr = sum(LUAREA)) %>%
          mutate(LU_agr = ifelse(LU_agr < 0, 0, LU_agr)) %>%
          mutate(lon = 0.5*lon - 180.25, lat = -0.5*lat + 90.25) 

        VISIT_nat <- fNleach_df %>%
          full_join(VY_load3, by = c("lon", "lat")) %>%
          mutate(
            n_nh3vlt = replace_na(n_nh3vlt, 0),
            LU_nat = replace_na(LU_nat, 0)
          ) %>%
          mutate(n_nh3vlt_grid = n_nh3vlt*LU_nat*10000*3600*24) %>% #kgN/grid/day
          select(year, lon, lat, n_nh3vlt_grid)

        VISIT_agr <- fNleach_df2 %>%
          full_join(VY_load4, by = c("lon", "lat")) %>%
          mutate(
            n_nh3vlt = replace_na(n_nh3vlt, 0),
            LU_agr = replace_na(LU_agr, 0)
          ) %>%
          mutate(n_nh3vlt_grid2 = n_nh3vlt*LU_agr*10000*3600*24) %>% #kgN/grid/day
          select(year, lon, lat, n_nh3vlt_grid2)

        VISIT_nat_agr <- VISIT_nat %>%
          left_join(VISIT_agr, by = c("year", "lon", "lat")) %>%
          mutate(n_nh3vlt = n_nh3vlt_grid + n_nh3vlt_grid2) %>%
          select(year, lon, lat, n_nh3vlt)

        VISITnh3emi_nat_ag_PLUMLU <- array(0, dim = c(720, 360, 12)) #array name
        year_vals <- sort(unique(VISIT_nat_agr$year))

        VISIT_nat_agr <- VISIT_nat_agr %>%
          mutate(
            lon_idx = round((lon + 179.75) / 0.5) + 1,
            lat_idx = round((89.75 - lat) / 0.5) + 1,
            month_idx = match(year, year_vals)
          )

        apply(VISIT_nat_agr, 1, function(row) {
          VISITnh3emi_nat_ag_PLUMLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Nleach"]
        })

        save(VISITnh3emi_nat_ag_PLUMLU, file = "Data_pd/VISITnh3emi_Nbud_geos_PLUMLU.rdat")


        #Make nitrogen budget model nh3emi array
        #Make VISIToRN input (nh3emi)
        load("Data_pd/OUTgasdis_NH3.rdat")
        OUTgasnh3_ag <- OUTgasdis_NH3 %>%
          mutate(OUTgasgrid = OUTgasgrid*1000000000/365.25) %>% #kgN/grid/day
          left_join(MAP_GIJ, by = "G") %>%
          mutate(
            lon = as.numeric(as.character(lon)),
            lat = as.numeric(as.character(lat))
          ) %>%
          mutate(lon = 0.5*lon - 180.25, lat = -0.5*lat + 90.25) %>%
          filter(YEMF == "2010") %>% #Year
          select(OUTgasgrid, lat, lon)

        VISIT_Nbud <- VISIT_nat %>%
          left_join(OUTgasnh3_ag, by = c("lat", "lon")) %>%
          replace_na(list(
            n_nh3vlt_grid = 0,
            OUTgasgrid = 0
          )) %>%
          mutate(
            n_nh3vlt_grid = n_nh3vlt_grid + OUTgasgrid,
            n_nh3vlt_grid = ifelse(n_nh3vlt_grid < 0, 0, n_nh3vlt_grid)  
          ) %>%
          select(year, lon, lat, n_nh3vlt_grid)

        VISITn_nh3vlt_grid_nat_ag_Nbud_PLUMLU2010 <- array(0, dim = c(720, 360, 12)) #array name
        year_vals <- sort(unique(VISIT_Nbud$year))

        VISIT_Nbud <- VISIT_Nbud %>%
          mutate(
            lon_idx = round((lon + 179.75) / 0.5) + 1,
            lat_idx = round((89.75 - lat) / 0.5) + 1,
            month_idx = match(year, year_vals)
          )

        apply(VISIT_Nbud, 1, function(row) {
          VISITn_nh3vlt_grid_nat_ag_Nbud_PLUMLU2010[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["n_nh3vlt_grid"]
        })

        save(VISITn_nh3vlt_grid_nat_ag_Nbud_PLUMLU2010, file = "Data_pd/VISITn_nh3vlt_grid_Nbud_geos_PLUMLU2010.rdat")


        annual_sum <- apply(VISITn_nh3vlt_grid_nat_ag_Nbud_PLUMLU2010, c(1, 2), sum, na.rm = TRUE)

        annual_sum <- annual_sum*30*365/360/1000000 #GgN/grid

        lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)   # 長さ 720
        lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)   # 長さ 360

        lon_col <- rep(lon_vec, times = length(lat_vec))         # 720 を 360 回
        lat_col <- rep(lat_vec, each  = length(lon_vec))         # 各latにつき 720 lon

        df <- data.frame(
          lon = lon_col,
          lat = lat_col,
          Value = as.vector(annual_sum)
        )

        annual_n_nh3vlt_grid2 <- df %>%
          select(lon, lat, Value) %>%
          left_join(df_fractional, by = c("lon", "lat")) %>%
          filter(!is.na(frac)) %>%
          mutate(Value = Value*frac) %>%
          group_by(iso3) %>%
          summarise(Value = sum(Value)) %>%
          rename(ISO3_code = iso3)


        #2つを比較
        n_top <- 20
        ranking_source <- "sum"   # "sum" / "emi" / "vlt"

        # --- (任意) 年がある場合だけ固定したい年を指定 ---
        # if ("Year" %in% names(annual_nh3emi))          annual_nh3emi          <- filter(annual_nh3emi, Year == 2010)
        # if ("Year" %in% names(annual_n_nh3vlt_grid2))  annual_n_nh3vlt_grid2  <- filter(annual_n_nh3vlt_grid2, Year == 2010)

        # 1) クレンジング＆国別集計
        emi <- annual_nh3emi %>%
          transmute(ISO3_code = str_to_upper(str_trim(as.character(ISO3_code))),
                    emi = as.numeric(Value)) %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%
          group_by(ISO3_code) %>%
          summarise(emi = sum(emi, na.rm = TRUE), .groups = "drop")

        vlt <- annual_n_nh3vlt_grid2 %>%
          transmute(ISO3_code = str_to_upper(str_trim(as.character(ISO3_code))),
                    vlt = as.numeric(Value)) %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%
          group_by(ISO3_code) %>%
          summarise(vlt = sum(vlt, na.rm = TRUE), .groups = "drop")

        # 2) 結合＆順位付け
        dfc <- emi %>%
          full_join(vlt, by = "ISO3_code") %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%      # 保険
          mutate(across(c(emi, vlt), ~replace_na(., 0))) %>%
          mutate(
            rank_sum = emi + vlt,
            rank_emi = emi,
            rank_vlt = vlt
          )

        rank_col <- switch(ranking_source,
          "emi" = "rank_emi",
          "vlt" = "rank_vlt",
          "sum" = "rank_sum"
        )

        top_iso3 <- dfc %>%
          filter(.data[[rank_col]] > 0) %>%
          slice_max(order_by = .data[[rank_col]], n = n_top, with_ties = FALSE) %>%
          pull(ISO3_code)

        # 3) ロング化（NA除去）
        df_long <- dfc %>%
          filter(ISO3_code %in% top_iso3) %>%
          arrange(desc(.data[[rank_col]])) %>%
          mutate(ISO3_code = factor(ISO3_code, levels = ISO3_code)) %>%
          pivot_longer(cols = c(emi, vlt),
                      names_to = "source", values_to = "value") %>%
          mutate(source = recode(source,
                                emi = "VISIT",
                                vlt = "VISIT + N budget model")) %>%
          drop_na(ISO3_code, value)

        # --- (任意) 単位統一：例) kg → Tg にしたい場合 ---
        # unit_div <- 1e9
        # df_long <- df_long %>% mutate(value = value / unit_div)

        # 4) 描画
        ggplot(df_long, aes(x = ISO3_code, y = value, fill = source)) +
          geom_col(position = position_dodge(width = 0.8), width = 0.72) +
          scale_y_continuous(labels = label_comma()) +
          labs(
            title = sprintf("上位%dか国の比較：NH3 排出 vs NH3 揮散", n_top),
            x = "国（ISO3コード）",
            y = "量（元データの単位）",
            fill = "系列"
          ) +
          theme_minimal(base_size = 12) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))


#no 国別比較
        #VISIT
        load("Data_pd/VISITno_ngas_VISITLU.rdat") #kgN/grid/day

        annual_sum_noemi <- apply(VISITno_ngas_VISITLU, c(1, 2), sum, na.rm = TRUE)

        annual_sum_noemi <- annual_sum_noemi*30*365/360 #kgN/grid

        lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)   # 長さ 720
        lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)   # 長さ 360

        lon_col <- rep(lon_vec, times = length(lat_vec))         # 720 を 360 回
        lat_col <- rep(lat_vec, each  = length(lon_vec))         # 各latにつき 720 lon

        df <- data.frame(
          lon = lon_col,
          lat = lat_col,
          Value = as.vector(annual_sum_noemi)
        )

        annual_noemi <- df %>%
          mutate(Value = Value/1000000) %>% #Gg/yr
          select(lon, lat, Value) %>%
          left_join(df_fractional, by = c("lon", "lat")) %>%
          filter(!is.na(frac)) %>%
          mutate(Value = Value*frac) %>%
          group_by(iso3) %>%
          summarise(Value = sum(Value)) %>%
          rename(ISO3_code = iso3)

        #窒素収支モデル
        nc_nat <- nc_open("visit_20250820_atmosclus_geos_TEST_no_ngas_natonly_rev.nc") #kg/m2/s
        nc_agr <- nc_open("visit_20250820_atmosclus_geos_TEST_no_ngas_agronly_rev.nc") 

        #VISIT natural
        fNleach <- ncvar_get(nc_nat, "no_ngas")

        time_vals <- ncvar_get(nc_nat, "time")
        time_units <- ncatt_get(nc_nat, "time", "units")$value

        origin_year <- 1850
        years <- origin_year + (time_vals / 12)
        target_idx <- which(years >= 2010 & years < 2011) #Institute Variables

        fNleach_slice <- ncvar_get(nc_nat, "no_ngas",
                                  start = c(1, 1, 1, min(target_idx)),  #PFT//natural or agricultural//
                                  count = c(720, 360, 1, length(target_idx)))

        lon <- ncvar_get(nc_nat, "lon")
        lat <- ncvar_get(nc_nat, "lat")

        df_list <- lapply(1:length(target_idx), function(ti){
          year_val <- years[target_idx[ti]]
          df <- as.data.frame.table(fNleach_slice[,,ti], responseName = "no_ngas")
          colnames(df) <- c("lon_idx", "lat_idx", "no_ngas")
          df <- df %>%
            mutate(
              lon = lon[as.integer(lon_idx)],
              lat = lat[as.integer(lat_idx)],
              year = year_val
            ) %>%
            select(year, lon, lat, no_ngas)
          return(df)
        })

        fNleach_df <- bind_rows(df_list)

        #VISIT agricultural
        fNleach2 <- ncvar_get(nc_agr, "no_ngas")

        time_vals2 <- ncvar_get(nc_agr, "time")
        time_units2 <- ncatt_get(nc_agr, "time", "units")$value

        years <- origin_year + (time_vals2 / 12)
        target_idx2 <- which(years >= 2010 & years < 2011) #Institute Variables

        fNleach_slice2 <- ncvar_get(nc_agr, "no_ngas",
                                  start = c(1, 1, 2, min(target_idx2)),  #PFT//natural or agricultural//
                                  count = c(720, 360, 1, length(target_idx2)))

        lon2 <- ncvar_get(nc_agr, "lon")
        lat2 <- ncvar_get(nc_agr, "lat")

        df_list2 <- lapply(1:length(target_idx2), function(ti){
          year_val2 <- years[target_idx[ti]]
          df2 <- as.data.frame.table(fNleach_slice2[,,ti], responseName = "no_ngas")
          colnames(df2) <- c("lon_idx", "lat_idx", "no_ngas")
          df2 <- df2 %>%
            mutate(
              lon = lon2[as.integer(lon_idx)],
              lat = lat2[as.integer(lat_idx)],
              year = year_val2
            ) %>%
            select(year, lon, lat, no_ngas)
          return(df2)
        })

        fNleach_df2 <- bind_rows(df_list2)

        #Natural only and agricultural only
        AIMPLUM_BaU <- "results_SSP2_BaU_NoCC.gdx" #input data scenario
        AIMPLUM_BaU_data <- gdxrrw::rgdx(AIMPLUM_BaU)
        VY_load <- rgdx.param(AIMPLUM_BaU, 'VY_load') %>%
          rename(G = i4)


        GA <- rgdx.param(data_prep, 'GA') #kha
        MAP_GIJ <- rgdx.set(data_prep, 'MAP_GIJ') %>%
          rename(lat = I) %>%
          rename(lon = J)

        VY_load2 <- VY_load %>%
          left_join(GA, by = "G") %>%
          mutate(LUAREA = value*GA*1000) #ha

        LUAREA_Region <- VY_load2 %>%
          group_by(i1, i2, i3) %>%
          summarise(sum_LUAREA = sum(LUAREA))

        VY_load2 <- VY_load2 %>%
          left_join(LUAREA_Region, by = c("i1", "i2", "i3")) %>%
          mutate(LURATIO = LUAREA/sum_LUAREA) %>%
          left_join(MAP_GIJ, by = c("G"))

        VY_load2$lat <- as.numeric(as.character(VY_load2$lat))
        VY_load2$lon <- as.numeric(as.character(VY_load2$lon))
        VY_load2$i2 <- as.numeric(as.character(VY_load2$i2))

        VY_load3 <- VY_load2 %>%
          filter(i2 == "2010") %>% #Year
          filter(i3 %in% c("AFR", "OL", "FRS", "OTH_AIR", "OTH_ARF")) %>%
          select(i3, LUAREA, lat, lon) %>% #ha
          group_by(lat, lon) %>%
          summarise(LU_nat = sum(LUAREA)) %>%
          mutate(lon = 0.5*lon - 180.25, lat = -0.5*lat + 90.25) 

        VY_load4 <- VY_load2 %>%
          filter(i2 == "2010") %>% #Year
          filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A", "PAS")) %>%
          select(i3, LUAREA, lat, lon) %>% #ha
          group_by(lat, lon) %>%
          summarise(LU_agr = sum(LUAREA)) %>%
          mutate(LU_agr = ifelse(LU_agr < 0, 0, LU_agr)) %>%
          mutate(lon = 0.5*lon - 180.25, lat = -0.5*lat + 90.25) 

        VISIT_nat <- fNleach_df %>%
          full_join(VY_load3, by = c("lon", "lat")) %>%
          mutate(
            no_ngas = replace_na(no_ngas, 0),
            LU_nat = replace_na(LU_nat, 0)
          ) %>%
          mutate(no_ngas_grid = no_ngas*LU_nat*10000*3600*24) %>% #kgN/grid/day
          select(year, lon, lat, no_ngas_grid)

        VISIT_agr <- fNleach_df2 %>%
          full_join(VY_load4, by = c("lon", "lat")) %>%
          mutate(
            no_ngas = replace_na(no_ngas, 0),
            LU_agr = replace_na(LU_agr, 0)
          ) %>%
          mutate(no_ngas_grid2 = no_ngas*LU_agr*10000*3600*24) %>% #kgN/grid/day
          select(year, lon, lat, no_ngas_grid2)

        VISIT_nat_agr <- VISIT_nat %>%
          left_join(VISIT_agr, by = c("year", "lon", "lat")) %>%
          mutate(no_ngas = no_ngas_grid + no_ngas_grid2) %>%
          select(year, lon, lat, no_ngas)

        VISITnoemi_nat_ag_PLUMLU <- array(0, dim = c(720, 360, 12)) #array name
        year_vals <- sort(unique(VISIT_nat_agr$year))

        VISIT_nat_agr <- VISIT_nat_agr %>%
          mutate(
            lon_idx = round((lon + 179.75) / 0.5) + 1,
            lat_idx = round((89.75 - lat) / 0.5) + 1,
            month_idx = match(year, year_vals)
          )

        apply(VISIT_nat_agr, 1, function(row) {
          VISITnoemi_nat_ag_PLUMLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Nleach"]
        })

        save(VISITnoemi_nat_ag_PLUMLU, file = "Data_pd/VISITnoemi_Nbud_geos_PLUMLU.rdat")


        #Make nitrogen budget model noemi array
        #Make VISIToRN input (noemi)
        load("Data_pd/OUTgasdis_NO.rdat")
        OUTgasno_ag <- OUTgasdis_NO %>%
          mutate(OUTgasgrid = OUTgasgrid*1000000000/365.25) %>% #kgN/grid/day
          left_join(MAP_GIJ, by = "G") %>%
          mutate(
            lon = as.numeric(as.character(lon)),
            lat = as.numeric(as.character(lat))
          ) %>%
          mutate(lon = 0.5*lon - 180.25, lat = -0.5*lat + 90.25) %>%
          filter(YEMF == "2010") %>% #Year
          select(OUTgasgrid, lat, lon)

        VISIT_Nbud <- VISIT_nat %>%
          left_join(OUTgasno_ag, by = c("lat", "lon")) %>%
          replace_na(list(
            no_ngas_grid = 0,
            OUTgasgrid = 0
          )) %>%
          mutate(
            no_ngas_grid = no_ngas_grid + OUTgasgrid,
            no_ngas_grid = ifelse(no_ngas_grid < 0, 0, no_ngas_grid)  
          ) %>%
          select(year, lon, lat, no_ngas_grid)

        VISITno_ngas_grid_nat_ag_Nbud_PLUMLU2010 <- array(0, dim = c(720, 360, 12)) #array name
        year_vals <- sort(unique(VISIT_Nbud$year))

        VISIT_Nbud <- VISIT_Nbud %>%
          mutate(
            lon_idx = round((lon + 179.75) / 0.5) + 1,
            lat_idx = round((89.75 - lat) / 0.5) + 1,
            month_idx = match(year, year_vals)
          )

        apply(VISIT_Nbud, 1, function(row) {
          VISITno_ngas_grid_nat_ag_Nbud_PLUMLU2010[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["no_ngas_grid"]
        })

        save(VISITno_ngas_grid_nat_ag_Nbud_PLUMLU2010, file = "Data_pd/VISITno_ngas_grid_Nbud_geos_PLUMLU2010.rdat")


        annual_sum <- apply(VISITno_ngas_grid_nat_ag_Nbud_PLUMLU2010, c(1, 2), sum, na.rm = TRUE)

        annual_sum <- annual_sum*30*365/360/1000000 #GgN/grid

        lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)   # 長さ 720
        lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)   # 長さ 360

        lon_col <- rep(lon_vec, times = length(lat_vec))         # 720 を 360 回
        lat_col <- rep(lat_vec, each  = length(lon_vec))         # 各latにつき 720 lon

        df <- data.frame(
          lon = lon_col,
          lat = lat_col,
          Value = as.vector(annual_sum)
        )

        annual_no_ngas_grid2 <- df %>%
          select(lon, lat, Value) %>%
          left_join(df_fractional, by = c("lon", "lat")) %>%
          filter(!is.na(frac)) %>%
          mutate(Value = Value*frac) %>%
          group_by(iso3) %>%
          summarise(Value = sum(Value)) %>%
          rename(ISO3_code = iso3)

        #2つを比較
        n_top <- 20
        ranking_source <- "sum"   # "sum" / "emi" / "vlt"

        # --- (任意) 年がある場合だけ固定したい年を指定 ---
        # if ("Year" %in% names(annual_noemi))          annual_noemi          <- filter(annual_noemi, Year == 2010)
        # if ("Year" %in% names(annual_n_novlt_grid2))  annual_n_novlt_grid2  <- filter(annual_n_novlt_grid2, Year == 2010)

        # 1) クレンジング＆国別集計
        emi <- annual_noemi %>%
          transmute(ISO3_code = str_to_upper(str_trim(as.character(ISO3_code))),
                    emi = as.numeric(Value)) %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%
          group_by(ISO3_code) %>%
          summarise(emi = sum(emi, na.rm = TRUE), .groups = "drop")

        vlt <- annual_no_ngas_grid2 %>%
          transmute(ISO3_code = str_to_upper(str_trim(as.character(ISO3_code))),
                    vlt = as.numeric(Value)) %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%
          group_by(ISO3_code) %>%
          summarise(vlt = sum(vlt, na.rm = TRUE), .groups = "drop")

        # 2) 結合＆順位付け
        dfc <- emi %>%
          full_join(vlt, by = "ISO3_code") %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%      # 保険
          mutate(across(c(emi, vlt), ~replace_na(., 0))) %>%
          mutate(
            rank_sum = emi + vlt,
            rank_emi = emi,
            rank_vlt = vlt
          )

        rank_col <- switch(ranking_source,
          "emi" = "rank_emi",
          "vlt" = "rank_vlt",
          "sum" = "rank_sum"
        )

        top_iso3 <- dfc %>%
          filter(.data[[rank_col]] > 0) %>%
          slice_max(order_by = .data[[rank_col]], n = n_top, with_ties = FALSE) %>%
          pull(ISO3_code)

        # 3) ロング化（NA除去）
        df_long <- dfc %>%
          filter(ISO3_code %in% top_iso3) %>%
          arrange(desc(.data[[rank_col]])) %>%
          mutate(ISO3_code = factor(ISO3_code, levels = ISO3_code)) %>%
          pivot_longer(cols = c(emi, vlt),
                      names_to = "source", values_to = "value") %>%
          mutate(source = recode(source,
                                emi = "VISIT",
                                vlt = "VISIT + N budget model")) %>%
          drop_na(ISO3_code, value)

        # --- (任意) 単位統一：例) kg → Tg にしたい場合 ---
        # unit_div <- 1e9
        # df_long <- df_long %>% mutate(value = value / unit_div)

        # 4) 描画
        ggplot(df_long, aes(x = ISO3_code, y = value, fill = source)) +
          geom_col(position = position_dodge(width = 0.8), width = 0.72) +
          scale_y_continuous(labels = label_comma()) +
          labs(
            title = sprintf("上位%dか国の比較：no 排出 vs no 揮散", n_top),
            x = "国（ISO3コード）",
            y = "量（元データの単位）",
            fill = "系列"
          ) +
          theme_minimal(base_size = 12) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))


#n2o 国別比較
        #VISIT
        load("Data_pd/VISITnn2o_ngas_VISITLU.rdat") #kgN/grid/day

        annual_sum_n2oemi <- apply(VISITnn2o_ngas_VISITLU, c(1, 2), sum, na.rm = TRUE)

        annual_sum_n2oemi <- annual_sum_n2oemi*30*365/360 #kgN/grid

        lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)   # 長さ 720
        lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)   # 長さ 360

        lon_col <- rep(lon_vec, times = length(lat_vec))         # 720 を 360 回
        lat_col <- rep(lat_vec, each  = length(lon_vec))         # 各latにつき 720 lon

        df <- data.frame(
          lon = lon_col,
          lat = lat_col,
          Value = as.vector(annual_sum_n2oemi)
        )

        annual_n2oemi <- df %>%
          mutate(Value = Value/1000000) %>% #Gg/yr
          select(lon, lat, Value) %>%
          left_join(df_fractional, by = c("lon", "lat")) %>%
          filter(!is.na(frac)) %>%
          mutate(Value = Value*frac) %>%
          group_by(iso3) %>%
          summarise(Value = sum(Value)) %>%
          rename(ISO3_code = iso3)

        #窒素収支モデル
        nc_nat <- nc_open("visit_20250820_atmosclus_geos_TEST_n2o_ngas_natonly_rev.nc") #kg/m2/s
        nc_agr <- nc_open("visit_20250820_atmosclus_geos_TEST_n2o_ngas_agronly_rev.nc") 

        #VISIT natural
        fNleach <- ncvar_get(nc_nat, "n2o_ngas")

        time_vals <- ncvar_get(nc_nat, "time")
        time_units <- ncatt_get(nc_nat, "time", "units")$value

        origin_year <- 1850
        years <- origin_year + (time_vals / 12)
        target_idx <- which(years >= 2010 & years < 2011) #Institute Variables

        fNleach_slice <- ncvar_get(nc_nat, "n2o_ngas",
                                  start = c(1, 1, 1, min(target_idx)),  #PFT//natural or agricultural//
                                  count = c(720, 360, 1, length(target_idx)))

        lon <- ncvar_get(nc_nat, "lon")
        lat <- ncvar_get(nc_nat, "lat")

        df_list <- lapply(1:length(target_idx), function(ti){
          year_val <- years[target_idx[ti]]
          df <- as.data.frame.table(fNleach_slice[,,ti], responseName = "n2o_ngas")
          colnames(df) <- c("lon_idx", "lat_idx", "n2o_ngas")
          df <- df %>%
            mutate(
              lon = lon[as.integer(lon_idx)],
              lat = lat[as.integer(lat_idx)],
              year = year_val
            ) %>%
            select(year, lon, lat, n2o_ngas)
          return(df)
        })

        fNleach_df <- bind_rows(df_list)

        #VISIT agricultural
        fNleach2 <- ncvar_get(nc_agr, "n2o_ngas")

        time_vals2 <- ncvar_get(nc_agr, "time")
        time_units2 <- ncatt_get(nc_agr, "time", "units")$value

        years <- origin_year + (time_vals2 / 12)
        target_idx2 <- which(years >= 2010 & years < 2011) #Institute Variables

        fNleach_slice2 <- ncvar_get(nc_agr, "n2o_ngas",
                                  start = c(1, 1, 2, min(target_idx2)),  #PFT//natural or agricultural//
                                  count = c(720, 360, 1, length(target_idx2)))

        lon2 <- ncvar_get(nc_agr, "lon")
        lat2 <- ncvar_get(nc_agr, "lat")

        df_list2 <- lapply(1:length(target_idx2), function(ti){
          year_val2 <- years[target_idx[ti]]
          df2 <- as.data.frame.table(fNleach_slice2[,,ti], responseName = "n2o_ngas")
          colnames(df2) <- c("lon_idx", "lat_idx", "n2o_ngas")
          df2 <- df2 %>%
            mutate(
              lon = lon2[as.integer(lon_idx)],
              lat = lat2[as.integer(lat_idx)],
              year = year_val2
            ) %>%
            select(year, lon, lat, n2o_ngas)
          return(df2)
        })

        fNleach_df2 <- bind_rows(df_list2)

        #Natural only and agricultural only
        AIMPLUM_BaU <- "results_SSP2_BaU_NoCC.gdx" #input data scenario
        AIMPLUM_BaU_data <- gdxrrw::rgdx(AIMPLUM_BaU)
        VY_load <- rgdx.param(AIMPLUM_BaU, 'VY_load') %>%
          rename(G = i4)


        GA <- rgdx.param(data_prep, 'GA') #kha
        MAP_GIJ <- rgdx.set(data_prep, 'MAP_GIJ') %>%
          rename(lat = I) %>%
          rename(lon = J)

        VY_load2 <- VY_load %>%
          left_join(GA, by = "G") %>%
          mutate(LUAREA = value*GA*1000) #ha

        LUAREA_Region <- VY_load2 %>%
          group_by(i1, i2, i3) %>%
          summarise(sum_LUAREA = sum(LUAREA))

        VY_load2 <- VY_load2 %>%
          left_join(LUAREA_Region, by = c("i1", "i2", "i3")) %>%
          mutate(LURATIO = LUAREA/sum_LUAREA) %>%
          left_join(MAP_GIJ, by = c("G"))

        VY_load2$lat <- as.numeric(as.character(VY_load2$lat))
        VY_load2$lon <- as.numeric(as.character(VY_load2$lon))
        VY_load2$i2 <- as.numeric(as.character(VY_load2$i2))

        VY_load3 <- VY_load2 %>%
          filter(i2 == "2010") %>% #Year
          filter(i3 %in% c("AFR", "OL", "FRS", "OTH_AIR", "OTH_ARF")) %>%
          select(i3, LUAREA, lat, lon) %>% #ha
          group_by(lat, lon) %>%
          summarise(LU_nat = sum(LUAREA)) %>%
          mutate(lon = 0.5*lon - 180.25, lat = -0.5*lat + 90.25) 

        VY_load4 <- VY_load2 %>%
          filter(i2 == "2010") %>% #Year
          filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A", "PAS")) %>%
          select(i3, LUAREA, lat, lon) %>% #ha
          group_by(lat, lon) %>%
          summarise(LU_agr = sum(LUAREA)) %>%
          mutate(LU_agr = ifelse(LU_agr < 0, 0, LU_agr)) %>%
          mutate(lon = 0.5*lon - 180.25, lat = -0.5*lat + 90.25) 

        VISIT_nat <- fNleach_df %>%
          full_join(VY_load3, by = c("lon", "lat")) %>%
          mutate(
            n2o_ngas = replace_na(n2o_ngas, 0),
            LU_nat = replace_na(LU_nat, 0)
          ) %>%
          mutate(n2o_ngas_grid = n2o_ngas*LU_nat*10000*3600*24) %>% #kgN/grid/day
          select(year, lon, lat, n2o_ngas_grid)

        VISIT_agr <- fNleach_df2 %>%
          full_join(VY_load4, by = c("lon", "lat")) %>%
          mutate(
            n2o_ngas = replace_na(n2o_ngas, 0),
            LU_agr = replace_na(LU_agr, 0)
          ) %>%
          mutate(n2o_ngas_grid2 = n2o_ngas*LU_agr*10000*3600*24) %>% #kgN/grid/day
          select(year, lon, lat, n2o_ngas_grid2)

        VISIT_nat_agr <- VISIT_nat %>%
          left_join(VISIT_agr, by = c("year", "lon", "lat")) %>%
          mutate(n2o_ngas = n2o_ngas_grid + n2o_ngas_grid2) %>%
          select(year, lon, lat, n2o_ngas)

        VISITn2oemi_nat_ag_PLUMLU <- array(0, dim = c(720, 360, 12)) #array name
        year_vals <- sort(unique(VISIT_nat_agr$year))

        VISIT_nat_agr <- VISIT_nat_agr %>%
          mutate(
            lon_idx = round((lon + 179.75) / 0.5) + 1,
            lat_idx = round((89.75 - lat) / 0.5) + 1,
            month_idx = match(year, year_vals)
          )

        apply(VISIT_nat_agr, 1, function(row) {
          VISITn2oemi_nat_ag_PLUMLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Nleach"]
        })

        save(VISITn2oemi_nat_ag_PLUMLU, file = "Data_pd/VISITn2oemi_Nbud_geos_PLUMLU.rdat")


        #Make nitrogen budget model n2oemi array
        #Make VISIToRN input (n2oemi)
        load("Data_pd/OUTgasdis_n2o.rdat")
        OUTgasn2o_ag <- OUTgasdis_N2O %>%
          mutate(OUTgasgrid = OUTgasgrid*1000000000/365.25) %>% #kgN/grid/day
          left_join(MAP_GIJ, by = "G") %>%
          mutate(
            lon = as.numeric(as.character(lon)),
            lat = as.numeric(as.character(lat))
          ) %>%
          mutate(lon = 0.5*lon - 180.25, lat = -0.5*lat + 90.25) %>%
          filter(YEMF == "2010") %>% #Year
          select(OUTgasgrid, lat, lon)

        VISIT_Nbud <- VISIT_nat %>%
          left_join(OUTgasn2o_ag, by = c("lat", "lon")) %>%
          replace_na(list(
            n2o_ngas_grid = 0,
            OUTgasgrid = 0
          )) %>%
          mutate(
            n2o_ngas_grid = n2o_ngas_grid + OUTgasgrid,
            n2o_ngas_grid = ifelse(n2o_ngas_grid < 0, 0, n2o_ngas_grid)  
          ) %>%
          select(year, lon, lat, n2o_ngas_grid)

        VISITn2o_ngas_grid_nat_ag_Nbud_PLUMLU2010 <- array(0, dim = c(720, 360, 12)) #array name
        year_vals <- sort(unique(VISIT_Nbud$year))

        VISIT_Nbud <- VISIT_Nbud %>%
          mutate(
            lon_idx = round((lon + 179.75) / 0.5) + 1,
            lat_idx = round((89.75 - lat) / 0.5) + 1,
            month_idx = match(year, year_vals)
          )

        apply(VISIT_Nbud, 1, function(row) {
          VISITn2o_ngas_grid_nat_ag_Nbud_PLUMLU2010[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["n2o_ngas_grid"]
        })

        save(VISITn2o_ngas_grid_nat_ag_Nbud_PLUMLU2010, file = "Data_pd/VISITn2o_ngas_grid_Nbud_geos_PLUMLU2010.rdat")


        annual_sum <- apply(VISITn2o_ngas_grid_nat_ag_Nbud_PLUMLU2010, c(1, 2), sum, na.rm = TRUE)

        annual_sum <- annual_sum*30*365/360/1000000 #GgN/grid

        lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)   # 長さ 720
        lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)   # 長さ 360

        lon_col <- rep(lon_vec, times = length(lat_vec))         # 720 を 360 回
        lat_col <- rep(lat_vec, each  = length(lon_vec))         # 各latにつき 720 lon

        df <- data.frame(
          lon = lon_col,
          lat = lat_col,
          Value = as.vector(annual_sum)
        )

        annual_n2o_ngas_grid2 <- df %>%
          select(lon, lat, Value) %>%
          left_join(df_fractional, by = c("lon", "lat")) %>%
          filter(!is.na(frac)) %>%
          mutate(Value = Value*frac) %>%
          group_by(iso3) %>%
          summarise(Value = sum(Value)) %>%
          rename(ISO3_code = iso3)

        #2つを比較
        n_top <- 20
        ranking_source <- "sum"   # "sum" / "emi" / "vlt"

        # --- (任意) 年がある場合だけ固定したい年を指定 ---
        # if ("Year" %in% names(annual_n2oemi))          annual_n2oemi          <- filter(annual_n2oemi, Year == 2010)
        # if ("Year" %in% names(annual_n_n2ovlt_grid2))  annual_n_n2ovlt_grid2  <- filter(annual_n_n2ovlt_grid2, Year == 2010)

        # 1) クレンジング＆国別集計
        emi <- annual_n2oemi %>%
          transmute(ISO3_code = str_to_upper(str_trim(as.character(ISO3_code))),
                    emi = as.numeric(Value)) %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%
          group_by(ISO3_code) %>%
          summarise(emi = sum(emi, na.rm = TRUE), .groups = "drop")

        vlt <- annual_n2o_ngas_grid2 %>%
          transmute(ISO3_code = str_to_upper(str_trim(as.character(ISO3_code))),
                    vlt = as.numeric(Value)) %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%
          group_by(ISO3_code) %>%
          summarise(vlt = sum(vlt, na.rm = TRUE), .groups = "drop")

        # 2) 結合＆順位付け
        dfc <- emi %>%
          full_join(vlt, by = "ISO3_code") %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%      # 保険
          mutate(across(c(emi, vlt), ~replace_na(., 0))) %>%
          mutate(
            rank_sum = emi + vlt,
            rank_emi = emi,
            rank_vlt = vlt
          )

        rank_col <- switch(ranking_source,
          "emi" = "rank_emi",
          "vlt" = "rank_vlt",
          "sum" = "rank_sum"
        )

        top_iso3 <- dfc %>%
          filter(.data[[rank_col]] > 0) %>%
          slice_max(order_by = .data[[rank_col]], n = n_top, with_ties = FALSE) %>%
          pull(ISO3_code)

        # 3) ロング化（NA除去）
        df_long <- dfc %>%
          filter(ISO3_code %in% top_iso3) %>%
          arrange(desc(.data[[rank_col]])) %>%
          mutate(ISO3_code = factor(ISO3_code, levels = ISO3_code)) %>%
          pivot_longer(cols = c(emi, vlt),
                      names_to = "source", values_to = "value") %>%
          mutate(source = recode(source,
                                emi = "VISIT",
                                vlt = "VISIT + N budget model")) %>%
          drop_na(ISO3_code, value)

        # --- (任意) 単位統一：例) kg → Tg にしたい場合 ---
        # unit_div <- 1e9
        # df_long <- df_long %>% mutate(value = value / unit_div)

        # 4) 描画
        ggplot(df_long, aes(x = ISO3_code, y = value, fill = source)) +
          geom_col(position = position_dodge(width = 0.8), width = 0.72) +
          scale_y_continuous(labels = label_comma()) +
          labs(
            title = sprintf("上位%dか国の比較：n2o 排出 vs n2o 揮散", n_top),
            x = "国（ISO3コード）",
            y = "量（元データの単位）",
            fill = "系列"
          ) +
          theme_minimal(base_size = 12) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))


#n2 国別比較
#VISIT
       load("Data_pd/VISITnn2_ngas_VISITLU.rdat")  # [720,360,12], kgN/grid/day

annual_sum_n2emi <- apply(VISITnn2_ngas_VISITLU, c(1, 2), sum, na.rm = TRUE)
annual_sum_n2emi <- annual_sum_n2emi * 30 * 365 / 360  # kgN/grid/yr（擬似日数換算）

lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)
lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)

lon_col <- rep(lon_vec, times = length(lat_vec))
lat_col <- rep(lat_vec, each  = length(lon_vec))

df <- data.frame(
  lon   = lon_col,
  lat   = lat_col,
  Value = as.vector(annual_sum_n2emi)
)

# df_fractional: 既存のグリッド→国按分テーブル（列: lon, lat, frac, iso3）
annual_n2emi <- df %>%
  mutate(Value = Value / 1e6) %>%                # kg → Gg
  select(lon, lat, Value) %>%
  left_join(df_fractional, by = c("lon", "lat")) %>%
  filter(!is.na(frac)) %>%
  mutate(Value = Value * frac) %>%
  group_by(iso3) %>%
  summarise(Value = sum(Value), .groups = "drop") %>%
  rename(ISO3_code = iso3)

## ------------------------------------------------------------
## 1) 窒素収支モデル（NetCDF: n2_ngas, kg/m2/s）× AIM-PLUM 面積(ha)
##    → VISIT自然 + PLUM農地 を合算し配列化（kgN/grid/day）
## ------------------------------------------------------------
nc_nat <- nc_open("visit_20250820_atmosclus_geos_TEST_n2_ngas_natonly.nc")  # kg/m2/s
nc_agr <- nc_open("visit_20250820_atmosclus_geos_TEST_n2_ngas_agronly.nc")  # kg/m2/s

# 時間インデックス（2010年12ヶ月）
time_vals  <- ncvar_get(nc_nat, "time")
origin_year <- 1850
years       <- origin_year + (time_vals / 12)
target_idx  <- which(years >= 2010 & years < 2011)

# 自然（pft=1）
n2_ngas_slice_nat <- ncvar_get(
  nc_nat, "n2_ngas",
  start = c(1, 1, 1, min(target_idx)),
  count = c(720, 360, 1, length(target_idx))
)
lon <- ncvar_get(nc_nat, "lon")
lat <- ncvar_get(nc_nat, "lat")

df_list_nat <- lapply(seq_along(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  tmp <- as.data.frame.table(n2_ngas_slice_nat[,,ti], responseName = "n2_ngas")
  colnames(tmp) <- c("lon_idx", "lat_idx", "n2_ngas")
  tmp %>%
    mutate(
      lon  = lon[as.integer(lon_idx)],
      lat  = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, n2_ngas)
})
n2_ngas_df_nat <- bind_rows(df_list_nat)

# 農地（pft=2）
time_vals2  <- ncvar_get(nc_agr, "time")
years2      <- origin_year + (time_vals2 / 12)
target_idx2 <- which(years2 >= 2010 & years2 < 2011)

n2_ngas_slice_agr <- ncvar_get(
  nc_agr, "n2_ngas",
  start = c(1, 1, 2, min(target_idx2)),
  count = c(720, 360, 1, length(target_idx2))
)
lon2 <- ncvar_get(nc_agr, "lon")
lat2 <- ncvar_get(nc_agr, "lat")

df_list_agr <- lapply(seq_along(target_idx2), function(ti){
  year_val2 <- years2[target_idx2[ti]]
  tmp <- as.data.frame.table(n2_ngas_slice_agr[,,ti], responseName = "n2_ngas")
  colnames(tmp) <- c("lon_idx", "lat_idx", "n2_ngas")
  tmp %>%
    mutate(
      lon  = lon2[as.integer(lon_idx)],
      lat  = lat2[as.integer(lat_idx)],
      year = year_val2
    ) %>%
    select(year, lon, lat, n2_ngas)
})
n2_ngas_df_agr <- bind_rows(df_list_agr)

nc_close(nc_nat); nc_close(nc_agr)

## AIM-PLUM から面積（ha）
AIMPLUM_BaU      <- "results_SSP2_BaU_NoCC.gdx"
AIMPLUM_BaU_data <- gdxrrw::rgdx(AIMPLUM_BaU)

VY_load <- rgdx.param(AIMPLUM_BaU, 'VY_load') %>% rename(G = i4)
GA      <- rgdx.param(data_prep, 'GA')  # kha
MAP_GIJ <- rgdx.set(data_prep, 'MAP_GIJ') %>% rename(lat = I, lon = J)

VY_load2 <- VY_load %>%
  left_join(GA, by = "G") %>%
  mutate(LUAREA = value * GA * 1000)  # kha → ha
LUAREA_Region <- VY_load2 %>%
  group_by(i1, i2, i3) %>%
  summarise(sum_LUAREA = sum(LUAREA), .groups = "drop")
VY_load2 <- VY_load2 %>%
  left_join(LUAREA_Region, by = c("i1", "i2", "i3")) %>%
  mutate(LURATIO = LUAREA / sum_LUAREA) %>%
  left_join(MAP_GIJ, by = "G") %>%
  mutate(
    lat = as.numeric(as.character(lat)),
    lon = as.numeric(as.character(lon)),
    i2  = as.numeric(as.character(i2))
  )

# 自然LU（ha）
VY_load3 <- VY_load2 %>%
  filter(i2 == "2010") %>%
  filter(i3 %in% c("AFR", "OL", "FRS", "OTH_AIR", "OTH_ARF")) %>%
  select(i3, LUAREA, lat, lon) %>%
  group_by(lat, lon) %>%
  summarise(LU_nat = sum(LUAREA), .groups = "drop") %>%
  mutate(lon = 0.5 * lon - 180.25,
         lat = -0.5 * lat +  90.25)

# 農地LU（ha）
VY_load4 <- VY_load2 %>%
  filter(i2 == "2010") %>%
  filter(i3 %in% c("PDRIR","WHTIR","GROIR","OSDIR","C_BIR",
                   "PDRRF","WHTRF","GRORF","OSDRF","C_BRF",
                   "OTH_A","PAS")) %>%
  select(i3, LUAREA, lat, lon) %>%
  group_by(lat, lon) %>%
  summarise(LU_agr = sum(LUAREA), .groups = "drop") %>%
  mutate(LU_agr = ifelse(LU_agr < 0, 0, LU_agr)) %>%
  mutate(lon = 0.5 * lon - 180.25,
         lat = -0.5 * lat +  90.25)

# 単位換算: kg/m2/s × m2(=ha*10000) × sec/day(=86400) → kg/grid/day
VISIT_nat <- n2_ngas_df_nat %>%
  full_join(VY_load3, by = c("lon", "lat")) %>%
  mutate(
    n2_ngas = replace_na(n2_ngas, 0),
    LU_nat  = replace_na(LU_nat,  0)
  ) %>%
  mutate(n2_ngas_grid = n2_ngas * LU_nat * 10000 * 3600 * 24) %>%
  select(year, lon, lat, n2_ngas_grid)

VISIT_agr <- n2_ngas_df_agr %>%
  full_join(VY_load4, by = c("lon", "lat")) %>%
  mutate(
    n2_ngas = replace_na(n2_ngas, 0),
    LU_agr  = replace_na(LU_agr,  0)
  ) %>%
  mutate(n2_ngas_grid2 = n2_ngas * LU_agr * 10000 * 3600 * 24) %>%
  select(year, lon, lat, n2_ngas_grid2)

VISIT_nat_agr <- VISIT_nat %>%
  left_join(VISIT_agr, by = c("year", "lon", "lat")) %>%
  mutate(
    n2_ngas_grid  = replace_na(n2_ngas_grid,  0),
    n2_ngas_grid2 = replace_na(n2_ngas_grid2, 0),
    n2_ngas       = n2_ngas_grid + n2_ngas_grid2   # kgN/grid/day
  ) %>%
  select(year, lon, lat, n2_ngas)

# [lon,lat,month] 配列へ
VISITn2emi_nat_ag_PLUMLU <- array(0, dim = c(720, 360, 12))
year_vals <- sort(unique(VISIT_nat_agr$year))
VISIT_nat_agr <- VISIT_nat_agr %>%
  mutate(
    lon_idx   = round((lon + 179.75) / 0.5) + 1,
    lat_idx   = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )
apply(VISIT_nat_agr, 1, function(row){
  VISITn2emi_nat_ag_PLUMLU[
    row[["lon_idx"]], row[["lat_idx"]], row[["month_idx"]]
  ] <<- row[["n2_ngas"]]
})
save(VISITn2emi_nat_ag_PLUMLU, file = "Data_pd/VISITn2emi_Nbud_geos_PLUMLU.rdat")

## ------------------------------------------------------------
## 2) VISIT自然 + （窒素収支モデル：農地N₂ガス排出 OUTgasdis_N2）で
##    N-bud版の配列と国別合計（Gg/yr）を作る
## ------------------------------------------------------------
load("Data_pd/OUTgasdis_N2.rdat")  # 想定: データフレーム OUTgasdis_N2 (G, YEMF, OUTgasgrid=TgN/yr)

# 読み込まれたオブジェクト名に対応（保険）
if (exists("OUTgasdis_N2")) {
  OUTgasN2_raw <- OUTgasdis_N2
} else if (exists("OUTgasdis")) {
  OUTgasN2_raw <- OUTgasdis
} else {
  stop("OUTgasdis_N2.rdat 内のオブジェクト名が不明です（OUTgasdis_N2 か OUTgasdis を想定）。")
}

OUTgasn2_ag <- OUTgasN2_raw %>%
  mutate(OUTgasgrid = OUTgasgrid * 1e9 / 365.25) %>%     # TgN/yr → kgN/day
  left_join(MAP_GIJ, by = "G") %>%
  mutate(
    lon = as.numeric(as.character(lon)),
    lat = as.numeric(as.character(lat))
  ) %>%
  mutate(lon = 0.5 * lon - 180.25,
         lat = -0.5 * lat + 90.25) %>%
  filter(YEMF == "2010") %>%
  select(OUTgasgrid, lat, lon)

# VISIT 自然（kgN/grid/day）に農地（N-bud, kgN/grid/day）を加算
VISIT_Nbud <- VISIT_nat %>%
  left_join(OUTgasn2_ag, by = c("lat", "lon")) %>%
  replace_na(list(n2_ngas_grid = 0, OUTgasgrid = 0)) %>%
  mutate(
    n2_ngas_grid = n2_ngas_grid + OUTgasgrid,
    n2_ngas_grid = ifelse(n2_ngas_grid < 0, 0, n2_ngas_grid)
  ) %>%
  select(year, lon, lat, n2_ngas_grid)

VISITn2_ngas_grid_nat_ag_Nbud_PLUMLU2010 <- array(0, dim = c(720, 360, 12))
year_vals <- sort(unique(VISIT_Nbud$year))
VISIT_Nbud <- VISIT_Nbud %>%
  mutate(
    lon_idx   = round((lon + 179.75) / 0.5) + 1,
    lat_idx   = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )
apply(VISIT_Nbud, 1, function(row){
  VISITn2_ngas_grid_nat_ag_Nbud_PLUMLU2010[
    row[["lon_idx"]], row[["lat_idx"]], row[["month_idx"]]
  ] <<- row[["n2_ngas_grid"]]
})
save(VISITn2_ngas_grid_nat_ag_Nbud_PLUMLU2010,
     file = "Data_pd/VISITn2_ngas_grid_Nbud_geos_PLUMLU2010.rdat")

# 国別合計（Gg/yr）
annual_sum <- apply(VISITn2_ngas_grid_nat_ag_Nbud_PLUMLU2010, c(1, 2), sum, na.rm = TRUE)
annual_sum <- annual_sum * 30 * 365 / 360 / 1e6   # kg/day ×(擬似日数) → kg/yr → Gg/yr

df_grid <- data.frame(
  lon   = lon_col,
  lat   = lat_col,
  Value = as.vector(annual_sum)
)
annual_n2_ngas_grid2 <- df_grid %>%
  select(lon, lat, Value) %>%
  left_join(df_fractional, by = c("lon", "lat")) %>%
  filter(!is.na(frac)) %>%
  mutate(Value = Value * frac) %>%
  group_by(iso3) %>%
  summarise(Value = sum(Value), .groups = "drop") %>%
  rename(ISO3_code = iso3)

## ------------------------------------------------------------
## 3) VISIT vs VISIT+N-bud 比較（上位n国）
## ------------------------------------------------------------
n_top <- 20
ranking_source <- "sum"  # "emi" | "vlt" | "sum"

emi <- annual_n2emi %>%          # VISIT
  transmute(ISO3_code = str_to_upper(str_trim(as.character(ISO3_code))),
            emi = as.numeric(Value)) %>%
  filter(!is.na(ISO3_code), ISO3_code != "") %>%
  group_by(ISO3_code) %>%
  summarise(emi = sum(emi, na.rm = TRUE), .groups = "drop")

vlt <- annual_n2_ngas_grid2 %>%  # VISIT + N budget model
  transmute(ISO3_code = str_to_upper(str_trim(as.character(ISO3_code))),
            vlt = as.numeric(Value)) %>%
  filter(!is.na(ISO3_code), ISO3_code != "") %>%
  group_by(ISO3_code) %>%
  summarise(vlt = sum(vlt, na.rm = TRUE), .groups = "drop")

dfc <- emi %>%
  full_join(vlt, by = "ISO3_code") %>%
  filter(!is.na(ISO3_code), ISO3_code != "") %>%
  mutate(across(c(emi, vlt), ~replace_na(., 0))) %>%
  mutate(
    rank_sum = emi + vlt,
    rank_emi = emi,
    rank_vlt = vlt
  )

rank_col <- switch(ranking_source,
  "emi" = "rank_emi",
  "vlt" = "rank_vlt",
  "sum" = "rank_sum"
)

top_iso3 <- dfc %>%
  filter(.data[[rank_col]] > 0) %>%
  slice_max(order_by = .data[[rank_col]], n = n_top, with_ties = FALSE) %>%
  pull(ISO3_code)

df_long <- dfc %>%
  filter(ISO3_code %in% top_iso3) %>%
  arrange(desc(.data[[rank_col]])) %>%
  mutate(ISO3_code = factor(ISO3_code, levels = ISO3_code)) %>%
  pivot_longer(cols = c(emi, vlt), names_to = "source", values_to = "value") %>%
  mutate(source = recode(source,
                         emi = "VISIT",
                         vlt = "VISIT + N budget model")) %>%
  drop_na(ISO3_code, value)

ggplot(df_long, aes(x = ISO3_code, y = value, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = sprintf("上位%dか国の比較：N₂排出（VISIT と VISIT+N budget model）", n_top),
    x = "国（ISO3コード）",
    y = "N₂排出（Gg N / 年）",
    fill = "系列"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#n_leach 国別比較
        #VISIT
        load("Data_pd/VISITnl_VISITLU.rdat") #kgN/grid/day

        annual_sum_n_leach <- apply(VISITnl_VISITLU, c(1, 2), sum, na.rm = TRUE)

        annual_sum_n_leach <- annual_sum_n_leach*30*365/360 #kgN/grid

        lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)   # 長さ 720
        lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)   # 長さ 360

        lon_col <- rep(lon_vec, times = length(lat_vec))         # 720 を 360 回
        lat_col <- rep(lat_vec, each  = length(lon_vec))         # 各latにつき 720 lon

        df <- data.frame(
          lon = lon_col,
          lat = lat_col,
          Value = as.vector(annual_sum_n_leach)
        )

        annual_n_leach <- df %>%
          mutate(Value = Value/1000000) %>% #Gg/yr
          select(lon, lat, Value) %>%
          left_join(df_fractional, by = c("lon", "lat")) %>%
          filter(!is.na(frac)) %>%
          mutate(Value = Value*frac) %>%
          group_by(iso3) %>%
          summarise(Value = sum(Value)) %>%
          rename(ISO3_code = iso3)

        #窒素収支モデル
        nc_nat <- nc_open("visit_20250820_atmosclus_geos_TEST_fNleach_natonly.nc") #kg/m2/s
        nc_agr <- nc_open("visit_20250820_atmosclus_geos_TEST_fNleach_agronly.nc") 

        #VISIT natural
        n_leach <- ncvar_get(nc_nat, "n_leach")

        time_vals <- ncvar_get(nc_nat, "time")
        time_units <- ncatt_get(nc_nat, "time", "units")$value

        origin_year <- 1850
        years <- origin_year + (time_vals / 12)
        target_idx <- which(years >= 2010 & years < 2011) #Institute Variables

        n_leach_slice <- ncvar_get(nc_nat, "n_leach",
                                  start = c(1, 1, 1, min(target_idx)),  #PFT//natural or agricultural//
                                  count = c(720, 360, 1, length(target_idx)))

        lon <- ncvar_get(nc_nat, "lon")
        lat <- ncvar_get(nc_nat, "lat")

        df_list <- lapply(1:length(target_idx), function(ti){
          year_val <- years[target_idx[ti]]
          df <- as.data.frame.table(n_leach_slice[,,ti], responseName = "n_leach")
          colnames(df) <- c("lon_idx", "lat_idx", "n_leach")
          df <- df %>%
            mutate(
              lon = lon[as.integer(lon_idx)],
              lat = lat[as.integer(lat_idx)],
              year = year_val
            ) %>%
            select(year, lon, lat, n_leach)
          return(df)
        })

        n_leach_df <- bind_rows(df_list)

        #VISIT agricultural
        n_leach2 <- ncvar_get(nc_agr, "n_leach")

        time_vals2 <- ncvar_get(nc_agr, "time")
        time_units2 <- ncatt_get(nc_agr, "time", "units")$value

        years <- origin_year + (time_vals2 / 12)
        target_idx2 <- which(years >= 2010 & years < 2011) #Institute Variables

        n_leach_slice2 <- ncvar_get(nc_agr, "n_leach",
                                  start = c(1, 1, 2, min(target_idx2)),  #PFT//natural or agricultural//
                                  count = c(720, 360, 1, length(target_idx2)))

        lon2 <- ncvar_get(nc_agr, "lon")
        lat2 <- ncvar_get(nc_agr, "lat")

        df_list2 <- lapply(1:length(target_idx2), function(ti){
          year_val2 <- years[target_idx[ti]]
          df2 <- as.data.frame.table(n_leach_slice2[,,ti], responseName = "n_leach")
          colnames(df2) <- c("lon_idx", "lat_idx", "n_leach")
          df2 <- df2 %>%
            mutate(
              lon = lon2[as.integer(lon_idx)],
              lat = lat2[as.integer(lat_idx)],
              year = year_val2
            ) %>%
            select(year, lon, lat, n_leach)
          return(df2)
        })

        n_leach_df2 <- bind_rows(df_list2)

        #Natural only and agricultural only
        AIMPLUM_BaU <- "results_SSP2_BaU_NoCC.gdx" #input data scenario
        AIMPLUM_BaU_data <- gdxrrw::rgdx(AIMPLUM_BaU)
        VY_load <- rgdx.param(AIMPLUM_BaU, 'VY_load') %>%
          rename(G = i4)


        GA <- rgdx.param(data_prep, 'GA') #kha
        MAP_GIJ <- rgdx.set(data_prep, 'MAP_GIJ') %>%
          rename(lat = I) %>%
          rename(lon = J)

        VY_load2 <- VY_load %>%
          left_join(GA, by = "G") %>%
          mutate(LUAREA = value*GA*1000) #ha

        LUAREA_Region <- VY_load2 %>%
          group_by(i1, i2, i3) %>%
          summarise(sum_LUAREA = sum(LUAREA))

        VY_load2 <- VY_load2 %>%
          left_join(LUAREA_Region, by = c("i1", "i2", "i3")) %>%
          mutate(LURATIO = LUAREA/sum_LUAREA) %>%
          left_join(MAP_GIJ, by = c("G"))

        VY_load2$lat <- as.numeric(as.character(VY_load2$lat))
        VY_load2$lon <- as.numeric(as.character(VY_load2$lon))
        VY_load2$i2 <- as.numeric(as.character(VY_load2$i2))

        VY_load3 <- VY_load2 %>%
          filter(i2 == "2010") %>% #Year
          filter(i3 %in% c("AFR", "OL", "FRS", "OTH_AIR", "OTH_ARF")) %>%
          select(i3, LUAREA, lat, lon) %>% #ha
          group_by(lat, lon) %>%
          summarise(LU_nat = sum(LUAREA)) %>%
          mutate(lon = 0.5*lon - 180.25, lat = -0.5*lat + 90.25) 

        VY_load4 <- VY_load2 %>%
          filter(i2 == "2010") %>% #Year
          filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A", "PAS")) %>%
          select(i3, LUAREA, lat, lon) %>% #ha
          group_by(lat, lon) %>%
          summarise(LU_agr = sum(LUAREA)) %>%
          mutate(LU_agr = ifelse(LU_agr < 0, 0, LU_agr)) %>%
          mutate(lon = 0.5*lon - 180.25, lat = -0.5*lat + 90.25) 

        VISIT_nat <- n_leach_df %>%
          full_join(VY_load3, by = c("lon", "lat")) %>%
          mutate(
            n_leach = replace_na(n_leach, 0),
            LU_nat = replace_na(LU_nat, 0)
          ) %>%
          mutate(n_leach_grid = n_leach*LU_nat*10000*3600*24) %>% #kgN/grid/day
          select(year, lon, lat, n_leach_grid)

        VISIT_agr <- n_leach_df2 %>%
          full_join(VY_load4, by = c("lon", "lat")) %>%
          mutate(
            n_leach = replace_na(n_leach, 0),
            LU_agr = replace_na(LU_agr, 0)
          ) %>%
          mutate(n_leach_grid2 = n_leach*LU_agr*10000*3600*24) %>% #kgN/grid/day
          select(year, lon, lat, n_leach_grid2)

        VISIT_nat_agr <- VISIT_nat %>%
          left_join(VISIT_agr, by = c("year", "lon", "lat")) %>%
          mutate(n_leach = n_leach_grid + n_leach_grid2) %>%
          select(year, lon, lat, n_leach)

        VISITn_leach_nat_ag_PLUMLU <- array(0, dim = c(720, 360, 12)) #array name
        year_vals <- sort(unique(VISIT_nat_agr$year))

        VISIT_nat_agr <- VISIT_nat_agr %>%
          mutate(
            lon_idx = round((lon + 179.75) / 0.5) + 1,
            lat_idx = round((89.75 - lat) / 0.5) + 1,
            month_idx = match(year, year_vals)
          )

        apply(VISIT_nat_agr, 1, function(row) {
          VISITn_leach_nat_ag_PLUMLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["n_leach"]
        })

        save(VISITn_leach_nat_ag_PLUMLU, file = "Data_pd/VISITn_leach_Nbud_geos_PLUMLU.rdat")


        #Make nitrogen budget model n_leach array
        #Make VISIToRN input (n_leach)
        load("Data_pd/OUTleadis2010.rdat")
        OUTlean_leach_ag <- OUTleadis %>%
          mutate(OUTleagrid = OUTleagrid*1000000000/365.25) %>% #kgN/grid/day
          left_join(MAP_GIJ, by = "G") %>%
          mutate(
            lon = as.numeric(as.character(lon)),
            lat = as.numeric(as.character(lat))
          ) %>%
          mutate(lon = 0.5*lon - 180.25, lat = -0.5*lat + 90.25) %>%
          filter(YEMF == "2010") %>% #Year
          select(OUTleagrid, lat, lon)

        VISIT_Nbud <- VISIT_nat %>%
          left_join(OUTlean_leach_ag, by = c("lat", "lon")) %>%
          replace_na(list(
            n_leach_grid = 0,
            OUTleagrid = 0
          )) %>%
          mutate(
            n_leach_grid = n_leach_grid + OUTleagrid,
            n_leach_grid = ifelse(n_leach_grid < 0, 0, n_leach_grid)  
          ) %>%
          select(year, lon, lat, n_leach_grid)

        VISITn_leach_grid_nat_ag_Nbud_PLUMLU2010 <- array(0, dim = c(720, 360, 12)) #array name
        year_vals <- sort(unique(VISIT_Nbud$year))

        VISIT_Nbud <- VISIT_Nbud %>%
          mutate(
            lon_idx = round((lon + 179.75) / 0.5) + 1,
            lat_idx = round((89.75 - lat) / 0.5) + 1,
            month_idx = match(year, year_vals)
          )

        apply(VISIT_Nbud, 1, function(row) {
          VISITn_leach_grid_nat_ag_Nbud_PLUMLU2010[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["n_leach_grid"]
        })

        save(VISITn_leach_grid_nat_ag_Nbud_PLUMLU2010, file = "Data_pd/VISITn_leach_grid_Nbud_geos_PLUMLU2010.rdat")


        annual_sum <- apply(VISITn_leach_grid_nat_ag_Nbud_PLUMLU2010, c(1, 2), sum, na.rm = TRUE)

        annual_sum <- annual_sum*30*365/360/1000000 #GgN/grid

        lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)   # 長さ 720
        lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)   # 長さ 360

        lon_col <- rep(lon_vec, times = length(lat_vec))         # 720 を 360 回
        lat_col <- rep(lat_vec, each  = length(lon_vec))         # 各latにつき 720 lon

        df <- data.frame(
          lon = lon_col,
          lat = lat_col,
          Value = as.vector(annual_sum)
        )

        annual_n_leach_grid2 <- df %>%
          select(lon, lat, Value) %>%
          left_join(df_fractional, by = c("lon", "lat")) %>%
          filter(!is.na(frac)) %>%
          mutate(Value = Value*frac) %>%
          group_by(iso3) %>%
          summarise(Value = sum(Value)) %>%
          rename(ISO3_code = iso3)

        #2つを比較
        n_top <- 20
        ranking_source <- "sum"   # "sum" / "" / "vlt"

        # --- (任意) 年がある場合だけ固定したい年を指定 ---
        # if ("Year" %in% names(annual_n_leach))          annual_n_leach          <- filter(annual_n_leach, Year == 2010)
        # if ("Year" %in% names(annual_n_n_leachvlt_grid2))  annual_n_n_leachvlt_grid2  <- filter(annual_n_n_leachvlt_grid2, Year == 2010)

        # 1) クレンジング＆国別集計
        emi <- annual_n_leach %>%
          transmute(ISO3_code = str_to_upper(str_trim(as.character(ISO3_code))),
                    emi = as.numeric(Value)) %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%
          group_by(ISO3_code) %>%
          summarise(emi = sum(emi, na.rm = TRUE), .groups = "drop")

        vlt <- annual_n_leach_grid2 %>%
          transmute(ISO3_code = str_to_upper(str_trim(as.character(ISO3_code))),
                    vlt = as.numeric(Value)) %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%
          group_by(ISO3_code) %>%
          summarise(vlt = sum(vlt, na.rm = TRUE), .groups = "drop")

        # 2) 結合＆順位付け
        dfc <- emi %>%
          full_join(vlt, by = "ISO3_code") %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%      # 保険
          mutate(across(c(emi, vlt), ~replace_na(., 0))) %>%
          mutate(
            rank_sum = emi + vlt,
            rank_emi = emi,
            rank_vlt = vlt
          )

        rank_col <- switch(ranking_source,
          "emi" = "rank_",
          "vlt" = "rank_vlt",
          "sum" = "rank_sum"
        )

        top_iso3 <- dfc %>%
          filter(.data[[rank_col]] > 0) %>%
          slice_max(order_by = .data[[rank_col]], n = n_top, with_ties = FALSE) %>%
          pull(ISO3_code)

        # 3) ロング化（NA除去）
        df_long <- dfc %>%
          filter(ISO3_code %in% top_iso3) %>%
          arrange(desc(.data[[rank_col]])) %>%
          mutate(ISO3_code = factor(ISO3_code, levels = ISO3_code)) %>%
          pivot_longer(cols = c(emi, vlt),
                      names_to = "source", values_to = "value") %>%
          mutate(source = recode(source,
                                emi = "VISIT",
                                vlt = "VISIT + N budget model")) %>%
          drop_na(ISO3_code, value)

        # --- (任意) 単位統一：例) kg → Tg にしたい場合 ---
        # unit_div <- 1e9
        # df_long <- df_long %>% mutate(value = value / unit_div)

        ggplot(df_long, aes(x = ISO3_code, y = value, fill = source)) +
          geom_col(position = position_dodge(width = 0.8), width = 0.72) +
          scale_y_continuous(labels = label_comma()) +
          labs(
            title = sprintf("上位%dか国の比較：n_leach 排出 vs n_leach 揮散", n_top),
            x = "国（ISO3コード）",
            y = "量（元データの単位）",
            fill = "系列"
          ) +
          theme_minimal(base_size = 12) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))


#nleach 土地利用別国別比較
#VISIT
        load("Data_pd/VISITnl_nat_VISITLU.rdat") #kgN/grid/day

        annual_sum_n_leach <- apply(VISITnl_nat_VISITLU, c(1, 2), sum, na.rm = TRUE)

        annual_sum_n_leach <- annual_sum_n_leach*30*365/360 #kgN/grid

        lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)   # 長さ 720
        lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)   # 長さ 360

        lon_col <- rep(lon_vec, times = length(lat_vec))         # 720 を 360 回
        lat_col <- rep(lat_vec, each  = length(lon_vec))         # 各latにつき 720 lon

        df <- data.frame(
          lon = lon_col,
          lat = lat_col,
          Value = as.vector(annual_sum_n_leach)
        )

        annual_n_leach_nat <- df %>%
          mutate(Value = Value/1000000) %>% #Gg/yr
          select(lon, lat, Value) %>%
          left_join(df_fractional, by = c("lon", "lat")) %>%
          filter(!is.na(frac)) %>%
          mutate(Value = Value*frac) %>%
          group_by(iso3) %>%
          summarise(Value = sum(Value)) %>%
          rename(ISO3_code = iso3)

        load("Data_pd/VISITnl_agr_VISITLU.rdat") #kgN/grid/day

        annual_sum_n_leach <- apply(VISITnl_agr_VISITLU, c(1, 2), sum, na.rm = TRUE)

        annual_sum_n_leach <- annual_sum_n_leach*30*365/360 #kgN/grid

        lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)   # 長さ 720
        lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)   # 長さ 360

        lon_col <- rep(lon_vec, times = length(lat_vec))         # 720 を 360 回
        lat_col <- rep(lat_vec, each  = length(lon_vec))         # 各latにつき 720 lon

        df <- data.frame(
          lon = lon_col,
          lat = lat_col,
          Value = as.vector(annual_sum_n_leach)
        )

        annual_n_leach_agr <- df %>%
          mutate(Value = Value/1000000) %>% #Gg/yr
          select(lon, lat, Value) %>%
          left_join(df_fractional, by = c("lon", "lat")) %>%
          filter(!is.na(frac)) %>%
          mutate(Value = Value*frac) %>%
          group_by(iso3) %>%
          summarise(Value = sum(Value)) %>%
          rename(ISO3_code = iso3)

#統合版
nc_nat <- nc_open("visit_20250820_atmosclus_geos_TEST_fNleach_natonly.nc") # kg/m2/s
nc_agr <- nc_open("visit_20250820__geos_TEST_fNleach_agronly.nc") # kg/m2/s

# --- natural ---
n_leach_nat <- ncvar_get(nc_nat, "n_leach")

time_vals  <- ncvar_get(nc_nat, "time")
time_units <- ncatt_get(nc_nat, "time", "units")$value

origin_year <- 1850
years       <- origin_year + (time_vals / 12)        # 1850 + (month_index+0.5)/12 みたいな感じ
target_idx  <- which(years >= 2010 & years < 2011)   # 2010 年 12 ヶ月

n_leach_nat_slice <- ncvar_get(
  nc_nat, "n_leach",
  start = c(1, 1, 1, min(target_idx)),              # pft=1 (natural)
  count = c(720, 360, 1, length(target_idx))
)

lon <- ncvar_get(nc_nat, "lon")
lat <- ncvar_get(nc_nat, "lat")

df_list_nat <- lapply(seq_along(target_idx), function(ti){
  year_val <- years[target_idx[ti]]                 # 2010.xx
  df <- as.data.frame.table(n_leach_nat_slice[,,ti], responseName = "n_leach")
  colnames(df) <- c("lon_idx", "lat_idx", "n_leach")
  df %>%
    mutate(
      lon  = lon[as.integer(lon_idx)],
      lat  = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, n_leach)
})

n_leach_df_nat <- bind_rows(df_list_nat)   # 単位: kg/m2/s

# --- agricultural（VISIT 版：今回は最終的には使わず、必要なら参考用） ---
n_leach_agr <- ncvar_get(nc_agr, "n_leach")

time_vals2  <- ncvar_get(nc_agr, "time")
time_units2 <- ncatt_get(nc_agr, "time", "units")$value

years2      <- origin_year + (time_vals2 / 12)
target_idx2 <- which(years2 >= 2010 & years2 < 2011)

n_leach_agr_slice <- ncvar_get(
  nc_agr, "n_leach",
  start = c(1, 1, 2, min(target_idx2)),             # pft=2 (cropland)
  count = c(720, 360, 1, length(target_idx2))
)

lon2 <- ncvar_get(nc_agr, "lon")
lat2 <- ncvar_get(nc_agr, "lat")

df_list_agr <- lapply(seq_along(target_idx2), function(ti){
  year_val2 <- years2[target_idx2[ti]]
  df2 <- as.data.frame.table(n_leach_agr_slice[,,ti], responseName = "n_leach")
  colnames(df2) <- c("lon_idx", "lat_idx", "n_leach")
  df2 %>%
    mutate(
      lon  = lon2[as.integer(lon_idx)],
      lat  = lat2[as.integer(lat_idx)],
      year = year_val2
    ) %>%
    select(year, lon, lat, n_leach)
})

n_leach_df_agr_VISIT <- bind_rows(df_list_agr)  # （今回は参考用）


nc_close(nc_nat)
nc_close(nc_agr)


#### 2. AIM-PLUM から 2010 年の Land Use（LU_nat, LU_agr）を取得 ####

AIMPLUM_BaU      <- "results_SSP2_BaU_NoCC.gdx"  # 入力シナリオ
AIMPLUM_BaU_data <- gdxrrw::rgdx(AIMPLUM_BaU)

VY_load <- rgdx.param(AIMPLUM_BaU, 'VY_load') %>%   # 土地利用量
  rename(G = i4)

GA <- rgdx.param(data_prep, 'GA')  # kha
MAP_GIJ <- rgdx.set(data_prep, 'MAP_GIJ') %>%
  rename(lat = I, lon = J)

VY_load2 <- VY_load %>%
  left_join(GA, by = "G") %>%
  mutate(LUAREA = value * GA * 1000)   # kha → ha

LUAREA_Region <- VY_load2 %>%
  group_by(i1, i2, i3) %>%
  summarise(sum_LUAREA = sum(LUAREA), .groups = "drop")

VY_load2 <- VY_load2 %>%
  left_join(LUAREA_Region, by = c("i1", "i2", "i3")) %>%
  mutate(LURATIO = LUAREA / sum_LUAREA) %>%
  left_join(MAP_GIJ, by = "G")

VY_load2$lat <- as.numeric(as.character(VY_load2$lat))
VY_load2$lon <- as.numeric(as.character(VY_load2$lon))
VY_load2$i2  <- as.numeric(as.character(VY_load2$i2))

# 自然系 LU (ha)
VY_load3 <- VY_load2 %>%
  filter(i2 == "2010") %>%  # Year
  filter(i3 %in% c("AFR", "OL", "FRS", "OTH_AIR", "OTH_ARF")) %>%
  select(i3, LUAREA, lat, lon) %>%
  group_by(lat, lon) %>%
  summarise(LU_nat = sum(LUAREA), .groups = "drop") %>%
  mutate(
    lon = 0.5 * lon - 180.25,
    lat = -0.5 * lat +  90.25
  )

# 農地系 LU (ha)  ※ここは VISIT_agr 用だが、後で N 収支モデルの農地 flux にも使える
VY_load4 <- VY_load2 %>%
  filter(i2 == "2010") %>%  # Year
  filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR",
                   "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF",
                   "OTH_A", "PAS")) %>%
  select(i3, LUAREA, lat, lon) %>%
  group_by(lat, lon) %>%
  summarise(LU_agr = sum(LUAREA), .groups = "drop") %>%
  mutate(LU_agr = ifelse(LU_agr < 0, 0, LU_agr)) %>%
  mutate(
    lon = 0.5 * lon - 180.25,
    lat = -0.5 * lat +  90.25
  )


#### 3. VISIT 自然地 leaching を「kgN/grid/day」に変換（2010年） ####

VISIT_nat <- n_leach_df_nat %>%
  full_join(VY_load3, by = c("lon", "lat")) %>%
  mutate(
    n_leach = replace_na(n_leach, 0),   # kg/m2/s
    LU_nat  = replace_na(LU_nat,  0)    # ha
  ) %>%
  mutate(
    # kg/m2/s * ha * 10000 m2/ha * 86400 s/day = kgN/grid/day
    n_leach_nat_grid = n_leach * LU_nat * 10000 * 3600 * 24
  ) %>%
  select(year, lon, lat, n_leach_nat_grid)


#### 4. N 収支モデル（OUTleadis）から農地 leaching を取得（kgN/grid/day） ####

load("Data_pd/OUTleadis2010.rdat")  # OUTleadis: グリッド別 leach（単位は元コード準拠）

OUTlean_leach_ag <- OUTleadis %>%
  mutate(OUTleagrid = OUTleagrid * 1000000000 / 365.25) %>%  # kgN/grid/day に変換（元コメントに従う）
  left_join(MAP_GIJ, by = "G") %>%
  mutate(
    lon = as.numeric(as.character(lon)),
    lat = as.numeric(as.character(lat))
  ) %>%
  mutate(
    lon = 0.5 * lon - 180.25,
    lat = -0.5 * lat + 90.25
  ) %>%
  filter(YEMF == "2010") %>%   # Year
  select(OUTleagrid, lat, lon) # kgN/grid/day


#### 5. 自然 + 農地（N 収支）を「分けて」持つテーブルにする ####

VISIT_Nbud_split <- VISIT_nat %>%             # 自然: VISIT
  left_join(OUTlean_leach_ag, by = c("lat", "lon")) %>%  # 農地: N 収支モデル
  replace_na(list(
    n_leach_nat_grid = 0,
    OUTleagrid       = 0
  )) %>%
  mutate(
    n_leach_nat = ifelse(n_leach_nat_grid < 0, 0, n_leach_nat_grid),  # 自然 kg/grid/day
    n_leach_agr = ifelse(OUTleagrid       < 0, 0, OUTleagrid),        # 農地 kg/grid/day
    n_leach_tot = n_leach_nat + n_leach_agr
  ) %>%
  select(year, lon, lat, n_leach_nat, n_leach_agr, n_leach_tot)


#### 6. （必要なら）配列に流し込む：自然/農地/合計 ####

VISITn_leach_nat_PLUMLU  <- array(0, dim = c(720, 360, 12))
VISITn_leach_agr_PLUMLU  <- array(0, dim = c(720, 360, 12))
VISITn_leach_tot_PLUMLU  <- array(0, dim = c(720, 360, 12))

year_vals <- sort(unique(VISIT_Nbud_split$year))

VISIT_Nbud_fill <- VISIT_Nbud_split %>%
  mutate(
    lon_idx   = round((lon + 179.75) / 0.5) + 1,
    lat_idx   = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_Nbud_fill, 1, function(row){
  i <- row[["lon_idx"]]
  j <- row[["lat_idx"]]
  k <- row[["month_idx"]]
  VISITn_leach_nat_PLUMLU[i, j, k] <<- row[["n_leach_nat"]]
  VISITn_leach_agr_PLUMLU[i, j, k] <<- row[["n_leach_agr"]]
  VISITn_leach_tot_PLUMLU[i, j, k] <<- row[["n_leach_tot"]]
})

save(VISITn_leach_nat_PLUMLU,
     file = "Data_pd/VISITn_leach_nat_geos_PLUMLU2010.rdat")
save(VISITn_leach_agr_PLUMLU,
     file = "Data_pd/VISITn_leach_agr_geos_PLUMLU2010.rdat")
save(VISITn_leach_tot_PLUMLU,
     file = "Data_pd/VISITn_leach_tot_geos_PLUMLU2010.rdat")


#### 7. 年合計（kg/grid/year）→ Gg/grid/year にして国別集約（自然・農地別） ####

# 自然
annual_nat <- apply(VISITn_leach_nat_PLUMLU, c(1, 2), sum, na.rm = TRUE)
# 農地
annual_agr <- apply(VISITn_leach_agr_PLUMLU, c(1, 2), sum, na.rm = TRUE)
# 合計（チェック用）
annual_tot <- apply(VISITn_leach_tot_PLUMLU, c(1, 2), sum, na.rm = TRUE)

# kg/grid/day × 12ヶ月分を年量に変換（もともとの係数に合わせる）
conv_factor <- 30 * 365 / 360   # ほぼ 365 日分

annual_nat  <- annual_nat  * conv_factor / 1e6   # GgN/grid/year
annual_agr  <- annual_agr  * conv_factor / 1e6
annual_tot  <- annual_tot  * conv_factor / 1e6

# lon/lat ベクトル
lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)
lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)

lon_col <- rep(lon_vec, times = length(lat_vec))
lat_col <- rep(lat_vec, each  = length(lon_vec))

df_nat <- data.frame(
  lon   = lon_col,
  lat   = lat_col,
  Value = as.vector(annual_nat)
)

df_agr <- data.frame(
  lon   = lon_col,
  lat   = lat_col,
  Value = as.vector(annual_agr)
)

df_tot <- data.frame(
  lon   = lon_col,
  lat   = lat_col,
  Value = as.vector(annual_tot)
)

# --- 国別集約（df_fractional: lon, lat, frac, iso3 を利用） ---

annual_n_leach_nat_country <- df_nat %>%
  left_join(df_fractional, by = c("lon", "lat")) %>%
  filter(!is.na(frac)) %>%
  mutate(Value = Value * frac) %>%          # グリッドのうち自国部分だけ
  group_by(iso3) %>%
  summarise(Value = sum(Value), .groups = "drop") %>%  # GgN/year
  rename(ISO3_code = iso3)

annual_n_leach_agr_country <- df_agr %>%
  left_join(df_fractional, by = c("lon", "lat")) %>%
  filter(!is.na(frac)) %>%
  mutate(Value = Value * frac) %>%
  group_by(iso3) %>%
  summarise(Value = sum(Value), .groups = "drop") %>%
  rename(ISO3_code = iso3)

# 合計（自然+農地）の国別（チェック用）
annual_n_leach_tot_country <- df_tot %>%
  left_join(df_fractional, by = c("lon", "lat")) %>%
  filter(!is.na(frac)) %>%
  mutate(Value = Value * frac) %>%
  group_by(iso3) %>%
  summarise(Value = sum(Value), .groups = "drop") %>%
  rename(ISO3_code = iso3)


#litterからの無機化 国別比較
        #VISIT
        load("Data_pd/VISITn_minerlz_lttr_VISITLU.rdat") #kgN/grid/day

        annual_sum_n_leach <- apply(VISITn_minerlz_lttr_VISITLU, c(1, 2), sum, na.rm = TRUE)

        annual_sum_n_leach <- annual_sum_n_leach*30*365/360 #kgN/grid

        lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)   # 長さ 720
        lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)   # 長さ 360

        lon_col <- rep(lon_vec, times = length(lat_vec))         # 720 を 360 回
        lat_col <- rep(lat_vec, each  = length(lon_vec))         # 各latにつき 720 lon

        df <- data.frame(
          lon = lon_col,
          lat = lat_col,
          Value = as.vector(annual_sum_n_leach)
        )

        annual_n_lttr <- df %>%
          mutate(Value = Value/1000000) %>% #Gg/yr
          select(lon, lat, Value) %>%
          left_join(df_fractional, by = c("lon", "lat")) %>%
          filter(!is.na(frac)) %>%
          mutate(Value = Value*frac) %>%
          group_by(iso3) %>%
          summarise(Value = sum(Value)) %>%
          rename(ISO3_code = iso3)

        #窒素収支モデル
        nc_nat <- nc_open("visit_20250820_atmosclus_geos_TEST_n_minerlz_lttr_natonly.nc") #kg/m2/s
        nc_agr <- nc_open("visit_20250820_atmosclus_geos_TEST_n_minerlz_lttr_agronly.nc") 

         #VISIT natural
        n_minerlz_lttr <- ncvar_get(nc_nat, "n_minerlz_lttr")

        time_vals <- ncvar_get(nc_nat, "time")
        time_units <- ncatt_get(nc_nat, "time", "units")$value

        origin_year <- 1850
        years <- origin_year + (time_vals / 12)
        target_idx <- which(years >= 2010 & years < 2011) #Institute Variables

        n_minerlz_lttr_slice <- ncvar_get(nc_nat, "n_minerlz_lttr",
                                  start = c(1, 1, 1, min(target_idx)),  #PFT//natural or agricultural//
                                  count = c(720, 360, 1, length(target_idx)))

        lon <- ncvar_get(nc_nat, "lon")
        lat <- ncvar_get(nc_nat, "lat")

        df_list <- lapply(1:length(target_idx), function(ti){
          year_val <- years[target_idx[ti]]
          df <- as.data.frame.table(n_minerlz_lttr_slice[,,ti], responseName = "n_minerlz_lttr")
          colnames(df) <- c("lon_idx", "lat_idx", "n_minerlz_lttr")
          df <- df %>%
            mutate(
              lon = lon[as.integer(lon_idx)],
              lat = lat[as.integer(lat_idx)],
              year = year_val
            ) %>%
            select(year, lon, lat, n_minerlz_lttr)
          return(df)
        })

        n_minerlz_lttr_df <- bind_rows(df_list)

        #VISIT agricultural
        n_minerlz_lttr2 <- ncvar_get(nc_agr, "n_minerlz_lttr")

        time_vals2 <- ncvar_get(nc_agr, "time")
        time_units2 <- ncatt_get(nc_agr, "time", "units")$value

        years <- origin_year + (time_vals2 / 12)
        target_idx2 <- which(years >= 2010 & years < 2011) #Institute Variables

        n_minerlz_lttr_slice2 <- ncvar_get(nc_agr, "n_minerlz_lttr",
                                  start = c(1, 1, 2, min(target_idx2)),  #PFT//natural or agricultural//
                                  count = c(720, 360, 1, length(target_idx2)))

        lon2 <- ncvar_get(nc_agr, "lon")
        lat2 <- ncvar_get(nc_agr, "lat")

        df_list2 <- lapply(1:length(target_idx2), function(ti){
          year_val2 <- years[target_idx[ti]]
          df2 <- as.data.frame.table(n_minerlz_lttr_slice2[,,ti], responseName = "n_minerlz_lttr")
          colnames(df2) <- c("lon_idx", "lat_idx", "n_minerlz_lttr")
          df2 <- df2 %>%
            mutate(
              lon = lon2[as.integer(lon_idx)],
              lat = lat2[as.integer(lat_idx)],
              year = year_val2
            ) %>%
            select(year, lon, lat, n_minerlz_lttr)
          return(df2)
        })

        n_minerlz_lttr_df2 <- bind_rows(df_list2)

        #Natural only and agricultural only
        AIMPLUM_BaU <- "results_SSP2_BaU_NoCC.gdx" #input data scenario
        AIMPLUM_BaU_data <- gdxrrw::rgdx(AIMPLUM_BaU)
        VY_load <- rgdx.param(AIMPLUM_BaU, 'VY_load') %>%
          rename(G = i4)


        GA <- rgdx.param(data_prep, 'GA') #kha
        MAP_GIJ <- rgdx.set(data_prep, 'MAP_GIJ') %>%
          rename(lat = I) %>%
          rename(lon = J)

        VY_load2 <- VY_load %>%
          left_join(GA, by = "G") %>%
          mutate(LUAREA = value*GA*1000) #ha

        LUAREA_Region <- VY_load2 %>%
          group_by(i1, i2, i3) %>%
          summarise(sum_LUAREA = sum(LUAREA))

        VY_load2 <- VY_load2 %>%
          left_join(LUAREA_Region, by = c("i1", "i2", "i3")) %>%
          mutate(LURATIO = LUAREA/sum_LUAREA) %>%
          left_join(MAP_GIJ, by = c("G"))

        VY_load2$lat <- as.numeric(as.character(VY_load2$lat))
        VY_load2$lon <- as.numeric(as.character(VY_load2$lon))
        VY_load2$i2 <- as.numeric(as.character(VY_load2$i2))

        VY_load3 <- VY_load2 %>%
          filter(i2 == "2010") %>% #Year
          filter(i3 %in% c("AFR", "OL", "FRS", "OTH_AIR", "OTH_ARF")) %>%
          select(i3, LUAREA, lat, lon) %>% #ha
          group_by(lat, lon) %>%
          summarise(LU_nat = sum(LUAREA)) %>%
          mutate(lon = 0.5*lon - 180.25, lat = -0.5*lat + 90.25)

        VY_load4 <- VY_load2 %>%
          filter(i2 == "2010") %>% #Year
          filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A", "PAS")) %>%
          select(i3, LUAREA, lat, lon) %>% #ha
          group_by(lat, lon) %>%
          summarise(LU_agr = sum(LUAREA)) %>%
          mutate(LU_agr = ifelse(LU_agr < 0, 0, LU_agr)) %>%
          mutate(lon = 0.5*lon - 180.25, lat = -0.5*lat + 90.25)

        VISIT_nat <- n_minerlz_lttr_df %>%
          full_join(VY_load3, by = c("lon", "lat")) %>%
          mutate(
            n_minerlz_lttr = replace_na(n_minerlz_lttr, 0),
            LU_nat = replace_na(LU_nat, 0)
          ) %>%
          mutate(n_minerlz_lttr_grid = n_minerlz_lttr*LU_nat*10000*3600*24) %>% #kgN/grid/day
          select(year, lon, lat, n_minerlz_lttr_grid)

        VISIT_agr <- n_minerlz_lttr_df2 %>%
          full_join(VY_load4, by = c("lon", "lat")) %>%
          mutate(
            n_minerlz_lttr = replace_na(n_minerlz_lttr, 0),
            LU_agr = replace_na(LU_agr, 0)
          ) %>%
          mutate(n_minerlz_lttr_grid2 = n_minerlz_lttr*LU_agr*10000*3600*24) %>% #kgN/grid/day
          select(year, lon, lat, n_minerlz_lttr_grid2)

        VISIT_nat_agr <- VISIT_nat %>%
          left_join(VISIT_agr, by = c("year", "lon", "lat")) %>%
          mutate(n_minerlz_lttr = n_minerlz_lttr_grid + n_minerlz_lttr_grid2) %>%
          select(year, lon, lat, n_minerlz_lttr)

        VISITn_minerlz_lttr_nat_ag_PLUMLU <- array(0, dim = c(720, 360, 12)) #array name
        year_vals <- sort(unique(VISIT_nat_agr$year))

        VISIT_nat_agr <- VISIT_nat_agr %>%
          mutate(
            lon_idx = round((lon + 179.75) / 0.5) + 1,
            lat_idx = round((89.75 - lat) / 0.5) + 1,
            month_idx = match(year, year_vals)
          )

        apply(VISIT_nat_agr, 1, function(row) {
          VISITn_minerlz_lttr_nat_ag_PLUMLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["n_minerlz_lttr"]
        })

        save(VISITn_minerlz_lttr_nat_ag_PLUMLU, file = "Data_pd/VISITn_minerlz_lttr_Nbud_geos_PLUMLU.rdat")


        #Make nitrogen budget model n_minerlz_lttr array
        #Make VISIToRN input (n_minerlz_lttr)
        load("Data_pd/OUTleadis2010.rdat")
        OUTlean_minerlz_lttr_ag <- OUTleadis %>%
          mutate(OUTleagrid = 0) %>% # set to zero (kgN/grid/day)
          left_join(MAP_GIJ, by = "G") %>%
          mutate(
            lon = as.numeric(as.character(lon)),
            lat = as.numeric(as.character(lat))
          ) %>%
          mutate(lon = 0.5*lon - 180.25, lat = -0.5*lat + 90.25) %>%
          filter(YEMF == "2010") %>% #Year
          select(OUTleagrid, lat, lon)

        VISIT_Nbud <- VISIT_nat %>%
          left_join(OUTlean_minerlz_lttr_ag, by = c("lat", "lon")) %>%
          replace_na(list(
            n_minerlz_lttr_grid = 0,
            OUTleagrid = 0
          )) %>%
          mutate(
            n_minerlz_lttr_grid = n_minerlz_lttr_grid + OUTleagrid,
            n_minerlz_lttr_grid = ifelse(n_minerlz_lttr_grid < 0, 0, n_minerlz_lttr_grid)
          ) %>%
          select(year, lon, lat, n_minerlz_lttr_grid)

        VISITn_minerlz_lttr_grid_nat_ag_Nbud_PLUMLU2010 <- array(0, dim = c(720, 360, 12)) #array name
        year_vals <- sort(unique(VISIT_Nbud$year))

        VISIT_Nbud <- VISIT_Nbud %>%
          mutate(
            lon_idx = round((lon + 179.75) / 0.5) + 1,
            lat_idx = round((89.75 - lat) / 0.5) + 1,
            month_idx = match(year, year_vals)
          )

        apply(VISIT_Nbud, 1, function(row) {
          VISITn_minerlz_lttr_grid_nat_ag_Nbud_PLUMLU2010[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["n_minerlz_lttr_grid"]
        })

        save(VISITn_minerlz_lttr_grid_nat_ag_Nbud_PLUMLU2010, file = "Data_pd/VISITn_minerlz_lttr_grid_Nbud_geos_PLUMLU2010.rdat")


        annual_sum <- apply(VISITn_minerlz_lttr_grid_nat_ag_Nbud_PLUMLU2010, c(1, 2), sum, na.rm = TRUE)

        annual_sum <- annual_sum*30*365/360/1000000 #GgN/grid

        lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)   # 長さ 720
        lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)   # 長さ 360

        lon_col <- rep(lon_vec, times = length(lat_vec))         # 720 を 360 回
        lat_col <- rep(lat_vec, each  = length(lon_vec))         # 各latにつき 720 lon

        df <- data.frame(
          lon = lon_col,
          lat = lat_col,
          Value = as.vector(annual_sum)
        )

        annual_n_minerlz_lttr_grid2 <- df %>%
          select(lon, lat, Value) %>%
          left_join(df_fractional, by = c("lon", "lat")) %>%
          filter(!is.na(frac)) %>%
          mutate(Value = Value*frac) %>%
          group_by(iso3) %>%
          summarise(Value = sum(Value)) %>%
          rename(ISO3_code = iso3)

        #2つを比較
        n_top <- 20
        ranking_source <- "sum"   # "sum" / "" / "vlt"

        # --- (任意) 年がある場合だけ固定したい年を指定 ---

        # 1) クレンジング＆国別集計
        emi <- annual_n_lttr %>%
          transmute(ISO3_code = str_to_upper(str_trim(as.character(ISO3_code))),
                    emi = as.numeric(Value)) %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%
          group_by(ISO3_code) %>%
          summarise(emi = sum(emi, na.rm = TRUE), .groups = "drop")

        vlt <- annual_n_minerlz_lttr_grid2 %>%
          transmute(ISO3_code = str_to_upper(str_trim(as.character(ISO3_code))),
                    vlt = as.numeric(Value)) %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%
          group_by(ISO3_code) %>%
          summarise(vlt = sum(vlt, na.rm = TRUE), .groups = "drop")

        # 2) 結合＆順位付け
        dfc <- emi %>%
          full_join(vlt, by = "ISO3_code") %>%
          filter(!is.na(ISO3_code), ISO3_code != "") %>%      # 保険
          mutate(across(c(emi, vlt), ~replace_na(., 0))) %>%
          mutate(
            rank_sum = emi + vlt,
            rank_emi = emi,
            rank_vlt = vlt
          )

        rank_col <- switch(ranking_source,
          "emi" = "rank_",
          "vlt" = "rank_vlt",
          "sum" = "rank_sum"
        )

        top_iso3 <- dfc %>%
          filter(.data[[rank_col]] > 0) %>%
          slice_max(order_by = .data[[rank_col]], n = n_top, with_ties = FALSE) %>%
          pull(ISO3_code)

        # 3) ロング化（NA除去）
        df_long <- dfc %>%
          filter(ISO3_code %in% top_iso3) %>%
          arrange(desc(.data[[rank_col]])) %>%
          mutate(ISO3_code = factor(ISO3_code, levels = ISO3_code)) %>%
          pivot_longer(cols = c(emi, vlt),
                      names_to = "source", values_to = "value") %>%
          mutate(source = recode(source,
                                emi = "VISIT",
                                vlt = "VISIT + N budget model")) %>%
          drop_na(ISO3_code, value)

        # --- (任意) 単位統一：例) kg → Tg にしたい場合 ---
        # unit_div <- 1e9
        # df_long <- df_long %>% mutate(value = value / unit_div)

        ggplot(df_long, aes(x = ISO3_code, y = value, fill = source)) +
          geom_col(position = position_dodge(width = 0.8), width = 0.72) +
          scale_y_continuous(labels = label_comma()) +
          labs(
            title = sprintf("上位%dか国の比較：n_minerlz_lttr 排出 vs n_minerlz_lttr 揮散", n_top),
            x = "国（ISO3コード）",
            y = "量（元データの単位）",
            fill = "系列"
          ) +
          theme_minimal(base_size = 12) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))





#Deposition 国別比較
# VISIT
load("Data_pd/VISITndno3_VISITLU.rdat")  # -> VISITdepo_no3_VISITLU
load("Data_pd/VISITndnh4_VISITLU.rdat")  # -> VISITdepo_nh4_VISITLU

# 月方向に合計して年合計（日ベース）
annual_sum_no3 <- apply(VISITndno3_VISITLU, c(1, 2), sum, na.rm = TRUE)
annual_sum_nh4 <- apply(VISITndnh4_VISITLU, c(1, 2), sum, na.rm = TRUE)

# kgN/grid/day × (30*365/360 日) ≒ kgN/grid/年
annual_sum_no3 <- annual_sum_no3 * 30 * 365 / 360
annual_sum_nh4 <- annual_sum_nh4 * 30 * 365 / 360

# lon / lat ベクトル
lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)
lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)

lon_col <- rep(lon_vec, times = length(lat_vec))
lat_col <- rep(lat_vec, each  = length(lon_vec))

df_depo_VISIT <- data.frame(
  lon = lon_col,
  lat = lat_col,
  NO3 = as.vector(annual_sum_no3),   # kgN/grid/yr
  NH4 = as.vector(annual_sum_nh4)    # kgN/grid/yr
)

# NO3 + NH4 を足し合わせて国別集計（VISIT のみ）
annual_depo_VISIT <- df_depo_VISIT %>%
  mutate(Value = (NO3 + NH4) / 1e6) %>%   # kg → Gg
  select(lon, lat, Value) %>%
  left_join(df_fractional, by = c("lon", "lat")) %>%
  filter(!is.na(frac)) %>%
  mutate(Value = Value * frac) %>%
  group_by(iso3) %>%
  summarise(Value = sum(Value), .groups = "drop") %>%
  rename(ISO3_code = iso3)

#統合版
nc_nat <- nc_open("visit_20250820_atmosclus_geos_TEST_depo_no3_natonly.nc") # kgN/m2/s
nc_agr <- nc_open("visit_20250820_atmosclus_geos_TEST_depo_no3_agronly.nc") 

# VISIT natural
depo_no3_nat <- ncvar_get(nc_nat, "depo_no")

time_vals  <- ncvar_get(nc_nat, "time")
origin_year <- 1850
years       <- origin_year + (time_vals / 12)
target_idx  <- which(years >= 2010 & years < 2011) # 2010年

depo_no3_slice <- ncvar_get(
  nc_nat, "depo_no",
  start = c(1, 1, 1, min(target_idx)),  # pft=1 (natural)
  count = c(720, 360, 1, length(target_idx))
)

lon <- ncvar_get(nc_nat, "lon")
lat <- ncvar_get(nc_nat, "lat")

df_list_no3_nat <- lapply(seq_along(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(depo_no3_slice[,,ti], responseName = "depo_no")
  colnames(df) <- c("lon_idx", "lat_idx", "depo_no")
  df %>%
    mutate(
      lon  = lon[as.integer(lon_idx)],
      lat  = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, depo_no)
})

depo_no3_df_nat <- bind_rows(df_list_no3_nat)

# VISIT agricultural
depo_no3_agr <- ncvar_get(nc_agr, "depo_no")

time_vals2  <- ncvar_get(nc_agr, "time")
years2      <- origin_year + (time_vals2 / 12)
target_idx2 <- which(years2 >= 2010 & years2 < 2011)

depo_no3_slice2 <- ncvar_get(
  nc_agr, "depo_no",
  start = c(1, 1, 2, min(target_idx2)),  # pft=2 (crop)
  count = c(720, 360, 1, length(target_idx2))
)

lon2 <- ncvar_get(nc_agr, "lon")
lat2 <- ncvar_get(nc_agr, "lat")

df_list_no3_agr <- lapply(seq_along(target_idx2), function(ti){
  year_val2 <- years2[target_idx2[ti]]
  df2 <- as.data.frame.table(depo_no3_slice2[,,ti], responseName = "depo_no")
  colnames(df2) <- c("lon_idx", "lat_idx", "depo_no")
  df2 %>%
    mutate(
      lon  = lon2[as.integer(lon_idx)],
      lat  = lat2[as.integer(lat_idx)],
      year = year_val2
    ) %>%
    select(year, lon, lat, depo_no)
})

depo_no3_df_agr <- bind_rows(df_list_no3_agr)

nc_close(nc_nat)
nc_close(nc_agr)

### 2-2. VISIT depo_nh4（natonly / agronly） → グリッドの kgN/grid/day

nc_nat <- nc_open("visit_20250820_atmosclus_geos_TEST_depo_nh4_natonly.nc") # kgN/m2/s
nc_agr <- nc_open("visit_20250820_atmosclus_geos_TEST_depo_nh4_agronly.nc") 

# VISIT natural
depo_nh4_nat <- ncvar_get(nc_nat, "depo_nh4")

time_vals  <- ncvar_get(nc_nat, "time")
years      <- origin_year + (time_vals / 12)
target_idx <- which(years >= 2010 & years < 2011)

depo_nh4_slice <- ncvar_get(
  nc_nat, "depo_nh4",
  start = c(1, 1, 1, min(target_idx)),  # pft=1
  count = c(720, 360, 1, length(target_idx))
)

lon  <- ncvar_get(nc_nat, "lon")
lat  <- ncvar_get(nc_nat, "lat")

df_list_nh4_nat <- lapply(seq_along(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(depo_nh4_slice[,,ti], responseName = "depo_nh4")
  colnames(df) <- c("lon_idx", "lat_idx", "depo_nh4")
  df %>%
    mutate(
      lon  = lon[as.integer(lon_idx)],
      lat  = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, depo_nh4)
})

depo_nh4_df_nat <- bind_rows(df_list_nh4_nat)

# VISIT agricultural
depo_nh4_agr <- ncvar_get(nc_agr, "depo_nh4")

time_vals2  <- ncvar_get(nc_agr, "time")
years2      <- origin_year + (time_vals2 / 12)
target_idx2 <- which(years2 >= 2010 & years2 < 2011)

depo_nh4_slice2 <- ncvar_get(
  nc_agr, "depo_nh4",
  start = c(1, 1, 2, min(target_idx2)),  # pft=2
  count = c(720, 360, 1, length(target_idx2))
)

lon2 <- ncvar_get(nc_agr, "lon")
lat2 <- ncvar_get(nc_agr, "lat")

df_list_nh4_agr <- lapply(seq_along(target_idx2), function(ti){
  year_val2 <- years2[target_idx2[ti]]
  df2 <- as.data.frame.table(depo_nh4_slice2[,,ti], responseName = "depo_nh4")
  colnames(df2) <- c("lon_idx", "lat_idx", "depo_nh4")
  df2 %>%
    mutate(
      lon  = lon2[as.integer(lon_idx)],
      lat  = lat2[as.integer(lat_idx)],
      year = year_val2
    ) %>%
    select(year, lon, lat, depo_nh4)
})

depo_nh4_df_agr <- bind_rows(df_list_nh4_agr)

nc_close(nc_nat)
nc_close(nc_agr)

### 2-3. AIM-PLUM の LU 情報（自然 LU_nat, 農地 LU_agr）

AIMPLUM_BaU      <- "results_SSP2_BaU_NoCC.gdx"
AIMPLUM_BaU_data <- gdxrrw::rgdx(AIMPLUM_BaU)

VY_load <- rgdx.param(AIMPLUM_BaU, 'VY_load') %>%
  rename(G = i4)

GA <- rgdx.param(data_prep, 'GA')  # kha
MAP_GIJ <- rgdx.set(data_prep, 'MAP_GIJ') %>%
  rename(lat = I, lon = J)

VY_load2 <- VY_load %>%
  left_join(GA, by = "G") %>%
  mutate(LUAREA = value * GA * 1000)  # kha → ha

LUAREA_Region <- VY_load2 %>%
  group_by(i1, i2, i3) %>%
  summarise(sum_LUAREA = sum(LUAREA), .groups = "drop")

VY_load2 <- VY_load2 %>%
  left_join(LUAREA_Region, by = c("i1", "i2", "i3")) %>%
  mutate(LURATIO = LUAREA / sum_LUAREA) %>%
  left_join(MAP_GIJ, by = "G")

VY_load2$lat <- as.numeric(as.character(VY_load2$lat))
VY_load2$lon <- as.numeric(as.character(VY_load2$lon))
VY_load2$i2  <- as.numeric(as.character(VY_load2$i2))

# 自然系（土地利用）の合計（ha）
VY_load3 <- VY_load2 %>%
  filter(i2 == "2010") %>%  # Year
  filter(i3 %in% c("AFR", "OL", "FRS", "OTH_AIR", "OTH_ARF")) %>%
  select(i3, LUAREA, lat, lon) %>%
  group_by(lat, lon) %>%
  summarise(LU_nat = sum(LUAREA), .groups = "drop") %>%
  mutate(
    lon = 0.5 * lon - 180.25,
    lat = -0.5 * lat +  90.25
  )

# 農地系（土地利用）の合計（ha）
VY_load4 <- VY_load2 %>%
  filter(i2 == "2010") %>%
  filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR",
                   "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF",
                   "OTH_A", "PAS")) %>%
  select(i3, LUAREA, lat, lon) %>%
  group_by(lat, lon) %>%
  summarise(LU_agr = sum(LUAREA), .groups = "drop") %>%
  mutate(LU_agr = ifelse(LU_agr < 0, 0, LU_agr)) %>%
  mutate(
    lon = 0.5 * lon - 180.25,
    lat = -0.5 * lat +  90.25
  )

### 2-4. VISIT depo_no3, depo_nh4 × LU → kgN/grid/day

# NO3, natural
VISIT_nat_no3 <- depo_no3_df_nat %>%
  full_join(VY_load3, by = c("lon", "lat")) %>%
  mutate(
    depo_no3 = replace_na(depo_no, 0),   # kg/m2/s
    LU_nat   = replace_na(LU_nat, 0)      # ha
  ) %>%
  mutate(
    # kg/m2/s × ha × 10000 m2/ha × 3600*24 s/day = kg/grid/day
    depo_no3_grid = depo_no3 * LU_nat * 10000 * 3600 * 24
  ) %>%
  select(year, lon, lat, depo_no3_grid)

# NO3, agricultural
VISIT_agr_no3 <- depo_no3_df_agr %>%
  full_join(VY_load4, by = c("lon", "lat")) %>%
  mutate(
    depo_no3 = replace_na(depo_no, 0),
    LU_agr   = replace_na(LU_agr, 0)
  ) %>%
  mutate(
    depo_no3_grid2 = depo_no3 * LU_agr * 10000 * 3600 * 24
  ) %>%
  select(year, lon, lat, depo_no3_grid2)

# NO3, natural+agri を足した配列（必要なら）
VISIT_depono3_nat_ag_PLUMLU <- array(0, dim = c(720, 360, 12))
VISIT_nat_agr_no3 <- VISIT_nat_no3 %>%
  left_join(VISIT_agr_no3, by = c("year", "lon", "lat")) %>%
  mutate(
    depo_no3_grid  = replace_na(depo_no3_grid, 0),
    depo_no3_grid2 = replace_na(depo_no3_grid2, 0),
    depo_no3       = depo_no3_grid + depo_no3_grid2
  ) %>%
  select(year, lon, lat, depo_no3)

year_vals <- sort(unique(VISIT_nat_agr_no3$year))
VISIT_nat_agr_no3 <- VISIT_nat_agr_no3 %>%
  mutate(
    lon_idx   = round((lon + 179.75) / 0.5) + 1,
    lat_idx   = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_nat_agr_no3, 1, function(row){
  VISIT_depono3_nat_ag_PLUMLU[
    as.integer(row[["lon_idx"]]),
    as.integer(row[["lat_idx"]]),
    as.integer(row[["month_idx"]])
  ] <<- as.numeric(row[["depo_no3"]])
})

save(VISIT_depono3_nat_ag_PLUMLU,
     file = "Data_pd/VISITdepo_no3_Nbud_geos_PLUMLU.rdat")

# NH4, natural
VISIT_nat_nh4 <- depo_nh4_df_nat %>%
  full_join(VY_load3, by = c("lon", "lat")) %>%
  mutate(
    depo_nh4 = replace_na(depo_nh4, 0),
    LU_nat   = replace_na(LU_nat, 0)
  ) %>%
  mutate(
    depo_nh4_grid = depo_nh4 * LU_nat * 10000 * 3600 * 24   # kg/grid/day
  ) %>%
  select(year, lon, lat, depo_nh4_grid)

# NH4, agricultural
VISIT_agr_nh4 <- depo_nh4_df_agr %>%
  full_join(VY_load4, by = c("lon", "lat")) %>%
  mutate(
    depo_nh4 = replace_na(depo_nh4, 0),
    LU_agr   = replace_na(LU_agr, 0)
  ) %>%
  mutate(
    depo_nh4_grid2 = depo_nh4 * LU_agr * 10000 * 3600 * 24
  ) %>%
  select(year, lon, lat, depo_nh4_grid2)

VISIT_deponh4_nat_ag_PLUMLU <- array(0, dim = c(720, 360, 12))
VISIT_nat_agr_nh4 <- VISIT_nat_nh4 %>%
  left_join(VISIT_agr_nh4, by = c("year", "lon", "lat")) %>%
  mutate(
    depo_nh4_grid  = replace_na(depo_nh4_grid, 0),
    depo_nh4_grid2 = replace_na(depo_nh4_grid2, 0),
    depo_nh4       = depo_nh4_grid + depo_nh4_grid2
  ) %>%
  select(year, lon, lat, depo_nh4)

year_vals <- sort(unique(VISIT_nat_agr_nh4$year))
VISIT_nat_agr_nh4 <- VISIT_nat_agr_nh4 %>%
  mutate(
    lon_idx   = round((lon + 179.75) / 0.5) + 1,
    lat_idx   = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_nat_agr_nh4, 1, function(row){
  VISIT_deponh4_nat_ag_PLUMLU[
    as.integer(row[["lon_idx"]]),
    as.integer(row[["lat_idx"]]),
    as.integer(row[["month_idx"]])
  ] <<- as.numeric(row[["depo_nh4"]])
})

save(VISIT_deponh4_nat_ag_PLUMLU,
     file = "Data_pd/VISITdepo_nh4_Nbud_geos_PLUMLU.rdat")

### 2-5. 自然部分（NO3+NH4）を合わせた VISIT_nat ＋ INdep で N budget モデルと統合

# 2つの VISIT_nat（NO3, NH4）を合わせる
VISIT_nat <- VISIT_nat_no3 %>%
  left_join(VISIT_nat_nh4, by = c("year", "lon", "lat")) %>%
  mutate(
    depo_no3_grid = replace_na(depo_no3_grid, 0),
    depo_nh4_grid = replace_na(depo_nh4_grid, 0),
    depo          = depo_no3_grid + depo_nh4_grid   # kgN/grid/day
  ) %>%
  select(year, lon, lat, depo)

# 窒素収支モデル: INdepdis.rdat を読み込み (OUTcrop+OUTres 相当)
load("Data_pd/INdepdis.rdat")
# ここでは列名を G, YEMF, INdepgrid と仮定しています。
# 実データに合わせて INdepgrid, YEMF などの列名を修正してください。

INdep_ag <- INdepdis %>%
  mutate(INdepgrid = N_deposition_grid * 1e9 / 365.25) %>%  # GgN/yr → kgN/day
  left_join(MAP_GIJ, by = "G") %>%
  mutate(
    lon = as.numeric(as.character(lon)),
    lat = as.numeric(as.character(lat))
  ) %>%
  mutate(
    lon = 0.5 * lon - 180.25,
    lat = -0.5 * lat + 90.25
  ) %>%
  filter(YEMF == "2010") %>%        # Year 列名は実データに合わせてください
  select(INdepgrid, lon, lat)

# VISIT_nat + INdep を足し合わせて N budget 版の沈着フラックスを構築
VISIT_Nbud <- VISIT_nat %>%
  left_join(INdep_ag, by = c("lat", "lon")) %>%
  replace_na(list(
    depo      = 0,
    INdepgrid = 0
  )) %>%
  mutate(
    depo = depo + INdepgrid,          # kgN/grid/day
    depo = ifelse(depo < 0, 0, depo)
  ) %>%
  select(year, lon, lat, depo)

# 3D 配列 [lon,lat,month] に格納
VISITdepo_nat_ag_Nbud_PLUMLU2010 <- array(0, dim = c(720, 360, 12))
year_vals <- sort(unique(VISIT_Nbud$year))

VISIT_Nbud <- VISIT_Nbud %>%
  mutate(
    lon_idx   = round((lon + 179.75) / 0.5) + 1,
    lat_idx   = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_Nbud, 1, function(row) {
  VISITdepo_nat_ag_Nbud_PLUMLU2010[
    as.integer(row[["lon_idx"]]),
    as.integer(row[["lat_idx"]]),
    as.integer(row[["month_idx"]])
  ] <<- as.numeric(row[["depo"]])
})

save(VISITdepo_nat_ag_Nbud_PLUMLU2010,
     file = "Data_pd/VISITdepo_Nbud_geos_PLUMLU2010.rdat")

# 年合計 → GgN/grid/yr に変換
annual_sum2 <- apply(VISITdepo_nat_ag_Nbud_PLUMLU2010, c(1, 2), sum, na.rm = TRUE)
annual_sum2 <- annual_sum2 * 30 * 365 / 360 / 1e6  # kg/day → Gg/yr

lon_col <- rep(lon_vec, times = length(lat_vec))
lat_col <- rep(lat_vec, each  = length(lon_vec))

df_depo2 <- data.frame(
  lon   = lon_col,
  lat   = lat_col,
  Value = as.vector(annual_sum2)
)

annual_depo2 <- df_depo2 %>%
  select(lon, lat, Value) %>%
  left_join(df_fractional, by = c("lon", "lat")) %>%
  filter(!is.na(frac)) %>%
  mutate(Value = Value * frac) %>%
  group_by(iso3) %>%
  summarise(Value = sum(Value), .groups = "drop") %>%
  rename(ISO3_code = iso3)

########################################################
# 3. VISIT vs VISIT+N budget の国別比較（deposition）
########################################################

n_top <- 20
ranking_source <- "sum"   # "sum" / "v1" / "v2"

# 1) クレンジング＆国別集計
d1 <- annual_depo_VISIT %>%
  filter(!is.na(ISO3_code), ISO3_code != "") %>%
  group_by(ISO3_code) %>%
  summarise(v1 = sum(as.numeric(Value), na.rm = TRUE), .groups = "drop")

d2 <- annual_depo2 %>%
  filter(!is.na(ISO3_code), ISO3_code != "") %>%
  group_by(ISO3_code) %>%
  summarise(v2 = sum(as.numeric(Value), na.rm = TRUE), .groups = "drop")

# 2) 結合＆順位付け
df_d <- d1 %>%
  full_join(d2, by = "ISO3_code") %>%
  filter(!is.na(ISO3_code), ISO3_code != "") %>%
  mutate(across(c(v1, v2), ~replace_na(., 0))) %>%
  mutate(
    rank_sum = v1 + v2,
    rank_v1  = v1,
    rank_v2  = v2
  )

rank_col <- switch(ranking_source,
  "v1"  = "rank_v1",
  "v2"  = "rank_v2",
  "sum" = "rank_sum"
)

top_iso3_d <- df_d %>%
  filter(.data[[rank_col]] > 0) %>%
  slice_max(order_by = .data[[rank_col]], n = n_top, with_ties = FALSE) %>%
  pull(ISO3_code)

# 3) ロング化してプロット用整形
df_d_long <- df_d %>%
  filter(ISO3_code %in% top_iso3_d) %>%
  arrange(desc(.data[[rank_col]])) %>%
  mutate(ISO3_code = factor(ISO3_code, levels = ISO3_code)) %>%
  pivot_longer(cols = c(v1, v2),
               names_to = "source", values_to = "value") %>%
  mutate(source = recode(source,
                         v1 = "VISIT (depo_no3+depo_nh4)",
                         v2 = "VISIT + N budget (INdep)")) %>%
  drop_na(ISO3_code, value)

# 4) 描画
ggplot(df_d_long, aes(x = ISO3_code, y = value, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    title = sprintf("大気窒素沈着量 上位%dか国の比較（VISIT vs VISIT+N budget）", n_top),
    x = "国（ISO3コード）",
    y = "大気窒素沈着量 [Gg N / yr]",
    fill = "データ源"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Fixation 国別比較
# N固定の空間分布（窒素収支モデル）
load("Data_pd/INfixdis.rdat")   # オブジェクト名 INfixdis を想定

# AIM グリッド G → lon, lat の対応表
MAP_GIJ <- rgdx.set(data_prep, 'MAP_GIJ') %>%
  rename(lat = I, lon = J)

# グリッドを国ごとに分解するための情報（lon, lat, iso3, frac）
# df_fractional は既存コードと同じものを想定
# df_fractional: lon, lat, iso3, frac


## 1. グリッド別 N固定量（TgN/yr）を lon/lat に付けて GgN/yr に変換 --------

# 列名の想定：
#   G    : AIM-PLUM グリッド番号
#   YEMF : 年（2010 など）
#   FixN : N固定量 [Tg N / yr / grid]
# 実際の列名が違えば FixN の部分を置き換えてください。

INfix_grid <- INfixdis %>%
  # 対象年を絞る（例：2010年）
  filter(YEMF == "2010") %>%        # 数値なら == 2010 に変更
  # グリッド番号 → lon/lat
  left_join(MAP_GIJ, by = "G") %>%
  mutate(
    lon = as.numeric(as.character(lon)),
    lat = as.numeric(as.character(lat))
  ) %>%
  # AIM グリッドインデックスを 0.5°中心座標に変換
  mutate(
    lon = 0.5 * lon - 180.25,
    lat = -0.5 * lat + 90.25
  ) %>%
  # Tg N → Gg N に変換（1 Tg = 1000 Gg）
  mutate(FixN_Gg = N_fix * 1000) %>%   # ★ここがポイント
  select(lon, lat, FixN_Gg)


## 2. df_fractional を使って国別に集約（Gg N/yr） -------------------------

INfix_country <- INfix_grid %>%
  left_join(df_fractional, by = c("lon", "lat")) %>%
  filter(!is.na(frac), !is.na(iso3)) %>%
  mutate(Value = FixN_Gg * frac) %>%   # グリッドを国別に按分
  group_by(iso3) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  rename(ISO3_code = iso3)

# INfix_country:
#   ISO3_code : 国コード
#   Value     : その国の N固定量 [Gg N / yr]


## 3. 上位 n か国を棒グラフで可視化 ------------------------------------

n_top <- 20

plot_df <- INfix_country %>%
  filter(!is.na(ISO3_code), ISO3_code != "") %>%
  arrange(desc(Value)) %>%
  slice_head(n = n_top) %>%
  mutate(ISO3_code = factor(ISO3_code, levels = ISO3_code))

ggplot(plot_df, aes(x = ISO3_code, y = Value)) +
  geom_col() +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    title = sprintf("窒素収支モデルによる生物学的N固定量 上位%dか国", n_top),
    x     = "国（ISO3コード）",
    y     = "N固定量 [Gg N / yr]"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Humusからの無機化　国別比較
#VISIT
load("Data_pd/VISITn_minerlz_hums_VISITLU.rdat")  # 配列: [720, 360, 12]、単位 kgN/grid/day

annual_sum_hums <- apply(VISITn_minerlz_hums_VISITLU, c(1, 2), sum, na.rm = TRUE)  # 月合計(12ヶ月分)
annual_sum_hums <- annual_sum_hums * 30 * 365 / 360  # kgN/grid/年 に換算（従来同様の近似）

lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)
lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)

lon_col <- rep(lon_vec, times = length(lat_vec))
lat_col <- rep(lat_vec, each  = length(lon_vec))

df_visit <- data.frame(
  lon   = lon_col,
  lat   = lat_col,
  Value = as.vector(annual_sum_hums)
)

# VISIT（国別, Gg/yr）
annual_n_hums <- df_visit %>%
  mutate(Value = Value / 1e6) %>%  # kg → Gg
  select(lon, lat, Value) %>%
  left_join(df_fractional, by = c("lon", "lat")) %>%
  filter(!is.na(frac)) %>%
  mutate(Value = Value * frac) %>%
  group_by(iso3) %>%
  summarise(Value = sum(Value), .groups = "drop") %>%
  rename(ISO3_code = iso3)

# ---------------------------
# 2) 窒素収支モデル（nc）→ LU と掛け算して kgN/grid/day に
# ---------------------------
nc_nat <- nc_open("visit_20250820_atmosclus_geos_TEST_n_minerlz_hums_natonly.nc")  # kg/m2/s
nc_agr <- nc_open("visit_20250820_atmosclus_geos_TEST_n_minerlz_hums_agronly.nc")  # kg/m2/s

# --- natural ---
var_nat     <- "n_minerlz_hums"
time_vals   <- ncvar_get(nc_nat, "time")
origin_year <- 1850
years       <- origin_year + (time_vals / 12)
target_idx  <- which(years >= 2010 & years < 2011)

hums_slice_nat <- ncvar_get(
  nc_nat, var_nat,
  start = c(1, 1, 1, min(target_idx)),  # pft=1: natural
  count = c(720, 360, 1, length(target_idx))
)

lon <- ncvar_get(nc_nat, "lon")
lat <- ncvar_get(nc_nat, "lat")

df_list_nat <- lapply(seq_along(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(hums_slice_nat[,,ti], responseName = var_nat)
  colnames(df) <- c("lon_idx", "lat_idx", var_nat)
  df %>%
    mutate(
      lon  = lon[as.integer(lon_idx)],
      lat  = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, !!var_nat)
})
df_nat <- bind_rows(df_list_nat)

# --- agr ---
var_agr     <- "n_minerlz_hums"
time_vals2  <- ncvar_get(nc_agr, "time")
years2      <- origin_year + (time_vals2 / 12)
target_idx2 <- which(years2 >= 2010 & years2 < 2011)

hums_slice_agr <- ncvar_get(
  nc_agr, var_agr,
  start = c(1, 1, 2, min(target_idx2)),  # pft=2: cropland
  count = c(720, 360, 1, length(target_idx2))
)

lon2 <- ncvar_get(nc_agr, "lon")
lat2 <- ncvar_get(nc_agr, "lat")

df_list_agr <- lapply(seq_along(target_idx2), function(ti){
  year_val2 <- years2[target_idx2[ti]]
  df2 <- as.data.frame.table(hums_slice_agr[,,ti], responseName = var_agr)
  colnames(df2) <- c("lon_idx", "lat_idx", var_agr)
  df2 %>%
    mutate(
      lon  = lon2[as.integer(lon_idx)],
      lat  = lat2[as.integer(lat_idx)],
      year = year_val2
    ) %>%
    select(year, lon, lat, !!var_agr)
})
df_agr <- bind_rows(df_list_agr)

nc_close(nc_nat)
nc_close(nc_agr)

# ---------------------------
# 3) AIM-PLUM の LU（ha）を作成（2010年）
# ---------------------------
AIMPLUM_BaU      <- "results_SSP2_BaU_NoCC.gdx"
AIMPLUM_BaU_data <- gdxrrw::rgdx(AIMPLUM_BaU)

VY_load <- rgdx.param(AIMPLUM_BaU, 'VY_load') %>% rename(G = i4)
GA      <- rgdx.param(data_prep, 'GA')  # kha
MAP_GIJ <- rgdx.set(data_prep, 'MAP_GIJ') %>% rename(lat = I, lon = J)

VY_load2 <- VY_load %>%
  left_join(GA, by = "G") %>%
  mutate(LUAREA = value * GA * 1000)  # kha → ha

LUAREA_Region <- VY_load2 %>%
  group_by(i1, i2, i3) %>%
  summarise(sum_LUAREA = sum(LUAREA), .groups = "drop")

VY_load2 <- VY_load2 %>%
  left_join(LUAREA_Region, by = c("i1", "i2", "i3")) %>%
  mutate(LURATIO = LUAREA / sum_LUAREA) %>%
  left_join(MAP_GIJ, by = "G")

VY_load2$lat <- as.numeric(as.character(VY_load2$lat))
VY_load2$lon <- as.numeric(as.character(VY_load2$lon))
VY_load2$i2  <- as.numeric(as.character(VY_load2$i2))

# 自然系
VY_load_nat <- VY_load2 %>%
  filter(i2 == "2010") %>%
  filter(i3 %in% c("AFR", "OL", "FRS", "OTH_AIR", "OTH_ARF")) %>%
  select(i3, LUAREA, lat, lon) %>%
  group_by(lat, lon) %>%
  summarise(LU_nat = sum(LUAREA), .groups = "drop") %>%
  mutate(
    lon = 0.5 * lon - 180.25,
    lat = -0.5 * lat +  90.25
  )

# 農地系
VY_load_agr <- VY_load2 %>%
  filter(i2 == "2010") %>%
  filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR",
                   "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF",
                   "OTH_A", "PAS")) %>%
  select(i3, LUAREA, lat, lon) %>%
  group_by(lat, lon) %>%
  summarise(LU_agr = sum(LUAREA), .groups = "drop") %>%
  mutate(LU_agr = ifelse(LU_agr < 0, 0, LU_agr)) %>%
  mutate(
    lon = 0.5 * lon - 180.25,
    lat = -0.5 * lat +  90.25
  )

# ---------------------------
# 4) kgN/grid/day に変換（nc は kg/m2/s、LU は ha）
#     n (kg/m2/s) * ha * 10,000 (m2/ha) * 86400 (s/day) = kg/grid/day
# ---------------------------
# natural
VISIT_nat_hums <- df_nat %>%
  full_join(VY_load_nat, by = c("lon", "lat")) %>%
  mutate(
    n_minerlz_hums = replace_na(n_minerlz_hums, 0),
    LU_nat         = replace_na(LU_nat, 0)
  ) %>%
  mutate(n_minerlz_hums_grid = n_minerlz_hums * LU_nat * 10000 * 3600 * 24) %>%
  select(year, lon, lat, n_minerlz_hums_grid)

# agr
VISIT_agr_hums <- df_agr %>%
  full_join(VY_load_agr, by = c("lon", "lat")) %>%
  mutate(
    n_minerlz_hums = replace_na(n_minerlz_hums, 0),
    LU_agr         = replace_na(LU_agr, 0)
  ) %>%
  mutate(n_minerlz_hums_grid2 = n_minerlz_hums * LU_agr * 10000 * 3600 * 24) %>%
  select(year, lon, lat, n_minerlz_hums_grid2)

# 合算（kgN/grid/day）
VISIT_nat_agr_hums <- VISIT_nat_hums %>%
  left_join(VISIT_agr_hums, by = c("year", "lon", "lat")) %>%
  mutate(
    n_minerlz_hums_grid  = replace_na(n_minerlz_hums_grid,  0),
    n_minerlz_hums_grid2 = replace_na(n_minerlz_hums_grid2, 0),
    n_minerlz_hums       = n_minerlz_hums_grid + n_minerlz_hums_grid2
  ) %>%
  select(year, lon, lat, n_minerlz_hums)

# ---------------------------
# 5) 配列に流し込み & 保存
# ---------------------------
VISITn_minerlz_hums_nat_ag_PLUMLU <- array(0, dim = c(720, 360, 12))
year_vals <- sort(unique(VISIT_nat_agr_hums$year))

VISIT_nat_agr_hums <- VISIT_nat_agr_hums %>%
  mutate(
    lon_idx   = round((lon + 179.75) / 0.5) + 1,
    lat_idx   = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_nat_agr_hums, 1, function(row) {
  VISITn_minerlz_hums_nat_ag_PLUMLU[
    row[["lon_idx"]],
    row[["lat_idx"]],
    row[["month_idx"]]
  ] <<- row[["n_minerlz_hums"]]
})

save(VISITn_minerlz_hums_nat_ag_PLUMLU,
     file = "Data_pd/VISITn_minerlz_hums_Nbud_geos_PLUMLU.rdat")

# ---------------------------
# 6) N 収支モデル入力（必要に応じた上乗せ：ここでは 0 に固定）
# ---------------------------
load("Data_pd/OUTleadis2010.rdat")
OUTlean_hums_ag <- OUTleadis %>%
  mutate(OUTleagrid = 0) %>%  # 追加要素なし（従来踏襲）
  left_join(MAP_GIJ, by = "G") %>%
  mutate(
    lon = as.numeric(as.character(lon)),
    lat = as.numeric(as.character(lat))
  ) %>%
  mutate(
    lon = 0.5 * lon - 180.25,
    lat = -0.5 * lat + 90.25
  ) %>%
  filter(YEMF == "2010") %>%
  select(OUTleagrid, lat, lon)

VISIT_Nbud_hums <- VISIT_nat_hums %>%
  left_join(OUTlean_hums_ag, by = c("lat", "lon")) %>%
  replace_na(list(
    n_minerlz_hums_grid = 0,
    OUTleagrid          = 0
  )) %>%
  mutate(
    n_minerlz_hums_grid = n_minerlz_hums_grid + OUTleagrid,
    n_minerlz_hums_grid = ifelse(n_minerlz_hums_grid < 0, 0, n_minerlz_hums_grid)
  ) %>%
  select(year, lon, lat, n_minerlz_hums_grid)

VISITn_minerlz_hums_grid_nat_ag_Nbud_PLUMLU2010 <- array(0, dim = c(720, 360, 12))
year_vals <- sort(unique(VISIT_Nbud_hums$year))

VISIT_Nbud_hums <- VISIT_Nbud_hums %>%
  mutate(
    lon_idx   = round((lon + 179.75) / 0.5) + 1,
    lat_idx   = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_Nbud_hums, 1, function(row) {
  VISITn_minerlz_hums_grid_nat_ag_Nbud_PLUMLU2010[
    row[["lon_idx"]],
    row[["lat_idx"]],
    row[["month_idx"]]
  ] <<- row[["n_minerlz_hums_grid"]]
})

save(VISITn_minerlz_hums_grid_nat_ag_Nbud_PLUMLU2010,
     file = "Data_pd/VISITn_minerlz_hums_grid_Nbud_geos_PLUMLU2010.rdat")

# ---------------------------
# 7) 国別集計（N 収支モデル版; 年間合計 → Gg）
# ---------------------------
annual_sum <- apply(VISITn_minerlz_hums_grid_nat_ag_Nbud_PLUMLU2010, c(1, 2), sum, na.rm = TRUE)
annual_sum <- annual_sum * 30 * 365 / 360 / 1e6  # GgN/grid/年

df_vlt <- data.frame(
  lon   = lon_col,
  lat   = lat_col,
  Value = as.vector(annual_sum)
)

annual_n_minerlz_hums_grid2 <- df_vlt %>%
  select(lon, lat, Value) %>%
  left_join(df_fractional, by = c("lon", "lat")) %>%
  filter(!is.na(frac)) %>%
  mutate(Value = Value * frac) %>%
  group_by(iso3) %>%
  summarise(Value = sum(Value), .groups = "drop") %>%
  rename(ISO3_code = iso3)

# ---------------------------
# 8) 可視化（VISIT vs VISIT+N budget model）
# ---------------------------
n_top <- 20
ranking_source <- "sum"  # "sum" / "emi" / "vlt"

emi <- annual_n_hums %>%
  transmute(
    ISO3_code = str_to_upper(str_trim(as.character(ISO3_code))),
    emi       = as.numeric(Value)
  ) %>%
  filter(!is.na(ISO3_code), ISO3_code != "") %>%
  group_by(ISO3_code) %>%
  summarise(emi = sum(emi, na.rm = TRUE), .groups = "drop")

vlt <- annual_n_minerlz_hums_grid2 %>%
  transmute(
    ISO3_code = str_to_upper(str_trim(as.character(ISO3_code))),
    vlt       = as.numeric(Value)
  ) %>%
  filter(!is.na(ISO3_code), ISO3_code != "") %>%
  group_by(ISO3_code) %>%
  summarise(vlt = sum(vlt, na.rm = TRUE), .groups = "drop")

dfc <- emi %>%
  full_join(vlt, by = "ISO3_code") %>%
  filter(!is.na(ISO3_code), ISO3_code != "") %>%
  mutate(across(c(emi, vlt), ~replace_na(., 0))) %>%
  mutate(
    rank_sum = emi + vlt,
    rank_emi = emi,
    rank_vlt = vlt
  )

rank_col <- switch(ranking_source,
  "emi" = "rank_emi",
  "vlt" = "rank_vlt",
  "sum" = "rank_sum"
)

top_iso3 <- dfc %>%
  filter(.data[[rank_col]] > 0) %>%
  slice_max(order_by = .data[[rank_col]], n = n_top, with_ties = FALSE) %>%
  pull(ISO3_code)

df_long <- dfc %>%
  filter(ISO3_code %in% top_iso3) %>%
  arrange(desc(.data[[rank_col]])) %>%
  mutate(ISO3_code = factor(ISO3_code, levels = ISO3_code)) %>%
  pivot_longer(cols = c(emi, vlt), names_to = "source", values_to = "value") %>%
  mutate(source = recode(source,
                         emi = "VISIT",
                         vlt = "VISIT + N budget model")) %>%
  drop_na(ISO3_code, value)

ggplot(df_long, aes(x = ISO3_code, y = value, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = sprintf("上位%dか国の比較：n_minerlz_hums（VISIT vs VISIT+N budget）", n_top),
    x = "国（ISO3コード）",
    y = "量（Gg N / 年）",
    fill = "系列"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#不動化　国別比較
#VISIT
load("Data_pd/VISITn_immbl_VISITLU.rdat")  # 配列: [720, 360, 12], 単位 kgN/grid/day

annual_sum_immbl <- apply(VISITn_immbl_VISITLU, c(1, 2), sum, na.rm = TRUE)   # 月合計(12ヶ月)
annual_sum_immbl <- annual_sum_immbl * 30 * 365 / 360                         # kgN/grid/年

lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)
lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)

lon_col <- rep(lon_vec, times = length(lat_vec))
lat_col <- rep(lat_vec, each  = length(lon_vec))

df_visit <- data.frame(
  lon   = lon_col,
  lat   = lat_col,
  Value = as.vector(annual_sum_immbl)
)

# VISIT（国別合計, Gg N/年）
annual_n_immbl <- df_visit %>%
  mutate(Value = Value / 1e6) %>%  # kg → Gg
  select(lon, lat, Value) %>%
  left_join(df_fractional, by = c("lon", "lat")) %>%
  filter(!is.na(frac)) %>%
  mutate(Value = Value * frac) %>%
  group_by(iso3) %>%
  summarise(Value = sum(Value), .groups = "drop") %>%
  rename(ISO3_code = iso3)

# ---------------------------
# 2) 窒素収支モデル（nc）→ LU と掛け算して kgN/grid/day に
# ---------------------------
nc_nat <- nc_open("visit_20250820_atmosclus_geos_TEST_n_immbl_natonly.nc")  # kg/m2/s
nc_agr <- nc_open("visit_20250820_atmosclus_geos_TEST_n_immbl_agronly.nc")  # kg/m2/s

var_nat <- "n_immbl"
time_vals <- ncvar_get(nc_nat, "time")
origin_year <- 1850
years <- origin_year + (time_vals / 12)
target_idx <- which(years >= 2010 & years < 2011)

immbl_slice_nat <- ncvar_get(
  nc_nat, var_nat,
  start = c(1, 1, 1, min(target_idx)),   # pft=1 (natural)
  count = c(720, 360, 1, length(target_idx))
)

lon <- ncvar_get(nc_nat, "lon")
lat <- ncvar_get(nc_nat, "lat")

df_list_nat <- lapply(seq_along(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(immbl_slice_nat[,,ti], responseName = var_nat)
  colnames(df) <- c("lon_idx", "lat_idx", var_nat)
  df %>%
    mutate(
      lon  = lon[as.integer(lon_idx)],
      lat  = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, !!var_nat)
})
df_nat <- bind_rows(df_list_nat)

var_agr <- "n_immbl"
time_vals2 <- ncvar_get(nc_agr, "time")
years2 <- origin_year + (time_vals2 / 12)
target_idx2 <- which(years2 >= 2010 & years2 < 2011)

immbl_slice_agr <- ncvar_get(
  nc_agr, var_agr,
  start = c(1, 1, 2, min(target_idx2)),  # pft=2 (cropland)
  count = c(720, 360, 1, length(target_idx2))
)

lon2 <- ncvar_get(nc_agr, "lon")
lat2 <- ncvar_get(nc_agr, "lat")

df_list_agr <- lapply(seq_along(target_idx2), function(ti){
  year_val2 <- years2[target_idx2[ti]]
  df2 <- as.data.frame.table(immbl_slice_agr[,,ti], responseName = var_agr)
  colnames(df2) <- c("lon_idx", "lat_idx", var_agr)
  df2 %>%
    mutate(
      lon  = lon2[as.integer(lon_idx)],
      lat  = lat2[as.integer(lat_idx)],
      year = year_val2
    ) %>%
    select(year, lon, lat, !!var_agr)
})
df_agr <- bind_rows(df_list_agr)

nc_close(nc_nat)
nc_close(nc_agr)

# ---------------------------
# 3) AIM-PLUM の LU（ha）を作成（2010年）
# ---------------------------
AIMPLUM_BaU      <- "results_SSP2_BaU_NoCC.gdx"
AIMPLUM_BaU_data <- gdxrrw::rgdx(AIMPLUM_BaU)

VY_load <- rgdx.param(AIMPLUM_BaU, 'VY_load') %>% rename(G = i4)
GA      <- rgdx.param(data_prep, 'GA')  # kha
MAP_GIJ <- rgdx.set(data_prep, 'MAP_GIJ') %>% rename(lat = I, lon = J)

VY_load2 <- VY_load %>%
  left_join(GA, by = "G") %>%
  mutate(LUAREA = value * GA * 1000)  # kha → ha

LUAREA_Region <- VY_load2 %>%
  group_by(i1, i2, i3) %>%
  summarise(sum_LUAREA = sum(LUAREA), .groups = "drop")

VY_load2 <- VY_load2 %>%
  left_join(LUAREA_Region, by = c("i1", "i2", "i3")) %>%
  mutate(LURATIO = LUAREA / sum_LUAREA) %>%
  left_join(MAP_GIJ, by = "G")

VY_load2$lat <- as.numeric(as.character(VY_load2$lat))
VY_load2$lon <- as.numeric(as.character(VY_load2$lon))
VY_load2$i2  <- as.numeric(as.character(VY_load2$i2))

# 自然系 LU
VY_load_nat <- VY_load2 %>%
  filter(i2 == "2010") %>%
  filter(i3 %in% c("AFR", "OL", "FRS", "OTH_AIR", "OTH_ARF")) %>%
  select(i3, LUAREA, lat, lon) %>%
  group_by(lat, lon) %>%
  summarise(LU_nat = sum(LUAREA), .groups = "drop") %>%
  mutate(
    lon = 0.5 * lon - 180.25,
    lat = -0.5 * lat +  90.25
  )

# 農地系 LU
VY_load_agr <- VY_load2 %>%
  filter(i2 == "2010") %>%
  filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR",
                   "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF",
                   "OTH_A", "PAS")) %>%
  select(i3, LUAREA, lat, lon) %>%
  group_by(lat, lon) %>%
  summarise(LU_agr = sum(LUAREA), .groups = "drop") %>%
  mutate(LU_agr = ifelse(LU_agr < 0, 0, LU_agr)) %>%
  mutate(
    lon = 0.5 * lon - 180.25,
    lat = -0.5 * lat +  90.25
  )

# ---------------------------
# 4) kgN/grid/day に変換（nc は kg/m2/s、LU は ha）
#     n (kg/m2/s) * ha * 10,000 (m2/ha) * 86400 (s/day) = kg/grid/day
# ---------------------------
# natural
VISIT_nat_immbl <- df_nat %>%
  full_join(VY_load_nat, by = c("lon", "lat")) %>%
  mutate(
    n_immbl = replace_na(n_immbl, 0),
    LU_nat  = replace_na(LU_nat, 0)
  ) %>%
  mutate(n_immbl_grid = n_immbl * LU_nat * 10000 * 3600 * 24) %>%  # kgN/grid/day
  select(year, lon, lat, n_immbl_grid)

# agr
VISIT_agr_immbl <- df_agr %>%
  full_join(VY_load_agr, by = c("lon", "lat")) %>%
  mutate(
    n_immbl = replace_na(n_immbl, 0),
    LU_agr  = replace_na(LU_agr, 0)
  ) %>%
  mutate(n_immbl_grid2 = n_immbl * LU_agr * 10000 * 3600 * 24) %>%  # kgN/grid/day
  select(year, lon, lat, n_immbl_grid2)

# 合算（kgN/grid/day）
VISIT_nat_agr_immbl <- VISIT_nat_immbl %>%
  left_join(VISIT_agr_immbl, by = c("year", "lon", "lat")) %>%
  mutate(
    n_immbl_grid  = replace_na(n_immbl_grid,  0),
    n_immbl_grid2 = replace_na(n_immbl_grid2, 0),
    n_immbl       = n_immbl_grid + n_immbl_grid2
  ) %>%
  select(year, lon, lat, n_immbl)

# ---------------------------
# 5) 配列に流し込み & 保存（nat+agr）
# ---------------------------
VISITn_immbl_nat_ag_PLUMLU <- array(0, dim = c(720, 360, 12))
year_vals <- sort(unique(VISIT_nat_agr_immbl$year))

VISIT_nat_agr_immbl <- VISIT_nat_agr_immbl %>%
  mutate(
    lon_idx   = round((lon + 179.75) / 0.5) + 1,
    lat_idx   = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_nat_agr_immbl, 1, function(row) {
  VISITn_immbl_nat_ag_PLUMLU[
    row[["lon_idx"]],
    row[["lat_idx"]],
    row[["month_idx"]]
  ] <<- row[["n_immbl"]]
})

save(VISITn_immbl_nat_ag_PLUMLU,
     file = "Data_pd/VISITn_immbl_Nbud_geos_PLUMLU.rdat")

# ---------------------------
# 6) N 収支モデル入力（上乗せ要素: ここでは 0 を維持）
# ---------------------------
load("Data_pd/OUTleadis2010.rdat")
OUTlean_immbl_ag <- OUTleadis %>%
  mutate(OUTleagrid = 0) %>%  # 追加なし
  left_join(MAP_GIJ, by = "G") %>%
  mutate(
    lon = as.numeric(as.character(lon)),
    lat = as.numeric(as.character(lat))
  ) %>%
  mutate(
    lon = 0.5 * lon - 180.25,
    lat = -0.5 * lat + 90.25
  ) %>%
  filter(YEMF == "2010") %>%
  select(OUTleagrid, lat, lon)

VISIT_Nbud_immbl <- VISIT_nat_immbl %>%
  left_join(OUTlean_immbl_ag, by = c("lat", "lon")) %>%
  replace_na(list(
    n_immbl_grid = 0,
    OUTleagrid   = 0
  )) %>%
  mutate(
    n_immbl_grid = n_immbl_grid + OUTleagrid,
    n_immbl_grid = ifelse(n_immbl_grid < 0, 0, n_immbl_grid)
  ) %>%
  select(year, lon, lat, n_immbl_grid)

VISITn_immbl_grid_nat_ag_Nbud_PLUMLU2010 <- array(0, dim = c(720, 360, 12))
year_vals <- sort(unique(VISIT_Nbud_immbl$year))

VISIT_Nbud_immbl <- VISIT_Nbud_immbl %>%
  mutate(
    lon_idx   = round((lon + 179.75) / 0.5) + 1,
    lat_idx   = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_Nbud_immbl, 1, function(row) {
  VISITn_immbl_grid_nat_ag_Nbud_PLUMLU2010[
    row[["lon_idx"]],
    row[["lat_idx"]],
    row[["month_idx"]]
  ] <<- row[["n_immbl_grid"]]
})

save(VISITn_immbl_grid_nat_ag_Nbud_PLUMLU2010,
     file = "Data_pd/VISITn_immbl_grid_Nbud_geos_PLUMLU2010.rdat")

# ---------------------------
# 7) 国別集計（N 収支モデル版; 年間合計 → Gg）
# ---------------------------
annual_sum <- apply(VISITn_immbl_grid_nat_ag_Nbud_PLUMLU2010, c(1, 2), sum, na.rm = TRUE)
annual_sum <- annual_sum * 30 * 365 / 360 / 1e6  # Gg N / grid / 年

df_vlt <- data.frame(
  lon   = lon_col,
  lat   = lat_col,
  Value = as.vector(annual_sum)
)

annual_n_immbl_grid2 <- df_vlt %>%
  select(lon, lat, Value) %>%
  left_join(df_fractional, by = c("lon", "lat")) %>%
  filter(!is.na(frac)) %>%
  mutate(Value = Value * frac) %>%
  group_by(iso3) %>%
  summarise(Value = sum(Value), .groups = "drop") %>%
  rename(ISO3_code = iso3)

# ---------------------------
# 8) 可視化（VISIT vs VISIT+N budget）
# ---------------------------
n_top <- 20
ranking_source <- "sum"  # "sum" / "emi" / "vlt"

emi <- annual_n_immbl %>%
  transmute(
    ISO3_code = str_to_upper(str_trim(as.character(ISO3_code))),
    emi       = as.numeric(Value)
  ) %>%
  filter(!is.na(ISO3_code), ISO3_code != "") %>%
  group_by(ISO3_code) %>%
  summarise(emi = sum(emi, na.rm = TRUE), .groups = "drop")

vlt <- annual_n_immbl_grid2 %>%
  transmute(
    ISO3_code = str_to_upper(str_trim(as.character(ISO3_code))),
    vlt       = as.numeric(Value)
  ) %>%
  filter(!is.na(ISO3_code), ISO3_code != "") %>%
  group_by(ISO3_code) %>%
  summarise(vlt = sum(vlt, na.rm = TRUE), .groups = "drop")

dfc <- emi %>%
  full_join(vlt, by = "ISO3_code") %>%
  filter(!is.na(ISO3_code), ISO3_code != "") %>%
  mutate(across(c(emi, vlt), ~replace_na(., 0))) %>%
  mutate(
    rank_sum = emi + vlt,
    rank_emi = emi,
    rank_vlt = vlt
  )

rank_col <- switch(ranking_source,
  "emi" = "rank_emi",
  "vlt" = "rank_vlt",
  "sum" = "rank_sum"
)

top_iso3 <- dfc %>%
  filter(.data[[rank_col]] > 0) %>%
  slice_max(order_by = .data[[rank_col]], n = n_top, with_ties = FALSE) %>%
  pull(ISO3_code)

df_long <- dfc %>%
  filter(ISO3_code %in% top_iso3) %>%
  arrange(desc(.data[[rank_col]])) %>%
  mutate(ISO3_code = factor(ISO3_code, levels = ISO3_code)) %>%
  pivot_longer(cols = c(emi, vlt), names_to = "source", values_to = "value") %>%
  mutate(source = recode(source,
                         emi = "VISIT",
                         vlt = "VISIT + N budget model")) %>%
  drop_na(ISO3_code, value)

ggplot(df_long, aes(x = ISO3_code, y = value, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = sprintf("上位%dか国の比較：n_immbl（VISIT vs VISIT+N budget）", n_top),
    x = "国（ISO3コード）",
    y = "量（Gg N / 年）",
    fill = "系列"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#NH4ストック　国別比較
# --- VISIT 単体 NH4 ストック (2010-01〜2011-01, 13ヶ月) ---
load("Data_pd/VISITn_nh4_m_VISITLU.rdat")  # VISITn_nh4_m_VISITLU[720,360,13]

# 0.5 度グリッドの中心座標
lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)
lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)

lon_col <- rep(lon_vec, times = length(lat_vec))
lat_col <- rep(lat_vec, each  = length(lon_vec))

# 2010-01 (1番目) と 2011-01 (13番目) のストック（kg/grid と仮定）
val_VISIT_2010Jan <- as.vector(VISITn_nh4_m_VISITLU[ , , 1])
val_VISIT_2011Jan <- as.vector(VISITn_nh4_m_VISITLU[ , , 13])

df_VISIT <- tibble(
  lon        = lon_col,
  lat        = lat_col,
  `2010-01`  = val_VISIT_2010Jan,
  `2011-01`  = val_VISIT_2011Jan
) %>%
  mutate(delta_VISIT_kg = `2011-01` - `2010-01`)   # kg/grid

# df_fractional: (lon, lat, iso3, frac) がすでにある前提
country_VISIT_delta <- df_VISIT %>%
  left_join(df_fractional, by = c("lon", "lat")) %>%
  filter(!is.na(frac)) %>%
  mutate(delta_VISIT_kg = delta_VISIT_kg * frac) %>%   # 国面積比で按分
  group_by(iso3) %>%
  summarise(delta_VISIT_kg = sum(delta_VISIT_kg, na.rm = TRUE), .groups = "drop") %>%
  mutate(delta_VISIT_Gg = delta_VISIT_kg / 1e6) %>%   # kg → Gg
  rename(ISO3_code = iso3) %>%
  select(ISO3_code, delta_VISIT_Gg)

# --- AIM-PLUM LU をグリッドへ（肥料コードと同じ） ---
AIMPLUM_BaU      <- "results_SSP2_BaU_NoCC.gdx"
AIMPLUM_BaU_data <- gdxrrw::rgdx(AIMPLUM_BaU)

VY_load <- rgdx.param(AIMPLUM_BaU, "VY_load") %>%
  rename(G = i4)

GA      <- rgdx.param(data_prep, "GA")      # kha
MAP_GIJ <- rgdx.set(  data_prep, "MAP_GIJ") %>%
  rename(lat = I, lon = J)

VY_load2 <- VY_load %>%
  left_join(GA, by = "G") %>%
  mutate(LUAREA = value * GA * 1000)  # ha

LUAREA_Region <- VY_load2 %>%
  group_by(i1, i2, i3) %>%
  summarise(sum_LUAREA = sum(LUAREA), .groups = "drop")

VY_load2 <- VY_load2 %>%
  left_join(LUAREA_Region, by = c("i1", "i2", "i3")) %>%
  mutate(LURATIO = LUAREA / sum_LUAREA) %>%
  left_join(MAP_GIJ, by = "G")

VY_load2$lat <- as.numeric(as.character(VY_load2$lat))
VY_load2$lon <- as.numeric(as.character(VY_load2$lon))
VY_load2$i2  <- as.numeric(as.character(VY_load2$i2))

# 自然地 LU（2010 年、AFR/OL/FRS/OTH_AIR/OTH_ARF）
VY_load3 <- VY_load2 %>%
  filter(i2 == 2010) %>%   # 年 2010
  filter(i3 %in% c("AFR", "OL", "FRS", "OTH_AIR", "OTH_ARF")) %>%
  select(i3, LUAREA, lat, lon) %>%
  group_by(lat, lon) %>%
  summarise(LU_nat = sum(LUAREA), .groups = "drop") %>%
  mutate(
    LU_nat = ifelse(LU_nat < 0, 0, LU_nat),
    lon    = 0.5 * lon - 180.25,
    lat    = -0.5 * lat + 90.25
  )

  # --- natonly NH4 ストック（2010-01〜2011-01 の 13ヶ月） ---
nc_nat <- nc_open("visit_20250820_atmosclus_geos_TEST_n_nh4_m_natonly.nc")  # 単位: gN/ha/month を想定

time_vals  <- ncvar_get(nc_nat, "time")
lon_nat    <- ncvar_get(nc_nat, "lon")
lat_nat    <- ncvar_get(nc_nat, "lat")

origin_year <- 1850
years       <- origin_year + (time_vals / 12)

# 2010-01 と 2011-01 に最も近いインデックス
idx_2010_01 <- which.min(abs(years - 2010.0))
idx_2011_01 <- which.min(abs(years - 2011.0))

if (idx_2010_01 > idx_2011_01) {
  stop("time 軸の順序が想定外（2010-01 が 2011-01 より後ろ）です。")
}

target_idx <- seq(idx_2010_01, idx_2011_01)   # 連続 13 ステップのはず
nmon       <- length(target_idx)
year_vals  <- years[target_idx]

if (nmon != 13) {
  warning(sprintf("抽出月数が %d でした（想定: 13）。継続します。", nmon))
}

# PFT = 1 (natural) の NH4 ストック [gN/ha/month]
n_nh4_nat_slice <- ncvar_get(
  nc_nat, "n_nh4_m",
  start = c(1, 1, 1, min(target_idx)),   # lon, lat, pft=1, time
  count = c(length(lon_nat), length(lat_nat), 1, nmon)
)

nc_close(nc_nat)

# 13ヶ月分をデータフレーム化
df_list_nat <- lapply(seq_len(nmon), function(ti) {
  year_val <- year_vals[ti]
  df <- as.data.frame.table(n_nh4_nat_slice[,,ti], responseName = "n_nh4_m")
  colnames(df) <- c("lon_idx", "lat_idx", "n_nh4_m")
  df %>%
    mutate(
      lon  = lon_nat[as.integer(lon_idx)],
      lat  = lat_nat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, n_nh4_m)
})
n_nh4_nat_df <- bind_rows(df_list_nat)

# --- PLUM 自然地 LU と掛けて kg/grid/month に変換 ---
VISIT_nat_NH4_PLUMLU <- n_nh4_nat_df %>%
  full_join(VY_load3, by = c("lon", "lat")) %>%
  mutate(
    n_nh4_m = replace_na(n_nh4_m, 0),  # gN/ha/month と仮定
    LU_nat  = replace_na(LU_nat, 0)    # ha
  ) %>%
  mutate(
    nh4_stock_kg = n_nh4_m * LU_nat / 1000   # g/ha × ha /1000 = kg/grid/月
  ) %>%
  select(year, lon, lat, nh4_stock_kg)

# --- 2010-01 & 2011-01 のストック差分 [kg/grid] ---
year_vals_nat <- sort(unique(VISIT_nat_NH4_PLUMLU$year))
year_2010_01  <- year_vals_nat[1]
year_2011_01  <- tail(year_vals_nat, 1)

delta_nat_grid <- VISIT_nat_NH4_PLUMLU %>%
  filter(year %in% c(year_2010_01, year_2011_01)) %>%
  mutate(tag = if_else(year == year_2010_01, "2010-01", "2011-01")) %>%
  select(lon, lat, tag, nh4_stock_kg) %>%
  tidyr::pivot_wider(
    names_from  = tag,
    values_from = nh4_stock_kg,
    values_fill = 0
  ) %>%
  mutate(delta_INT_kg = `2011-01` - `2010-01`)   # kg/grid

country_INT_delta <- delta_nat_grid %>%
  left_join(df_fractional, by = c("lon", "lat")) %>%
  filter(!is.na(frac)) %>%
  mutate(delta_INT_kg = delta_INT_kg * frac) %>%   # 国面積比で按分
  group_by(iso3) %>%
  summarise(delta_INT_kg = sum(delta_INT_kg, na.rm = TRUE), .groups = "drop") %>%
  mutate(delta_INT_Gg = delta_INT_kg / 1e6) %>%    # kg → Gg
  rename(ISO3_code = iso3) %>%
  select(ISO3_code, delta_INT_Gg)

  n_top <- 20  # 上位何カ国を見るか

df_comp <- country_VISIT_delta %>%
  full_join(country_INT_delta, by = "ISO3_code") %>%
  mutate(
    delta_VISIT_Gg = replace_na(delta_VISIT_Gg, 0),
    delta_INT_Gg   = replace_na(delta_INT_Gg, 0),
    # ランキング用：大きい方の絶対値
    rank_val       = pmax(abs(delta_VISIT_Gg), abs(delta_INT_Gg))
  )

top_iso3 <- df_comp %>%
  filter(rank_val > 0) %>%
  slice_max(order_by = rank_val, n = n_top, with_ties = FALSE) %>%
  pull(ISO3_code)

df_long <- df_comp %>%
  filter(ISO3_code %in% top_iso3) %>%
  arrange(desc(rank_val)) %>%
  mutate(ISO3_code = factor(ISO3_code, levels = ISO3_code)) %>%
  pivot_longer(
    cols      = c(delta_VISIT_Gg, delta_INT_Gg),
    names_to  = "source",
    values_to = "value"
  ) %>%
  mutate(
    source = dplyr::recode(
      source,
      delta_VISIT_Gg = "VISIT only (VISITLU)",
      delta_INT_Gg   = "Integrated nat only (PLUMLU)"
    )
  )

ggplot(df_long, aes(x = ISO3_code, y = value, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    title = sprintf("NH₄ ストック変化量 (2011-01 − 2010-01, Gg N) の比較：上位 %d か国", n_top),
    x = "国（ISO3コード）",
    y = "ストック変化量（Gg N）",
    fill = "系列"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#NO3ストック　国別比較
# --- VISIT 単体 NO3 ストック (2010-01〜2011-01, 13ヶ月) ---
load("Data_pd/VISITn_no3_m_VISITLU.rdat")  # VISITn_no3_m_VISITLU[720,360,13], 単位: kg/grid と想定

# 0.5 度グリッド中心座標
lon_vec <- -179.75 + 0.5 * (seq_len(720) - 1)
lat_vec <-   89.75 - 0.5 * (seq_len(360) - 1)

lon_col <- rep(lon_vec, times = length(lat_vec))
lat_col <- rep(lat_vec, each  = length(lon_vec))

# 2010-01 (1番目) と 2011-01 (13番目) のストック（kg/grid）
val_VISIT_2010Jan_no3 <- as.vector(VISITn_no3_m_VISITLU[ , , 1])
val_VISIT_2011Jan_no3 <- as.vector(VISITn_no3_m_VISITLU[ , , 13])

df_VISIT_no3 <- tibble(
  lon        = lon_col,
  lat        = lat_col,
  `2010-01`  = val_VISIT_2010Jan_no3,
  `2011-01`  = val_VISIT_2011Jan_no3
) %>%
  mutate(delta_VISIT_no3_kg = `2011-01` - `2010-01`)   # kg/grid

country_VISIT_delta_no3 <- df_VISIT_no3 %>%
  left_join(df_fractional, by = c("lon", "lat")) %>%
  filter(!is.na(frac)) %>%
  mutate(delta_VISIT_no3_kg = delta_VISIT_no3_kg * frac) %>%   # 国面積比で按分
  group_by(iso3) %>%
  summarise(delta_VISIT_no3_kg = sum(delta_VISIT_no3_kg, na.rm = TRUE), .groups = "drop") %>%
  mutate(delta_VISIT_no3_Gg = delta_VISIT_no3_kg / 1e6) %>%    # kg → Gg
  rename(ISO3_code = iso3) %>%
  select(ISO3_code, delta_VISIT_no3_Gg)

# --- natonly NO3 ストック（2010-01〜2011-01 の 13ヶ月）---
nc_nat_no3 <- nc_open("visit_20250820_atmosclus_geos_TEST_n_no3_m_natonly.nc")  # 単位: gN/ha/month を想定

time_vals_no3 <- ncvar_get(nc_nat_no3, "time")
lon_nat_no3   <- ncvar_get(nc_nat_no3, "lon")
lat_nat_no3   <- ncvar_get(nc_nat_no3, "lat")

origin_year <- 1850
years_no3   <- origin_year + (time_vals_no3 / 12)

# 2010-01 と 2011-01 の最近傍インデックス
idx_2010_01_no3 <- which.min(abs(years_no3 - 2010.0))
idx_2011_01_no3 <- which.min(abs(years_no3 - 2011.0))

if (idx_2010_01_no3 > idx_2011_01_no3) {
  stop("time 軸の順序が想定外（2010-01 が 2011-01 より後ろ）です。")
}

target_idx_no3 <- seq(idx_2010_01_no3, idx_2011_01_no3)   # 連続 13 ステップ
nmon_no3       <- length(target_idx_no3)
year_vals_no3  <- years_no3[target_idx_no3]

if (nmon_no3 != 13) {
  warning(sprintf("NO3 抽出月数が %d でした（想定: 13）。継続します。", nmon_no3))
}

# PFT = 1 (natural) の NO3 ストック [gN/ha/month]
n_no3_nat_slice <- ncvar_get(
  nc_nat_no3, "n_no3_m",
  start = c(1, 1, 1, min(target_idx_no3)),   # lon, lat, pft=1, time
  count = c(length(lon_nat_no3), length(lat_nat_no3), 1, nmon_no3)
)

nc_close(nc_nat_no3)

# 13ヶ月分をデータフレーム化
df_list_nat_no3 <- lapply(seq_len(nmon_no3), function(ti) {
  year_val <- year_vals_no3[ti]
  df <- as.data.frame.table(n_no3_nat_slice[,,ti], responseName = "n_no3_m")
  colnames(df) <- c("lon_idx", "lat_idx", "n_no3_m")
  df %>%
    mutate(
      lon  = lon_nat_no3[as.integer(lon_idx)],
      lat  = lat_nat_no3[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, n_no3_m)
})
n_no3_nat_df <- bind_rows(df_list_nat_no3)

# --- PLUM 自然地 LU と掛けて kg/grid/month に変換 ---
# 単位仮定: n_no3_m [gN/ha/month], LU_nat [ha]
VISIT_nat_NO3_PLUMLU <- n_no3_nat_df %>%
  full_join(VY_load3, by = c("lon", "lat")) %>%
  mutate(
    n_no3_m = replace_na(n_no3_m, 0),  # gN/ha/month
    LU_nat  = replace_na(LU_nat, 0)    # ha
  ) %>%
  mutate(
    no3_stock_kg = n_no3_m * LU_nat / 1000   # g/ha × ha /1000 = kg/grid/月
  ) %>%
  select(year, lon, lat, no3_stock_kg)

# --- 2010-01 & 2011-01 の NO3 ストック差分 [kg/grid] ---
year_vals_nat_no3 <- sort(unique(VISIT_nat_NO3_PLUMLU$year))
year_2010_01_no3  <- year_vals_nat_no3[1]
year_2011_01_no3  <- tail(year_vals_nat_no3, 1)

delta_nat_grid_no3 <- VISIT_nat_NO3_PLUMLU %>%
  filter(year %in% c(year_2010_01_no3, year_2011_01_no3)) %>%
  mutate(tag = if_else(year == year_2010_01_no3, "2010-01", "2011-01")) %>%
  select(lon, lat, tag, no3_stock_kg) %>%
  tidyr::pivot_wider(
    names_from  = tag,
    values_from = no3_stock_kg,
    values_fill = 0
  ) %>%
  mutate(delta_INT_no3_kg = `2011-01` - `2010-01`)   # kg/grid

  country_INT_delta_no3 <- delta_nat_grid_no3 %>%
  left_join(df_fractional, by = c("lon", "lat")) %>%
  filter(!is.na(frac)) %>%
  mutate(delta_INT_no3_kg = delta_INT_no3_kg * frac) %>%   # 国面積比で按分
  group_by(iso3) %>%
  summarise(delta_INT_no3_kg = sum(delta_INT_no3_kg, na.rm = TRUE), .groups = "drop") %>%
  mutate(delta_INT_no3_Gg = delta_INT_no3_kg / 1e6) %>%    # kg → Gg
  rename(ISO3_code = iso3) %>%
  select(ISO3_code, delta_INT_no3_Gg)

  n_top <- 20  # 上位何カ国か

df_comp_no3 <- country_VISIT_delta_no3 %>%
  full_join(country_INT_delta_no3, by = "ISO3_code") %>%
  mutate(
    delta_VISIT_no3_Gg = replace_na(delta_VISIT_no3_Gg, 0),
    delta_INT_no3_Gg   = replace_na(delta_INT_no3_Gg, 0),
    # ランキング用：絶対値の大きい方
    rank_val           = pmax(abs(delta_VISIT_no3_Gg), abs(delta_INT_no3_Gg))
  )

top_iso3_no3 <- df_comp_no3 %>%
  filter(rank_val > 0) %>%
  slice_max(order_by = rank_val, n = n_top, with_ties = FALSE) %>%
  pull(ISO3_code)

df_long_no3 <- df_comp_no3 %>%
  filter(ISO3_code %in% top_iso3_no3) %>%
  arrange(desc(rank_val)) %>%
  mutate(ISO3_code = factor(ISO3_code, levels = ISO3_code)) %>%
  pivot_longer(
    cols      = c(delta_VISIT_no3_Gg, delta_INT_no3_Gg),
    names_to  = "source",
    values_to = "value"
  ) %>%
  mutate(
    source = dplyr::recode(
      source,
      delta_VISIT_no3_Gg = "VISIT only (VISITLU)",
      delta_INT_no3_Gg   = "Integrated nat only (PLUMLU)"
    )
  )

ggplot(df_long_no3, aes(x = ISO3_code, y = value, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    title = sprintf("NO₃ ストック変化量 (2011-01 − 2010-01, Gg N) の比較：上位 %d か国", n_top),
    x = "国（ISO3コード）",
    y = "ストック変化量（Gg N）",
    fill = "系列"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#国別比較まとめ
# --- 既存の差分データフレーム作成（そのまま） ---
Fert_d <- nbud %>%
  left_join(nmip2, by = "ISO3_code") %>%
  mutate(diff = nbud - nmip2) %>%
  select(ISO3_code, diff)

Man_d <-INmanure_country %>%
  rename(diff = Value)

Crop_d <- annual_uptake2 %>%
  left_join(annual_uptake, by = "ISO3_code") %>%
  mutate(diff = Value.y - Value.x) %>%
  select(ISO3_code, diff)

Nh3_d <- annual_n_nh3vlt_grid2 %>%
  left_join(annual_nh3emi, by = "ISO3_code") %>%
  mutate(diff = Value.y - Value.x) %>%
  select(ISO3_code, diff)

No_d <- annual_no_ngas_grid2 %>%
  left_join(annual_noemi, by = "ISO3_code") %>%
  mutate(diff = Value.y - Value.x) %>%
  select(ISO3_code, diff)

N2o_d <- annual_n2o_ngas_grid2 %>%
  left_join(annual_n2oemi, by = "ISO3_code") %>%
  mutate(diff = Value.y - Value.x) %>%
  select(ISO3_code, diff)

N2_d <- annual_n2_ngas_grid2 %>%
  left_join(annual_n2emi, by = "ISO3_code") %>%
  mutate(diff = Value.y - Value.x) %>%
  select(ISO3_code, diff)

hums_d <- annual_n_minerlz_hums_grid2 %>%
  left_join(annual_n_hums, by = "ISO3_code") %>%
  mutate(diff = Value.x - Value.y) %>%   # N入力（モデル − VISIT）
  select(ISO3_code, diff)

immbl_d <- annual_n_immbl_grid2 %>%
  left_join(annual_n_immbl, by = "ISO3_code") %>%
  mutate(diff = Value.y - Value.x) %>%   # N出力（VISIT − モデル）
  select(ISO3_code, diff)

# Nleach は未使用（コメントアウトのまま）
# Litter（落葉からの無機化）
Litter_d <- annual_n_minerlz_lttr_grid2 %>%
  left_join(annual_n_lttr, by = "ISO3_code") %>%
  mutate(diff = Value.x - Value.y) %>%
  select(ISO3_code, diff)

# Deposition
Dep_d <- annual_depo2 %>%
  left_join(annual_depo_VISIT, by = "ISO3_code") %>%
  mutate(diff = Value.x - Value.y) %>%
  select(ISO3_code, diff)

# Fixation（VISIT=0扱い）
Fix_d <- INfix_country %>%
  rename(diff = Value)

# --- 対象国 ---
sel_iso3 <- c("CHN","IND","BRA","USA","RUS","COD","IDN","AUS","CAN")

# --- すべての差分データを結合（hums_d と immbl_d を追加） ---
df_diff <- bind_rows(
  mutate(Fert_d,   flow = "Fertilizer (N input)"),
  mutate(Man_d,    flow = "Manure (N input)"),
  mutate(Fix_d,    flow = "Biological N fixation (N input)"),
  mutate(Dep_d,    flow = "Atmospheric deposition (N input)"),
  mutate(Litter_d, flow = "Mineralization from litter (N input)"),
  mutate(hums_d,   flow = "Mineralization from hums (N input)"),  # ← 追加
  mutate(immbl_d,  flow = "Immobilization (N output)"),           # ← 追加
  mutate(Nh3_d,    flow = "NH3 volatilization (N output)"),
  mutate(No_d,     flow = "NO emission (N output)"),
  mutate(N2_d,     flow = "N2 emission (N output)"),
  mutate(N2o_d,    flow = "N2O emission (N output)"),
  mutate(Crop_d,   flow = "Crop uptake (N output)")
) %>%
  mutate(
    ISO3_code = stringr::str_to_upper(stringr::str_trim(as.character(ISO3_code))),
    diff      = as.numeric(diff)
  )

# --- 凡例順（表示順） ---
flow_levels <- c(
  "Fertilizer (N input)",
  "Manure (N input)",
  "Biological N fixation (N input)",
  "Atmospheric deposition (N input)",
  "Mineralization from litter (N input)",
  "Mineralization from hums (N input)",   # ← 追加
  "Immobilization (N output)",            # ← 追加
  "NH3 volatilization (N output)",
  "NO emission (N output)",
  "N2 emission (N output)",
  "N2O emission (N output)",
  "Crop uptake (N output)"
)

# --- 対象9か国に絞り込み＆欠損を0で補完 ---
df_sel <- df_diff %>%
  dplyr::filter(ISO3_code %in% sel_iso3) %>%
  dplyr::select(ISO3_code, flow, diff) %>%
  tidyr::complete(ISO3_code = sel_iso3, flow = flow_levels, fill = list(diff = 0)) %>%
  dplyr::mutate(
    ISO3_code = factor(ISO3_code, levels = sel_iso3),
    flow      = factor(flow, levels = flow_levels)
  )

# --- 描画 ---
p_stack_sel <- ggplot2::ggplot(df_sel, aes(x = ISO3_code, y = diff, fill = flow)) +
  ggplot2::geom_col(width = 0.75) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, color = "grey40") +
  ggplot2::scale_y_continuous(labels = scales::label_comma()) +
  ggplot2::labs(
    title = "要因分解（指定9か国）：N収支モデル − VISIT",
    subtitle = "単位：Gg N / 年（正：N収支モデルが大きい、負：VISITが大きい）",
    x = "国（ISO3）",
    y = "差（Gg N / 年）",
    fill = "要因"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# --- 保存 ---
ggsave("Factors.png",
       p_stack_sel, width = 10, height = 6, units = "in", dpi = 300, bg = "white")

#統合版優位
# --- 既存: 各フローごとの差分データフレーム（ここはそのまま） ---
Fert_d <- nbud %>%
  left_join(nmip2, by = "ISO3_code") %>%
  mutate(diff = nbud - nmip2) %>%
  select(ISO3_code, diff)

Man_d <- INmanure_country %>%
  rename(diff = Value)

Crop_d <- annual_uptake2 %>%
  left_join(annual_uptake, by = "ISO3_code") %>%
  mutate(diff = Value.y - Value.x) %>%
  select(ISO3_code, diff)

Nh3_d <- annual_n_nh3vlt_grid2 %>%
  left_join(annual_nh3emi, by = "ISO3_code") %>%
  mutate(diff = Value.y - Value.x) %>%
  select(ISO3_code, diff)

No_d <- annual_no_ngas_grid2 %>%
  left_join(annual_noemi, by = "ISO3_code") %>%
  mutate(diff = Value.y - Value.x) %>%
  select(ISO3_code, diff)

N2o_d <- annual_n2o_ngas_grid2 %>%
  left_join(annual_n2oemi, by = "ISO3_code") %>%
  mutate(diff = Value.y - Value.x) %>%
  select(ISO3_code, diff)

N2_d <- annual_n2_ngas_grid2 %>%
  left_join(annual_n2emi, by = "ISO3_code") %>%
  mutate(diff = Value.y - Value.x) %>%
  select(ISO3_code, diff)

hums_d <- annual_n_minerlz_hums_grid2 %>%
  left_join(annual_n_hums, by = "ISO3_code") %>%
  mutate(diff = Value.x - Value.y) %>%   # N入力（モデル − VISIT）
  select(ISO3_code, diff)

immbl_d <- annual_n_immbl_grid2 %>%
  left_join(annual_n_immbl, by = "ISO3_code") %>%
  mutate(diff = Value.y - Value.x) %>%   # N出力（VISIT − モデル）
  select(ISO3_code, diff)

Litter_d <- annual_n_minerlz_lttr_grid2 %>%
  left_join(annual_n_lttr, by = "ISO3_code") %>%
  mutate(diff = Value.x - Value.y) %>%
  select(ISO3_code, diff)

Dep_d <- annual_depo2 %>%
  left_join(annual_depo_VISIT, by = "ISO3_code") %>%
  mutate(diff = Value.x - Value.y) %>%
  select(ISO3_code, diff)

Fix_d <- INfix_country %>%
  rename(diff = Value)

# --- すべての差分を縦持ちに結合 ---
df_diff <- bind_rows(
  mutate(Fert_d,   flow = "Fertilizer (N input)"),
  mutate(Man_d,    flow = "Manure (N input)"),
  mutate(Fix_d,    flow = "Biological N fixation (N input)"),
  mutate(Dep_d,    flow = "Atmospheric deposition (N input)"),
  mutate(Litter_d, flow = "Mineralization from litter (N input)"),
  mutate(hums_d,   flow = "Mineralization from hums (N input)"),
  mutate(immbl_d,  flow = "Immobilization (N output)"),
  mutate(Nh3_d,    flow = "NH3 volatilization (N output)"),
  mutate(No_d,     flow = "NO emission (N output)"),
  mutate(N2_d,     flow = "N2 emission (N output)"),
  mutate(N2o_d,    flow = "N2O emission (N output)"),
  mutate(Crop_d,   flow = "Crop uptake (N output)")
) %>%
  mutate(
    ISO3_code = stringr::str_to_upper(stringr::str_trim(as.character(ISO3_code))),
    diff      = as.numeric(diff)
  ) %>%
  filter(!is.na(ISO3_code))

# --- 凡例（フロー）の順序 ---
flow_levels <- c(
  "Fertilizer (N input)",
  "Manure (N input)",
  "Biological N fixation (N input)",
  "Atmospheric deposition (N input)",
  "Mineralization from litter (N input)",
  "Mineralization from hums (N input)",
  "Immobilization (N output)",
  "NH3 volatilization (N output)",
  "NO emission (N output)",
  "N2 emission (N output)",
  "N2O emission (N output)",
  "Crop uptake (N output)"
)

# --- 各国ごとの「統合版 − VISIT」の総差分を計算してランキング ---
rank_tbl <- df_diff %>%
  group_by(ISO3_code) %>%
  summarise(total_diff = sum(diff, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_diff))   # ← 統合版が優位（diff>0）な順

# 例えば上位20か国だけ表示したい場合
n_top <- 20
ISO3_levels <- head(rank_tbl$ISO3_code, n_top)

# --- 対象国をランキング順に並べて積み上げ棒グラフ用DFに整形 ---
df_sel <- df_diff %>%
  filter(ISO3_code %in% ISO3_levels) %>%
  group_by(ISO3_code, flow) %>%
  summarise(diff = sum(diff, na.rm = TRUE), .groups = "drop") %>%
  tidyr::complete(ISO3_code = ISO3_levels, flow = flow_levels, fill = list(diff = 0)) %>%
  mutate(
    ISO3_code = factor(ISO3_code, levels = ISO3_levels),
    flow      = factor(flow, levels = flow_levels)
  )

# --- 描画 ---
p_stack_sel <- ggplot2::ggplot(df_sel, aes(x = ISO3_code, y = diff, fill = flow)) +
  ggplot2::geom_col(width = 0.75) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, color = "grey40") +
  ggplot2::scale_y_continuous(labels = scales::label_comma()) +
  ggplot2::labs(
    title    = sprintf("要因分解：統合版 − VISIT（統合版優位な順, 上位%dか国）", n_top),
    subtitle = "単位：Gg N / 年（正：N収支モデルが大きい、負：VISITが大きい）",
    x = "国（ISO3）",
    y = "差（Gg N / 年）",
    fill = "要因"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

ggsave("Factors_ranked_top20.png",
       p_stack_sel, width = 10, height = 6, units = "in", dpi = 300, bg = "white")


#流量データ確認
arr <- aperm(VISITro_nat_ag, c(2, 1, 3))  # [lat, lon, month]

## 2. SpatRaster を作る（この時点では経度 0〜360° と仮定）
r0 <- rast(arr)   # nrow = 360, ncol = 720, nlyr = 12

ext(r0) <- c(0, 360, -90, 90)   # ★ 0〜360° 系として定義
crs(r0) <- "EPSG:4326"

## 3. 0〜360° → -180〜180° に変換（ここで 180°平行移動問題を解決）
r <- rotate(r0)   # ★ これがカギ。手動で列を動かすのはやめる

## 4. 1月だけ取り出す
r1 <- r[[1]]

df <- as.data.frame(r1, xy = TRUE)
names(df) <- c("lon", "lat", "runoff")

# ★ ここで経度を 180°回転させる（今 180°ずれているのを元に戻すイメージ）
df$lon <- df$lon - 180
df$lon[df$lon < -180] <- df$lon[df$lon < -180] + 360
# （もしこれで逆向きにずれるようなら +180 / >180→-360 に変えてください）

# 0／NA と それ以外に分ける
df0 <- subset(df, runoff == 0 | is.na(runoff))
df1 <- subset(df, runoff > 0)

# グラデーションの上限を決める
zmax <- quantile(df1$runoff, 0.99, na.rm = TRUE)

ggplot() +
  # 0（＋NA）は固定色
  geom_raster(
    data = df0,
    aes(x = lon, y = lat),
    fill = "grey20"             # 0 の色
  ) +
  # それ以外は連続グラデーション
  geom_raster(
    data = df1,
    aes(x = lon, y = lat, fill = runoff)
  ) +
  borders("world", colour = "grey70", size = 0.2) +
  coord_quickmap(expand = FALSE) +
  scale_fill_viridis_c(
    limits = c(0, zmax),
    oob    = scales::squish,
    name   = "Runoff"
  ) +
  labs(title = "VISIT runoff (January)") +
  theme_bw()


#窒素濃度に関する分析
QoutNint <- test_sim$QoutNint                   # ∑ QoutN(t)
Qout      <- test_sim$Qout                      # [lon, lat, time]

# 時間方向に積算した水量 ∑ Qout(t)
Qout_int <- apply(Qout, c(1, 2), sum, na.rm = TRUE)  # 720×360

# 0 で割るのを防ぐ
C_fw_ratio <- QoutNint / Qout_int
C_fw_ratio[Qout_int == 0] <- NA

# mg/L にしたい場合（Nconc と同じスケールを使うなら）
C_fw_mgL <- C_fw_ratio * 1e6 

thr <- 2.5  # mg N/L

# 比率 R = C / thr
R <- C_fw_mgL / thr
R[C_fw_mgL <= 0] <- NA   # 0 以下は NA 扱い（必要に応じて）

# 1) 6段階の境界（比率）
breaks_ratio <- c(0, 0.3, 0.6, 1.0, 2, 3, Inf)

# 2) R をベクトルにして cut → 行列に戻す
classes_vec <- cut(
  as.vector(R),
  breaks = breaks_ratio,
  include.lowest = TRUE,
  labels = FALSE
)

# C_fw_mgL と同じサイズの行列に戻す
Z <- matrix(classes_vec,
            nrow = nrow(C_fw_mgL),
                        ncol = ncol(C_fw_mgL),
            byrow = FALSE)

# R が NA のところはクラスも NA に
Z[is.na(R)] <- NA

# 3) 色とラベル
cols <- c("#08306B", "#4292C6", "#C6DBEF",   # 不足側（濃青〜淡青）
          "#FEE0D2", "#FB6A4A", "#CB181D")   # 超過側（淡赤〜赤）

class_labels <- c(
  "<= 0.3",
  "0.3–0.6",
  "0.6–1.0",
  "1.0–2.0",
  "2.0–3.0",
  "> 3.0"
)

png("output/C_fw_ratio_6class.png",
    width = 1200, height = 800, res = 150)

image.plot(
  x = seq(-179.75, 179.75, by = 0.5),
  y = seq( -89.75,  89.75, by = 0.5),
  z = Z[, ncol(Z):1],                  # 緯度方向を反転

  # ★ Z は 1〜6 の整数なので、それに合わせて break を指定
  breaks = seq(0.5, 6.5, by = 1),      # (0.5,1.5]→1, ..., (5.5,6.5]→6
  col    = cols,
  na.col = "grey80",

  main = "N concentration relative to threshold (2.5 mg N/L)",
  xlab = "lon",
  ylab = "lat",

  axis.args = list(
    at     = 1:6,
    labels = class_labels
  )
)

map("world", add = TRUE, col = "black", lwd = 1)

dev.off()


QoutNint_clipped <- pmin(pmax(test_sim$QoutNint, 0), 5.0e+08)
png(filename = "output/251201QoutNint_geos_Nbud.png", width = 1200, height = 800, res = 150)

image.plot(
  x = seq(-179.75, 179.75, by = 0.5),
  y = seq(-89.75, 89.75, by = 0.5),
  z = QoutNint_clipped[, ncol(QoutNint_clipped):1],
  zlim = c(0, 5.0e+08),
  col = tim.colors(64)
)

map("world", add = TRUE, col = "black", lwd = 1)

dev.off()

Q_out <- Qout_int      # 720×360

Q_out_pos <- Q_out
Q_out_pos[Q_out_pos <= 0] <- NA

Q_log <- log10(Q_out_pos)

zmin_Q <- min(Q_log, na.rm = TRUE)
zmax_Q <- max(Q_log, na.rm = TRUE)

png("output/Q_out_log_map.png",
    width = 1200, height = 800, res = 150)

image.plot(
  x    = seq(-179.75, 179.75, by = 0.5),
  y    = seq( -89.75,  89.75, by = 0.5),
  z    = Q_log[, ncol(Q_log):1],
  zlim = c(zmin_Q, zmax_Q),
  col  = tim.colors(64),
  xlab = "lon",
  ylab = "lat",
  main = "Log10 of cumulative water outflow"
)

map("world", add = TRUE, col = "black", lwd = 1)

dev.off()



#Tian et al. (2022)　確認

  #Tian et al. (2022) NOy deposition
fp <- "Tian2022/ndep_noy_0p5deg_2010.nc"

r <- rast(fp)

df_Tian_noy <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
names(df) <- c("lon", "lat", "ndep_noy") 

df_Tian_noy <- df_Tian_noy %>%
  mutate(ndep_noy = ndep_noy/1000000000) #GgN/yr


  #VISITのインプット NOy deposition
con <- gzfile("Tian2022/NMIP2_ninput_2010_noydep.tsv.gz", "rt") #kgN/ha/yr
df  <- read.table(con, header = FALSE,
                  col.names = c("lat_idx","lon_idx","Nnoydep"))
close(con)

df$lon  <- -179.75 + 0.5*df$lon_idx
df$lat  <-   89.75 - 0.5*df$lat_idx
df$year <- 2010
df$Nnoydep_total <- pmax(df$Nnoydep, 0)

df <- df %>%
  select(lon, lat, Nnoydep_total)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

NMIP2_noydep <- df %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  mutate(Nnoydep_kgN = Nnoydep_total*GAIJ/1000) %>% #GgN/yr
  select(lon, lat, Nnoydep_kgN) %>%
  left_join(df_fractional, by = c("lon", "lat"))%>%
  filter(!is.na(frac)) %>%
  mutate(Nnoydep_kgN = Nnoydep_kgN*frac) %>%
  group_by(iso3) %>%
  summarise(Nnoydep = sum(Nnoydep_kgN)) %>%
  rename(ISO3_code = iso3, Value = Nnoydep)

  #Tian et al. (2022) NHx deposition
fp <- "Tian2022/ndep_nhx_0p5deg_2010.nc"

r <- rast(fp)

df_Tian_nhx <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
names(df) <- c("lon", "lat", "ndep_nhx") 

df_Tian_nhx <- df_Tian_nhx %>%
  mutate(ndep_nhx = ndep_nhx/1000000000) #GgN/yr


  #VISITのインプット NHx deposition
con <- gzfile("Tian2022/NMIP2_ninput_2010_nhxdep.tsv.gz", "rt") #kgN/ha/yr
df  <- read.table(con, header = FALSE,
                  col.names = c("lat_idx","lon_idx","Nnhxdep"))
close(con)

df$lon  <- -179.75 + 0.5*df$lon_idx
df$lat  <-   89.75 - 0.5*df$lat_idx
df$year <- 2010
df$Nnhxdep_total <- pmax(df$Nnhxdep, 0)

df <- df %>%
  select(lon, lat, Nnhxdep_total)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

NMIP2_nhxdep <- df %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  mutate(Nnhxdep_kgN = Nnhxdep_total*GAIJ/1000) %>% #GgN/yr
  select(lon, lat, Nnhxdep_kgN) %>%
  left_join(df_fractional, by = c("lon", "lat"))%>%
  filter(!is.na(frac)) %>%
  mutate(Nnhxdep_kgN = Nnhxdep_kgN*frac) %>%
  group_by(iso3) %>%
  summarise(Nnhxdep = sum(Nnhxdep_kgN)) %>%
  rename(ISO3_code = iso3, Value = Nnhxdep)

library(ggplot2)
library(dplyr)
library(maps)
library(viridis)

# 世界地図の海岸線データ
world_map <- map_data("world")

ggplot() +
  geom_tile(
    data = VY_load3,
    aes(x = lon, y = lat, fill = LU_nat),
    width = 0.5, height = 0.5
  ) +
  geom_path(
    data = world_map,
    aes(x = long, y = lat, group = group),
    color = "black", linewidth = 0.2
  ) +
  coord_quickmap(
    xlim = c(-180, 180),
    ylim = c(-56, 84),
    expand = FALSE
  ) +
  scale_fill_viridis(
    name = "Natural land area (ha)",
    na.value = "white"
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Natural land area by 0.5° × 0.5° grid"
  ) +
  theme_bw(base_size = 14)


##配列地図化
# 3次元配列: [lon_idx, lat_idx, month]
arr <- VISITnl_nat_2010

# 月別合計 → 年間GgN
annual_ggn <- apply(arr, c(1, 2), sum, na.rm = TRUE) * 30 * 365 / 360 / 1000000
# 結果は [lon_idx, lat_idx] の2次元配列

df_map <- expand.grid(
  lon_idx = 1:dim(annual_ggn)[1],
  lat_idx = 1:dim(annual_ggn)[2]
) %>%
  mutate(
    value = as.vector(annual_ggn),
    lon = -179.75 + (lon_idx - 1) * 0.5,
    lat =  89.75  - (lat_idx - 1) * 0.5
  )

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_raster(data = df_map, aes(x = lon, y = lat, fill = value)) +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.2) +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  scale_fill_viridis_c(
    name = "Annual N leaching\n(GgN/yr)",
    na.value = "white"
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Annual nitrogen leaching"
  ) +
  theme_bw()

annual_ggn_ref <- apply(VISITnl_nat_2010_NMIP3, c(1, 2), sum, na.rm = TRUE) * 30 * 365 / 360 / 1000000
arr <- VISITnl_nat_2100mit_NMIP3

annual_ggn <- apply(arr, c(1, 2), sum, na.rm = TRUE) * 30 * 365 / 360 / 1000000

df_map <- expand.grid(
  lon_idx = 1:dim(annual_ggn)[1],
  lat_idx = 1:dim(annual_ggn)[2]
) %>%
  mutate(
    value = as.vector(annual_ggn),
    lon = -179.75 + (lon_idx - 1) * 0.5,
    lat =  89.75  - (lat_idx - 1) * 0.5
  )
  fill_min <- 0
fill_max <- max(annual_ggn_ref, na.rm = TRUE)
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_raster(data = df_map, aes(x = lon, y = lat, fill = value)) +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.2) +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  scale_fill_viridis_c(
    name = "Annual N leaching\n(GgN/yr)",
    limits = c(fill_min, fill_max),
    oob = scales::squish,
    na.value = "white"
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Annual nitrogen leaching"
  ) +
  theme_bw()



  sum(OUTleadis$OUTleagrid[OUTleadis$YEMF == 2100], na.rm = TRUE)