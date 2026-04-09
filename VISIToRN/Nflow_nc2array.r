#Natural and agricultural visitlu (Manure)
nc_visitLU <- nc_open("visit_20250625_atmosclus_TEST_n_manurein.nc") #kg/m2/s

time_vals <- ncvar_get(nc_visitLU, "time")
time_units <- ncatt_get(nc_visitLU, "time", "units")$value

origin_year <- 1850
years <- origin_year + (time_vals / 12)
target_idx <- which(years >= 2010 & years < 2011) #Institute Variables

fNleach_slice <- ncvar_get(nc_visitLU, "n_manurein",
                           start = c(1, 1, 1, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fNleach_slice[,,ti], responseName = "Nmanurein")
  colnames(df) <- c("lon_idx", "lat_idx", "Nmanurein")
  df <- df %>%
    mutate(
      lon = lon[as.integer(lon_idx)],
      lat = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, Nmanurein)
  return(df)
})

fNmanurein_df5 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITnat_VISITLU <- fNmanurein_df5 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(Nmanurein)) %>%
  mutate(Nmanurein = Nmanurein * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, Nmanurein)

fNmanurein_slice <- ncvar_get(nc_visitLU, "n_manurein",
                           start = c(1, 1, 2, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fNmanurein_slice[,,ti], responseName = "Nmanurein")
  colnames(df) <- c("lon_idx", "lat_idx", "Nmanurein")
  df <- df %>%
    mutate(
      lon = lon[as.integer(lon_idx)],
      lat = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, Nmanurein)
  return(df)
})

fNmanurein_df6 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITagr_VISITLU <- fNmanurein_df6 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(Nmanurein)) %>%
  mutate(Nmanurein = Nmanurein * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, Nmanurein)

VISIT_VISITLU <- VISITnat_VISITLU %>%
  left_join(VISITagr_VISITLU, by = c("year", "lon", "lat")) %>%
  mutate(Nmanurein = Nmanurein.x + Nmanurein.y) %>%
  select(year, lon, lat, Nmanurein)

VISITnm_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITnm_nat_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITnm_agr_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
year_vals <- sort(unique(VISIT_VISITLU$year))

VISIT_VISITLU <- VISIT_VISITLU %>%
  mutate(
    lon_idx = round((lon + 179.75) / 0.5) + 1,
    lat_idx = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_VISITLU, 1, function(row) {
  VISITnm_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Nmanurein"]
})

save(VISITnm_VISITLU, file = "Data_pd/VISITnm_VISITLU.rdat")

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
  VISITnm_nat_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Nmanurein"]
})

save(VISITnm_nat_VISITLU, file = "Data_pd/VISITnm_nat_VISITLU.rdat")

apply(VISITagr_VISITLU, 1, function(row) {
  VISITnm_agr_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Nmanurein"]
})

save(VISITnm_agr_VISITLU, file = "Data_pd/VISITnm_agr_VISITLU.rdat")


#Natural and agricultural visitlu (Fert)
nc_visitLU <- nc_open("visit_20250625_atmosclus_TEST_n_fertin.nc") #kg/m2/s

time_vals <- ncvar_get(nc_visitLU, "time")
time_units <- ncatt_get(nc_visitLU, "time", "units")$value

origin_year <- 1850
years <- origin_year + (time_vals / 12)
target_idx <- which(years >= 2010 & years < 2011) #Institute Variables

fNleach_slice <- ncvar_get(nc_visitLU, "n_fertin",
                           start = c(1, 1, 1, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fNleach_slice[,,ti], responseName = "fertin")
  colnames(df) <- c("lon_idx", "lat_idx", "fertin")
  df <- df %>%
    mutate(
      lon = lon[as.integer(lon_idx)],
      lat = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, fertin)
  return(df)
})

ffertin_df5 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITnat_VISITLU <- ffertin_df5 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(fertin)) %>%
  mutate(fertin = fertin * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, fertin)

ffertin_slice <- ncvar_get(nc_visitLU, "n_fertin",
                           start = c(1, 1, 2, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(ffertin_slice[,,ti], responseName = "fertin")
  colnames(df) <- c("lon_idx", "lat_idx", "fertin")
  df <- df %>%
    mutate(
      lon = lon[as.integer(lon_idx)],
      lat = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, fertin)
  return(df)
})

ffertin_df6 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITagr_VISITLU <- ffertin_df6 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(fertin)) %>%
  mutate(fertin = fertin * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, fertin)

VISIT_VISITLU <- VISITnat_VISITLU %>%
  left_join(VISITagr_VISITLU, by = c("year", "lon", "lat")) %>%
  mutate(fertin = fertin.x + fertin.y) %>%
  select(year, lon, lat, fertin)

VISITnf_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITnf_nat_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITnf_agr_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
year_vals <- sort(unique(VISIT_VISITLU$year))

VISIT_VISITLU <- VISIT_VISITLU %>%
  mutate(
    lon_idx = round((lon + 179.75) / 0.5) + 1,
    lat_idx = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_VISITLU, 1, function(row) {
  VISITnf_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["fertin"]
})

save(VISITnf_VISITLU, file = "Data_pd/VISITnf_VISITLU.rdat")

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
  VISITnf_nat_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["fertin"]
})

save(VISITnf_nat_VISITLU, file = "Data_pd/VISITnf_nat_VISITLU.rdat")

apply(VISITagr_VISITLU, 1, function(row) {
  VISITnf_agr_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["fertin"]
})

save(VISITnf_agr_VISITLU, file = "Data_pd/VISITnf_agr_VISITLU.rdat")


#Natural andno3 agricultural visitlu (depo_no)
nc_visitLU <- nc_open("visit_20250625_atmosclus_TEST_depo_no3.nc") #kg/m2/s

time_vals <- ncvar_get(nc_visitLU, "time")
time_units <- ncatt_get(nc_visitLU, "time", "units")$value

origin_year <- 1850
years <- origin_year + (time_vals / 12)
target_idx <- which(years >= 2010 & years < 2011) #Institute Variables

fNleach_slice <- ncvar_get(nc_visitLU, "depo_no",
                           start = c(1, 1, 1, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fNleach_slice[,,ti], responseName = "depo_no")
  colnames(df) <- c("lon_idx", "lat_idx", "depo_no")
  df <- df %>%
    mutate(
      lon = lon[as.integer(lon_idx)],
      lat = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, depo_no)
  return(df)
})

fdepono3_df5 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITnat_VISITLU <- fdepono3_df5 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(depo_no)) %>%
  mutate(depo_no = depo_no * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, depo_no)

fdepono3_slice <- ncvar_get(nc_visitLU, "depo_no",
                           start = c(1, 1, 2, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fdepono3_slice[,,ti], responseName = "depo_no")
  colnames(df) <- c("lon_idx", "lat_idx", "depo_no")
  df <- df %>%
    mutate(
      lon = lon[as.integer(lon_idx)],
      lat = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, depo_no)
  return(df)
})

fdepono3_df6 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITagr_VISITLU <- fdepono3_df6 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(depo_no)) %>%
  mutate(depo_no = depo_no * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, depo_no)

VISIT_VISITLU <- VISITnat_VISITLU %>%
  left_join(VISITagr_VISITLU, by = c("year", "lon", "lat")) %>%
  mutate(depo_no = depo_no.x + depo_no.y) %>%
  select(year, lon, lat, depo_no)

VISITndno3_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITndno3_nat_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITndno3_agr_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
year_vals <- sort(unique(VISIT_VISITLU$year))

VISIT_VISITLU <- VISIT_VISITLU %>%
  mutate(
    lon_idx = round((lon + 179.75) / 0.5) + 1,
    lat_idx = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_VISITLU, 1, function(row) {
  VISITndno3_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["depo_no"]
})

save(VISITndno3_VISITLU, file = "Data_pd/VISITndno3_VISITLU.rdat")

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
  VISITndno3_nat_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["depo_no"]
})

save(VISITndno3_nat_VISITLU, file = "Data_pd/VISITndno3_nat_VISITLU.rdat")

apply(VISITagr_VISITLU, 1, function(row) {
  VISITndno3_agr_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["depo_no"]
})

save(VISITndno3_agr_VISITLU, file = "Data_pd/VISITndno3_agr_VISITLU.rdat")


#Natural and agricultural visitlu (depo nh4)
nc_visitLU <- nc_open("visit_20250625_atmosclus_TEST_depo_nh4.nc") #kg/m2/s

time_vals <- ncvar_get(nc_visitLU, "time")
time_units <- ncatt_get(nc_visitLU, "time", "units")$value

origin_year <- 1850
years <- origin_year + (time_vals / 12)
target_idx <- which(years >= 2010 & years < 2011) #Institute Variables

fNleach_slice <- ncvar_get(nc_visitLU, "depo_nh4",
                           start = c(1, 1, 1, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fNleach_slice[,,ti], responseName = "Ndepo_nh4")
  colnames(df) <- c("lon_idx", "lat_idx", "Ndepo_nh4")
  df <- df %>%
    mutate(
      lon = lon[as.integer(lon_idx)],
      lat = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, Ndepo_nh4)
  return(df)
})

fNdepo_nh4_df5 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITnat_VISITLU <- fNdepo_nh4_df5 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(Ndepo_nh4)) %>%
  mutate(Ndepo_nh4 = Ndepo_nh4 * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, Ndepo_nh4)

fNdepo_nh4_slice <- ncvar_get(nc_visitLU, "depo_nh4",
                           start = c(1, 1, 2, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fNdepo_nh4_slice[,,ti], responseName = "Ndepo_nh4")
  colnames(df) <- c("lon_idx", "lat_idx", "Ndepo_nh4")
  df <- df %>%
    mutate(
      lon = lon[as.integer(lon_idx)],
      lat = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, Ndepo_nh4)
  return(df)
})

fNdepo_nh4_df6 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITagr_VISITLU <- fNdepo_nh4_df6 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(Ndepo_nh4)) %>%
  mutate(Ndepo_nh4 = Ndepo_nh4 * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, Ndepo_nh4)

VISIT_VISITLU <- VISITnat_VISITLU %>%
  left_join(VISITagr_VISITLU, by = c("year", "lon", "lat")) %>%
  mutate(Ndepo_nh4 = Ndepo_nh4.x + Ndepo_nh4.y) %>%
  select(year, lon, lat, Ndepo_nh4)

VISITndnh4_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITndnh4_nat_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITndnh4_agr_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
year_vals <- sort(unique(VISIT_VISITLU$year))

VISIT_VISITLU <- VISIT_VISITLU %>%
  mutate(
    lon_idx = round((lon + 179.75) / 0.5) + 1,
    lat_idx = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_VISITLU, 1, function(row) {
  VISITndnh4_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Ndepo_nh4"]
})

save(VISITndnh4_VISITLU, file = "Data_pd/VISITndnh4_VISITLU.rdat")

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
  VISITndnh4_nat_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Ndepo_nh4"]
})

save(VISITndnh4_nat_VISITLU, file = "Data_pd/VISITndnh4_nat_VISITLU.rdat")

apply(VISITagr_VISITLU, 1, function(row) {
  VISITndnh4_agr_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Ndepo_nh4"]
})

save(VISITndnh4_agr_VISITLU, file = "Data_pd/VISITndnh4_agr_VISITLU.rdat")


#Natural and agricultural visitlu (NH3 Volatilization)
nc_visitLU <- nc_open("visit_20250625_atmosclus_TEST_n_nh3vlt.nc") #kg/m2/s

time_vals <- ncvar_get(nc_visitLU, "time")
time_units <- ncatt_get(nc_visitLU, "time", "units")$value

origin_year <- 1850
years <- origin_year + (time_vals / 12)
target_idx <- which(years >= 2010 & years < 2011) #Institute Variables

fNleach_slice <- ncvar_get(nc_visitLU, "n_nh3vlt",
                           start = c(1, 1, 1, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fNleach_slice[,,ti], responseName = "Nnh3vlt")
  colnames(df) <- c("lon_idx", "lat_idx", "Nnh3vlt")
  df <- df %>%
    mutate(
      lon = lon[as.integer(lon_idx)],
      lat = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, Nnh3vlt)
  return(df)
})

fNnh3vlt_df5 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITnat_VISITLU <- fNnh3vlt_df5 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(Nnh3vlt)) %>%
  mutate(Nnh3vlt = Nnh3vlt * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, Nnh3vlt)

fNnh3vlt_slice <- ncvar_get(nc_visitLU, "n_nh3vlt",
                           start = c(1, 1, 2, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fNnh3vlt_slice[,,ti], responseName = "Nnh3vlt")
  colnames(df) <- c("lon_idx", "lat_idx", "Nnh3vlt")
  df <- df %>%
    mutate(
      lon = lon[as.integer(lon_idx)],
      lat = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, Nnh3vlt)
  return(df)
})

fNnh3vlt_df6 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITagr_VISITLU <- fNnh3vlt_df6 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(Nnh3vlt)) %>%
  mutate(Nnh3vlt = Nnh3vlt * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, Nnh3vlt)

VISIT_VISITLU <- VISITnat_VISITLU %>%
  left_join(VISITagr_VISITLU, by = c("year", "lon", "lat")) %>%
  mutate(Nnh3vlt = Nnh3vlt.x + Nnh3vlt.y) %>%
  select(year, lon, lat, Nnh3vlt)

VISITnnh3vlt_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITnnh3vlt_nat_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITnnh3vlt_agr_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
year_vals <- sort(unique(VISIT_VISITLU$year))

VISIT_VISITLU <- VISIT_VISITLU %>%
  mutate(
    lon_idx = round((lon + 179.75) / 0.5) + 1,
    lat_idx = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_VISITLU, 1, function(row) {
  VISITnnh3vlt_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Nnh3vlt"]
})

save(VISITnnh3vlt_VISITLU, file = "Data_pd/VISITnnh3vlt_VISITLU.rdat")

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
  VISITnnh3vlt_nat_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Nnh3vlt"]
})

save(VISITnnh3vlt_nat_VISITLU, file = "Data_pd/VISITnnh3vlt_nat_VISITLU.rdat")

apply(VISITagr_VISITLU, 1, function(row) {
  VISITnnh3vlt_agr_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Nnh3vlt"]
})

save(VISITnnh3vlt_agr_VISITLU, file = "Data_pd/VISITnnh3vlt_agr_VISITLU.rdat")


#Natural and agricultural visitlu (N2O emission)
nc_visitLU <- nc_open("visit_20250625_atmosclus_TEST_n2o_ngas_rev.nc") #kg/m2/s

time_vals <- ncvar_get(nc_visitLU, "time")
time_units <- ncatt_get(nc_visitLU, "time", "units")$value

origin_year <- 1850
years <- origin_year + (time_vals / 12)
target_idx <- which(years >= 2010 & years < 2011) #Institute Variables

fNleach_slice <- ncvar_get(nc_visitLU, "n2o_ngas",
                           start = c(1, 1, 1, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fNleach_slice[,,ti], responseName = "Nn2o_ngas")
  colnames(df) <- c("lon_idx", "lat_idx", "Nn2o_ngas")
  df <- df %>%
    mutate(
      lon = lon[as.integer(lon_idx)],
      lat = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, Nn2o_ngas)
  return(df)
})

fNn2o_ngas_df5 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITnat_VISITLU <- fNn2o_ngas_df5 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(Nn2o_ngas)) %>%
  mutate(Nn2o_ngas = Nn2o_ngas * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, Nn2o_ngas)

fNn2o_ngas_slice <- ncvar_get(nc_visitLU, "n2o_ngas",
                           start = c(1, 1, 2, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fNn2o_ngas_slice[,,ti], responseName = "Nn2o_ngas")
  colnames(df) <- c("lon_idx", "lat_idx", "Nn2o_ngas")
  df <- df %>%
    mutate(
      lon = lon[as.integer(lon_idx)],
      lat = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, Nn2o_ngas)
  return(df)
})

fNn2o_ngas_df6 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITagr_VISITLU <- fNn2o_ngas_df6 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(Nn2o_ngas)) %>%
  mutate(Nn2o_ngas = Nn2o_ngas * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, Nn2o_ngas)

VISIT_VISITLU <- VISITnat_VISITLU %>%
  left_join(VISITagr_VISITLU, by = c("year", "lon", "lat")) %>%
  mutate(Nn2o_ngas = Nn2o_ngas.x + Nn2o_ngas.y) %>%
  select(year, lon, lat, Nn2o_ngas)

VISITnn2o_ngas_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITnn2o_ngas_nat_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITnn2o_ngas_agr_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
year_vals <- sort(unique(VISIT_VISITLU$year))

VISIT_VISITLU <- VISIT_VISITLU %>%
  mutate(
    lon_idx = round((lon + 179.75) / 0.5) + 1,
    lat_idx = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_VISITLU, 1, function(row) {
  VISITnn2o_ngas_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Nn2o_ngas"]
})

save(VISITnn2o_ngas_VISITLU, file = "Data_pd/VISITnn2o_ngas_VISITLU.rdat")

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
  VISITnn2o_ngas_nat_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Nn2o_ngas"]
})

save(VISITnn2o_ngas_nat_VISITLU, file = "Data_pd/VISITnn2o_ngas_nat_VISITLU.rdat")

apply(VISITagr_VISITLU, 1, function(row) {
  VISITnn2o_ngas_agr_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Nn2o_ngas"]
})

save(VISITnn2o_ngas_agr_VISITLU, file = "Data_pd/VISITnn2o_ngas_agr_VISITLU.rdat")


#Natural and agricultural visitlu (N2 emission)
nc_visitLU <- nc_open("visit_20250625_atmosclus_TEST_n2_ngas.nc") #kg/m2/s

time_vals <- ncvar_get(nc_visitLU, "time")
time_units <- ncatt_get(nc_visitLU, "time", "units")$value

origin_year <- 1850
years <- origin_year + (time_vals / 12)
target_idx <- which(years >= 2010 & years < 2011) #Institute Variables

fNleach_slice <- ncvar_get(nc_visitLU, "n2_ngas",
                           start = c(1, 1, 1, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fNleach_slice[,,ti], responseName = "Nn2_ngas")
  colnames(df) <- c("lon_idx", "lat_idx", "Nn2_ngas")
  df <- df %>%
    mutate(
      lon = lon[as.integer(lon_idx)],
      lat = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, Nn2_ngas)
  return(df)
})

fNn2_ngas_df5 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITnat_VISITLU <- fNn2_ngas_df5 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(Nn2_ngas)) %>%
  mutate(Nn2_ngas = Nn2_ngas * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, Nn2_ngas)

fNn2_ngas_slice <- ncvar_get(nc_visitLU, "n2_ngas",
                           start = c(1, 1, 2, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fNn2_ngas_slice[,,ti], responseName = "Nn2_ngas")
  colnames(df) <- c("lon_idx", "lat_idx", "Nn2_ngas")
  df <- df %>%
    mutate(
      lon = lon[as.integer(lon_idx)],
      lat = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, Nn2_ngas)
  return(df)
})

fNn2_ngas_df6 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITagr_VISITLU <- fNn2_ngas_df6 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(Nn2_ngas)) %>%
  mutate(Nn2_ngas = Nn2_ngas * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, Nn2_ngas)

VISIT_VISITLU <- VISITnat_VISITLU %>%
  left_join(VISITagr_VISITLU, by = c("year", "lon", "lat")) %>%
  mutate(Nn2_ngas = Nn2_ngas.x + Nn2_ngas.y) %>%
  select(year, lon, lat, Nn2_ngas)

VISITnn2_ngas_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITnn2_ngas_nat_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITnn2_ngas_agr_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
year_vals <- sort(unique(VISIT_VISITLU$year))

VISIT_VISITLU <- VISIT_VISITLU %>%
  mutate(
    lon_idx = round((lon + 179.75) / 0.5) + 1,
    lat_idx = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_VISITLU, 1, function(row) {
  VISITnn2_ngas_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Nn2_ngas"]
})

save(VISITnn2_ngas_VISITLU, file = "Data_pd/VISITnn2_ngas_VISITLU.rdat")

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
  VISITnn2_ngas_nat_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Nn2_ngas"]
})

save(VISITnn2_ngas_nat_VISITLU, file = "Data_pd/VISITnn2_ngas_nat_VISITLU.rdat")

apply(VISITagr_VISITLU, 1, function(row) {
  VISITnn2_ngas_agr_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["Nn2_ngas"]
})

save(VISITnn2_ngas_agr_VISITLU, file = "Data_pd/VISITnn2_ngas_agr_VISITLU.rdat")


#Natural and agricultural visitlu (n_minerlz_lttr)
nc_visitLU <- nc_open("visit_20250625_atmosclus_TEST_n_minerlz_lttr.nc") #kg/m2/s

time_vals <- ncvar_get(nc_visitLU, "time")
time_units <- ncatt_get(nc_visitLU, "time", "units")$value

origin_year <- 1850
years <- origin_year + (time_vals / 12)
target_idx <- which(years >= 2010 & years < 2011) #Institute Variables

fNleach_slice <- ncvar_get(nc_visitLU, "n_minerlz_lttr",
                           start = c(1, 1, 1, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fNleach_slice[,,ti], responseName = "n_minerlz_lttr")
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

fn_minerlz_lttr_df5 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITnat_VISITLU <- fn_minerlz_lttr_df5 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(n_minerlz_lttr)) %>%
  mutate(n_minerlz_lttr = n_minerlz_lttr * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, n_minerlz_lttr)

fn_minerlz_lttr_slice <- ncvar_get(nc_visitLU, "n_minerlz_lttr",
                           start = c(1, 1, 2, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fn_minerlz_lttr_slice[,,ti], responseName = "n_minerlz_lttr")
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

fn_minerlz_lttr_df6 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITagr_VISITLU <- fn_minerlz_lttr_df6 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(n_minerlz_lttr)) %>%
  mutate(n_minerlz_lttr = n_minerlz_lttr * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, n_minerlz_lttr)

VISIT_VISITLU <- VISITnat_VISITLU %>%
  left_join(VISITagr_VISITLU, by = c("year", "lon", "lat")) %>%
  mutate(n_minerlz_lttr = n_minerlz_lttr.x + n_minerlz_lttr.y) %>%
  select(year, lon, lat, n_minerlz_lttr)

VISITn_minerlz_lttr_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITn_minerlz_lttr_nat_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITn_minerlz_lttr_agr_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
year_vals <- sort(unique(VISIT_VISITLU$year))

VISIT_VISITLU <- VISIT_VISITLU %>%
  mutate(
    lon_idx = round((lon + 179.75) / 0.5) + 1,
    lat_idx = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_VISITLU, 1, function(row) {
  VISITn_minerlz_lttr_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["n_minerlz_lttr"]
})

save(VISITn_minerlz_lttr_VISITLU, file = "Data_pd/VISITn_minerlz_lttr_VISITLU.rdat")

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
  VISITn_minerlz_lttr_nat_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["n_minerlz_lttr"]
})

save(VISITn_minerlz_lttr_nat_VISITLU, file = "Data_pd/VISITn_minerlz_lttr_nat_VISITLU.rdat")

apply(VISITagr_VISITLU, 1, function(row) {
  VISITn_minerlz_lttr_agr_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["n_minerlz_lttr"]
})

save(VISITn_minerlz_lttr_agr_VISITLU, file = "Data_pd/VISITn_minerlz_lttr_agr_VISITLU.rdat")


#Natural and agricultural visitlu (NO emission)
nc_visitLU <- nc_open("visit_20250625_atmosclus_TEST_no_ngas_rev.nc") #kg/m2/s

time_vals <- ncvar_get(nc_visitLU, "time")
time_units <- ncatt_get(nc_visitLU, "time", "units")$value

origin_year <- 1850
years <- origin_year + (time_vals / 12)
target_idx <- which(years >= 2010 & years < 2011) #Institute Variables

fNleach_slice <- ncvar_get(nc_visitLU, "no_ngas",
                           start = c(1, 1, 1, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

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

fno_ngas_df5 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITnat_VISITLU <- fno_ngas_df5 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(no_ngas)) %>%
  mutate(no_ngas = no_ngas * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, no_ngas)

fno_ngas_slice <- ncvar_get(nc_visitLU, "no_ngas",
                           start = c(1, 1, 2, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fno_ngas_slice[,,ti], responseName = "no_ngas")
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

fno_ngas_df6 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITagr_VISITLU <- fno_ngas_df6 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(no_ngas)) %>%
  mutate(no_ngas = no_ngas * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, no_ngas)

VISIT_VISITLU <- VISITnat_VISITLU %>%
  left_join(VISITagr_VISITLU, by = c("year", "lon", "lat")) %>%
  mutate(no_ngas = no_ngas.x + no_ngas.y) %>%
  select(year, lon, lat, no_ngas)

VISITno_ngas_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITno_ngas_nat_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITno_ngas_agr_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
year_vals <- sort(unique(VISIT_VISITLU$year))

VISIT_VISITLU <- VISIT_VISITLU %>%
  mutate(
    lon_idx = round((lon + 179.75) / 0.5) + 1,
    lat_idx = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_VISITLU, 1, function(row) {
  VISITno_ngas_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["no_ngas"]
})

save(VISITno_ngas_VISITLU, file = "Data_pd/VISITno_ngas_VISITLU.rdat")

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
  VISITno_ngas_nat_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["no_ngas"]
})

save(VISITno_ngas_nat_VISITLU, file = "Data_pd/VISITno_ngas_nat_VISITLU.rdat")

apply(VISITagr_VISITLU, 1, function(row) {
  VISITno_ngas_agr_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["no_ngas"]
})

save(VISITno_ngas_agr_VISITLU, file = "Data_pd/VISITno_ngas_agr_VISITLU.rdat")


#Natural and agricultural visitlu (Fixation)
nc_visitLU <- nc_open("visit_20250625_atmosclus_TEST_n_biofix.nc") #kg/m2/s

time_vals <- ncvar_get(nc_visitLU, "time")
time_units <- ncatt_get(nc_visitLU, "time", "units")$value

origin_year <- 1850
years <- origin_year + (time_vals / 12)
target_idx <- which(years >= 2010 & years < 2011) #Institute Variables

fNleach_slice <- ncvar_get(nc_visitLU, "n_biofix",
                           start = c(1, 1, 1, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fNleach_slice[,,ti], responseName = "n_biofix")
  colnames(df) <- c("lon_idx", "lat_idx", "n_biofix")
  df <- df %>%
    mutate(
      lon = lon[as.integer(lon_idx)],
      lat = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, n_biofix)
  return(df)
})

fn_biofix_df5 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITnat_VISITLU <- fn_biofix_df5 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(n_biofix)) %>%
  mutate(n_biofix = n_biofix * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, n_biofix)

fn_biofix_slice <- ncvar_get(nc_visitLU, "n_biofix",
                           start = c(1, 1, 2, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fn_biofix_slice[,,ti], responseName = "n_biofix")
  colnames(df) <- c("lon_idx", "lat_idx", "n_biofix")
  df <- df %>%
    mutate(
      lon = lon[as.integer(lon_idx)],
      lat = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, n_biofix)
  return(df)
})

fn_biofix_df6 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITagr_VISITLU <- fn_biofix_df6 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(n_biofix)) %>%
  mutate(n_biofix = n_biofix * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, n_biofix)

VISIT_VISITLU <- VISITnat_VISITLU %>%
  left_join(VISITagr_VISITLU, by = c("year", "lon", "lat")) %>%
  mutate(n_biofix = n_biofix.x + n_biofix.y) %>%
  select(year, lon, lat, n_biofix)

VISITn_biofix_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITn_biofix_nat_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITn_biofix_agr_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
year_vals <- sort(unique(VISIT_VISITLU$year))

VISIT_VISITLU <- VISIT_VISITLU %>%
  mutate(
    lon_idx = round((lon + 179.75) / 0.5) + 1,
    lat_idx = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_VISITLU, 1, function(row) {
  VISITn_biofix_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["n_biofix"]
})

save(VISITn_biofix_VISITLU, file = "Data_pd/VISITn_biofix_VISITLU.rdat")

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
  VISITn_biofix_nat_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["n_biofix"]
})

save(VISITn_biofix_nat_VISITLU, file = "Data_pd/VISITn_biofix_nat_VISITLU.rdat")

apply(VISITagr_VISITLU, 1, function(row) {
  VISITn_biofix_agr_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["n_biofix"]
})

save(VISITn_biofix_agr_VISITLU, file = "Data_pd/VISITn_biofix_agr_VISITLU.rdat")


#Natural and agricultural visitlu (uptake no3)
nc_visitLU <- nc_open("visit_20250625_atmosclus_TEST_uptake_no3.nc") #kg/m2/s

time_vals <- ncvar_get(nc_visitLU, "time")
time_units <- ncatt_get(nc_visitLU, "time", "units")$value

origin_year <- 1850
years <- origin_year + (time_vals / 12)
target_idx <- which(years >= 2010 & years < 2011) #Institute Variables

fNleach_slice <- ncvar_get(nc_visitLU, "uptake_no3",
                           start = c(1, 1, 1, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

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

fuptake_no3_df5 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITnat_VISITLU <- fuptake_no3_df5 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(uptake_no3)) %>%
  mutate(uptake_no3 = uptake_no3 * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, uptake_no3)

fuptake_no3_slice <- ncvar_get(nc_visitLU, "uptake_no3",
                           start = c(1, 1, 2, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fuptake_no3_slice[,,ti], responseName = "uptake_no3")
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

fuptake_no3_df6 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITagr_VISITLU <- fuptake_no3_df6 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(uptake_no3)) %>%
  mutate(uptake_no3 = uptake_no3 * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, uptake_no3)

VISIT_VISITLU <- VISITnat_VISITLU %>%
  left_join(VISITagr_VISITLU, by = c("year", "lon", "lat")) %>%
  mutate(uptake_no3 = uptake_no3.x + uptake_no3.y) %>%
  select(year, lon, lat, uptake_no3)

VISITnuptake_no3_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITnuptake_no3_nat_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITnuptake_no3_agr_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
year_vals <- sort(unique(VISIT_VISITLU$year))

VISIT_VISITLU <- VISIT_VISITLU %>%
  mutate(
    lon_idx = round((lon + 179.75) / 0.5) + 1,
    lat_idx = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_VISITLU, 1, function(row) {
  VISITnuptake_no3_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["uptake_no3"]
})

save(VISITnuptake_no3_VISITLU, file = "Data_pd/VISITnuptake_no3_VISITLU.rdat")

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
  VISITnuptake_no3_nat_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["uptake_no3"]
})

save(VISITnuptake_no3_nat_VISITLU, file = "Data_pd/VISITnuptake_no3_nat_VISITLU.rdat")

apply(VISITagr_VISITLU, 1, function(row) {
  VISITnuptake_no3_agr_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["uptake_no3"]
})

save(VISITnuptake_no3_agr_VISITLU, file = "Data_pd/VISITnuptake_no3_agr_VISITLU.rdat")


#Natural and agricultural visitlu (uptake nh4)
nc_visitLU <- nc_open("visit_20250625_atmosclus_TEST_uptake_nh4.nc") #kg/m2/s

time_vals <- ncvar_get(nc_visitLU, "time")
time_units <- ncatt_get(nc_visitLU, "time", "units")$value

origin_year <- 1850
years <- origin_year + (time_vals / 12)
target_idx <- which(years >= 2010 & years < 2011) #Institute Variables

fNleach_slice <- ncvar_get(nc_visitLU, "uptake_nh4",
                           start = c(1, 1, 1, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

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

fuptake_nh4_df5 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITnat_VISITLU <- fuptake_nh4_df5 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(uptake_nh4)) %>%
  mutate(uptake_nh4 = uptake_nh4 * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, uptake_nh4)

fuptake_nh4_slice <- ncvar_get(nc_visitLU, "uptake_nh4",
                           start = c(1, 1, 2, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fuptake_nh4_slice[,,ti], responseName = "uptake_nh4")
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

fuptake_nh4_df6 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITagr_VISITLU <- fuptake_nh4_df6 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(uptake_nh4)) %>%
  mutate(uptake_nh4 = uptake_nh4 * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, uptake_nh4)

VISIT_VISITLU <- VISITnat_VISITLU %>%
  left_join(VISITagr_VISITLU, by = c("year", "lon", "lat")) %>%
  mutate(uptake_nh4 = uptake_nh4.x + uptake_nh4.y) %>%
  select(year, lon, lat, uptake_nh4)

VISITnuptake_nh4_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITnuptake_nh4_nat_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITnuptake_nh4_agr_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
year_vals <- sort(unique(VISIT_VISITLU$year))

VISIT_VISITLU <- VISIT_VISITLU %>%
  mutate(
    lon_idx = round((lon + 179.75) / 0.5) + 1,
    lat_idx = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_VISITLU, 1, function(row) {
  VISITnuptake_nh4_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["uptake_nh4"]
})

save(VISITnuptake_nh4_VISITLU, file = "Data_pd/VISITnuptake_nh4_VISITLU.rdat")

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
  VISITnuptake_nh4_nat_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["uptake_nh4"]
})

save(VISITnuptake_nh4_nat_VISITLU, file = "Data_pd/VISITnuptake_nh4_nat_VISITLU.rdat")

apply(VISITagr_VISITLU, 1, function(row) {
  VISITnuptake_nh4_agr_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["uptake_nh4"]
})

save(VISITnuptake_nh4_agr_VISITLU, file = "Data_pd/VISITnuptake_nh4_agr_VISITLU.rdat")


#Natural and agricultural visitlu (n_immbl)
nc_visitLU <- nc_open("visit_20250625_atmosclus_TEST_n_immbl.nc") #kg/m2/s

time_vals <- ncvar_get(nc_visitLU, "time")
time_units <- ncatt_get(nc_visitLU, "time", "units")$value

origin_year <- 1850
years <- origin_year + (time_vals / 12)
target_idx <- which(years >= 2010 & years < 2011) #Institute Variables

fNleach_slice <- ncvar_get(nc_visitLU, "n_immbl",
                           start = c(1, 1, 1, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fNleach_slice[,,ti], responseName = "n_immbl")
  colnames(df) <- c("lon_idx", "lat_idx", "n_immbl")
  df <- df %>%
    mutate(
      lon = lon[as.integer(lon_idx)],
      lat = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, n_immbl)
  return(df)
})

fn_immbl_df5 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITnat_VISITLU <- fn_immbl_df5 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(n_immbl)) %>%
  mutate(n_immbl = n_immbl * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, n_immbl)

fn_immbl_slice <- ncvar_get(nc_visitLU, "n_immbl",
                           start = c(1, 1, 2, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fn_immbl_slice[,,ti], responseName = "n_immbl")
  colnames(df) <- c("lon_idx", "lat_idx", "n_immbl")
  df <- df %>%
    mutate(
      lon = lon[as.integer(lon_idx)],
      lat = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, n_immbl)
  return(df)
})

fn_immbl_df6 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITagr_VISITLU <- fn_immbl_df6 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(n_immbl)) %>%
  mutate(n_immbl = n_immbl * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, n_immbl)

VISIT_VISITLU <- VISITnat_VISITLU %>%
  left_join(VISITagr_VISITLU, by = c("year", "lon", "lat")) %>%
  mutate(n_immbl = n_immbl.x + n_immbl.y) %>%
  select(year, lon, lat, n_immbl)

VISITn_immbl_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITn_immbl_nat_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITn_immbl_agr_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
year_vals <- sort(unique(VISIT_VISITLU$year))

VISIT_VISITLU <- VISIT_VISITLU %>%
  mutate(
    lon_idx = round((lon + 179.75) / 0.5) + 1,
    lat_idx = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_VISITLU, 1, function(row) {
  VISITn_immbl_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["n_immbl"]
})

save(VISITn_immbl_VISITLU, file = "Data_pd/VISITn_immbl_VISITLU.rdat")

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
  VISITn_immbl_nat_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["n_immbl"]
})

save(VISITn_immbl_nat_VISITLU, file = "Data_pd/VISITn_immbl_nat_VISITLU.rdat")

apply(VISITagr_VISITLU, 1, function(row) {
  VISITn_immbl_agr_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["n_immbl"]
})

save(VISITn_immbl_agr_VISITLU, file = "Data_pd/VISITn_immbl_agr_VISITLU.rdat")


#Natural and agricultural visitlu (n_minerlz_hums)
nc_visitLU <- nc_open("visit_20250625_atmosclus_TEST_n_minerlz_hums.nc") #kg/m2/s

time_vals <- ncvar_get(nc_visitLU, "time")
time_units <- ncatt_get(nc_visitLU, "time", "units")$value

origin_year <- 1850
years <- origin_year + (time_vals / 12)
target_idx <- which(years >= 2010 & years < 2011) #Institute Variables

fNleach_slice <- ncvar_get(nc_visitLU, "n_minerlz_hums",
                           start = c(1, 1, 1, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fNleach_slice[,,ti], responseName = "n_minerlz_hums")
  colnames(df) <- c("lon_idx", "lat_idx", "n_minerlz_hums")
  df <- df %>%
    mutate(
      lon = lon[as.integer(lon_idx)],
      lat = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, n_minerlz_hums)
  return(df)
})

fn_minerlz_hums_df5 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITnat_VISITLU <- fn_minerlz_hums_df5 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(n_minerlz_hums)) %>%
  mutate(n_minerlz_hums = n_minerlz_hums * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, n_minerlz_hums)

fn_minerlz_hums_slice <- ncvar_get(nc_visitLU, "n_minerlz_hums",
                           start = c(1, 1, 2, min(target_idx)),  #PFT//natural or agricultural//
                           count = c(720, 360, 1, length(target_idx)))

lon <- ncvar_get(nc_visitLU, "lon")
lat <- ncvar_get(nc_visitLU, "lat")

df_list <- lapply(1:length(target_idx), function(ti){
  year_val <- years[target_idx[ti]]
  df <- as.data.frame.table(fn_minerlz_hums_slice[,,ti], responseName = "n_minerlz_hums")
  colnames(df) <- c("lon_idx", "lat_idx", "n_minerlz_hums")
  df <- df %>%
    mutate(
      lon = lon[as.integer(lon_idx)],
      lat = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, n_minerlz_hums)
  return(df)
})

fn_minerlz_hums_df6 <- bind_rows(df_list)

gridarea <- rgdx.param(data_prep, "GAIJ") %>% #kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITagr_VISITLU <- fn_minerlz_hums_df6 %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(n_minerlz_hums)) %>%
  mutate(n_minerlz_hums = n_minerlz_hums * GAIJ * 10000000 * 3600 * 24) %>% #kgN/grid/day
  select(year, lon, lat, n_minerlz_hums)

VISIT_VISITLU <- VISITnat_VISITLU %>%
  left_join(VISITagr_VISITLU, by = c("year", "lon", "lat")) %>%
  mutate(n_minerlz_hums = n_minerlz_hums.x + n_minerlz_hums.y) %>%
  select(year, lon, lat, n_minerlz_hums)

VISITn_minerlz_hums_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITn_minerlz_hums_nat_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
VISITn_minerlz_hums_agr_VISITLU <- array(0, dim = c(720, 360, 12)) #array name
year_vals <- sort(unique(VISIT_VISITLU$year))

VISIT_VISITLU <- VISIT_VISITLU %>%
  mutate(
    lon_idx = round((lon + 179.75) / 0.5) + 1,
    lat_idx = round((89.75 - lat) / 0.5) + 1,
    month_idx = match(year, year_vals)
  )

apply(VISIT_VISITLU, 1, function(row) {
  VISITn_minerlz_hums_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["n_minerlz_hums"]
})

save(VISITn_minerlz_hums_VISITLU, file = "Data_pd/VISITn_minerlz_hums_VISITLU.rdat")

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
  VISITn_minerlz_hums_nat_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["n_minerlz_hums"]
})

save(VISITn_minerlz_hums_nat_VISITLU, file = "Data_pd/VISITn_minerlz_hums_nat_VISITLU.rdat")

apply(VISITagr_VISITLU, 1, function(row) {
  VISITn_minerlz_hums_agr_VISITLU[row["lon_idx"], row["lat_idx"], row["month_idx"]] <<- row["n_minerlz_hums"]
})

save(VISITn_minerlz_hums_agr_VISITLU, file = "Data_pd/VISITn_minerlz_hums_agr_VISITLU.rdat")


#Natural and agricultural visitlu (no3_m)
# --- 0) NetCDF を開く（PFTを含む統合ファイル） ---
nc_no3 <- nc_open("visit_20250625_atmosclus_TEST_n_no3_m.nc")  # 単位: gN/ha/month

# --- 1) 時間・座標の取得 ---
time_vals  <- ncvar_get(nc_no3, "time")                       # 1850-01 からの月数 or 月年換算用
time_units <- ncatt_get(nc_no3, "time", "units")$value        # 例: "months since 1850-01-01 00:00:00"
lon        <- ncvar_get(nc_no3, "lon")
lat        <- ncvar_get(nc_no3, "lat")

origin_year <- 1850
years       <- origin_year + (time_vals / 12)                 # 例: 2010.000, 2010.0833...

# --- 2) 2010-01〜2011-01 の13か月を“確実に”抽出（最近傍でインデックス化）---
idx_2010_01 <- which.min(abs(years - 2010.0))                 # 2010-01 に最も近い
idx_2011_01 <- which.min(abs(years - 2011.0))                 # 2011-01 に最も近い
if (idx_2010_01 > idx_2011_01) {
  stop("time 軸の順序が想定外（2010-01 が 2011-01 より後ろ）です。")
}
target_idx  <- seq(idx_2010_01, idx_2011_01)                  # 連続 13 ステップのはず
nmon        <- length(target_idx)
if (nmon != 13) {
  warning(sprintf("抽出月数が %d でした（想定: 13）。継続します。", nmon))
}
year_vals   <- years[target_idx]                               # 長さ 13（小数年表現）

# --- 3) PFT = 1 (natural) の n_no3_m を抽出（g/ha/month）---
n_no3_nat_slice <- ncvar_get(
  nc_no3, "n_no3_m",
  start = c(1, 1, 1, min(target_idx)),   # lon, lat, pft=1(natural), time
  count = c(length(lon), length(lat), 1, nmon)
)

# 行列→データフレーム（月ごと）
df_list_nat <- lapply(seq_len(nmon), function(ti) {
  year_val <- year_vals[ti]
  df <- as.data.frame.table(n_no3_nat_slice[,,ti], responseName = "n_no3_m")
  colnames(df) <- c("lon_idx", "lat_idx", "n_no3_m")
  df %>%
    mutate(
      lon  = lon[as.integer(lon_idx)],
      lat  = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, n_no3_m)
})
n_no3_nat_df <- bind_rows(df_list_nat)

# --- 4) PFT = 2 (agricultural) の n_no3_m を抽出（g/ha/month）---
n_no3_agr_slice <- ncvar_get(
  nc_no3, "n_no3_m",
  start = c(1, 1, 2, min(target_idx)),   # lon, lat, pft=2(agri), time
  count = c(length(lon), length(lat), 1, nmon)
)

df_list_agr <- lapply(seq_len(nmon), function(ti) {
  year_val <- year_vals[ti]
  df <- as.data.frame.table(n_no3_agr_slice[,,ti], responseName = "n_no3_m")
  colnames(df) <- c("lon_idx", "lat_idx", "n_no3_m")
  df %>%
    mutate(
      lon  = lon[as.integer(lon_idx)],
      lat  = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, n_no3_m)
})
n_no3_agr_df <- bind_rows(df_list_agr)

# 使い終わったら閉じる
nc_close(nc_no3)

# --- 5) グリッド面積（GAIJ: kha = 千ha）を取得し、kg/grid/month へ換算 ---
#    g/ha/month × kha = kg/grid/month  （※1 kha = 1000 ha, g×1000 / 1000 = kg）
gridarea <- rgdx.param(data_prep, "GAIJ") %>%  # GAIJ: kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITnat_NO3_VISITLU <- n_no3_nat_df %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(n_no3_m)) %>%
  mutate(n_no3_m = n_no3_m * GAIJ) %>%        # ← kg/grid/month
  select(year, lon, lat, n_no3_m)

VISITagr_NO3_VISITLU <- n_no3_agr_df %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(n_no3_m)) %>%
  mutate(n_no3_m = n_no3_m * GAIJ) %>%        # ← kg/grid/month
  select(year, lon, lat, n_no3_m)

# --- 6) natural + agricultural を合算（kg/grid/month）---
VISIT_NO3_VISITLU <- VISITnat_NO3_VISITLU %>%
  full_join(VISITagr_NO3_VISITLU,
            by = c("year", "lon", "lat"),
            suffix = c("_nat", "_agr")) %>%
  mutate(
    n_no3_m_nat = replace_na(n_no3_m_nat, 0),
    n_no3_m_agr = replace_na(n_no3_m_agr, 0),
    n_no3_m     = n_no3_m_nat + n_no3_m_agr
  ) %>%
  select(year, lon, lat, n_no3_m)

# --- 7) lon/lat → インデックス化し、[lon, lat, month] 配列（第3次元=13）へ格納 ---
# インデックス式は従来どおり（0.5度グリッド、原点の取り方に注意）
to_idx <- function(lon_deg, lat_deg) {
  lon_idx <- round((lon_deg + 179.75) / 0.5) + 1
  lat_idx <- round((89.75  - lat_deg) / 0.5) + 1
  list(lon_idx = lon_idx, lat_idx = lat_idx)
}

# 月インデックスの丸め誤差対策（数値一致→丸め一致へ）
round_match <- function(x, table_x, k = 8) match(round(x, k), round(table_x, k))

# 配列を 12→13 に
VISITn_no3_m_VISITLU     <- array(0, dim = c(720, 360, nmon))  # total
VISITn_no3_m_nat_VISITLU <- array(0, dim = c(720, 360, nmon))  # natural only
VISITn_no3_m_agr_VISITLU <- array(0, dim = c(720, 360, nmon))  # agricultural only

# 参照ベクトル（13か月）
year_vals_total <- sort(unique(VISIT_NO3_VISITLU$year))
stopifnot(length(year_vals_total) == nmon)

# 参照ベクトル（インデックス境界チェック用）
lon_len <- 720L
lat_len <- 360L
nmon    <- length(year_vals_total)

# --- total ---
VISIT_NO3_VISITLU_idx <- VISIT_NO3_VISITLU %>%
  mutate(
    lon_idx   = as.integer(round((lon + 179.75) / 0.5) + 1L),
    lat_idx   = as.integer(round((89.75  - lat) / 0.5) + 1L),
    # 月インデックスは丸め誤差を吸収して 1..13 を割り当て
    month_idx = as.integer(factor(round(year, 8), levels = round(year_vals_total, 8)))
  ) %>%
  # 配列範囲外や NA を除去
  filter(!is.na(month_idx),
         lon_idx >= 1L, lon_idx <= lon_len,
         lat_idx >= 1L, lat_idx <= lat_len) %>%
  select(lon_idx, lat_idx, month_idx, n_no3_m)

# 行列インデックスで一括代入（高速・安全）
idx_total <- cbind(VISIT_NO3_VISITLU_idx$lon_idx,
                   VISIT_NO3_VISITLU_idx$lat_idx,
                   VISIT_NO3_VISITLU_idx$month_idx)
VISITn_no3_m_VISITLU[idx_total] <- VISIT_NO3_VISITLU_idx$n_no3_m


# --- natural only ---
VISITnat_NO3_VISITLU_idx <- VISITnat_NO3_VISITLU %>%
  mutate(
    lon_idx   = as.integer(round((lon + 179.75) / 0.5) + 1L),
    lat_idx   = as.integer(round((89.75  - lat) / 0.5) + 1L),
    month_idx = as.integer(factor(round(year, 8), levels = round(year_vals, 8)))
  ) %>%
  filter(!is.na(month_idx),
         lon_idx >= 1L, lon_idx <= lon_len,
         lat_idx >= 1L, lat_idx <= lat_len) %>%
  select(lon_idx, lat_idx, month_idx, n_no3_m)

idx_nat <- cbind(VISITnat_NO3_VISITLU_idx$lon_idx,
                 VISITnat_NO3_VISITLU_idx$lat_idx,
                 VISITnat_NO3_VISITLU_idx$month_idx)
VISITn_no3_m_nat_VISITLU[idx_nat] <- VISITnat_NO3_VISITLU_idx$n_no3_m


# --- agricultural only ---
VISITagr_NO3_VISITLU_idx <- VISITagr_NO3_VISITLU %>%
  mutate(
    lon_idx   = as.integer(round((lon + 179.75) / 0.5) + 1L),
    lat_idx   = as.integer(round((89.75  - lat) / 0.5) + 1L),
    month_idx = as.integer(factor(round(year, 8), levels = round(year_vals, 8)))
  ) %>%
  filter(!is.na(month_idx),
         lon_idx >= 1L, lon_idx <= lon_len,
         lat_idx >= 1L, lat_idx <= lat_len) %>%
  select(lon_idx, lat_idx, month_idx, n_no3_m)

idx_agr <- cbind(VISITagr_NO3_VISITLU_idx$lon_idx,
                 VISITagr_NO3_VISITLU_idx$lat_idx,
                 VISITagr_NO3_VISITLU_idx$month_idx)
VISITn_no3_m_agr_VISITLU[idx_agr] <- VISITagr_NO3_VISITLU_idx$n_no3_m

# --- 8) 保存（ファイル名は従来のままでもOK。中身の第3次元が 13 になります） ---
save(VISITn_no3_m_VISITLU,     file = "Data_pd/VISITn_no3_m_VISITLU.rdat")
save(VISITn_no3_m_nat_VISITLU, file = "Data_pd/VISITn_no3_m_nat_VISITLU.rdat")
save(VISITn_no3_m_agr_VISITLU, file = "Data_pd/VISITn_no3_m_agr_VISITLU.rdat")


#Natural and agricultural visitlu (nh4_m)
# --- 0) NetCDF を開く（PFTを含む統合ファイル：NH4版） ---
nc_nh4 <- nc_open("visit_20250625_atmosclus_TEST_n_nh4_m.nc")  # 単位: gN/ha/month を想定

# --- 1) 時間・座標の取得 ---
time_vals  <- ncvar_get(nc_nh4, "time")
time_units <- ncatt_get(nc_nh4, "time", "units")$value
lon        <- ncvar_get(nc_nh4, "lon")
lat        <- ncvar_get(nc_nh4, "lat")

origin_year <- 1850
years       <- origin_year + (time_vals / 12)   # 例: 2010.000, 2010.0833...

# --- 2) 2010-01〜2011-01 の13か月を“確実に”抽出（最近傍でインデックス化）---
idx_2010_01 <- which.min(abs(years - 2010.0))   # 2010-01 に最も近い
idx_2011_01 <- which.min(abs(years - 2011.0))   # 2011-01 に最も近い

if (idx_2010_01 > idx_2011_01) {
  stop("time 軸の順序が想定外（2010-01 が 2011-01 より後ろ）です。")
}

target_idx  <- seq(idx_2010_01, idx_2011_01)    # 連続 13 ステップのはず
nmon        <- length(target_idx)
if (nmon != 13) {
  warning(sprintf("抽出月数が %d でした（想定: 13）。継続します。", nmon))
}

year_vals   <- years[target_idx]

# --- 3) PFT = 1 (natural) の n_nh4_m を抽出（g/ha/month）---
n_nh4_nat_slice <- ncvar_get(
  nc_nh4, "n_nh4_m",
  start = c(1, 1, 1, min(target_idx)),   # lon, lat, pft=1(natural), time
  count = c(length(lon), length(lat), 1, nmon)
)

df_list_nat <- lapply(seq_len(nmon), function(ti) {
  year_val <- year_vals[ti]
  df <- as.data.frame.table(n_nh4_nat_slice[,,ti], responseName = "n_nh4_m")
  colnames(df) <- c("lon_idx", "lat_idx", "n_nh4_m")
  df %>%
    mutate(
      lon  = lon[as.integer(lon_idx)],
      lat  = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, n_nh4_m)
})
n_nh4_nat_df <- bind_rows(df_list_nat)

# --- 4) PFT = 2 (agricultural) の n_nh4_m を抽出（g/ha/month）---
n_nh4_agr_slice <- ncvar_get(
  nc_nh4, "n_nh4_m",
  start = c(1, 1, 2, min(target_idx)),   # lon, lat, pft=2(agri), time
  count = c(length(lon), length(lat), 1, nmon)
)

df_list_agr <- lapply(seq_len(nmon), function(ti) {
  year_val <- year_vals[ti]
  df <- as.data.frame.table(n_nh4_agr_slice[,,ti], responseName = "n_nh4_m")
  colnames(df) <- c("lon_idx", "lat_idx", "n_nh4_m")
  df %>%
    mutate(
      lon  = lon[as.integer(lon_idx)],
      lat  = lat[as.integer(lat_idx)],
      year = year_val
    ) %>%
    select(year, lon, lat, n_nh4_m)
})
n_nh4_agr_df <- bind_rows(df_list_agr)

# 使い終わったら閉じる
nc_close(nc_nh4)

# --- 5) グリッド面積（GAIJ: kha = 千ha）を取得し、kg/grid/month へ換算 ---
#    g/ha/month × kha = kg/grid/month  （※1 kha = 1000 ha, g×1000 / 1000 = kg）
gridarea <- rgdx.param(data_prep, "GAIJ") %>%  # GAIJ: kha
  rename(lat = I, lon = J) %>%
  mutate(
    lon = 0.5 * as.numeric(lon) - 180.25,
    lat = -0.5 * as.numeric(lat) + 90.25
  )

VISITnat_NH4_VISITLU <- n_nh4_nat_df %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(n_nh4_m)) %>%
  mutate(n_nh4_m = n_nh4_m * GAIJ) %>%        # ← kg/grid/month とみなす
  select(year, lon, lat, n_nh4_m)

VISITagr_NH4_VISITLU <- n_nh4_agr_df %>%
  left_join(gridarea, by = c("lon", "lat")) %>%
  filter(!is.na(n_nh4_m)) %>%
  mutate(n_nh4_m = n_nh4_m * GAIJ) %>%        # ← kg/grid/month
  select(year, lon, lat, n_nh4_m)

# --- 6) natural + agricultural を合算（kg/grid/month）---
VISIT_NH4_VISITLU <- VISITnat_NH4_VISITLU %>%
  full_join(VISITagr_NH4_VISITLU,
            by = c("year", "lon", "lat"),
            suffix = c("_nat", "_agr")) %>%
  mutate(
    n_nh4_m_nat = replace_na(n_nh4_m_nat, 0),
    n_nh4_m_agr = replace_na(n_nh4_m_agr, 0),
    n_nh4_m     = n_nh4_m_nat + n_nh4_m_agr
  ) %>%
  select(year, lon, lat, n_nh4_m)

# --- 7) lon/lat → インデックス化し、[lon, lat, month] 配列（第3次元=13）へ格納 ---

to_idx <- function(lon_deg, lat_deg) {
  lon_idx <- round((lon_deg + 179.75) / 0.5) + 1
  lat_idx <- round((89.75  - lat_deg) / 0.5) + 1
  list(lon_idx = lon_idx, lat_idx = lat_idx)
}

# 配列を 13 ヶ月に
VISITn_nh4_m_VISITLU     <- array(0, dim = c(720, 360, nmon))  # total
VISITn_nh4_m_nat_VISITLU <- array(0, dim = c(720, 360, nmon))  # natural only
VISITn_nh4_m_agr_VISITLU <- array(0, dim = c(720, 360, nmon))  # agricultural only

# 参照ベクトル（13か月）
year_vals_total <- sort(unique(VISIT_NH4_VISITLU$year))
stopifnot(length(year_vals_total) == nmon)

lon_len <- 720L
lat_len <- 360L
nmon    <- length(year_vals_total)

# --- total ---
VISIT_NH4_VISITLU_idx <- VISIT_NH4_VISITLU %>%
  mutate(
    lon_idx   = as.integer(round((lon + 179.75) / 0.5) + 1L),
    lat_idx   = as.integer(round((89.75  - lat) / 0.5) + 1L),
    month_idx = as.integer(factor(round(year, 8),
                                  levels = round(year_vals_total, 8)))
  ) %>%
  filter(!is.na(month_idx),
         lon_idx >= 1L, lon_idx <= lon_len,
         lat_idx >= 1L, lat_idx <= lat_len) %>%
  select(lon_idx, lat_idx, month_idx, n_nh4_m)

idx_total <- cbind(VISIT_NH4_VISITLU_idx$lon_idx,
                   VISIT_NH4_VISITLU_idx$lat_idx,
                   VISIT_NH4_VISITLU_idx$month_idx)
VISITn_nh4_m_VISITLU[idx_total] <- VISIT_NH4_VISITLU_idx$n_nh4_m

# --- natural only ---
VISITnat_NH4_VISITLU_idx <- VISITnat_NH4_VISITLU %>%
  mutate(
    lon_idx   = as.integer(round((lon + 179.75) / 0.5) + 1L),
    lat_idx   = as.integer(round((89.75  - lat) / 0.5) + 1L),
    month_idx = as.integer(factor(round(year, 8),
                                  levels = round(year_vals, 8)))
  ) %>%
  filter(!is.na(month_idx),
         lon_idx >= 1L, lon_idx <= lon_len,
         lat_idx >= 1L, lat_idx <= lat_len) %>%
  select(lon_idx, lat_idx, month_idx, n_nh4_m)

idx_nat <- cbind(VISITnat_NH4_VISITLU_idx$lon_idx,
                 VISITnat_NH4_VISITLU_idx$lat_idx,
                 VISITnat_NH4_VISITLU_idx$month_idx)
VISITn_nh4_m_nat_VISITLU[idx_nat] <- VISITnat_NH4_VISITLU_idx$n_nh4_m

# --- agricultural only ---
VISITagr_NH4_VISITLU_idx <- VISITagr_NH4_VISITLU %>%
  mutate(
    lon_idx   = as.integer(round((lon + 179.75) / 0.5) + 1L),
    lat_idx   = as.integer(round((89.75  - lat) / 0.5) + 1L),
    month_idx = as.integer(factor(round(year, 8),
                                  levels = round(year_vals, 8)))
  ) %>%
  filter(!is.na(month_idx),
         lon_idx >= 1L, lon_idx <= lon_len,
         lat_idx >= 1L, lat_idx <= lat_len) %>%
  select(lon_idx, lat_idx, month_idx, n_nh4_m)

idx_agr <- cbind(VISITagr_NH4_VISITLU_idx$lon_idx,
                 VISITagr_NH4_VISITLU_idx$lat_idx,
                 VISITagr_NH4_VISITLU_idx$month_idx)
VISITn_nh4_m_agr_VISITLU[idx_agr] <- VISITagr_NH4_VISITLU_idx$n_nh4_m

# --- 8) 保存（NO3と同様に NH4 用のファイル名で） ---
save(VISITn_nh4_m_VISITLU,
     file = "Data_pd/VISITn_nh4_m_VISITLU.rdat")
save(VISITn_nh4_m_nat_VISITLU,
     file = "Data_pd/VISITn_nh4_m_nat_VISITLU.rdat")
save(VISITn_nh4_m_agr_VISITLU,
     file = "Data_pd/VISITn_nh4_m_agr_VISITLU.rdat")