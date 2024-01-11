# tikoraluk/ERA5_extract.R
# Script for extracting ERA5 data


# Setup -------------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(ncdf4)
library(tidync)
library(tidyverse) # Base suite of functions
library(data.table) # Faster daily means across hourly data
library(FNN)
library(geosphere)
library(doParallel); registerDoParallel(cores = 50)

# ERA5 files
ERA5_lwr_files <- dir("../../oliver/data/ERA/ERA5/LWR", full.names = T, pattern = "ERA5")
ERA5_swr_files <- dir("../../oliver/data/ERA/ERA5/SWR", full.names = T, pattern = "ERA5")
ERA5_lhf_files <- dir("../../oliver/data/ERA/ERA5/SLHF", full.names = T, pattern = "ERA5")
ERA5_shf_files <- dir("../../oliver/data/ERA/ERA5/SSHF", full.names = T, pattern = "ERA5")
ERA5_u_files <- dir("../../oliver/data/ERA/ERA5/U10", full.names = T, pattern = "ERA5")
ERA5_v_files <- dir("../../oliver/data/ERA/ERA5/V10", full.names = T, pattern = "ERA5")
ERA5_mslp_files <- dir("../../oliver/data/ERA/ERA5/MSLP", full.names = T, pattern = "ERA5")
ERA5_t2m_files <- dir("../../oliver/data/ERA/ERA5/T2M", full.names = T, pattern = "ERA5")
ERA5_tcc_files <- dir("../../oliver/data/ERA/ERA5/CLOUD/", full.names = T, pattern = "ERA5")
ERA5_pcp_files <- dir("../../oliver/data/ERA/ERA5/PRCP", full.names = T, pattern = "ERA5")
ERA5_evp_files <- dir("../../oliver/data/ERA/ERA5/EVAP", full.names = T, pattern = "ERA5")

# Combine
ERA5_files <- data.frame(files = c(ERA5_lhf_files, ERA5_shf_files, ERA5_lwr_files, ERA5_swr_files,
                                   ERA5_u_files, ERA5_v_files, ERA5_mslp_files, ERA5_t2m_files,
                                   ERA5_tcc_files, ERA5_pcp_files, ERA5_evp_files)) |> 
  mutate(var_group = sapply(strsplit(files, "_"), "[[", 2))


# Functions ---------------------------------------------------------------

# Filter out only annual files
# NB: This assumes a certain file path structure
# file_list <- ERA5_t2m_files; year_range <- c(1993, 2022) # tester...
# file_list <- ERA5_t2m_files; year_range <- year_range
annual_filter <- function(file_list, year_range){
  file_splt <- strsplit(file_list, "_")
  file_year <-  do.call(rbind, strsplit(sapply(file_splt, "[[", 3), ".nc"))
  ids.to.remove <- sapply(file_splt, function(i) length(i) > 3)
  file_annual <- data.frame(file_name = file_list[!ids.to.remove],
                            year = as.numeric(file_year[!ids.to.remove])) |> 
    dplyr::filter(year >= year_range[1], year <= year_range[2])
  return(file_annual)
  # rm(file_list, year_range, file_splt, file_year, ids.to.remove, file_annual); gc()
}

# Function for loading a single ERA 5 NetCDF file
# The ERA5 data are saved as annual single variables
# testers...
# file_name <- "../../oliver/data/ERA/ERA5/LWR/ERA5_LWR_1993.nc"
# file_name <- "../../oliver/data/ERA/ERA5/EVAP/ERA5_EVAP_1993.nc"
# ncdump::NetCDF(file_name)$variable[1:6]
load_ERA5 <- function(file_name, lon_range, lat_range){
  
  # Find the necessary time shift
  var_name <- sapply(strsplit(file_name, "_"), "[[", 2)
  if(var_name %in% c("SLHF", "SSHF", "LWR", "SWR")){
    time_shift = 43200
  } else{
    time_shift = 0
  }
  
  # Load data
  nc_file <- nc_open(file_name)
  nc_lon <- ncvar_get(nc_file, "longitude")
  nc_lat <- ncvar_get(nc_file, "latitude")
  nc_time <- ncvar_get(nc_file, "time")
  idx_lon <- which(nc_lon %between% lon_range)
  idx_lat <- which(nc_lon %between% lat_range)
  nc_lon_sub <- nc_lon[idx_lon]
  nc_lat_sub <- nc_lat[idx_lat]
  nc_array <- ncvar_get(nc_file, names(nc_file$var)[1],
                        start = c(idx_lon[1], idx_lat[1], 1),
                        count = c(length(idx_lon), length(idx_lat), length(nc_time)))
  nc_df <- t(as.data.frame(nc_array)) |> 
    as.data.frame() |> 
    `colnames<-`(nc_lon_sub) |> 
    mutate(lat = rep(nc_lat_sub, length(nc_time)),
           time = rep(nc_time, length(nc_lat_sub))) |> 
    pivot_longer(cols = c(!lat, -time), names_to = "lon", values_to = names(nc_file$var)[1]) |> 
    mutate(across(everything(), as.numeric))
  
  # Load data
  res <- tidync(file_name) |> 
    # hyper_filter(longitude = longitude == 10, # tester...
                 # latitude = latitude == 76) |> 
    hyper_filter(longitude = dplyr::between(longitude, lon_range[1], lon_range[2]),
                 latitude = dplyr::between(latitude, lat_range[1], lat_range[2])) |>
    hyper_tibble() |> 
    dplyr::rename(lon = longitude, lat = latitude, t = time) |> 
    # mutate(lon = if_else(lon > 180, lon-360, lon)) |>  # Shift to +- 180 scale
    # na.omit() |>  
    mutate(t = as.POSIXct(t * 3600, origin = '1900-01-01', tz = "GMT")) |> 
    mutate(t = t+time_shift) |> # Time shift for heat flux integrals
    mutate(t = as.Date(t))
  
  # Switch to data.table for faster means
  res_dt <- data.table(res)
  setkey(res_dt, lon, lat, t)
  res_mean <- res_dt[, lapply(.SD, mean), by = list(lon, lat, t)]
  return(res_mean)
}

# Function for processing ERA5 data
# file_df <- filter(ERA5_files, var_group == "EVAP")
# lon_range <- c(10, 18); lat_range <- c(77, 79); year_range <- c(1993, 2022) # testers...
process_ERA5 <- function(file_df, lon_range, lat_range, year_range){

  # The base data rounded to days
  print(paste0("Began loading ",file_df$var_group[1]," at ", Sys.time()))
  # system.time(
  res_base <- plyr::ldply(annual_filter(file_df$files, year_range)$file_name, load_ERA5, 
                          .parallel = TRUE, .paropts = c(.inorder = FALSE),
                          lon_range = lon_range, lat_range = lat_range)
  # ) # 46 seconds for 1, 119 for 4, xxx for 30
  
  # The base data
  # system.time(
  # res_base <- plyr::ldply(file_df$files, load_ERA5, .parallel = F, .progress = "text")
  # ) # 66 seconds for one, 378 seconds for 4
  
  # Combine the little half days
  print(paste0("Began meaning ",file_df$var_group[1]," at ", Sys.time()))
  res_dt <- data.table(res_base)
  setkey(res_dt, lon, lat, t)
  res_mean <- res_dt[, lapply(.SD, mean), by = list(lon, lat, t)]
  
  # The clims+anoms
  print(paste0("Began anoms on ",file_df$var_group[1]," at ", Sys.time()))
  # system.time(
  res_anom <- res_mean %>% 
    pivot_longer(cols = c(-lon, -lat, -t), names_to = "var", values_to = "val") %>% 
    plyr::ddply(., c("lon", "lat", "var"), calc_clim_anom, .parallel = T, point_accuracy = 8)
  # ) # 53 seconds for four
  saveRDS(res_anom, paste0("data/ERA5_",file_df$var_name[1],"_anom.Rda"))
  
  # The time series
  print(paste0("Began ts for ",file_df$var_name[1]," at ", Sys.time()))
  # system.time(
  res_ts <- res_mean %>% 
    right_join(OISST_regions, by = c("lon", "lat")) %>%
    na.omit() %>% 
    dplyr::select(-lon, -lat) %>% 
    group_by(region, t) %>% 
    summarise_all("mean") %>% 
    ungroup()
  # ) # 1 second for one, 2 seconds for four
  saveRDS(res_ts, paste0("data/ERA5_",file_df$var_name[1],"_ts.Rda"))
  rm(res_base, res_anom, res_ts); gc()
  return()
}