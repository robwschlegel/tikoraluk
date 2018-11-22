# The purpose of this script is to identify which pixels in both the NAPA and OISST
# products have n days of ice coverage, and then to make an ice mask from those
# pixls with >= 50% annual ice coverage.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ncdf4)
library(R.matlab)
library(lubridate)
doMC::registerDoMC(cores = 50)


# Data --------------------------------------------------------------------

# The NAPA to OISST lon/lat mask
load("metadata/lon_lat_NAPA_OISST.RData")

# The OISST lon values for subsetting
load("metadata/lon_OISST.RData")

# The water mask to correctly remove land ice from calculations
load("metadata/only_water.RData")


# Functions ---------------------------------------------------------------

# Function for creating day, month, year means for sst
ice_DMY <- function(df, product){
  
  # Daily values
  df_daily <- df %>% 
    mutate(month = "daily")
  
  # Monthly values
  df_monthly <- df %>% 
    mutate(t = floor_date(t, "month")) %>% 
    group_by(nav_lon, nav_lat, t) %>% 
    summarise(ice = mean(ice, na.rm = T)) %>% 
    mutate(month = "monthly") %>% 
    ungroup()
  
  # Yearly values
  df_yearly <- df %>% 
    mutate(t = floor_date(t, "year")) %>% 
    group_by(nav_lon, nav_lat, t) %>% 
    summarise(ice = mean(ice, na.rm = T)) %>% 
    mutate(month = "yearly") %>% 
    ungroup()
  
  # daily values by month
  df_month <- df %>% 
    mutate(month = lubridate::month(t, label = T))
  
  # All together now...
  df_ALL <- rbind(df_daily, df_monthly, df_yearly, df_month) %>% 
    mutate(product = product,
           month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
                                            "daily", "monthly", "yearly"))) %>% 
    group_by(nav_lon, nav_lat, month)
  return(df_ALL)
}


# Function for calculating simple decadal trends
dt <- function(df){
  date_sub <- data.frame(monthly = seq(as.Date("1994-01-01"), as.Date("2015-12-01"), by = "month"))
  res <- df %>% 
    mutate(monthly = floor_date(t, unit = "month")) %>% 
    group_by(monthly) %>%
    summarise_if(is.numeric, .funs = c("mean"), na.rm = T) %>%
    filter(monthly %in% date_sub$monthly)
  if(nrow(na.omit(res)) <= 2) return(NA)
  res_dt <- round(as.numeric(coef(lm(ice ~ monthly, data = res))[2]) * 120, 4)
  return(as.numeric(res_dt))
}


# Function for finding mmm diff between products
# df <- ALL_ice
ice_diff <- function(df){
  
  # Decadal trends
  ice_dt <- df %>% 
    group_by(nav_lon, nav_lat, month, product) %>% 
    nest() %>% 
    mutate(dt = map(data, dt)) %>% 
    select(-data) %>% 
    unnest()
  
  # Summary
  ice_sum <- df %>% 
    group_by(nav_lon, nav_lat, month, product) %>%
    summarise_if(is.numeric, .funs = c("min", "median", "mean", "max", "sd"), na.rm = T) %>%
    replace(is.na(.), NA) %>% 
    left_join(ice_dt, by = c("nav_lon", "nav_lat", "month", "product")) %>% 
    rename_at(vars(-(nav_lon:product)), ~ paste0("ice_",.))
  is.na(ice_sum) <- sapply(ice_sum, is.infinite)
  
  # Differences
  ice_sum_dif <- left_join(filter(ice_sum, product == "NAPA"), 
                            filter(ice_sum, product == "OISST"),
                            by = c("nav_lon", "nav_lat", "month")) %>%
    mutate(ice_min = ice_min.x - ice_min.y,
           ice_median = ice_median.x - ice_median.y,
           ice_mean = ice_mean.x - ice_mean.y,
           ice_max = ice_max.x - ice_max.y, 
           ice_sd = ice_sd.x - ice_sd.y, 
           ice_dt = ice_dt.x - ice_dt.y, 
           product = "difference") %>% 
    select(nav_lon, nav_lat, product, month, ice_min:ice_max) %>% 
    rbind(ice_sum)
  return(ice_sum_dif)
}


# Function for running numbers on ice from NAPA and OISST
## tester...
# lon_row <- 100
ice_ON <- function(lon_row){
  
  ### Begin
  print(paste("Began run", lon_row, "at", Sys.time()))
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  
  ### Load data
  load(paste0("../data/NAPA_ice_sub_",lon_row_pad,".RData"))
  mat_file <- readMat(paste0("../../oliver/data/sst/noaa_oi_v2/avhrr/timeseries/avhrr-only-v2.ts.",lon_row_pad,".mat")) # ~7 seconds
  
  ### Prep NAPA data
  NAPA_ice <- NAPA_ice_sub %>% 
    mutate(ice = ifelse(ice == 0, NA, ice)) %>%  # Remove the landmask as it is 0 for some reason...
    select(-date_end) %>% 
    dplyr::rename(t = date_start) %>% 
    right_join(only_water, by = c("nav_lon", "nav_lat")) %>%
    select(nav_lon, nav_lat, t, ice) %>%
    na.omit() %>% 
    ice_DMY(., "NAPA")
  rm(NAPA_ice_sub)
  
  ### Prep OISST data
  OISST_ice <- as.data.frame(t(mat_file$ice.ts)) %>% 
    setNames(., as.numeric(mat_file$lat)) %>% 
    mutate(t = as.Date(as.POSIXct((mat_file$time - 719529) * 86400,
                                  origin = "1970-01-01", tz = "UTC"))) %>% 
    gather(-t, key = lat_O, value = ice) %>% 
    filter(t >= as.Date("1993-10-01"),
           t <= as.Date("2015-12-29")) %>% 
    mutate(lon_O = mat_file$lon[as.numeric(lon_row)],
           lat_O = as.numeric(lat_O)) %>%
    left_join(lon_lat_NAPA_OISST, by = c("lon_O", "lat_O")) %>% 
    select(nav_lon, nav_lat, t, ice) %>% 
    na.omit() %>%
    right_join(only_water, by = c("nav_lon", "nav_lat")) %>%
    na.omit() %>%
    ice_DMY(., "OISST")
  rm(mat_file)
  
  ### Combine
  ALL_ice <- rbind(OISST_ice, NAPA_ice)
  rm(NAPA_ice, OISST_ice)
  
  ### MMM
  ALL_ice_res <- ice_diff(ALL_ice)

  ### Finish
  print(paste("Completed run",lon_row,"at",Sys.time()))
  return(ALL_ice_res)
}


# Calculations ------------------------------------------------------------

# system.time(
#   test <- ice_ON(100)
# ) # 13 seconds

# Re-run on Thursday, November 16th, 2018
# OISST_NAPA_ice_summary <- plyr::ldply(1:1440, .fun = ice_ON, .parallel = T)
# save(OISST_NAPA_ice_summary, file = "../data/OISST_NAPA_ice_summary.RData")

