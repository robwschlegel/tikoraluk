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


# Functions ---------------------------------------------------------------

# Function for calculating simple decadal trends
# df <- ALL_ice %>%
#   ungroup() %>%
#   # na.omit() %>%
#   filter(round(nav_lon, 2) == 0.16,
#          round(nav_lat, 2) == 50.00) %>%
#   select(-(nav_lon:product), t, ice)
dt <- function(df, val, ...){
  if(nrow(na.omit(df)) == 0) return(NA)
  date_sub <- seq(as.Date("1994-01-01"), as.Date("2015-12-01"), by = "month")
  res <- df %>% 
    # na.omit() %>% 
    mutate(monthly = floor_date(t, unit = "month")) %>% 
    group_by(monthly) %>% 
    summarise_if(is.numeric, .funs = c("mean"), na.rm = T) %>%
    filter(monthly %in% date_sub) %>% 
    replace(is.na(.), 0) %>% 
    do(dt = round(as.numeric(coef(lm(ice ~ monthly, data = .))[2]) * 120, 4)) %>% 
    mutate(dt = as.numeric(dt))
  return(as.numeric(res))
}


# Function for finding mmm diff between products
# df <- ALL_ice
ice_diff <- function(df){
  ice_dt <- df %>% 
    group_by(nav_lon, nav_lat, month, product) %>% 
    nest() %>% 
    mutate(dt = map(data, dt)) %>% 
    select(-data) %>% 
    unnest()
  ice_sum <- df %>% 
    group_by(nav_lon, nav_lat, month, product) %>%
    summarise_if(is.numeric, .funs = c("min", "median", "mean", "max", "sd"), na.rm = T) %>%
    replace(is.na(.), NA) %>% 
    left_join(ice_dt, by = c("nav_lon", "nav_lat", "month", "product")) %>% 
    rename_at(vars(-(nav_lon:product)), ~ paste0("ice_",.))
  is.na(ice_sum) <- sapply(ice_sum, is.infinite)
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
# lon_row <- 1
ice_ON <- function(lon_row){
  print(paste("Began run", lon_row, "at", Sys.time()))
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  
  ### Load NAPA data
  load(paste0("../data/NAPA_ice_sub_",lon_row_pad,".RData"))
  NAPA_ice <- NAPA_ice_sub %>% 
    mutate(ice = ifelse(ice == 0, NA, ice)) %>% # Set the landmask to NA as it is 0 for some reason...
    select(-date_end) %>% 
    dplyr::rename(t = date_start) %>% 
    mutate(month = lubridate::month(t, label = T)) %>%
    select(nav_lon, nav_lat, month, t, ice) %>%
    mutate(product = "NAPA")
  rm(NAPA_ice_sub)
  
  ### Load OISST data
  mat_file <- readMat(paste0("../../oliver/data/sst/noaa_oi_v2/avhrr/timeseries/avhrr-only-v2.ts.",lon_row_pad,".mat")) # ~7 seconds
  OISST_ice <- as.data.frame(t(mat_file$ice.ts)) %>% 
    setNames(., as.numeric(mat_file$lat)) %>% 
    mutate(t = as.Date(as.POSIXct((mat_file$time - 719529) * 86400,
                                  origin = "1970-01-01", tz = "UTC"))) %>% 
    gather(-t, key = lat, value = ice) %>% 
    mutate(lon = mat_file$lon[as.numeric(lon_row)],
           lat = as.numeric(lat),
           ice = ifelse(is.nan(ice), NA, ice)) %>%
    select(lon, lat, t, ice) %>% 
    # na.omit() %>% 
    left_join(lon_lat_NAPA_OISST, by = c("lon" = "lon_O", "lat" = "lat_O")) %>% 
    mutate(month = lubridate::month(t, label = T)) %>% 
    select(nav_lon, nav_lat, month, t, ice) %>% 
    filter(t >= as.Date("1993-10-01"),
           t <= as.Date("2015-12-29")) %>% 
    inner_join(unique(select(NAPA_ice, nav_lon, nav_lat)), by = c("nav_lon", "nav_lat")) %>% 
    mutate(product = "OISST")
  rm(mat_file)
  
  ### Combine
  ALL_ice <- rbind(OISST_ice, NAPA_ice) %>% 
    mutate(month = "overall") %>% 
    rbind(OISST_ice, NAPA_ice) %>% 
    mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
                                            "overall"))) %>% 
    group_by(nav_lon, nav_lat, month)
  rm(NAPA_ice, OISST_ice)
  
  ### MMM
  ALL_ice_res <- ice_diff(ALL_ice)

  ### Finish
  print(paste("Completed run",lon_row,"at",Sys.time()))
  return(ALL_ice_res)
}


# Calculations ------------------------------------------------------------

# system.time(
#   test <- ice_ON(1)
# ) # 60 seconds 

# Run on Thursday, November 15th, 2018
# system.time(
#   OISST_NAPA_ice_summary <- plyr::ldply(1:1440, .fun = ice_ON, .parallel = T)
# ) # 1627 seconds at 50 cores
# save(OISST_NAPA_ice_summary, file = "../data/OISST_NAPA_ice_summary.RData")

