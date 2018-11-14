# The purpose of this script is to identify which pixels in both the NAPA and OISST
# products have n days of ice coverage, and then to make an ice mask from those
# pixls with >= 50% annual ice coverage.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ncdf4)
library(R.matlab)


# Data --------------------------------------------------------------------

# The NAPA to OISST lon/lat mask
load("metadata/lon_lat_NAPA_OISST.RData")

# The OISST lon values for subsetting
load("metadata/lon_OISST.RData")

# OISST MatLab files
OISST_list <- dir("../../oliver/data/sst/noaa_oi_v2/avhrr/timeseries", pattern = "avhrr", full.names = T)


# Functions ---------------------------------------------------------------

# Function for loading OISST data saved in .mat format on tikoraluk
load_ice <- function(lon_row){
  
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  
  ### Load OISST data
  mat_file <- readMat(paste0("../../oliver/data/sst/noaa_oi_v2/avhrr/timeseries/avhrr-only-v2.ts.",lon_row_pad,".mat")) # ~7 seconds
  mat_file_ts <- as.data.frame(t(mat_file$ice.ts)) %>% 
    setNames(., as.numeric(mat_file$lat)) %>% 
    mutate(t = as.Date(as.POSIXct((mat_file$time - 719529) * 86400,
                                  origin = "1970-01-01", tz = "UTC"))) %>% 
    gather(-t, key = lat, value = ice) %>% 
    mutate(lon = mat_file$lon[as.numeric(lon_row)],
           lat = as.numeric(lat),
           ice = ifelse(is.nan(ice), NA, ice)) %>%
    select(lon, lat, t, ice) %>% 
    na.omit() # ~2 seconds
  
  
  ### Load NAPA data
  load(paste0("../data/MHW.NAPA.calc.",lon_row_pad,".RData"))
  
  nc <- nc_open(as.character(file_name))
  
  date_start <- ymd(str_sub(basename(as.character(file_name)), start = 29, end = 36))
  date_end <- ymd(str_sub(basename(as.character(file_name)), start = 38, end = 45))
  date_seq <- seq(date_start, date_end, by = "day")
  
  sst <- as.data.frame(ncvar_get(nc, varid = "sst")) %>% 
    mutate(lon = as.numeric(nc$dim$x$vals)) %>%
    gather(-lon, key = lat, value = temp) %>%
    mutate(t = rep(date_seq, each = 388080),
           lat = rep(rep(as.numeric(nc$dim$y$vals), each = 528), times = 5)) %>%
    select(lon, lat, t, temp) %>%
    inner_join(coords, by = c("lon", "lat")) %>% 
    select(nav_lon, nav_lat, t, temp)
  
  nc_close(nc)
  
  

  

}

# Calculations ------------------------------------------------------------


