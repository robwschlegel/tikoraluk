# The purpose of this script is to go about loading MATLAB files into R


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(R.matlab)
# library(anytime)


# Load data ---------------------------------------------------------------

# Function for loading OISST data saved in .mat format on tikoraluk
load_OISST_mat <- function(df){
  mat_file <- readMat(df$files) # ~7 seconds
  mat_file_ts <- as.data.frame(t(mat_file$sst.ts)) %>% 
    setNames(., as.numeric(mat_file$lat)) %>% 
    mutate(date = as.Date(as.POSIXct((mat_file$time - 719529) * 86400,
                                     origin = "1970-01-01", tz = "UTC"))) %>% 
    gather(-date, key = lat, value = temp) %>% 
    mutate(lon = mat_file$lon[df$x],
           lat = as.numeric(lat),
           temp = ifelse(is.nan(temp), NA, temp)) %>%
    select(lon, lat, date, temp) # ~2 seconds
}

file_list <- dir("../../oliver/data/sst/noaa_oi_v2/avhrr/timeseries/", full.names = T)
system.time(
mat_file <- readMat(file_list[1])
) # 7 seconds
system.time(
mat_file_ts <- as.data.frame(t(mat_file$sst.ts)) %>% 
  setNames(., as.numeric(mat_file$lat)) %>% 
  mutate(date = as.Date(as.POSIXct((mat_file$time - 719529) * 86400,
                           origin = "1970-01-01", tz = "UTC"))) %>% 
    gather(-date, key = lat, value = temp) %>% 
    mutate(lon = mat_file$lon[1],
           lat = as.numeric(lat),
           temp = ifelse(is.nan(temp), NA, temp)) %>%
    select(lon, lat, date, temp)
) # 2 seconds


# Visual test -------------------------------------------------------------

# ggplot(filter(mat_file_ts, date == as.Date("2000-07-01")), aes(lon, lat, fill = temp)) +
#   geom_raster()


# Example -----------------------------------------------------------------

file_list <- data.frame(files = dir("../../oliver/data/sst/noaa_oi_v2/avhrr/timeseries/", full.names = T),
                        x = 1:length(dir("../../oliver/data/sst/noaa_oi_v2/avhrr/timeseries/", full.names = T)))
