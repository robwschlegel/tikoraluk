# The purpose of this script is to go about loading the OISST data
# saved as MATLAB files into R.


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(R.matlab)


# Load data ---------------------------------------------------------------

# The file list
file_list <- data.frame(file = dir("../../oliver/data/sst/noaa_oi_v2/avhrr/timeseries", full.names = T),
                        file_num = sapply(strsplit(sapply(strsplit(
                          dir("../../oliver/data/sst/noaa_oi_v2/avhrr/timeseries", 
                                                full.names = T), "ts."), "[[", 2), ".mat"), "[[", 1))

# Function for loading OISST data saved in .mat format on tikoraluk
load_OISST_mat <- function(df){
  mat_file <- readMat(df$file) # ~7 seconds
  mat_file_ts <- as.data.frame(t(mat_file$sst.ts)) %>% 
    setNames(., as.numeric(mat_file$lat)) %>% 
    mutate(t = as.Date(as.POSIXct((mat_file$time - 719529) * 86400,
                                     origin = "1970-01-01", tz = "UTC"))) %>% 
    gather(-t, key = lat, value = temp) %>% 
    mutate(lon = mat_file$lon[as.numeric(df$file_num)],
           lat = as.numeric(lat),
           temp = ifelse(is.nan(temp), NA, temp)) %>%
    select(lon, lat, t, temp) %>% 
    na.omit() # ~2 seconds
}


# Visual test -------------------------------------------------------------

# ggplot(filter(mat_file_ts, date == as.Date("2000-07-01")), aes(lon, lat, fill = temp)) +
#   geom_raster()

