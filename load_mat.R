# The purpose of this script is to go about loading MATLAB files into R


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(R.matlab)
library(heatwaveR)
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

# Run MHW algorithm on nested data
df <- OISST %>% 
  unnest(sst)

detect_event_event <- function(df){
  res <- detect_event(ts2clm(df, climatologyPeriod = c("1982-01-01", "2011-12-31")))$event
}

detect_event_nested <- function(df){
  res <- df %>% 
    na.omit() %>% 
    group_by(lon, lat) %>% 
    dplyr::rename(t = date) %>% 
    nest() %>% 
    mutate(event = map(data, detect_event_event)) %>% 
    select(-data) %>%
    unnest()
}

# Visual test -------------------------------------------------------------

# ggplot(filter(mat_file_ts, date == as.Date("2000-07-01")), aes(lon, lat, fill = temp)) +
#   geom_raster()


# Example -----------------------------------------------------------------

file_list <- data.frame(files = dir("../../oliver/data/sst/noaa_oi_v2/avhrr/timeseries/", full.names = T),
                        x = 1:length(dir("../../oliver/data/sst/noaa_oi_v2/avhrr/timeseries/", full.names = T)))

system.time(
OISST <- file_list %>% 
  slice(5) %>% 
  mutate(file = x) %>% 
  nest(-file) %>% 
  mutate(sst = map(data, load_OISST_mat)) %>% 
  select(-data)
)

system.time(
OISST_event <- OISST %>%
  mutate(event = map(sst, detect_event_nested)) %>% 
  select(event) %>%
  unnest()
) # 100 seconds

