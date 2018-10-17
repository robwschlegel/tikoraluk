# The purpose of this script is to provide one central script
# that can be source() called to put all of the necessary 
# functions for loading OISST data and calculating MHWs from them.

# The script 'MHW_calc.R' is where the code that iteratively 
# calculates MHWs from the .mat files is kept.


# Load libraries ----------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(heatwaveR)
if(packageVersion("heatwaveR") != "0.3.4"){
  devtools::install_github("robwschlegel/heatwaveR")
}


# Source scripts ----------------------------------------------------------

source("load_mat.R")


# MHW functions -----------------------------------------------------------

# Function for calculating and saving MHW results
MHW_calc <- function(df){
  OISST <- load_OISST_mat(df)
  MHW_res <- OISST %>%
    group_by(lon, lat) %>%
    nest() %>% 
    mutate(clim = purrr::map(data, ts2clm, robust = FALSE,
                      climatologyPeriod = c("1982-01-01", "2011-12-31")),
           event = purrr::map(clim, detect_event),
           cat = purrr::map(event, category, climatology = TRUE)) %>% 
    select(-data, -clim)
  save(MHW_res, file = paste0("../data/MHW.calc.", df$file_num,".RData"))
}


# Distance functions ------------------------------------------------------

# Convert degrees to radians
deg2rad <- function(deg) return(deg*pi/180)

# Calculates the geodesic distance between two points specified by radian lat/lon using the haversine formula:
gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}
