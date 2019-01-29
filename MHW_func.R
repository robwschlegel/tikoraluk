# The purpose of this script is to provide one central script
# that can be source() called to put all of the necessary 
# functions for loading OISST data and calculating MHWs from them.

# The script 'MHW_calc.R' is where the code that iteratively 
# calculates MHWs from the .mat files is kept.


# Load libraries ----------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(heatwaveR)
if(packageVersion("heatwaveR") != "0.3.6"){
  devtools::install_github("robwschlegel/heatwaveR")
  library(heatwaveR)
}


# Source scripts ----------------------------------------------------------

source("load_mat.R")
source("MHW_prep.R")


# File locations ----------------------------------------------------------

# The OISST RData files
OISST_RData <-  data.frame(file = dir(path = "../data", pattern = "MHW.calc.*.RData", full.names = T),
                          file_num = sapply(strsplit(sapply(strsplit(
                            dir(path = "../data", pattern = "MHW.calc.", full.names = T), 
                            "calc."), "[[", 2), ".RData"), "[[", 1))


# The NAPA RData files
NAPA_RData <-  data.frame(file = dir(path = "../data", pattern = "NAPA_sst_sub", full.names = T),
           file_num = sapply(strsplit(sapply(strsplit(
             dir(path = "../data", pattern = "NAPA_sst_sub", full.names = T), 
             "sub_"), "[[", 2), ".RData"), "[[", 1))


# Meta-data ---------------------------------------------------------------

load("metadata/lon_lat_NAPA_OISST.RData")


# MHW functions -----------------------------------------------------------

# Function for calculating and saving MHW results from OISST data
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

# Function for calculating and saving MHW results from OISST data
# But that match the date range and lon/lat extent of the NAPA data
MHW_match_calc <- function(df){
  load(as.character(df$file))
  MHW_match_res <- MHW_clim(MHW_res) %>% 
    select(lon, lat, t, temp) %>% 
    filter(t >= as.Date("1993-10-01"),
           t <= as.Date("2015-12-29"),
           lon %in% lon_lat_NAPA_OISST$lon_O,
           lat %in% lon_lat_NAPA_OISST$lat_O) %>% 
    group_by(lon, lat) %>%
    nest() %>% 
    mutate(clim = purrr::map(data, ts2clm, robust = FALSE,
                             climatologyPeriod = c("1993-10-01", "2015-12-29")),
           event = purrr::map(clim, detect_event),
           cat = purrr::map(event, category, climatology = TRUE)) %>% 
    select(-data, -clim)
  save(MHW_match_res, file = paste0("../data/MHW.match.calc.", df$file_num,".RData"))
}

# Function for calculating and saving MHW results from NAPA data
MHW_NAPA_calc <- function(df){
  load(as.character(df$file))
  MHW_res <- NAPA_sst_sub %>%
    na.omit() %>% 
    filter(temp != 0) %>%
    group_by(nav_lon, nav_lat) %>%
    nest() %>% 
    mutate(clim = purrr::map(data, ts2clm, robust = FALSE,
                             climatologyPeriod = c("1993-10-01", "2015-12-29")),
           event = purrr::map(clim, detect_event),
           cat = purrr::map(event, category, climatology = TRUE)) %>% 
    select(-data, -clim)
  save(MHW_res, file = paste0("../data/MHW.NAPA.calc.", df$file_num,".RData"))
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
