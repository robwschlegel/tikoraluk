# The purpose of this script is to provide an easy source for 
# calculating all of the historic MCSs in one go

# I haven't presently put in any failsafes for oversaving...

# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(tidync)
# remotes::install_github("robwschlegel/heatwaveR")
library(heatwaveR); packageVersion("heatwaveR")
source("MHW_prep.R")
library(doParallel); registerDoParallel(cores = 50)


# Data --------------------------------------------------------------------

# The OISST files
OISST_files <- dir("../data/OISST", pattern = "avhrr-only", full.names = T)


# Functions ---------------------------------------------------------------

# Function for loading OISST data, calculating MCSs, and saving the results
# lon_row <- 1
MCS_calc <- function(lon_row){
  
  # Begin
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  print(paste("Began run", lon_row_pad, "at", Sys.time()))
  
  # Load data
  SST <- tidync(OISST_files[lon_row]) %>% 
    hyper_tibble() %>% 
    mutate(time = as.Date(time, origin = "1970-01-01")) %>% 
    dplyr::rename(t = time, temp = sst)
  
  # Make calculations
  MCS_res <- SST %>% 
    group_by(lon, lat) %>%
    nest() %>% 
    mutate(clim = purrr::map(data, ts2clm, climatologyPeriod = c("1982-01-01", "2011-12-31"), pctile = 10),
           event = purrr::map(clim, detect_event, coldSpells = T), 
           cat = purrr::map(event, category, climatology = T, season = "peak")) %>%
    select(-data, -clim)
  
  # Finish
  save(MCS_res, file = paste0("../data/MCS/MCS.calc", lon_row_pad,".RData"))
  rm(SST, MCS_res); gc()
  print(paste("Completed run",lon_row_pad,"at",Sys.time()))
}


# Calculations ------------------------------------------------------------

# system.time(
#   MCS_calc(2)
# ) # 150 seconds

# Ran on Monday, June 15th, 2020
plyr::l_ply(1:1440, .fun = MCS_calc, .parallel = T)


# Trends ------------------------------------------------------------------


