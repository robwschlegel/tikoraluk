# The purpose of this script is to provide an easy source for 
# calculating all of the historic MCSs in one go

# I haven't presently put in any failsafes for oversaving...

# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(heatwaveR)
if(packageVersion("heatwaveR") != "0.3.6"){
  devtools::install_github("robwschlegel/heatwaveR")
  library(heatwaveR)
}
source("MHW_prep.R")
doMC::registerDoMC(cores = 50)


# Data --------------------------------------------------------------------

# The OISST RData files
OISST_RData <- c(file = dir(path = "../data", pattern = "MHW.calc.*.RData", full.names = T))


# Functions ---------------------------------------------------------------

# Function for loading OISST data, calculating MCSs, and saving the results
# lon_row <- 1
MCS_calc <- function(lon_row){
  
  # Begin
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  print(paste("Began run", lon_row_pad, "at", Sys.time()))
  
  # Load data
  load(OISST_RData[lon_row])
  SST <- MHW_clim(MHW_res) %>% 
    select(lon, lat, t, temp)
  rm(MHW_res)
  
  # Make calculations
  MCS_res <- SST %>% 
    group_by(lon, lat) %>%
    nest() %>% 
    mutate(clim = purrr::map(data, ts2clm, climatologyPeriod = c("1982-01-01", "2011-12-31"), pctile = 10),
           event = purrr::map(clim, detect_event, coldSpells = T)) %>% 
           # cat = purrr::map(event, category, climatology = TRUE)) %>% # Curently the code for detecting MCS categories does not work...
    select(-data, -clim)
  
  # Finish
  save(MCS_res, file = paste0("../data/MCS.calc", lon_row_pad,".RData"))
  print(paste("Completed run",lon_row_pad,"at",Sys.time()))
}


# Calculations ------------------------------------------------------------

# system.time(
#   MCS_calc(100)
# ) # 47 seconds

# Run on Wednesday, November 28th, 2018
plyr::ldply(1:1440, .fun = MCS_calc, .parallel = T)
