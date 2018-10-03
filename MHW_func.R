# The purpose of this script is to provide one central script
# that can be source() called to put all of the necessary 
# functions for loading OISST data and calculating MHWs from them.

# The script 'MHW_calc.R' is where the code that iteratively 
# calculates MHWs from the .mat files is kept.


# Load libraries ----------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(heatwaveR, lib.loc = "~/R-packages")
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

# MHW_res <- OISST %>%
#   filter(lon == unique(lon)[1],
#          lat == unique(lat)[1]) %>% 
#   group_by(lon, lat) %>%
#   nest() %>%
#   mutate(clim = purrr::map(data, ts2clm, robust = FALSE,
#                     climatologyPeriod = c("1982-01-01", "2011-12-31")),
#          event = purrr::map(clim, detect_event),
#          cat = purrr::map(event, category, climatology = TRUE)) %>% 
#   select(-data, -clim)
# 
# 
# test <- ts2clm(MHW_res, robust = FALSE, climatologyPeriod = c("1982-01-01", "2011-12-31"))


