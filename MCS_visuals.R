# The purpose of this script is to provide some space were specific MCS visualisations may be made


# Libraries ---------------------------------------------------------------

library(tidyverse)


# Data --------------------------------------------------------------------

# The MCS results
MCS_RData <-  c(file = dir(path = "../data", pattern = "MCS.calc.*.RData", full.names = T))


# Functions ---------------------------------------------------------------

load_MCS_sub <- function(file_name, date_range,
                         lon_range, lat_range){
  load(MCS_RData)
  res <- MHW_clim(MHW_res) %>% 
    filter(t == chosen_date) %>% 
    select(lon, lat, t, temp)
  rm(MHW_res)
  return(res)
}


# Visuals -----------------------------------------------------------------


