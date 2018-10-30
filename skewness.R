# The purpose of this function is to load AVISO and NAPA data
# and to then calculate the skewness of the flow of the data based on SSH/SLA


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ncdf4)
library(doMC); doMC::registerDoMC(cores = 50)


# Files -------------------------------------------------------------------

AVISO_files <- dir(path = "../data", pattern = "CMEMS", full.names = T)

NAPA_files <- dir(path = "../../data/NAPA025/1d_grid_T_2D", full.names = T)

# The NAPA to OISST lon/lat mask
load("metadata/lon_lat_NAPA_OISST.RData")

# The OISST lon values for subsetting
load("metadata/lon_OISST.RData")


# Functions ---------------------------------------------------------------

# Calculate the skewness of SSH/SLA for a given pixel
skewness <- function(df){
  res <- df %>% 
    unique() %>% 
    group_by(lon, lat) %>% 
    summarise_if(.predicate = is.numeric, .funs = e1071::skewness, na.rm = T)
  res[is.na(res)] <- NA
  return(res)
}


# Skewness ----------------------------------------------------------------

# Calculate skewness for NAPA data

# Calculate skewness for AVISO data

# Calculate the difference between the two

