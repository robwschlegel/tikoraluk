# The purpose of this function is to load AVISO and NAPA data
# And to then calculate the skewness of the flow of the data based on SSHa


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ncdf4)
library(doMC); doMC::registerDoMC(cores = 50)


# Files -------------------------------------------------------------------

AVISO_files <- dir(path = "../data", pattern = "CMEMS", full.names = T)

NAPA_files <- dir(path = "../../data/NAPA025/1d_grid_T_2D", full.names = T)

# The NAPA to OISST lon/lat mask
load("metadata/lon_lat_NAPA_OISST.RData")


# Functions ---------------------------------------------------------------

AVISO_ssh <- function(file_name){
  nc <- nc_open("../data/CMEMS_dataset-duacs-rep-global-merged-allsat-phy-l4_1993-01.nc")
  test <- ncvar_get(nc, varid = )
}

skewness <- function(df){
  
}


# Skewness ----------------------------------------------------------------


