# The purpose of this function is to load AVISO and NAPA data
# And to then calculate the skewness of the flow (U and V) of the data


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ncdf4)


# Files -------------------------------------------------------------------

AVISO_files <- dir()

NAPA_files <- dir()


# Functions ---------------------------------------------------------------

AVISO_UV <- function(file_name){
  nc <- nc_open("~/Downloads/CMEMS_dataset-duacs-rep-global-merged-allsat-phy-l4_1993-01.nc")
  test <- ncvar_get(nc, varid = )
}

NAPA_UV <- function(file_name){
  
}

skewness <- function(df){
  
}


# Skewness ----------------------------------------------------------------


