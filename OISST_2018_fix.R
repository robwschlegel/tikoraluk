# The purpose of this script is to fix the date indexing errors that occurred
# when adding the 2018 NOAA OISST data to the pre-existing 1982 - 2017 NetCDF files

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(rerddap)
library(ncdf4)
library(abind)
# library(tidync)
doMC::registerDoMC(cores = 25) # 50 cores exceeds available RAM

# The information for the NOAA OISST data
# info(datasetid = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", url = "https://www.ncei.noaa.gov/erddap/")


# Meta-data ---------------------------------------------------------------

load("../tikoraluk/metadata/lon_lat_OISST.RData")
load("../tikoraluk/metadata/lon_OISST.RData")
lon_lat_OISST <- dplyr::arrange(lon_lat_OISST, lon, lat)
OISST_files <- dir("../data/OISST", pattern = "avhrr-only", full.names = T)
dates_2018 <- seq(as.Date("2018-01-01"), as.Date("2018-12-31"), by = "day")
dates_2018_int <- as.integer(dates_2018)


# Functions ---------------------------------------------------------------

# OISST_file <- OISST_files[1]
fix_2018 <- function(OISST_file){
  # nc <- nc_open(OISST_file, write = T)
  nc <- nc_open("../data/OISST/avhrr-only-v2.ts.0001-test.nc", write = T)
  time_old <- as.vector(nc$dim$time$vals)
  tail(time_old, 500)
  as.Date(tail(time_old,1), origin = "1970-01-01")
  time_old_good <- as.vector(nc$dim$time$vals)[nc$dim$time$vals > 0]
  ncatt_get(nc = nc, varid = "time")
  # for (i in 1:length(dates_2018_int)) {
  #   ncatt_put(nc = nc, varid = "time", attval = )
  # }
  tail(ncvar_get(nc, "time"), 500)
  # 17531 # Last good date
  ncvar_put(nc = nc, varid = "time", vals = dates_2018_int, verbose = TRUE,
            start = length(time_old_good)+1)
  as.Date(17896, origin = "1970-01-01")
  # for(i in 1:length(dates_2018_int)){
  #   ncvar_put(nc = nc, varid = "time", vals = dates_2018_int, verbose = FALSE,
  #             start = c(length(nc$dim$time$vals)+i), count = c(-1))
  # }
  nc_close(nc)
}


# Fix ---------------------------------------------------------------------


