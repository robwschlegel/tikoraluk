# The purpose of this script is to fix the date indexing errors that occurred
# when adding the 2018 NOAA OISST data to the pre-existing 1982 - 2017 NetCDF files

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(rerddap)
library(ncdf4)
library(abind)
# library(tidync)
doMC::registerDoMC(cores = 50)


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
  nc <- nc_open(OISST_file, write = T)
  time_old <- as.vector(nc$dim$time$vals)
  time_old_good <- as.vector(nc$dim$time$vals)[nc$dim$time$vals > 0]
  if(length(time_old) > length(time_old_good)){
    dates_2018_int_sub <- dates_2018_int[!dates_2018_int %in% time_old_good]
    ncvar_put(nc = nc, varid = "time", vals = dates_2018_int_sub,
              start = length(time_old_good)+1, verbose = FALSE)
  }
  nc_close(nc)
  return()
}


# Fix ---------------------------------------------------------------------

# fix_2018(OISST_files[1])

# plyr::ldply(OISST_files, .fun = fix_2018, .parallel = T)


# Load and visualise ------------------------------------------------------

# nc <- nc_open("../data/OISST/avhrr-only-v2.ts.0720.nc")
# # nc_close(nc)
# #
# res <- ncvar_get(nc, varid = "sst")
# dimnames(res) <- list(lat = nc$dim$lat$vals,
#                       # lon = nc$dim$lon$vals,
#                       t = as.vector(nc$dim$time$vals))
# nc_close(nc)
# 
# res2 <- as.data.frame(reshape2::melt(res, value.name = "temp"), row.names = NULL) %>%
#   filter(lat == 36.125) %>%
#   # dplyr::rename(t = Var2) %>%
#   # mutate(t = as.Date(t, origin = "1981-12-31")) %>%
#   mutate(t = as.Date(t, origin = "1970-01-01")) %>%
#   # select(lon, everything()) %>%
#   na.omit()
# 
# ggplot(res2, aes(x = t, y = temp)) +
#   geom_line()
