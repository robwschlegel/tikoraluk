# The purpose of this script is to correct mistakes made while 
# writing the daily script
# It is necessary to start over from the pre-downloaded 1982- 2017
# OISST data and recalculate ALL of the values


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ncdf4)
library(abind)
library(rerddap)
library(heatwaveR, lib.loc = "../R-packages/")
cat(paste0("heatwaveR version = ",packageDescription("heatwaveR")$Version))
# doMC::registerDoMC(cores = 25)

source("MHW_prep.R")


# Meta-data ---------------------------------------------------------------

load("../MHWapp/shiny/lon_OISST.RData")
load("../tikoraluk/metadata/lon_lat_OISST.RData")
lon_lat_OISST <- arrange(lon_lat_OISST, lon, lat)

# File locations
base_files <- dir("../data/MHW", pattern = "MHW.calc.", full.names = T)
# MHW_event_files <- dir("../data/event", pattern = "MHW.event.", full.names = T)
# seas_thresh_files <- dir("../data/thresh", pattern = "MHW.seas.thresh.", full.names = T)
# cat_lon_files <- dir("../data/cat_lon", full.names = T)
# cat_clim_files <- as.character(dir(path = "../data/cat_clim", pattern = "cat.clim", 
                                   # full.names = TRUE, recursive = TRUE))


# Download 2018 data ------------------------------------------------------

# This downloads the data
OISST_dl <- function(times){
  oisst_res <- griddap(x = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", 
                       url = "https://www.ncei.noaa.gov/erddap/", 
                       time = times, 
                       depth = c(0, 0),
                       latitude = c(-89.875, 89.875),
                       longitude = c(0.125, 359.875),
                       fields = "sst")
}

OISST_2018_1 <- OISST_dl(c("2018-01-01T00:00:00Z", "2018-06-30T00:00:00Z"))
OISST_2018_2 <- OISST_dl(c("2018-07-01T00:00:00Z", "2018-12-31T00:00:00Z"))


# Prep 2018 data ----------------------------------------------------------

# This then preps them for further use
OISST_prep <- function(nc_file){
  
  # Open the NetCDF connection
  nc <- nc_open(nc_file$summary$filename)
  
  # Extract the SST values and add the lon/lat/time dimension names
  res <- ncvar_get(nc, varid = "sst")
  dimnames(res) <- list(lon = nc$dim$longitude$vals,
                        lat = nc$dim$latitude$vals,
                        t = nc$dim$time$vals)
  
  # Convert the data into a 'long' dataframe for use in the 'tidyverse' ecosystem
  res <- as.data.frame(reshape2::melt(res, value.name = "temp"), row.names = NULL) %>% 
    mutate(t = as.Date(as.POSIXct(t, origin = "1970-01-01 00:00:00")),
           temp = ifelse(is.na(temp), NA, temp),
           temp = round(temp, 2),
           lon = ifelse(lon > 180, lon-360, lon))
  
  # Close the NetCDF connection and finish
  nc_close(nc)
  return(res)
}

OISST_2018_prep_1 <- OISST_prep(OISST_2018_1)
OISST_2018_prep_2 <- OISST_prep(OISST_2018_2)

OISST_2018_prep <- rbind(OISST_2018_prep_1, OISST_2018_prep_2)
rm(OISST_2018_prep_1, OISST_2018_prep_2)

# Load, process, append, and save all 1982 - 2018 MHW data ----------------

OISST_acast <- function(df){
  # Ensurecorrect grid size
  lon_lat_OISST_sub <- lon_lat_OISST %>% 
    filter(lon == df$lon[1])
  # Round data for massive file size reduction
  df$temp <- round(df$temp, 2)
  # Force grid
  res <- df %>%
    right_join(lon_lat_OISST_sub, by = c("lon", "lat"))
  # Create array
  res_array <- base::array(res$temp, dim = c(720,1,1))
  dimnames(res_array) <- list(lat = lon_lat_OISST_sub$lat,
                              lon = unique(lon_lat_OISST_sub$lon),
                              t = unique(na.omit(res$t)))
  return(res_array)
}

# tester...
# lon_step <- lon_OISST[721]
load_proc_app_save <- function(lon_step){
  
  ### Determine correct lon/row/slice
  lon_row <- which(lon_OISST == lon_step)
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  load(base_files[lon_row])
  
  ### Create SST NetCDF files
  # Filter out lon slice for 2018 data
  MHW_temp_new <- OISST_2018_prep %>% 
    filter(lon == lon_step)
  # Add on to old old temp data
  MHW_temp <- MHW_clim(MHW_res) %>% 
    select(lon, lat, t, temp) %>% 
    mutate(lon = ifelse(lon > 180, lon-360, lon)) %>% 
    rbind(MHW_temp_new)
  # Acast temp data
  MHW_acast <- MHW_temp %>% 
    mutate(t = as.integer(t)) %>% 
    na.omit() %>% 
    mutate(t2 = t) %>% 
    group_by(t2) %>%
    nest() %>%
    mutate(data2 = purrr::map(data, OISST_acast)) %>%
    select(-data)
  # Create array for NetCDF
  MHW_abind <- abind(MHW_acast$data2, along = 3)
  
}

