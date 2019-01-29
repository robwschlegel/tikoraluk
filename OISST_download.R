# The purpose of this script is to serve as a place to 
# download NOAA OISST data.
# Ultimately the code here will serve as the basis for 
# the daily updating script for the MHWtracker.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(rerddap)
library(ncdf4)
library(abind)
library(tidync)
doMC::registerDoMC(cores = 50)

# The information for the NOAA OISST data
info(datasetid = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", url = "https://www.ncei.noaa.gov/erddap/")


# Meta-data ---------------------------------------------------------------

load("../tikoraluk/metadata/lon_lat_OISST.RData")
load("../tikoraluk/metadata/lon_OISST.RData")
lon_lat_OISST <- dplyr::arrange(lon_lat_OISST, lon, lat)
OISST_files <- dir("/data/home/data/OISST",
                   pattern = "avhrr-only", full.names = T)


# Functions ---------------------------------------------------------------

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
           temp = round(temp, 2))
  
  # Close the NetCDF connection and finish
  nc_close(nc)
  return(res)
}

# Function for creating arrays from data.frames
OISST_acast <- function(df, var, lon_sub = T){
  if(lon_sub){
    lon_lat_OISST_sub <- lon_lat_OISST %>% 
      filter(lon == df$lon[1])
  } else {
    lon_lat_OISST_sub <- lon_lat_OISST
  }
  if("temp" %in% colnames(df)){
    df$temp <- round(df$temp, 2)
  }
  res <- df %>%
    right_join(lon_lat_OISST_sub, by = c("lon", "lat")) %>%
    reshape2::acast(lat~lon, value.var = var)
}

# Function for merging 2018 data
# tester...
# lon_step <- lon_OISST[1200]
OISST_merge_2018 <- function(lon_step){
  
  ### Determine the correct lon slice/file
  # Determine lon slice
  lon_row <- which(lon_OISST == lon_step[1])
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  
  print(paste0("Began run on ",lon_row_pad," at ",Sys.time()))
  
  # Determine file name
  ncdf_file_name <- paste0("../data/OISST/avhrr-only-v2.ts.",lon_row_pad,".nc")
  
  ### Grab the old data
  # The 1982-2017 NetCDF files were not saved in a way that allows them to be writable...
  # So it is necessary to re-write all of the data in a writeable NetCDF file
  # for future access to these data once the daily script is running
  nc_old <- nc_open(filename = ncdf_file_name)
  nc_old_data_1 <- ncvar_get(nc_old, varid = "sst")
  dimnames(nc_old_data_1) <- list(lat = nc_old$dim$lat$vals,
                                  t = nc_old$dim$time$vals)
  nc_old_lon <- nc_old$dim$lon$vals[1]
  nc_close(nc_old)
  
  nc_old_data_2 <- as.data.frame(reshape2::melt(nc_old_data_1, value.name = "temp"), row.names = NULL) %>%
    mutate(#t = as.Date(t, origin = "1970-01-01"),
           lon = nc_old_lon) %>%
    select(lon, everything()) %>%
    # filter(lat == -70.375) %>%
    na.omit()
  
  ### Grab the 2018 lon slice
  OISST_step_1 <- OISST_2018_prep %>% 
    filter(lon  == lon_step) %>% 
    mutate(temp = ifelse(is.na(temp), NA, temp),
           t = as.integer(t),
           lon = ifelse(lon > 180, lon-360, lon)) %>% 
    na.omit()
  
  # Combine old and 2018 data
  sst_ALL <- rbind(nc_old_data_2, OISST_step_1)
  rm(nc_old_data_2, OISST_step_1)
  
  ### Define dimensions
  # lon
  xvals <- unique(sst_ALL$lon)
  if(length(xvals) > 1) stop("Too many lon values. Should only be one.")
  # xvals_df <- data.frame(lon = lon_lat_OISST$lon)
  nx <- length(xvals)
  lon_def <- ncdim_def("lon", "degrees_east", xvals)
  
  # lat
  yvals <- unique(lon_lat_OISST$lat)
  # yvals_df <- data.frame(lat = lon_lat_OISST$lat)
  ny <- length(yvals)
  lat_def <- ncdim_def("lat", "degrees_north", yvals)
  
  # time
  tunits <- "days since 1970-01-01 00:00:00"
  tvals <- seq(min(sst_ALL$t), max(sst_ALL$t))
  nt <- length(tvals)
  time_def <- ncdim_def("time", tunits, tvals, unlim = T)
  
  ### Create data arrays
  dfa <- sst_ALL %>%
    group_by(t) %>%
    nest() %>%
    mutate(data2 = purrr::map(data, OISST_acast, "temp")) %>%
    select(-data)
  rm(sst_ALL)
  
  dfa_temp <- abind(dfa$data2, along = 3)
  rm(dfa)
  
  ### Create new, WRITABLE, NetCDF file
  # Define variables
  temp_def <- ncvar_def(name = "sst", units = "deg_C", 
                        dim = list(lat_def, lon_def, time_def), 
                        longname = "Sea Surface Temperature",
                        missval = -999, prec = "float")
  
  # Create file
  ncdf_file_name_2018 <- paste0("OISST/avhrr-only-v2.ts.",lon_row_pad,".nc")
  ncout <- nc_create(filename = ncdf_file_name_2018, vars = list(temp_def), force_v4 = T)
  
  # Put variables
  
  ncvar_put(nc = ncout, varid = temp_def, vals = dfa_temp)
  
  # Additional attributes
  
  ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
  ncatt_put(ncout,"lat","axis","Y")
  ncatt_put(ncout,"time","axis","T")
  
  # Global attributes
  # ncatt_put(ncout, 0, writable, TRUE)
  # ncatt_put(ncout,0,"title",title$value)
  # ncatt_put(ncout,0,"institution",institution$value)
  # ncatt_put(ncout,0,"source",datasource$value)
  # ncatt_put(ncout,0,"references",references$value)
  # history <- paste("P.J. Bartlein", date(), sep=", ")
  # ncatt_put(ncout,0,"history",history)
  # ncatt_put(ncout,0,"Conventions",Conventions$value)
  
  ### Close file and exit
  nc_close(ncout)
  print(paste0("Finished run on ",lon_row_pad," at ",Sys.time()))
}

# Download ----------------------------------------------------------------

OISST_2018_1 <- OISST_dl(c("2018-01-01T00:00:00Z", "2018-06-30T00:00:00Z"))
OISST_2018_2 <- OISST_dl(c("2018-07-01T00:00:00Z", "2018-12-31T00:00:00Z"))

# 2018 dates
dates_2018 <- seq(as.Date("2018-01-01"), as.Date("2018-12-31"), by = "day")


# Prep --------------------------------------------------------------------

OISST_2018_prep_1 <- OISST_prep(OISST_2018_1)
OISST_2018_prep_2 <- OISST_prep(OISST_2018_2)

OISST_2018_prep <- rbind(OISST_2018_prep_1, OISST_2018_prep_2)
rm(OISST_2018_prep_1, OISST_2018_prep_2)

# Append data -------------------------------------------------------------

system.time(
  OISST_merge_2018(lon_OISST[1])
) # 121 seconds for one

plyr::ldply(lon_OISST, .fun = OISST_merge_2018, .parallel = TRUE)


# Load and visualise ------------------------------------------------------

nc <- nc_open("OISST_2018/avhrr-only-v2.ts.0001.nc", write = T)

nc <- nc_open("../data/OISST_2018/avhrr-only-v2.ts.0001.nc", write = T)
nc <- nc_open("../data/OISST/avhrr-only-v2.ts.0001.nc", write = T)
nc <- nc_open("avhrr-only-v2.ts.0001.nc", write = T)
nc <- nc_open("OISST/avhrr-only-v2.ts.1200.nc", write = T)
nc_close(nc)
# nc$writable <- TRUE

temp_def_test <- ncvar_def(name = "sst_test", units = "deg_C", 
                      dim = list(lat_def, lon_def, time_def), 
                      longname = "Sea Surface Temperature",
                      missval = -999, prec = "float")

ncvar_add(nc = nc, v = temp_def_test)
ncvar_put(nc = nc, varid = temp_def_test, vals = dfa_temp)

# 
# res <- ncvar_get(nc, varid = "sst")
# dimnames(res) <- list(lat = nc$dim$lat$vals,
#                       # lon = nc$dim$lon$vals,
#                       t = nc$dim$time$vals)
# nc_close(nc)
# 
# res2 <- as.data.frame(reshape2::melt(res, value.name = "temp"), row.names = NULL) %>%
#   mutate(t = as.Date(t, origin = "1970-01-01")) %>%
#   # select(lon, everything()) %>%
#   filter(lat == -70.375) %>%
#   na.omit()
# 
# ggplot(res2, aes(x = t, y = temp)) +
#   geom_line()
