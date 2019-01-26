# The purpose of this script is to serve as a place to 
# download NOAA OISST data.
# Ultimately the code here will serve as the basis for 
# the daily updating script for the MHWtracker.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(rerddap)
library(ncdf4)

# The information for the NOAA OISST data
info(datasetid = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", url = "https://www.ncei.noaa.gov/erddap/")


# Functions ---------------------------------------------------------------

# This downloads the data
OISST_dl <- function(times){
  oisst_res <- griddap(x = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", 
                       url = "https://www.ncei.noaa.gov/erddap/", 
                       time = times, 
                       depth = c(0, 0),
                       latitude = c(-90, 90),
                       longitude = c(-180, 180),
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

# One then runs this function to add the data to each of the NetCDF files
OISST_save <- function(OISST_data){
  # Subset the data
  
  # Open the corresponding NetCDF file
  
  # Add the data
  
  # Close the file and exit
}

# Download ----------------------------------------------------------------

# 2018 dates
dates_2018 <- seq(as.Date("2018-01-01"), as.Date("2018-12-31"), by = "day")

# Loop ALL the data!
for(i in length(dates_2018)){
  OISST_step_1 <- OISST_dl(paste0(as.character(dates_2018)[i],"T00:00:00Z"))
  OISST_step_2 <- OISST_prep(OISST_step_1)
}
