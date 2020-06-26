# MUR_1km_download.R
# The purpose of this file is to download super-high-res MUR 1 KM data
# This product is ENORMOUS so be very specific with pixel ranges etc.


# Setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(tidync)
library(ncdf4)

# File location
base_URL <- "https://podaac-opendap.jpl.nasa.gov/opendap/allData/ghrsst/data/GDS2/L4/GLOB/JPL/MUR/v4.1"

# File date
file_date <- as.Date("2019-09-01")

# File name
file_name <- paste0(base_URL,"/",year(file_date),"/",yday(file_date),"/",
                    gsub("-", "", file_date),"090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc")

# Have a peek at the data
nc_open(file_name)
nc_close(file_name)


# Download ----------------------------------------------------------------

# Function for downloading a given subset from a desired date
download_MUR_1km <- function(date_range, lon_range, lat_range){
  
  
  MUR_dat <- tidync(file_name) %>%
    hyper_filter(lon = between(lon, lon_range[1], lon_range[2]),
                 lat = between(lat, lat_range[1], lat_range[2])) %>% 
    hyper_tibble() %>% 
    select(lon, lat, time, sst) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    mutate(t = as.Date(t, origin = "1978-01-01"))
}

# Download one day
system.time(
  download_MUR_1km(date_range = seq(as.Date("2019-09-01")))
)

# Download all dates in parallel

