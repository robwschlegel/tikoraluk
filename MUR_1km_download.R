# MUR_1km_download.R
# The purpose of this file is to download super-high-res MUR 1 KM data
# This product is ENORMOUS so be very specific with pixel ranges etc.


# Setup -------------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(lubridate)
library(tidync)
library(doParallel)
registerDoParallel(cores = 50)
# library(ncdf4)

# File location
base_URL <- "https://podaac-opendap.jpl.nasa.gov/opendap/allData/ghrsst/data/GDS2/L4/GLOB/JPL/MUR/v4.1"

# File date
# test_date <- as.Date("2019-09-01")

# File name
# test_name <- paste0(base_URL,"/",year(file_date),"/",yday(file_date),"/",
                    # gsub("-", "", file_date),"090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc")

# Have a peek at the data
# nc_open(test_name)
# nc_close(test_name)


# Download ----------------------------------------------------------------

# Function for downloading a given subset from a desired date
download_MUR_1km <- function(file_date, lon_range, lat_range){
  
  file_name <- paste0(base_URL,"/",year(file_date),"/",yday(file_date),"/",
         gsub("-", "", file_date),"090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc")
  suppressWarnings( # I think the tidync function does something funny with the remote access
  MUR_dat <- tidync(file_name) %>%
    hyper_filter(lon = between(lon, lon_range[1], lon_range[2]),
                 lat = between(lat, lat_range[1], lat_range[2])) %>% 
    hyper_tibble() %>% 
    select(lon, lat, time, analysed_sst) %>% 
    dplyr::rename(t = time, temp = analysed_sst) %>% 
    na.omit() %>% 
    mutate(t = as.Date(as.POSIXct(t, origin = "1981-01-01 00:00:00.0")),
           temp = temp-273.15) # Convert from K to C
  )
  return(MUR_dat)
}

# Download one day
system.time(
  Lab_test <- download_MUR_1km(file_date = as.Date("2019-09-01"),
                   lon_range = c(-62, -57.5), lat_range = c(56, 58.5))
) # 18 seconds

# Download all dates
Lab_sub <- plyr::ldply(seq(as.Date("2019-09-01"), as.Date("2019-10-31"), by = "day"), download_MUR_1km,
                            .parallel = F, lon_range = c(-62, -57.5), lat_range = c(56, 58.5))
write_csv(Lab_sub, "extracts/labrador_coast_1km.csv")

# Test visual
ggplot(data = filter(Lab_test, t == "2019-09-01"), aes(x = lon, y = lat)) +
  geom_raster(aes(fill = temp)) +
  # borders() +
  scale_fill_viridis_c()
