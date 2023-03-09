# The purpose of this script is to serve as a place to 
# download NOAA OISST data.
# Ultimately the code here will serve as the basis for 
# the daily updating script for the MHWtracker.


# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(rerddap)
library(ncdf4)
library(tidync)
library(abind)
library(geosphere)
library(doParallel); registerDoParallel(cores = 25) # 50 cores exceeds available RAM

# The information for the NOAA OISST data
# info(datasetid = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", url = "https://www.ncei.noaa.gov/erddap/")


# Meta-data ---------------------------------------------------------------

load("../tikoraluk/metadata/lon_lat_OISST.RData")
load("../tikoraluk/metadata/lon_OISST.RData")
lon_lat_OISST <- dplyr::arrange(lon_lat_OISST, lon, lat)
OISST_files <- dir("../data/OISST", pattern = "avhrr-only", full.names = T)

# Calculate square kilometres per pixel
# This function assumes a lon lat column on a 0.25 degree grid
grid_size <- function(df){
  # Distance for longitude
  lon_dist <- distm(c(df$lon-0.125, df$lat), c(df$lon+0.125, df$lat), fun = distHaversine)/1000
  # Distance for latitude
  lat_dist <- distm(c(df$lon, df$lat+0.125), c(df$lon, df$lat-0.125), fun = distHaversine)/1000
  # Total area
  sq_area <- data.frame(sq_area = lon_dist*lat_dist)
  # Combine and exit
  res <- cbind(df, sq_area)
  return(res)
}
# lon_lat_OISST_area <- plyr::ddply(mutate(lon_lat_OISST, plyr_idx = 1:n()), c("plyr_idx"), grid_size, .parallel = T)
# lon_lat_OISST_area$plyr_idx <- NULL
# save(lon_lat_OISST_area, file = "metadata/lon_lat_OISST_area.RData")

# Current date range
# load()


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
# lon_step <- lon_OISST[1]
# df <- OISST_2018_prep
OISST_merge_2018 <- function(lon_step, df){
  
  ### Determine the correct lon slice/file
  # Determine lon slice
  lon_row <- which(lon_OISST == lon_step)
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  
  print(paste0("Began run on ",lon_row_pad," at ",Sys.time()))
  
  # Determine file name
  ncdf_file_name <- paste0("../data/OISST/avhrr-only-v2.ts.",lon_row_pad,".nc")
  # ncdf_file_name <- paste0("../data/OISST_2018/avhrr-only-v2.ts.",lon_row_pad,"-test.nc")
  
  ### Open NetCDF and determine dates present
  nc <- nc_open(ncdf_file_name, write = T)
  
  # time_vals <- as.Date(nc$dim$time$vals, origin = "1970-01-01")
  time_vals <- seq(nc$dim$time$vals[1], length.out = length(nc$dim$time$vals))
  
  ### Grab the lon slice and data not yet in the NetCDF file
  OISST_step_1 <- df %>% 
    filter(lon  == lon_step,
           as.integer(t) > max(time_vals))
  
  if(nrow(OISST_step_1) > 0){
    
    ### Create data arrays
    OISST_step_2 <- OISST_step_1 %>% 
      mutate(temp = ifelse(is.na(temp), NA, temp),
             t = as.POSIXct(t),
             lon = ifelse(lon > 180, lon-360, lon)) %>% 
      na.omit()
    
    dfa <- OISST_step_2 %>%
      group_by(t) %>%
      nest() %>%
      mutate(data2 = purrr::map(data, OISST_acast, "temp")) %>%
      select(-data)
    
    dfa_temp <- abind(dfa$data2, along = 3)
    
    ### Add data to the corresponding NetCDF file
    for(i in 1:length(dfa$t)){
      ncvar_put(nc = nc, varid = "sst", vals = dfa_temp[,,i], verbose = FALSE,
                start = c(1,1,length(nc$dim$time$vals)+i), count = c(720,1,1))
    }
  }
  # sst <- ncvar_get(nc, "sst")
  
  ### Close file and exit
  # nc_sync(nc)
  nc_close(nc)
  print(paste0("Finished run on ",lon_row_pad," at ",Sys.time()))
}

# Function for downloading a specific set of pre-defined pixels
# NB: One may think this would be faster than extracting a square bbox, but this is almost never the case
# I've saved this function here because it is still a promising method of extracting data
# It shows promise when one needs to extract multiple different bbox from a single NetCDF file
# For example see `download_MUR_ALL()` in ~/WP1/functions.R
OISST_pix_dl <- function(file_date, pixel_coords){
  file_date_flat <- gsub("-", "", file_date)
  OISST_url <- paste0("https://www.ncei.noaa.gov/thredds/dodsC/OisstBase/NetCDF/V2.1/AVHRR/",
                      substr(file_date_flat, 1, 6),"/oisst-avhrr-v02r01.",file_date_flat,".nc")
  OISST_raster <- brick(OISST_url, varname = "sst")
  OISST_res <- data.frame(pixel_coords,
                          t = file_date,
                          temp = as.vector(raster::extract(OISST_raster,
                                                           pixel_coords[c("lon", "lat")], method = "simple")))
  rm(OISST_raster); gc()
  return(OISST_res)
}

# Download ----------------------------------------------------------------

OISST_2018_1 <- OISST_dl(c("2018-01-01T00:00:00Z", "2018-06-30T00:00:00Z"))
OISST_2018_2 <- OISST_dl(c("2018-07-01T00:00:00Z", "2018-12-31T00:00:00Z"))

# 2018 dates
# dates_2018 <- seq(as.Date("2018-01-01"), as.Date("2018-12-31"), by = "day")


# Prep --------------------------------------------------------------------

OISST_2018_prep_1 <- OISST_prep(OISST_2018_1)
OISST_2018_prep_2 <- OISST_prep(OISST_2018_2)

OISST_2018_prep <- rbind(OISST_2018_prep_1, OISST_2018_prep_2)
rm(OISST_2018_prep_1, OISST_2018_prep_2)


# Append data -------------------------------------------------------------

# system.time(
#   OISST_merge_2018(lon_OISST[1], df = OISST_2018_prep)
# ) # 10 seconds for one

# plyr::ldply(lon_OISST, .fun = OISST_merge_2018, .parallel = TRUE, df = OISST_2018_prep)


# Load and visualise ------------------------------------------------------

# nc <- nc_open("../data/OISST/avhrr-only-v2.ts.0001.nc")
# # nc_close(nc)
# #
# res <- ncvar_get(nc, varid = "sst")
# dimnames(res) <- list(lat = nc$dim$lat$vals)#,
#                       # lon = nc$dim$lon$vals,
#                       # t = as.vector(nc$dim$time$vals))
# nc_close(nc)
# 
# res2 <- as.data.frame(reshape2::melt(res, value.name = "temp"), row.names = NULL) %>%
#   filter(lat == 36.125) %>%
#   dplyr::rename(t = Var2) %>%
#   mutate(t = as.Date(t, origin = "1981-12-31")) %>%
#   # mutate(t = as.Date(t, origin = "1970-01-01")) %>%
#   # select(lon, everything()) %>%
#   na.omit()
# 
# ggplot(res2, aes(x = t, y = temp)) +
#   geom_line()


# Monthly global averages -------------------------------------------------

# Function that returns monthly means per pixel
pixel_monthly <- function(file_idx){
  file_sub <- OISST_files[file_idx]
  res <- tidync(file_sub) %>% 
    hyper_tibble() %>% 
    dplyr::rename(temp = sst, t = time) %>% 
    mutate(t = as.Date(t, origin = "1970-01-01"))
  res_month <- res %>% 
    mutate(t = lubridate::round_date(t, "month")) %>% 
    group_by(lon, lat, t) %>% 
    summarise(temp = round(mean(temp, na.rm = T), 3), .groups = "drop")
  rm(res); gc()
  return(res_month)
}
registerDoParallel(cores = 50)
OISST_monthly <- plyr::ldply(1:1440, pixel_monthly, .parallel = T)

# These are then averaged into the global monthly values
OISST_global_monthly <- OISST_monthly %>% 
  group_by(t) %>% 
  summarise(temp = round(mean(temp, na.rm = T), 3), .groups = "drop")
write_csv(OISST_global_monthly, "extracts/OISST_global_monthly.csv")

# Plot the slope
OISST_global_monthly_2020 <- OISST_global_monthly %>% 
  filter(t <= "2020-12-31")
slope <- lm(temp~seq(1:nrow(OISST_global_monthly_2020)), OISST_global_monthly_2020)
slope_label <- round(as.numeric(slope$coefficients[2])*12*10, 3)
ggplot(filter(OISST_global_monthly_2020), aes(x = t, y = temp)) +
  geom_line() +
  geom_smooth(method = "lm") +
  geom_label(aes(x = as.Date("1990-01-01"), y = 14.2, label = paste0("slope = ",slope_label,"°C/dec"))) +
  scale_x_date(expand = c(0, 0)) +
  labs(x = NULL, y = "Temperature (°C)", title = "Global monthly NOAA OISST: 1982-2020")


# Extract pixels ----------------------------------------------------------

# Function for extracting and prepping a single pixel
extract_pixel <- function(lon_1, lat_1, max_date){
  sst_1 <- tidync(OISST_files[which(lon_OISST == lon_1)]) %>% 
    hyper_filter(lat = lat == lat_1) %>% 
    hyper_tibble() %>% 
    mutate(t = as.Date(time, origin = "1970-01-01")) %>% 
    filter(t <= max_date) %>%
    dplyr::rename(temp = sst) %>% 
    dplyr::select(t, temp)
}

# Get pixels
sst_1 <- extract_pixel(112.625, -29.375, "2022-12-31")
save(sst_1, file = "extracts/sst_1.RData")
sst_2 <- extract_pixel(9.125, 43.625, "2022-12-31")
save(sst_2, file = "extracts/sst_2.RData")
sst_3 <- extract_pixel(293.125, 43.125, "2022-12-31")
save(sst_3, file = "extracts/sst_3.RData")

