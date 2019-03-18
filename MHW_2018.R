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

source("../tikoraluk/MHW_prep.R")


# Meta-data ---------------------------------------------------------------

load("../MHWapp/shiny/lon_OISST.RData")
load("../tikoraluk/metadata/lon_lat_OISST.RData")
lon_lat_OISST <- arrange(lon_lat_OISST, lon, lat)

# File locations
base_files <- dir("../data/MHW", pattern = "MHW.calc.", full.names = T)
MHW_event_files <- dir("../data/event", pattern = "MHW.event.", full.names = T)
# seas_thresh_files <- dir("../data/thresh", pattern = "MHW.seas.thresh.", full.names = T)
cat_lon_files <- dir("../data/cat_lon", full.names = T)
# cat_clim_files <- as.character(dir(path = "../data/cat_clim", pattern = "cat.clim", 
                                   # full.names = TRUE, recursive = TRUE))


# Download 2018 data ------------------------------------------------------

# This downloads the data
# OISST_dl <- function(times){
#   oisst_res <- griddap(x = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", 
#                        url = "https://www.ncei.noaa.gov/erddap/", 
#                        time = times, 
#                        depth = c(0, 0),
#                        latitude = c(-89.875, 89.875),
#                        longitude = c(0.125, 359.875),
#                        fields = "sst")
# }
# 
# OISST_2018_1 <- OISST_dl(c("2018-01-01T00:00:00Z", "2018-06-30T00:00:00Z"))
# OISST_2018_2 <- OISST_dl(c("2018-07-01T00:00:00Z", "2018-12-31T00:00:00Z"))


# Prep 2018 data ----------------------------------------------------------

# This then preps them for further use
# OISST_prep <- function(nc_file){
#   
#   # Open the NetCDF connection
#   nc <- nc_open(nc_file$summary$filename)
#   
#   # Extract the SST values and add the lon/lat/time dimension names
#   res <- ncvar_get(nc, varid = "sst")
#   dimnames(res) <- list(lon = nc$dim$longitude$vals,
#                         lat = nc$dim$latitude$vals,
#                         t = nc$dim$time$vals)
#   
#   # Convert the data into a 'long' dataframe for use in the 'tidyverse' ecosystem
#   res <- as.data.frame(reshape2::melt(res, value.name = "temp"), row.names = NULL) %>% 
#     mutate(t = as.Date(as.POSIXct(t, origin = "1970-01-01 00:00:00")),
#            temp = ifelse(is.na(temp), NA, temp),
#            temp = round(temp, 2),
#            lon = ifelse(lon > 180, lon-360, lon))
#   
#   # Close the NetCDF connection and finish
#   nc_close(nc)
#   return(res)
# }

# OISST_2018_prep_1 <- OISST_prep(OISST_2018_1)
# OISST_2018_prep_2 <- OISST_prep(OISST_2018_2)
# 
# OISST_2018_prep <- rbind(OISST_2018_prep_1, OISST_2018_prep_2)
# rm(OISST_2018_prep_1, OISST_2018_prep_2)

# Load, process, append, and save all 1982 - 2018 MHW data ----------------

# OISST_acast <- function(df){
#   # Ensurecorrect grid size
#   lon_lat_OISST_sub <- lon_lat_OISST %>% 
#     filter(lon == df$lon[1])
#   # Round data for massive file size reduction
#   df$temp <- round(df$temp, 2)
#   # Force grid
#   res <- df %>%
#     right_join(lon_lat_OISST_sub, by = c("lon", "lat"))
#   # Create array
#   res_array <- base::array(res$temp, dim = c(720,1,1))
#   dimnames(res_array) <- list(lat = lon_lat_OISST_sub$lat,
#                               lon = unique(lon_lat_OISST_sub$lon),
#                               t = unique(na.omit(res$t)))
#   return(res_array)
# }

# Function that loads and merges sst/seas/thresh for a given lon_step
# tester...
# lon_step <- lon_OISST[721]
# start_date <- min(previous_event_index$date_end)
# end_date <- as.Date("2018-12-31")
sst_seas_thresh_merge <- function(lon_step, start_date, end_date){
  # OISST data
  nc_OISST <- nc_open(dir("../data/OISST", full.names = T)[which(lon_step == lon_OISST)])
  # lat_vals <- as.vector(nc_OISST$dim$lat$vals)
  time_index <- as.Date(ncvar_get(nc_OISST, "time"), origin = "1970-01-01")
  # time_old_index <- time_index[time_index <= max(current_dates)]
  time_extract_index <- time_index[which(start_date == time_index):which(end_date == time_index)]
  sst_raw <- ncvar_get(nc_OISST, "sst",
                       start = c(1,1,(which(min(time_extract_index) == time_index))),
                       count = c(-1,-1,length(time_extract_index)))
  if(length(time_extract_index) == 1) dim(sst_raw) <- c(720,1,1)
  dimnames(sst_raw) <- list(lat = nc_OISST$dim$lat$vals,
                            t = time_extract_index)
  nc_close(nc_OISST)
  
  # Prep SST for further use
  sst <- as.data.frame(reshape2::melt(sst_raw, value.name = "temp"), row.names = NULL) %>%
    mutate(t = as.Date(t, origin = "1970-01-01")) %>%
    na.omit() %>% 
    dplyr::rename(ts_x = t, ts_y = temp) %>% 
    group_by(lat) %>%
    nest() %>%
    mutate(whole_data = map(data, heatwaveR:::make_whole_fast)) %>%
    select(-data) %>%
    unnest() %>% 
    dplyr::rename(t = ts_x, temp = ts_y)
  
  # seas.thresh data
  nc_seas <- nc_open(dir("../data/thresh", full.names = T)[which(lon_step == lon_OISST)])
  seas <- ncvar_get(nc_seas, "seas")
  dimnames(seas) <- list(lat = nc_seas$dim$lat$vals,
                         doy = nc_seas$dim$time$vals)
  thresh <- ncvar_get(nc_seas, "thresh")
  dimnames(thresh) <- list(lat = nc_seas$dim$lat$vals,
                           doy = nc_seas$dim$time$vals)
  nc_close(nc_seas)
  seas <- as.data.frame(reshape2::melt(seas, value.name = "seas"), row.names = NULL) %>%
    na.omit() %>% 
    dplyr::arrange(lat, doy)
  thresh <- as.data.frame(reshape2::melt(thresh, value.name = "thresh"), row.names = NULL) %>%
    na.omit() %>% 
    dplyr::arrange(lat, doy)
  
  # Check for which pixels have temperatures above the threshold
  sst_seas_thresh <- sst %>% 
    left_join(seas, by = c("lat", "doy")) %>%
    left_join(thresh, by = c("lat", "doy")) 
  return(sst_seas_thresh)
}

# Function for calculating new MHW event metrics and categories
# df <- previous_event_index[340,]
event_cat_calc <- function(df, sst_seas_thresh, MHW_event_data, MHW_cat_data){
  
  # Extract necessary SST
  sst_step_1 <- sst_seas_thresh %>% 
    filter(lat == df$lat[1],
           t >= df$date_end)
  
  # Calculate events
  event_base <- detect_event(sst_step_1)
  event_step_1 <- event_base$event %>% 
    mutate(lon = df$lon, lat = df$lat) %>% 
    dplyr::select(lon, lat, event_no, duration:intensity_max, intensity_cumulative)
  event_step_2 <- MHW_event_data %>% 
    filter(lat == df$lat[1],
           date_end != as.Date("2017-12-31")) %>% 
    rbind(event_step_1) %>% 
    mutate(event_no = seq(1:n())) %>%
    mutate_all(round, 3)
  
  # Calculate categories
  cat_step_1 <- category(event_base, climatology = T)$climatology %>% 
    mutate(event_no = event_no + df$previous_event[1],
           lon = df$lon,
           lat = df$lat) %>% 
    select(lon, lat, t, event_no, intensity, category)
  cat_step_2 <- MHW_cat_data %>%
    filter(lat == df$lat) %>%
    filter(!event_no %in% cat_step_1$event_no) %>% 
    rbind(cat_step_1) %>%
    mutate(intensity = round(intensity, 2))
  
  # Exit
  event_cat <- list(event = event_step_2,
                    cat = cat_step_2)
  return(event_cat)
}

# The function that loads old data, calculates new data,
# sticks it all together and re-saves it
# tester...
# lon_step <- lon_OISST[721]
MHW_load_proc_save <- function(lon_step){
  
  ### Determine correct lon/row/slice
  lon_row <- which(lon_OISST == lon_step)
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  print(paste0("Began run on ",lon_row_pad," at ",Sys.time()))
  
  ### Get old event and cat results
  load(base_files[lon_row])
  MHW_event_data <- MHW_event(MHW_res) %>% 
    mutate(lon = ifelse(lon > 180, lon-360, lon)) %>% 
    dplyr::select(lon, lat, event_no, duration:intensity_max, intensity_cumulative)
  MHW_cat_data <- MHW_cat_clim(MHW_res) %>% 
    mutate(lon = ifelse(lon > 180, lon-360, lon))

  ### Find the index of previous events
  previous_event_index <- MHW_event_data %>% 
    group_by(lat) %>% 
    mutate(previous_event = as.integer(ifelse(max(date_end) == as.Date("2017-12-31"), 
                                   max(event_no)-1, max(event_no)))) %>%
    filter(event_no == previous_event)
  
  ### Get necessary temp data
  sst_seas_thresh <- sst_seas_thresh_merge(lon_step, 
                                           min(previous_event_index$date_end),
                                           as.Date("2018-12-31"))
  
  ### Calculate new event/cat data
  MHW_event_cat <- previous_event_index %>% 
    mutate(lat2 = lat) %>% 
    group_by(lat2) %>% 
    nest() %>% 
    mutate(event_cat_res = map(data, event_cat_calc,
                               sst_seas_thresh = sst_seas_thresh,
                               MHW_event_data = MHW_event_data,
                               MHW_cat_data = MHW_cat_data)) %>% 
    select(-data, -lat2) %>% 
    unnest()
  # Save results and exit
  MHW_event_new <- MHW_event_cat %>% 
    filter(row_number() %% 2 == 1) %>% 
    unnest()
  # MHW_event_new_test <- MHW_event_new %>% 
  #   filter(lat == df$lat)
  saveRDS(MHW_event_new, file = MHW_event_files[lon_row])
  
  MHW_cat_new <- MHW_event_cat %>% 
    filter(row_number() %% 2 == 0) %>% 
    unnest()
  # MHW_cat_new_test <- MHW_cat_new %>% 
  #   filter(lat == df$lat)
  saveRDS(MHW_cat_new, file = cat_lon_files[lon_row])


  print(paste0("Finished run on ",lon_row_pad," at ",Sys.time()))  
}


# Run all of the lon_steps ------------------------------------------------

doMC::registerDoMC(cores = 50)

# plyr::ldply(lon_OISST, .fun = MHW_load_proc_save, .parallel = TRUE)
