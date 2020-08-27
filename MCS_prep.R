# MCS_prep.R
# This script houses functions and useful bits used by other scripts


# Setup -------------------------------------------------------------------

source("MHW_prep.R")


# Functions ---------------------------------------------------------------

# Subset event metric files
load_MCS_event_sub <- function(file_name, date_range,
                               lon_range = NA, lat_range){
  load(file_name)
  res <- MCS_res %>% 
    dplyr::select(lon, lat, event) %>% 
    unnest(event) %>% 
    filter(row_number() %% 2 == 0) %>% 
    unnest(event) %>% 
    filter(date_start >= date_range[1], date_start <= date_range[2],
           # lon >= lon_range[1], lon <= lon_range[2],
           lat >= lat_range[1], lat <= lat_range[2]) #%>%
  # select(lon, lat, t, temp)
  rm(MCS_res)
  return(res)
}

# Subset category files
load_MCS_cat_sub <- function(file_name, date_range,
                             lon_range = NA, lat_range){
  load(file_name)
  res <- MCS_res %>% 
    dplyr::select(lon, lat, cat) %>% 
    unnest(cat) %>% 
    filter(row_number() %% 2 == 0) %>% 
    unnest(cat) %>% 
    filter(peak_date >= date_range[1], peak_date <= date_range[2],
           # lon >= lon_range[1], lon <= lon_range[2],
           lat >= lat_range[1], lat <= lat_range[2]) #%>%
  # select(lon, lat, t, temp)
  rm(MCS_res)
  return(res)
}

# Subset climatology files
load_MCS_clim_sub <- function(file_name, date_range,
                              lon_range = NA, lat_range){
  load(file_name)
  res <- MCS_res %>% 
    dplyr::select(lon, lat, event) %>% 
    unnest(event) %>% 
    filter(row_number() %% 2 == 1) %>% 
    unnest(event) %>% 
    filter(t >= date_range[1], t <= date_range[2],
           # lon >= lon_range[1], lon <= lon_range[2],
           lat >= lat_range[1], lat <= lat_range[2]) #%>%
  # select(lon, lat, t, temp)
  rm(MCS_res)
  return(res)
}

# Function for loading all data streams
load_MCS_ALL <- function(bbox){
  # Load event data
  event_data <- plyr::ldply(MCS_RData[which(lon_OISST >= bbox[3] & lon_OISST <= bbox[4])], 
                            .fun = load_MCS_event_sub, .parallel = T, 
                            date_range = c("1982-01-01", "2020-12-31"),
                            lat_range = c(bbox[1], bbox[2]))
  
  # Load category data
  cat_data <- plyr::ldply(MCS_RData[which(lon_OISST >= bbox[3] & lon_OISST <= bbox[4])], 
                          .fun = load_MCS_cat_sub, .parallel = T, 
                          date_range = c("1982-01-01", "2020-12-31"),
                          # date_range = date_range,
                          lat_range = c(bbox[1], bbox[2]))
  
  # Load clim data
  clim_data <- plyr::ldply(MCS_RData[which(lon_OISST >= bbox[3] & lon_OISST <= bbox[4])], 
                           .fun = load_MCS_clim_sub, .parallel = T, 
                           date_range = c("1982-01-01", "2020-12-31"),
                           # date_range = date_range,
                           lat_range = c(bbox[1], bbox[2]))
  
  # Combine into list and exut
  list_data <- list(event_data = event_data,
                    cat_data = cat_data,
                    clim_data = clim_data)
  gc()
  return(list_data)
}

# Function that loads and merges sst/seas/thresh for a given lon_step
# lon_step <- lon_OISST[2]
# date_range <- as.Date("2018-01-01")
# date_range <- c(as.Date("2016-02-01"), as.Date("2017-04-01"))
sst_seas_thresh_merge <- function(lon_step, date_range){
  
  # Establish lon row number
  lon_row <- which(lon_OISST == lon_step)
  
  # Establish date range
  if(length(date_range) == 1) date_range <- c(date_range, Sys.Date())
  
  # OISST data
  tidync_OISST <- tidync(OISST_files[lon_row]) %>% 
    hyper_filter(time = between(time, as.integer(date_range[1]), as.integer(date_range[2]))) %>% 
    hyper_tibble() %>% 
    mutate(time = as.Date(time, origin = "1970-01-01"),
           year = year(time)) %>% 
    dplyr::rename(t = time, temp = sst) %>%
    mutate(doy = yday(t)) %>% 
    group_by(year) %>% 
    mutate(doy = ifelse(!leap_year(year),
                        ifelse(doy > 59, doy+1, doy), doy)) %>% 
    ungroup() %>%
    select(lon, lat, t, doy, temp)
  
  # Merge to seas/thresh and exit
  sst_seas_thresh <- tidync_OISST %>%
    left_join(hyper_tibble(tidync(seas_thresh_files[lon_row])),
              by = c("lon", "lat", "doy" = "time")) %>%
    mutate(anom = round(temp - seas, 2))
  return(sst_seas_thresh)
}