# NB: The most recent version of this script may be found at: 'MCSglobal/code/functions.R'

# MCS_prep.R
# This script houses functions and useful bits used by other scripts


# Setup -------------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
source("MHW_prep.R")
library(XML)


# Meta-data ---------------------------------------------------------------

## Potential colour palettes
# w3schools.com/colors/colors_groups.asp
# sciviscolor.org/home/environmental-palettes/

# Consider ROYGBIV
# Because MHWs use ROY it could be good to use GBIV for MCSs
# Maybe don't worry about perceptual symmetry
# Just pick the best colours from a colour wheel

# Load an XML file containing weird rgb vlaues and convert to hex
rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255)

# BlueSpectrum colour palette from sciviscolor.org/home/environmental-palettes/
BlueSpectrum <- t(data.frame(xmlToList(xmlParse("metadata/BlueSpectrum.xml"))$ColorMap[1:27], stringsAsFactors = F)) %>% 
  data.frame(., stringsAsFactors = F) %>% 
  remove_rownames(.) %>% 
  select(r, g, b) %>% 
  mutate(r = round(as.numeric(r)*255), g = round(as.numeric(g)*255), b = round(as.numeric(b)*255),
         hex = rgb2hex(r, g, b))
write_csv(BlueSpectrum, "metadata/BlueSpecturm.csv")

# BlueWater colour palette from sciviscolor.org/home/environmental-palettes/
BlueWater <- t(data.frame(xmlToList(xmlParse("metadata/BlueWater.xml"))$ColorMap[1:12], stringsAsFactors = F)) %>% 
  data.frame(., stringsAsFactors = F) %>% 
  remove_rownames(.) %>% 
  select(r, g, b) %>% 
  mutate(r = round(as.numeric(r)*255), g = round(as.numeric(g)*255), b = round(as.numeric(b)*255),
         hex = rgb2hex(r, g, b))
# write_csv(BlueWater, "metadata/BlueWater.csv")

# This is negotiable...
# MCS_palette <- c(BlueSpectrum$hex[6], BlueSpectrum$hex[13], BlueSpectrum$hex[20], BlueSpectrum$hex[27])
MCS_palette <- c(BlueWater$hex[10], BlueWater$hex[7], BlueWater$hex[4], BlueWater$hex[2])

# Set line colours
lineCol <- c(
  "Temperature" = "black",
  "Climatology" = "grey40",
  "Threshold" = "darkorchid",
  "2x Threshold" = "darkorchid",
  "3x Threshold" = "darkorchid",
  "4x Threshold" = "darkorchid",
  "< -1.5C" = "darkorchid"
)

# Set category fill colours
fillCol <- c(
  "Moderate" = MCS_palette[1],
  "Strong" = MCS_palette[2],
  "Severe" = MCS_palette[3],
  "Extreme" = MCS_palette[4],
  "Ice" = "thistle1"
)

# The MCS colour palette
MCS_colours <- c(
  "I Moderate" = "#A4D4E0",
  "II Strong" = "#5B80A6",
  "III Severe" = "#2A3C66",
  "IV Extreme" = "#111433",
  "V Ice" = "thistle1"
)

# OISST coords
# The lon coords for the OISST data
load("metadata/lon_OISST.RData")
lon_OISST <- ifelse(lon_OISST > 180, lon_OISST-360, lon_OISST)
lat_OISST <- seq(-89.875, 89.875, by = 0.25)
lon_lat_OISST <- base::expand.grid(lon_OISST, lat_OISST) %>% 
  dplyr::rename(lon = Var1, lat = Var2) %>% 
  arrange(lon, lat) %>% 
  data.frame()

# File locations
OISST_files <- dir("../data/OISST", pattern = "avhrr-only", full.names = T)
MCS_event_files <- dir("../data/MCS", full.names = T)
MCS_lon_files <- dir("../data/cat_lon/MCS", full.names = T)
MCS_cat_files <- dir("../data/cat_clim/MCS", full.names = T, recursive = T)
MCS_count_trend_files <- dir("annual_summary_MCS", pattern = "count_trend", full.names = T)
seas_thresh_files <- dir("../data/thresh", pattern = "MHW.seas.thresh.", full.names = T)

# Metadata
load("metadata/OISST_ocean_coords.Rdata")

# The base map
load("metadata/map_base.Rdata")

# Disable scientific notation
options(scipen = 9999)


# Functions ---------------------------------------------------------------

# Subset event metric files
load_MCS_event_sub <- function(file_name, date_range,
                               lon_range = NA, lat_range){
  res <- readRDS(file_name) %>% 
    dplyr::select(lon, lat, event) %>% 
    unnest(event) %>% 
    filter(row_number() %% 2 == 0) %>% 
    unnest(event) %>% 
    filter(date_start >= date_range[1], date_start <= date_range[2],
           # lon >= lon_range[1], lon <= lon_range[2],
           lat >= lat_range[1], lat <= lat_range[2]) #%>%
  # select(lon, lat, t, temp)
  return(res)
}

# Subset category files
load_MCS_cat_sub <- function(file_name, date_range,
                             lon_range = NA, lat_range){
  res <- readRDS(file_name) %>% 
    dplyr::select(lon, lat, cat) %>% 
    unnest(cat) %>% 
    filter(row_number() %% 2 == 0) %>% 
    unnest(cat) %>% 
    filter(peak_date >= date_range[1], peak_date <= date_range[2],
           # lon >= lon_range[1], lon <= lon_range[2],
           lat >= lat_range[1], lat <= lat_range[2]) #%>%
  # select(lon, lat, t, temp)
  return(res)
}

# Subset climatology files
load_MCS_clim_sub <- function(file_name, date_range,
                              lon_range = NA, lat_range){
  res <- readRDS(file_name) %>% 
    dplyr::select(lon, lat, event) %>% 
    unnest(event) %>% 
    filter(row_number() %% 2 == 1) %>% 
    unnest(event) %>% 
    filter(t >= date_range[1], t <= date_range[2],
           # lon >= lon_range[1], lon <= lon_range[2],
           lat >= lat_range[1], lat <= lat_range[2]) #%>%
  # select(lon, lat, t, temp)
  return(res)
}

# Function for loading all data streams
load_MCS_ALL <- function(bbox){
  # Load event data
  event_data <- plyr::ldply(MCS_lon_files[which(lon_OISST >= bbox[3] & lon_OISST <= bbox[4])], 
                            .fun = load_MCS_event_sub, .parallel = T, 
                            date_range = c("1982-01-01", "2020-12-31"),
                            lat_range = c(bbox[1], bbox[2]))
  
  # Load category data
  cat_data <- plyr::ldply(MCS_lon_files[which(lon_OISST >= bbox[3] & lon_OISST <= bbox[4])], 
                          .fun = load_MCS_cat_sub, .parallel = T, 
                          date_range = c("1982-01-01", "2020-12-31"),
                          # date_range = date_range,
                          lat_range = c(bbox[1], bbox[2]))
  
  # Load clim data
  clim_data <- plyr::ldply(MCS_lon_files[which(lon_OISST >= bbox[3] & lon_OISST <= bbox[4])], 
                           .fun = load_MCS_clim_sub, .parallel = T, 
                           date_range = c("1982-01-01", "2020-12-31"),
                           # date_range = date_range,
                           lat_range = c(bbox[1], bbox[2]))
  
  # Combine into list and exit
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

