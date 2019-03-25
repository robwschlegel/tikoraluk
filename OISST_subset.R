# OISST_subset.R
# The purpose of this script is to provide a place to put convenience functions
# for subsetting OISST data etc.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ncdf4)
doMC::registerDoMC(cores = 50)


# Meta-data ---------------------------------------------------------------

OISST_files <- dir(path = "~/data/OISST", pattern = "avhrr-only", full.names = T)
load("metadata/lon_OISST.RData")
lat_OISST <- seq(-89.875, 89.875, by = 0.25)


# Functions ---------------------------------------------------------------

# testers...
# df <- data.frame(lon = c(-39, -37),
                 # lat = c(23, 26))
OISST_extract <- function(df){
  
  df_360 <- df %>% 
    mutate(lon = ifelse(lon < 0, lon+360, lon))
  
  lon_rows <- which(lon_OISST <= max(df_360$lon) & lon_OISST >= min(df_360$lon))
  lat_rows <- which(lat_OISST <= max(df$lat) & lat_OISST >= min(df$lat))
  
  OISST_sub <- plyr::ldply(lon_rows, OISST_one_lon,
                           .parallel = T, lat_rows = lat_rows)
  
  OISST_corrected <- OISST_sub %>% 
    mutate(lon = ifelse(min(df$lon) < 0,
                        ifelse(lon > 180, lon-360, lon), lon))
  return(OISST_sub)
}

# lon_row <- lon_rows[1]
OISST_one_lon <- function(lon_row, lat_rows){
  lon_step <- lon_OISST[lon_row]
  # OISST data
  nc_OISST <- nc_open(OISST_files[lon_row])
  # lat_vals <- as.vector(nc_OISST$dim$lat$vals)
  time_index <- as.Date(ncvar_get(nc_OISST, "time"), origin = "1970-01-01")
  # time_old_index <- time_index[time_index <= max(current_dates)]
  # time_extract_index <- time_index[which(min(previous_event_index$date_end) == time_index):length(time_index)]
  sst_raw <- ncvar_get(nc_OISST, "sst", start = c(min(lat_rows),1,1), count = c(length(lat_rows),1,-1))
  dimnames(sst_raw) <- list(lat = nc_OISST$dim$lat$vals[lat_rows], t = time_index)
  nc_close(nc_OISST)

  # Prep SST for further use
  sst <- as.data.frame(reshape2::melt(sst_raw, value.name = "temp"), row.names = NULL) %>%
    mutate(lon = lon_step,
           t = as.Date(t, origin = "1970-01-01")) %>% 
    select(lon, lat, t, temp)
  return(sst)
}


# Chosen extractions ------------------------------------------------------

open_atlantic <- OISST_extract(data.frame(lon = c(-39, -37),
                                          lat = c(23, 26)))
write_csv(x = open_atlantic, path = "extracts/open_atlantic.csv.gz")
