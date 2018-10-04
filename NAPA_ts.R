# The purpose of this script is to open and extract
# from every NAPA NetCDF file a specific series of pixels
# in order to create time series that are then useful for
# any range of other applicatios.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ncdf4)
library(doMC); doMC::registerDoMC(cores = 50)


# Data --------------------------------------------------------------------

# The file location
NAPA_files <- dir("../../data/NAPA025/1d_grid_T_2D", full.names = T)

# The NAPA lon/lat values
load("NAPA/mask_long.RData")


# Functions ---------------------------------------------------------------

## Testing...
# file_name <- NAPA_files[1]

load_NAPA_sst_sub <- function(df){
  nc <- nc_open(as.character(df$file_name))
  date_start <- ymd(str_sub(basename(as.character(df$file_name)), start = 29, end = 36))
  date_end <- ymd(str_sub(basename(as.character(df$file_name)), start = 38, end = 45))
  date_seq <- seq(date_start, date_end, by = "day")
  sst <- as.data.frame(ncvar_get(nc, varid = "sst")[df$lon_sub,,]) %>% 
    mutate(lon = as.numeric(nc$dim$x$vals)[df$lon_sub]) %>% 
    gather(-lon, key = lat, value = temp) %>%
    mutate(lat = rep(as.numeric(nc$dim$y$vals), times = 5),
           temp = ifelse(temp == 0, NA, temp),
           t = rep(seq(date_start, date_end, by = "day"), each = 735)) %>%
    select(lon, lat, t, temp) %>%
    # na.omit() %>% 
    filter(lon == df$lon_sub) %>%
    left_join(mask_long, by = c("lon", "lat"))
  nc_close(nc)
  return(sst)
}

# length(unique(mask_long$lon))
## Tester...
# df <- lon_sub_list %>% 
#   select(-x)
save_NAPA_sst_sub <- function(df){
  lon_sub <- df$lon_sub
  NAPA_sst_sub <- data.frame(file_name = NAPA_files,
                             lon_sub = rep(lon_sub, times = length(NAPA_files)),
                             y = 1:length(NAPA_files)) %>% 
    group_by(y) %>%
    nest() %>% 
    mutate(sst = purrr::map(data, load_test)) %>% 
    unnest(sst)
  save(NAPA_sst_sub, file = paste0("../data/NAPA_sst_sub_",lon_sub,".RData"))
}


# Process data ------------------------------------------------------------

lon_sub_list <- data.frame(lon_sub = 1:length(unique(mask_long$lon)),
                           x = 1:length(unique(mask_long$lon)))

# Run Thursday, October 4th, 2018
system.time(
save_NAPA_sst_sub(lon_sub_list[1,])
) # 466 seconds

system.time(
  plyr::ddply(lon_sub_list[2:10,], .variables = "x",
              .fun = save_NAPA_sst_sub, .parallel = TRUE)
) # xxx seconds on 50 cores

system.time(
  save_NAPA_sst_sub(lon_sub_list[2,])
) # 507 seconds

load_test <- function(df){
  nc <- nc_open(as.character(df$file_name))
  date_start <- ymd(str_sub(basename(as.character(df$file_name)), start = 29, end = 36))
  date_end <- ymd(str_sub(basename(as.character(df$file_name)), start = 38, end = 45))
  date_seq <- seq(date_start, date_end, by = "day")
  sst <- as.data.frame(ncvar_get(nc, varid = "sst")[df$lon_sub,,]) %>% 
    mutate(lon = as.numeric(nc$dim$x$vals)[df$lon_sub]) %>% 
    gather(-lon, key = lat, value = temp) %>%
    mutate(lat = rep(as.numeric(nc$dim$y$vals), times = 5),
           temp = ifelse(temp == 0, NA, temp),
           t = rep(seq(date_start, date_end, by = "day"), each = 735)) %>%
    select(lon, lat, t, temp) %>%
    # na.omit() %>% 
    filter(lon == df$lon_sub) %>%
    left_join(mask_long, by = c("lon", "lat"))
  nc_close(nc)
  return(sst)
}

NAPA_sst_sub <- data.frame(file_name = NAPA_files,
                           lon_sub = rep(2, times = length(NAPA_files)),
                           y = 1:length(NAPA_files)) %>% 
  group_by(y) %>%
  nest() %>% 
  mutate(sst = purrr::map(data, load_test)) %>% 
  unnest(sst)

