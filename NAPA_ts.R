# The purpose of this script is to open and extract
# from every NAPA NetCDF file a specific series of pixels
# in order to create time series that are then useful for
# any range of other applicatios.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ncdf4)
library(doMC); doMC::registerDoMC(cores = 50)
library(stringr)

# source("load_mat.R")

# Data --------------------------------------------------------------------

# The file location
NAPA_files <- dir("../../data/NAPA025/1d_grid_T_2D", full.names = T)

# The NAPA to OISST lon/lat mask
load("metadata/lon_lat_NAPA_OISST.RData")

# The OISST lon values for subsetting
load("metadata/lon_OISST.RData")


# Functions ---------------------------------------------------------------

file_name = NAPA_files[100]

load_NAPA_sst_sub <- function(file_name){
  nc <- nc_open(as.character(file_name))
  date_start <- ymd(str_sub(basename(as.character(file_name)), start = 29, end = 36))
  date_end <- ymd(str_sub(basename(as.character(file_name)), start = 38, end = 45))
  date_seq <- seq(date_start, date_end, by = "day")
  # sst <- as.data.frame(ncvar_get(nc, varid = "sst")[df$lon_sub,,]) %>% 
  sst <- as.data.frame(ncvar_get(nc, varid = "sst")) %>% 
    mutate(lon = as.numeric(nc$dim$x$vals)) %>%
    # mutate(lon = as.numeric(nc$dim$x$vals)[coords$lon]) %>% 
    # mutate(lon = coords$lon) %>% 
    gather(-lon, key = lat, value = temp) %>%
    mutate(t = rep(date_seq, each = 388080),
           lat = rep(as.numeric(nc$dim$y$vals), each = 2640)) %>%
    select(lon, lat, t, temp) %>%
    # na.omit() %>% 
    # filter(lon %in% coords$lon,
    #        lat %in% coords$lat) %>%
    inner_join(coords[,1:4], by = c("lon", "lat")) %>% 
    select(nav_lon, nav_lat, t, temp)
  
  # ggplot(filter(sst, t == min(t)), aes(x = lon, y = lat, fill = temp)) +
  #   geom_raster() +
  #   scale_fill_viridis_c()
  # 
  # ggplot(filter(sst, t == min(t)), aes(x = nav_lon, y = nav_lat, colour = temp)) +
  #   geom_point() +
  #   scale_colour_viridis_c()
  
  nc_close(nc)
  return(sst)
}



lon_sub <- lon_OISST[1]
save_NAPA_sst_sub <- function(lon_sub){
  # return(lon_sub)
  coords <- lon_lat_NAPA_OISST %>% 
    filter(lon_O == lon_sub)
  
  # test <- data.frame(file_name = NAPA_files[1:10],
  #                    x = 1:10)
  system.time(
    NAPA_sst_sub <- plyr::ldply(NAPA_files, .fun = load_NAPA_sst_sub, .parallel = TRUE)
  ) # 70 seconds at 50 cores
  
  # sst_sub_ts <- sst_sub %>% 
  #   filter(lon == 304, lat == 367)
  # ggplot(sst_sub_ts, aes(x = t, y = temp)) +
  #   geom_line()
  # 
  # sst_sub_2d <- sst_sub %>% 
  #   filter(t == as.Date("1993-10-01"))
  # ggplot(sst_sub_2d, aes(x = nav_lon, y = nav_lat, colour = temp)) +
  #   geom_point() +
  #   scale_colour_viridis_c()
  
  # return()
  # lon_sub <- df$lon_sub
  # lon_sub_label <- str_pad(lon_sub, 4, pad = "0")
  # NAPA_sst_sub <- data.frame(file_name = NAPA_files,
  #                            coords_sub = rep(coords, times = length(NAPA_files)),
  #                            y = 1:length(NAPA_files)) #%>%
  #   group_by(y) %>%
  #   nest() %>% 
  #   mutate(sst = purrr::map(data, load_NAPA_sst_sub)) %>% 
  #   unnest(sst)
  lon_sub_label <- str_pad(which(lon_OISST == lon_sub), width = 4, pad = "0", side = "left")
  save(NAPA_sst_sub, file = paste0("../data/NAPA_sst_sub_",lon_sub_label,".RData"))
}


# Process data ------------------------------------------------------------

plyr::laply(lon_OISST[1], save_NAPA_sst_sub)



# Visualise ---------------------------------------------------------------

NAPA_saves <- dir("../data", pattern = "NAPA", full.names = T)

NAPA_data <- lapply(NAPA_saves, function(x) {
  load(file = x)
  get(ls()[ls()!= "filename"])
})

# names(mylist) <- all.files

NAPA_all <- do.call("rbind", NAPA_data)

NAPA_all_sub <- NAPA_all %>% 
  filter(t == as.Date("2000-01-01"))

ggplot(NAPA_all_sub, aes(x = nav_lon, y = nav_lat, colour = temp)) +
  geom_point(size = 0.001) +
  scale_colour_viridis_c()
