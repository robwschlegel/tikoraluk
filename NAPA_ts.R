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


# Data --------------------------------------------------------------------

# The file location
NAPA_files <- dir("../../data/NAPA025/1d_grid_T_2D", full.names = T)

# The NAPA to OISST lon/lat mask
load("metadata/lon_lat_NAPA_OISST.RData")

# The OISST lon values for subsetting
load("metadata/lon_OISST.RData")


# Functions ---------------------------------------------------------------

# Function for loading the individual NAPA NetCDF files and subsetting accordingly 
load_NAPA_sst_sub <- function(file_name, coords){
  
  nc <- nc_open(as.character(file_name))
  
  date_start <- ymd(str_sub(basename(as.character(file_name)), start = 29, end = 36))
  date_end <- ymd(str_sub(basename(as.character(file_name)), start = 38, end = 45))
  date_seq <- seq(date_start, date_end, by = "day")
  
  sst <- as.data.frame(ncvar_get(nc, varid = "sst")) %>% 
    mutate(lon = as.numeric(nc$dim$x$vals)) %>%
    gather(-lon, key = lat, value = temp) %>%
    mutate(t = rep(date_seq, each = 388080),
           lat = rep(as.numeric(nc$dim$y$vals), each = 2640)) %>%
    select(lon, lat, t, temp) %>%
    inner_join(coords, by = c("lon", "lat")) %>% 
    select(nav_lon, nav_lat, t, temp)
  
  nc_close(nc)
  return(sst)
}

# Function for combining and saving the subsetted NAPA data
save_NAPA_sst_sub <- function(df){
  
  coords <- lon_lat_NAPA_OISST %>% 
    filter(lon_O == df$lon)
  
  # system.time(
  NAPA_sst_sub <- plyr::ldply(NAPA_files,
                              .fun = load_NAPA_sst_sub, 
                              .parallel = TRUE, 
                              coords = coords)
  # )

  lon_sub_label <- str_pad(which(lon_OISST == df$lon), width = 4, pad = "0", side = "left")
  
  if(nrow(NAPA_sst_sub) > 0){
    save(NAPA_sst_sub, file = paste0("../data/NAPA_sst_sub_",lon_sub_label,".RData"))
  }
}


# Process data ------------------------------------------------------------

lon_OISST_multi <- data.frame(lon = lon_OISST,
                              x = 1:length(lon_OISST))

# Run on Thursday, October 4th, 2018
system.time(
  plyr::ddply(lon_OISST_multi[1,], .variables = "x",
              .fun = save_NAPA_sst_sub)
) # 66 seconds at 50 cores
system.time(
  plyr::ddply(lon_OISST_multi[2:100,], .variables = "x", 
              .fun = save_NAPA_sst_sub)
) # 5734 seconds at 50 cores
system.time(
  plyr::ddply(lon_OISST_multi[101:400,], .variables = "x", 
              .fun = save_NAPA_sst_sub)
) # 17878 seconds at 50 cores
system.time(
  plyr::ddply(lon_OISST_multi[401:700,], .variables = "x", 
              .fun = save_NAPA_sst_sub)
) # 19811 seconds at 50 cores
system.time(
  plyr::ddply(lon_OISST_multi[701:1000,], .variables = "x", 
              .fun = save_NAPA_sst_sub)
) # 17650 seconds at 50 cores
system.time(
  plyr::ddply(lon_OISST_multi[1001:1440,], .variables = "x", 
              .fun = save_NAPA_sst_sub)
) # 25262 seconds at 50 cores


# Visualise ---------------------------------------------------------------

NAPA_saves <- dir("../data", pattern = "NAPA", full.names = T)

NAPA_data <- lapply(NAPA_saves, function(x) {
  load(file = x)
  get(ls()[ls()!= "filename"])
})

NAPA_all <- do.call("rbind", NAPA_data)

NAPA_all_sub <- NAPA_all %>% 
  filter(t == as.Date("2000-01-01"))

ggplot(NAPA_all_sub, aes(x = nav_lon, y = nav_lat, colour = temp)) +
  geom_point(size = 0.001) +
  scale_colour_viridis_c()
