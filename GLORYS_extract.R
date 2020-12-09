# tikoraluk/GLORYS_extract.R
# This script houses code used to extract data from GLORYS files
# NB: This is designed to work on the global files downloaded by Sofi


# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(tidync)
library(FNN)
library(doParallel); registerDoParallel(cores = 50)


# Extraction coordinates --------------------------------------------------

JP_points <- read_csv("extracts/npp_light_2020-10-09.csv") %>% 
  dplyr::select(Long_converted, Lat_converted) %>% 
  dplyr::rename(lon = Long_converted, lat = Lat_converted) %>% 
  distinct() %>% 
  na.omit()

# Have a peek at the data
ggplot(data = JP_points, aes(x = lon, y = lat)) +
  borders() +
  geom_point(colour = "red") +
  coord_quickmap(expand = F) +
  labs(x = NULL, y = NULL)


# GLORYS files ------------------------------------------------------------

# Surface files
GLORYS_files <- dir("../../data/GLORYS_Global", pattern = "thetao_depth_0", full.names = T)

# The GLORYS grid
# GLORYS_grid <- tidync(GLORYS_files[1]) %>% 
  # hyper_filter(latitude = between(latitude, 1, 10)) %>% 
  # tidync::activate("D1") %>%
  # hyper_tibble()
GLORYS_ocean <- tidync(GLORYS_files[1]) %>% 
  hyper_filter(time = time == 376956) %>% 
  hyper_tibble() %>%
  dplyr::select(longitude, latitude) %>% 
  distinct()


# Functions ---------------------------------------------------------------
# Due to the dispersed nature of the data points I think it will be more efficient
# to load each pixel individually

# Find the nearest grid cells for each site
# testers...
# coords <- JP_points
grid_match <- function(coords){
  grid_index <-  knnx.index(data = as.matrix(GLORYS_ocean[,5:6]),
                             query = as.matrix(coords[,1:2]), k = 1) %>% 
    data.frame(idx = .) %>% 
    distinct()
  grid_points <- GLORYS_grid[grid_index$idx,] %>% 
    mutate(row_idx = 1:n())
  return(grid_points)
}

# Extract one year of data for one pixel
# testers...
# coord <- grid_points[1,]
# GLORYS_file <- GLORYS_files[1] # Problem files: 4, 13, 14
extract_GLORYS_one <- function(GLORYS_file, coord){
  # system.time(
  tidync_GLORYS_one <- tidync(GLORYS_file) %>% 
    hyper_filter(x = x == coord$x, 
                 y = y == coord$y) %>%
    hyper_tibble() %>% 
    left_join(coord, by = c("x", "y")) %>% 
    mutate(t = as.Date(as.POSIXct(time_counter*3600, origin = "1950-01-01")))
  # ) ~1 second
  
  # May need to correct time stamps
  if(min(tidync_GLORYS_one$time_counter) == 1){
    tidync_GLORYS_one$t <- seq(as.Date("1996-01-01"), as.Date("1996-12-31"), by = "day")
  }
  if(min(tidync_GLORYS_one$time_counter) < -1){
    tidync_GLORYS_one$t <- seq(from = as.Date("2005-01-01"), length.out = nrow(tidync_GLORYS_one), by = "day")
  }
  
  # Columns may also have different names
  if("nav_lat.x" %in% colnames(tidync_GLORYS_one)){
    tidync_GLORYS_one <- tidync_GLORYS_one %>% 
      dplyr::rename(temp = votemper, lon = nav_lon.x, lat = nav_lat.x) %>%
      dplyr::select(lon, lat, t, temp)
  } else{
    tidync_GLORYS_one <- tidync_GLORYS_one %>% 
      dplyr::rename(temp = votemper, lon = nav_lon, lat = nav_lat) %>%
      dplyr::select(lon, lat, t, temp)
  }
}

# Extract all years of data for one pixel
extract_GLORYS_all <- function(coord){
  # system.time(
    tidync_GLORYS_all <- plyr::ldply(GLORYS_files, extract_GLORYS_one, coord = coord, .parallel = T)
  # ) # ~2 seconds
}

# Extract all years of data for all pixels
extract_GLORYS_coords <- function(coords){
  match_coords <- grid_match(coords)
  system.time(
  tidync_GLORYS_full <- plyr::ddply(match_coords[1:2,], c("row_idx"), extract_GLORYS_all, 
                                    .progress = "text", .paropts = c(.inorder = )) %>% 
    dplyr::select(-row_idx)
  ) # 3 seconds for 2
  
  # Join site coords and exit
}


# Extract data ------------------------------------------------------------


# Annual summaries --------------------------------------------------------


