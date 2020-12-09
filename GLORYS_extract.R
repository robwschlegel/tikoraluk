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
  dplyr::rename(site_lon = Long_converted, site_lat = Lat_converted) %>% 
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
GLORYS_ocean <- tidync(GLORYS_files[1]) %>% 
  hyper_filter(time = time == 376956) %>% 
  hyper_tibble() %>%
  dplyr::select(longitude, latitude) %>% 
  distinct() %>% 
  mutate(idx = 1:n())


# Functions ---------------------------------------------------------------
# Due to the dispersed nature of the data points I think it will be more efficient
# to load each pixel individually

# Find the nearest grid cells for each site
# testers...
# coords <- JP_points
grid_match <- function(coords){
  grid_index <- data.frame(coords,
                           idx = knnx.index(data = as.matrix(GLORYS_ocean[,1:2]),
                                            query = as.matrix(coords[,1:2]), k = 1))
  grid_points <- left_join(grid_index, GLORYS_ocean, by = c("idx")) %>% 
    mutate(plyr_idx = 1:n(), idx = NULL)
  return(grid_points)
}

# Extract one year of data for one pixel
# testers...
# coord <- grid_points[2,]
# GLORYS_file <- GLORYS_files[1] # Problem files: 4, 13, 14
extract_GLORYS_one <- function(GLORYS_file, coord){
  # system.time(
  tidync_GLORYS_one <- tidync(GLORYS_file) %>% 
    hyper_filter(longitude = longitude == coord$longitude, 
                 latitude = latitude == coord$latitude) %>%
    hyper_tibble() %>% 
    mutate(t = as.Date(as.POSIXct(time*3600, origin = "1950-01-01"))) %>% 
    dplyr::rename(lon = longitude, lat = latitude, temp = thetao) %>% 
    dplyr::select(lon, lat, t, temp)
  # ) # <1 second
}

# Extract all years of data for one pixel
extract_GLORYS_all <- function(coord){
  # system.time(
    tidync_GLORYS_all <- plyr::ldply(GLORYS_files, extract_GLORYS_one, coord = coord, .parallel = T)
  # ) # ~4 seconds
}

# Extract all years of data for all pixels
extract_GLORYS_coords <- function(coords){
  
  # Get nearest coordinates
  match_coords <- grid_match(coords)
  
  # Extract all data
  # system.time(
  tidync_GLORYS_full <- plyr::ddply(match_coords, c("plyr_idx"), extract_GLORYS_all, 
                                    .progress = "text", .paropts = c(.inorder = FALSE)) %>% 
    dplyr::select(-plyr_idx)
  # ) # 31 seconds for 2, 7 seconds if already run once
  
  # Join site coords and exit
  tidync_GLORYS_full <- tidync_GLORYS_full %>% 
    left_join(match_coords[,1:4], by = c("lon" = "longitude", "lat" = "latitude")) %>% 
    dplyr::select(site_lon, site_lat, lon, lat, t, temp)
  return(tidync_GLORYS_full)
}


# Extract data ------------------------------------------------------------

# system.time(
JP_data <- extract_GLORYS_coords(JP_points)
# ) # 74 minutes
write_csv(JP_data, file = "extracts/JP_data.csv")


# Annual summaries --------------------------------------------------------

JP_annual <- JP_data %>% 
  mutate(t = lubridate::year(t)) %>% 
  group_by(lon, lat, t) %>% 
  summarise(temp = mean(temp, na.rm = T))
write_csv(JP_annual, file = "extracts/JP_annual.csv")


# Pixel distance from sites -----------------------------------------------
# The distance that the pixels are from the sites is important meta-data


