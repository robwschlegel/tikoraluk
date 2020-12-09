# tikoraluk/GLORYS_extract.R
# This script houses code used to extract data from GLORYS files
# NB: This is designed to work on the global files downloaded by Sofi


# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(tidync)
library(FNN)


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

GLORYS_files <- dir("../../sofiad/TEMP_ANOMALIES", pattern = "new", full.names = T)

# The GLORYS grid
GLORYS_grid <-   tidync_GLORYS <- tidync(GLORYS_files[1]) %>% 
  tidync::activate("D1,D0") %>% 
  hyper_tibble()



# Extract data ------------------------------------------------------------
# Due to the dispersed nature of the data points I think it will be more efficient
# to load each pixel individually

# Find the nearest grid cells for each site
# testers...
# coords <- JP_points
grid_match <- function(coords){
  grid_index <-  knnx.index(data = as.matrix(GLORYS_grid[,1:2]),
                             query = as.matrix(coords[,1:2]), k = 1) %>% 
    data.frame(idx = .) %>% 
    distinct()
  grid_points <- GLORYS_grid[grid_index$idx,] %>% 
    mutate(row_idx = 1:n())
  return(grid_points)
}

# Extract data for one point
# testers...
# coord <- grid_points[1,]
# GLORYS_file <- GLORYS_files[1]
extract_GLORYS_one <- function(GLORYS_file, coord){
  system.time(
    tidync_GLORYS <- tidync(GLORYS_file) %>% 
      # tidync::activate("D1,D0") %>% 
      # hyper_dims()
      hyper_filter(x = x == coord$x, 
                   y = y == coord$y) %>%
      # hyper_filter(time = between(time, 1, 2)) %>% 
      hyper_tibble() %>% 
      left_join(coord, by = c("x", "y")) %>% 
      # mutate(t = as.Date(time_counter, origin = "1950-01-01"))
      # mutate(t = as.POSIXct(time_counter*3600, origin = "1950-01-01"))
      mutate(t = as.Date(as.POSIXct(time_counter*3600, origin = "1950-01-01")))
  )
}

system.time(
  tidync_GLORYS <- tidync(GLORYS_files[1]) %>% 
    # tidync::activate("D1,D0") %>% 
    # hyper_dims()
    hyper_filter(x = between(x, 100, 400), 
                 y = between(y, 100, 400)) %>%
    # hyper_filter(time = between(time, 1, 2)) %>% 
    hyper_tibble() %>% 
    left_join(GLORYS_grid, by = c("x", "y")) %>% 
    # mutate(t = as.Date(time_counter, origin = "1950-01-01"))
    # mutate(t = as.POSIXct(time_counter*3600, origin = "1950-01-01"))
    mutate(t = as.Date(as.POSIXct(time_counter*3600, origin = "1950-01-01")))
)

nc <- ncdf4::nc_open(GLORYS_files[1])
ncdf4::ncvar_get()
