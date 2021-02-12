# tikoraluk/GLORYS_extract.R
# This script houses code used to extract data from GLORYS files
# NB: This is designed to work on the global files downloaded by Sofi


# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(tidync)
library(FNN)
library(geosphere)
library(doParallel); registerDoParallel(cores = 50)


# Extraction coordinates --------------------------------------------------

JP_points <- read_csv("extracts/npp_light_2020-10-09.csv") %>% 
  dplyr::select(Long_converted, Lat_converted) %>% 
  dplyr::rename(site_lon = Long_converted, site_lat = Lat_converted) %>% 
  distinct() %>% 
  na.omit() %>% 
  data.frame()

# Have a peek at the data
# ggplot(data = JP_points, aes(x = site_lon, y = site_lat)) +
#   borders() +
#   geom_point(colour = "red") +
#   coord_quickmap(expand = F) +
#   labs(x = NULL, y = NULL)


# GLORYS files ------------------------------------------------------------

# Surface files
GLORYS_files <- c(dir("../../sofiad/UVT_FILES", pattern = "T_...._2D.nc", full.names = T),
                  dir(c(paste0("../../sofiad/UVT_FILES/INTERP_DATA/F",seq(1994, 2007))), 
                      pattern = c("T_interp_...._2D_new_1"), full.names = T),
                  dir(c(paste0("../../sofiad/UVT_FILES/INTERP_DATA/F",seq(1994, 2007))), 
                      pattern = c("T_interp_...._2D_new_2"), full.names = T))

# The GLORYS grid
GLORYS_grid <- tidync(GLORYS_files[1]) %>% # Be careful if this index above is changed
  tidync::activate("D2,D1") %>%
  hyper_tibble() %>% 
  distinct() #%>% 
  # mutate(y = y-1) # Note that the non-interpolated files have a y index from 2:3058

# The pixels with ocean data for the FNN matching
GLORYS_ocean <- tidync(GLORYS_files[1]) %>%  # Be careful if this index above is changed
  hyper_filter(time_counter = time_counter == 385692, deptht = deptht < 0.5) %>% 
  hyper_tibble() %>% 
  na.omit() %>%  
  dplyr::select(x, y) %>% 
  distinct() %>% 
  left_join(GLORYS_grid, by = c("x", "y")) %>% 
  dplyr::select(x, y, nav_lon, nav_lat) %>%
  mutate(idx = 1:n())

# The surface and deepest depth per pixel
# GLORYS_surface_bottom <- tidync(GLORYS_files[1]) %>%  # Be careful if this index above is changed
#   hyper_filter(time_counter = time_counter == 385692) %>% 
#   hyper_tibble() %>% 
#   na.omit() %>% 
#   dplyr::rename(depth = deptht) %>% 
#   dplyr::select(x, y, depth) %>% 
#   group_by(x, y) %>% 
#   filter(depth == min(depth) | depth == max(depth)) %>%
#   ungroup() %>% 
#   distinct()
# save(GLORYS_surface_bottom, file = "extracts/GLORYS_surface_bottom.RData")
load("extracts/GLORYS_surface_bottom.RData")

# Depth index for interp files
GLORYS_depth <- tidync(GLORYS_files[1]) %>% # Be careful if this index above is changed
  tidync::activate("D0") %>%
  hyper_tibble() %>% 
  distinct() # Be careful not to join to this index until the main data has gone through distinct()

# Find the nearest grid cells for each site
# testers...
# coords <- JP_points
grid_match <- function(coords){
  grid_index <- data.frame(coords,
                           idx = knnx.index(data = as.matrix(GLORYS_ocean[,3:4]),
                                            query = as.matrix(coords[,1:2]), k = 1))
  grid_points <- left_join(grid_index, GLORYS_ocean, by = c("idx")) %>% 
    mutate(dist = round(distHaversine(cbind(site_lon, site_lat),
                                      cbind(nav_lon, nav_lat))/1000, 2),
           plyr_idx = 1:n(), idx = NULL)
  return(grid_points)
}


# Function for extracting one point ---------------------------------------

# Due to the dispersed nature of the data points I think it will be more efficient
# to load each pixel individually

# Extract one year of data for one pixel
# testers...
# coord <- grid_points[2,]
# coord <- match_coords[2,]
# GLORYS_file <- GLORYS_files[146]
extract_GLORYS_one <- function(GLORYS_file, coord){
  
  # print(paste0("Began run on ", GLORYS_file))
  
  # Extract the data
  # system.time(
  tidync_GLORYS_one <- tidync(GLORYS_file) %>% 
    hyper_filter(x = x == coord$x,
                 y = y == coord$y) %>%
    hyper_tibble() %>% 
    distinct() %>% 
    mutate(t = as.Date(as.POSIXct(time_counter*3600, origin = "1950-01-01"))) %>% 
    dplyr::rename(temp = votemper, depth = deptht) %>% 
    dplyr::select(x, y, t, depth, temp) %>% 
    left_join(coord, by = c("x", "y")) %>% 
    dplyr::select(site_lon, site_lat, nav_lon, nav_lat, dist, t, depth, temp) %>% 
    distinct() # This shouldn't do anything..
  # ) # 114 seconds, 94 seconds if already run, 2 seconds if the system isn't hanging...
  
  # Correct depth if necessary and exit
  if(str_detect(GLORYS_file, "INTERP")){
    list_fix <- str_split(GLORYS_file, "_")
    depth_fix <- as.numeric(sapply(str_split(sapply(list_fix, "[[", 8), ".nc"), "[[", 1))
    year_fix <- as.numeric(sapply(list_fix, "[[", 5))
    tidync_GLORYS_one <- tidync_GLORYS_one %>% 
      mutate(depth = GLORYS_depth$deptht[depth_fix],
             t = seq(from = as.Date(paste0(year_fix,"-01-01")), by = "day", length.out = nrow(.)))
  }
  return(tidync_GLORYS_one)
}

# Extract all years of data for one pixel
extract_GLORYS_all <- function(coord){
  
  print(paste0("Began run on ", coord$plyr_idx))
  
  # system.time(
  tidync_GLORYS_all <- plyr::ldply(GLORYS_files, extract_GLORYS_one, coord = coord, 
                                   .parallel = T, .paropts = c(.inorder = FALSE))
  # ) # 98 seconds, 65 if run, 1 second if not hanging
  return(tidync_GLORYS_all)
}

# Extract all years of data for all pixels
extract_GLORYS_coords <- function(coords){
  
  # Match the coordinates
  match_coords <- grid_match(coords)
  
  # Extract all data
  # system.time(
  tidync_GLORYS_full <- plyr::ddply(match_coords, c("plyr_idx"), extract_GLORYS_all, 
                                    .parallel = F, .paropts = c(.inorder = FALSE)) %>% 
    dplyr::select(-plyr_idx)
  # ) # 196 seconds for 2, 43 seconds if already run once, 4 seconds if not hanging...
  
  return(tidync_GLORYS_full)
}


# Functions for extracting data over the shelf ----------------------------

# Extract data for one latitude slice
# testers...
# lat_step <- 2368
# shelf_file <- shelf_files[1]
extract_GLORYS_lat <- function(lat_step, shelf_file, coords){
  
  # print(paste0("Began run on ", GLORYS_file))
  
  # Extract the data
  # system.time(
  tidync_GLORYS_raw <- tidync(shelf_file) %>% 
    hyper_filter(x = between(x, min(coords$x), max(coords$x)),
                 y = y == lat_step) %>%
    hyper_tibble() %>% 
    distinct() 
  # ) # 114 seconds, 17 seconds if already run, 1 seconds if the system isn't hanging...
  
  # Correct depth if necessary and exit
  if(str_detect(shelf_file, "INTERP")){
    list_fix <- str_split(shelf_file, "_")
    depth_fix <- as.numeric(sapply(str_split(sapply(list_fix, "[[", 8), ".nc"), "[[", 1))
    year_fix <- as.numeric(sapply(list_fix, "[[", 5))
    tidync_GLORYS_raw <- tidync_GLORYS_raw %>% 
      group_by(x, y) %>% 
      mutate(deptht = GLORYS_depth$deptht[depth_fix],
             t = seq(from = as.Date(paste0(year_fix,"-01-01")), by = "day", length.out = n())) %>%
      ungroup()
  } else{
    tidync_GLORYS_raw <- tidync_GLORYS_raw %>% 
      mutate(t = as.Date(as.POSIXct(time_counter*3600, origin = "1950-01-01"))) %>% 
      distinct()
  }
  
  # Finish up
  tidync_GLORYS_lat <- tidync_GLORYS_raw %>% 
    dplyr::rename(temp = votemper, depth = deptht) %>% 
    right_join(shelf_depths, by = c("x", "y", "depth")) %>% 
    na.omit() %>% 
    distinct() %>% 
    left_join(GLORYS_ocean, by = c("x", "y")) %>% 
    dplyr::select(nav_lon, nav_lat, t, depth, temp) %>% 
    distinct() # This shouldn't do anything..
  rm(tidync_GLORYS_raw); gc()
  return(tidync_GLORYS_lat)
}

# Function for multicoring the xtraction of shelf data from one file
extract_GLORYS_all_lat <- function(shelf_file, coords){
  
  print(paste0("Began run on ", shelf_file, " at ", Sys.time()))
  
  lat_range <- unique(coords$y)
  
  system.time(
    tidync_GLORYS_all_lat <- plyr::ldply(lat_range, extract_GLORYS_lat, 
                                         shelf_file = shelf_file, coords = coords, 
                                         .parallel = T, .paropts = c(.inorder = FALSE))
  )
  return(tidync_GLORYS_all_lat)
}

# Extract data given a specific grid and depth
# NB: This extracts the surface and bottom depths for pixels over the continental shelf
# bbox <- data.frame(site_lon = c(-180, 180), site_lat = c(60, 82))
extract_GLORYS_shelf <- function(bbox){
  
  # Match the coordinates
  match_coords <- grid_match(bbox)
  if(-180 %in% bbox$site_lon & 180 %in% bbox$site_lon){
    lon_range <- range(GLORYS_ocean$x)
  } else {
    lon_range <- range(bbox$site_lon)
  }
  lat_range <- range(match_coords$y)
  
  # Depth data
  # NB: 200 is too restrictive
  coords <- GLORYS_surface_bottom %>% 
    filter(y >= min(lat_range),
           y <= max(lat_range)) %>% 
    group_by(x, y) %>% 
    filter(max(depth) < 230) %>% 
    ungroup() %>% 
    distinct()
  
  # Shelf files
  # We don't want depth integers of 28 or greater as these are deeper than the shelf
  shelf_files <- GLORYS_files[!str_detect(GLORYS_files, c("_28.nc"))]
  shelf_files <- shelf_files[!str_detect(shelf_files, c("_29.nc"))]
  
  # Extract all data
  shelf_grid_res <- plyr::ldply(shelf_files[1], extract_GLORYS_all_lat, coords = coords,
                                .parallel = F, .paropts = c(.inorder = FALSE))
}


# Extract data ------------------------------------------------------------

# Set cores
registerDoParallel(cores = 25)

# system.time(
# JP_data <- extract_GLORYS_coords(JP_points)
# ) # ~ 18 minutes for one site
# write_csv(JP_data, file = "extracts/JP_data.csv")

# Extract Arctic data
arctic_bbox <- data.frame(site_lon = c(-180, 180), site_lat = c(60, 82))
arctic_shelf_data <- extract_GLORYS_shelf(arctic_bbox)
write_csv(arctic_shelf_data, file = "extracts/arctic_shelf_data.csv")

# Test output
arctic_shelf_data %>% 
  filter(t == "1993-01-01", depth > 1) %>% 
  ggplot(aes(x = nav_lon, y = nav_lat)) +
  borders() +
  geom_point(aes(colour = depth)) +
  coord_quickmap(ylim = c(60, 82))


# Monthly summaries -------------------------------------------------------

arctic_shelf_monthly <- arctic_shelf_data %>% 
  mutate(year = lubridate::year(t),
         month = lubridate::month(t)) %>% 
  group_by(nav_lon, nav_lat, year, month) %>% 
  summarise(temp = mean(temp, na.rm = T))
write_csv(arctic_shelf_monthly, file = "extracts/arctic_shelf_monthly.csv")


# Annual summaries --------------------------------------------------------

# JP_annual <- JP_data %>% 
#   mutate(t = lubridate::year(t)) %>% 
#   group_by(site_lon, site_lat, nav_lon, nav_lat, dist, t, depth) %>% 
#   summarise(temp = mean(temp, na.rm = T), .groups = "drop")
# write_csv(JP_annual, file = "extracts/JP_annual.csv")


# Pixel distance from sites -----------------------------------------------
# The distance that the pixels are from the sites is important meta-data


