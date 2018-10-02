# The purpose of this script is to figure out how to 
# best deal with data on a curvilinear grid.

# The BIO model is on such a grid, so it is necessary to 
# interpolate any cartesian etc. gridded data (e.g. OISST)
# to a curvilinear projection to allow for direct comparisons


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(geosphere)
library(ncdf4)


# Data --------------------------------------------------------------------

# The info file
nc_rotatedAngle <- nc_open("../../NAPA025/mesh_grid/RotatedAngle_CREG025.nc")
nc_close(nc_rotatedAngle)
nc_bathy <- nc_open("../../NAPA025/mesh_grid/bathy_creg025_extended_5m.nc")
nc_close(nc_bathy)
nc_mesh_mask <- nc_open("../../NAPA025/mesh_grid/CREG025_mesh_mask.nc")
nc_close(nc_mesh_mask)

# The data location
nc_files <- dir("../../NAPA025/1d_grid_T_2D/", full.names = T)

# First peek at the BIO model data
# time is in seconds since 1900-01-01 00:00:00
nc <- nc_open(nc_files[1])
# names(nc$var)
# sst <- ncvar_get(nc, varid = "sst")[,,1] # Select only top (surface) layer
date_start <- parse_datetime(str_sub(basename(nc_files[1]), start = 29, end = 36))
date_end <- parse_datetime(str_sub(basename(nc_files[1]), start = 38, end = 45))
# max(sst, na.rm = T)
# min(sst, na.rm = T)
## Use this if multiple depths are being pulled out
# dimnames(sst) <- list(lon = nc$dim$x$vals,
#                       lat = nc$dim$y$vals, 
#                       depth = nc$dim$deptht$vals)
# Use this for only one depth
# dimnames(sst) <- list(lon = nc$dim$x$vals,
#                       lat = nc$dim$y$vals)
# sst <- as_tibble(gather(sst, key = "lon", value = "temp"))

# sst$t <- as.Date(nc$dim$time_counter$vals, origin = '1900-01-01')

## Grab sst in one shot
lon <- as.data.frame(ncvar_get(nc_mesh_mask, varid = "nav_lon")) %>% 
  setNames(., as.numeric(nc_mesh_mask$dim$y$vals)) %>%
  mutate(lon = as.numeric(nc_mesh_mask$dim$x$vals)) %>% 
  gather(-lon, key = lat, value = nav_lon) %>% 
  mutate(lat = as.numeric(lat))
lat <- as.data.frame(ncvar_get(nc_mesh_mask, varid = "nav_lat")) %>% 
  setNames(., as.numeric(nc_mesh_mask$dim$y$vals)) %>%
  mutate(lon = as.numeric(nc_mesh_mask$dim$x$vals)) %>% 
  gather(-lon, key = lat, value = nav_lat) %>% 
  mutate(lat = as.numeric(lat)) 
sst <- as.data.frame(ncvar_get(nc, varid = "sst")) %>% 
  mutate(lon = as.numeric(nc$dim$x$vals)) %>% 
  gather(-lon, key = lat, value = temp) %>% 
  mutate(lat = rep(as.numeric(nc$dim$y$vals), each = 528, times = 5),
         temp = ifelse(temp == 0, NA, temp),
         t = rep(seq(date_start, date_end, by = "day"), each = 388080)) %>%
  select(lon, lat, t, temp) %>% 
  na.omit() %>% 
  left_join(lon, by = c("lon", "lat")) %>% 
  left_join(lat, by = c("lon", "lat"))


# Visualise ---------------------------------------------------------------

sst_sub <- sst %>% 
  filter(t == min(t))

## Cartesian conversion
ggplot(sst_sub, aes(x = nav_lon, y = nav_lat, colour = temp)) +
  geom_point(size = 0.001) +
  scale_colour_viridis_c() +
  coord_map() +
  ggtitle(paste0("Date: ",unique(t)))
ggsave(filename = "graph/BIO_cartesian_conversion.pdf", width = 16, height = 8)
ggsave(filename = "graph/BIO_cartesian_conversion.png", width = 16, height = 8)

## Cartesian conversion
ggplot(sst_sub, aes(x = nav_lon, y = nav_lat, colour = temp)) +
  geom_point(size = 0.001) +
  scale_colour_viridis_c()
ggsave(filename = "graph/BIO_equal_conversion.pdf", width = 12, height = 12)
ggsave(filename = "graph/BIO_equal_conversion.png", width = 12, height = 12)

## Cartesian conversion
ggplot(sst_sub, aes(x = lon, y = lat, fill = temp)) +
  geom_raster() +
  scale_fill_viridis_c()
ggsave(filename = "graph/BIO_integer_native.pdf", width = 12, height = 12)
ggsave(filename = "graph/BIO_integer_native.png", width = 12, height = 12)
