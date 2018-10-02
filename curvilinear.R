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
nc_files <- dir("../../NAPA025/5d_grid_T/", full.names = T)

# First peek at the BIO model data
# time is in seconds since 1900-01-01 00:00:00
nc <- nc_open(nc_files[1])
# names(nc$var)
# sst <- ncvar_get(nc, varid = "toce")[,,1] # Select only top (surface) layer
# date_start <- parse_datetime(str_sub(basename(nc_files[1]), start = 26, end = 33))
# date_end <- parse_datetime(str_sub(basename(nc_files[1]), start = 35, end = 42))
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
sst <- as.data.frame(ncvar_get(nc, varid = "toce")[,,1]) %>% 
  setNames(., as.numeric(nc$dim$y$vals)) %>% 
  mutate(lon = as.numeric(nc$dim$x$vals)) %>% 
  gather(-lon, key = lat, value = temp) %>% 
  mutate(lat = as.numeric(lat),
         temp = ifelse(temp == 0, NA, temp)) %>%
  select(lon, lat, temp) %>% 
  na.omit()


# Visualise ---------------------------------------------------------------

ggplot(sst, aes(x = lon, y = lat, fill = temp)) +
  geom_raster() +
  scale_fill_viridis_c()

