# The purpose of this script is to figure out how to 
# best deal with data on a curvilinear grid.

# The NAPA model is on such a grid, so it is necessary to 
# interpolate any cartesian etc. gridded data (e.g. OISST)
# to a curvilinear projection to allow for direct comparisons


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(gganimate)
library(geosphere)
library(ncdf4)
library(akima)
library(FNN)


# Data --------------------------------------------------------------------

# The info file
# nc_rotatedAngle <- nc_open("../../data/NAPA025/mesh_grid/RotatedAngle_CREG025.nc")
# nc_close(nc_rotatedAngle)
# nc_bathy <- nc_open("../../data/NAPA025/mesh_grid/bathy_creg025_extended_5m.nc")
# nc_close(nc_bathy)
# nc_mesh_mask <- nc_open("../../data/NAPA025/mesh_grid/CREG025_mesh_mask.nc")
# nc_close(nc_mesh_mask)

# The data location
nc_files <- dir("../../data/NAPA025/1d_grid_T_2D/", full.names = T)

# First peek at the NAPA model data
# time is in seconds since 1900-01-01 00:00:00
nc <- nc_open(nc_files[1])
# names(nc$var)
# sst <- ncvar_get(nc, varid = "sst")[,,1] # Select only top (surface) layer
date_start <- ymd(str_sub(basename(nc_files[1]), start = 29, end = 36))
date_end <- ymd(str_sub(basename(nc_files[1]), start = 38, end = 45))
date_seq <- seq(date_start, date_end, by = "day")
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

nc_close(nc)
# nc_close(nc_mesh_mask)


# The mask ----------------------------------------------------------------

## Already run
# The lon/lat curvilinear mask in a long format
# mask_long <- left_join(lon, lat, by = c("lon", "lat"))
# save(mask_long, file = "NAPA/mask_long.RData")
# 
# mask_list <- list(lon = as.data.frame(ncvar_get(nc_mesh_mask, varid = "nav_lon")),
#                   lat = as.data.frame(ncvar_get(nc_mesh_mask, varid = "nav_lat")))
# save(mask_list, file = "NAPA/mask_list.RData")


# Visualise ---------------------------------------------------------------

sst_sub <- sst %>% 
  filter(t == min(t))

## Cartesian conversion
ggplot(sst_sub, aes(x = nav_lon, y = nav_lat, colour = temp)) +
  geom_point(size = 0.001) +
  # borders() +
  scale_colour_viridis_c() +
  coord_map() +
  labs(x = "", y = "") +
  ggtitle(paste0("Date: ", unique(sst_sub$t)))
# ggsave(filename = "graph/NAPA_cartesian_conversion.pdf", width = 16, height = 8)
# ggsave(filename = "graph/NAPA_cartesian_conversion.png", width = 16, height = 8)

## Equal conversion
ggplot(sst_sub, aes(x = nav_lon, y = nav_lat, colour = temp)) +
  geom_point(size = 0.001) +
  scale_colour_viridis_c() +
  labs(x = "", y = "") +
  ggtitle(paste0("Date: ", unique(sst_sub$t)))
# ggsave(filename = "graph/NAPA_equal_conversion.pdf", width = 12, height = 12)
# ggsave(filename = "graph/NAPA_equal_conversion.png", width = 12, height = 12)

## Native output
ggplot(sst_sub, aes(x = lon, y = lat, fill = temp)) +
  geom_raster() +
  scale_fill_viridis_c() +
  labs(x = "", y = "") +
  ggtitle(paste0("Date: ", unique(sst_sub$t)))
# ggsave(filename = "graph/NAPA_integer_native.pdf", width = 12, height = 12)
# ggsave(filename = "graph/NAPA_integer_native.png", width = 12, height = 12)

## Polar output
ggplot(sst_sub, aes(x = -nav_lon, y = -nav_lat, colour = temp)) +
  geom_point(size = 0.001) +
  scale_colour_viridis_c() +
  coord_polar() +
  labs(x = "", y = "") +
  ggtitle(paste0("Date: ", unique(sst_sub$t)))
# ggsave(filename = "graph/NAPA_polar_conversion.pdf", width = 12, height = 12)
# ggsave(filename = "graph/NAPA_polar_conversion.png", width = 12, height = 12)


# Animate -----------------------------------------------------------------

# ggplot(sst, aes(x = lon, y = lat, fill = temp)) +
#   geom_raster() +
#   scale_fill_viridis_c() +
#   labs(title = 'Date: {frame_time}', x = '', y = '') +
#   transition_time(t)
# anim_save(filename = "NAPA_integer_native_anim.gif", path = "anim")


# Interpolate -------------------------------------------------------------

# The NAPA curvilinear lon/lat values
load("NAPA/mask_long.RData")
load("NAPA/mask_list.RData")
mask_long <- mask_long %>% 
  mutate(nav_lon_corrected = ifelse(nav_lon < 0, nav_lon+360, nav_lon))
bound <- list(list(lon = mask_long$nav_lon, lat = mask_long$nav_lat))

# First load one day of OISST for the whole planet
OISST_files <- dir("../../oliver/data/sst/noaa_oi_v2/avhrr/1993", 
                   pattern = "avhrr-only-v2", full.names = T, include.dirs = T)

nc <- nc_open(OISST_files[274]) # 1993-10-01

# name_stem <- substr(basename(nc.file), 16, 72)

date_stamp <- ymd(substr(basename(OISST_files)[274], 15, 22))

oisst <- as.data.frame(ncvar_get(nc, varid = "sst")) %>% 
  mutate(lon = as.numeric(nc$dim$lon$vals)) %>% 
  gather(-lon, key = lat, value = temp) %>% 
  mutate(lat = rep(as.numeric(nc$dim$lat$vals), each = 1440),
         temp = ifelse(temp == 0, NA, temp),
         t = date_stamp) %>%
  select(lon, lat, t, temp) %>% 
  na.omit()

nc_close(nc)

ggplot(oisst, aes(x = lon, y = lat, fill = temp)) +
  geom_raster() +
  scale_fill_viridis_c() +
  coord_equal()

## Then extract just the area maching to the NAPA model
## NB: This is waaaay to computationally intensive...
# system.time(oisst_inside <- with(oisst, mgcv::inSide(bound, lon, lat))) # xxx seconds
# ggplot(oisst_inside, aes(x = lon, y = lat, fill = temp)) +
#   geom_raster() +
#   scale_fill_viridis_c() +
#   coord_equal()

## Or find the nearest neighbour for each pixel between NAPA and OISST
match_index <- knnx.index(data =  as.matrix(oisst[,1:2]),
                          query = as.matrix(mask_long[,5:4]), k = 1)
oisst_match <- oisst %>% 
  slice(unique(match_index))
ggplot(oisst_match, aes(x = lon, y = lat, fill = temp)) +
  geom_raster() +
  scale_fill_viridis_c() +
  coord_equal()

# Initial interpolation
system.time(
oisst_interp <- data.frame(interpp(x = oisst_match[, "lon"], y = oisst_match[, "lat"], oisst_match[, "temp"],
                        xo = mask_long$nav_lon_corrected, yo = mask_long$nav_lat, linear = TRUE, 
                        extrap = FALSE, duplicate = "mean"))
)

ggplot(oisst_interp, aes(x = x, y = y, colour = z)) +
  geom_point(size = 0.001) +
  borders() +
  scale_colour_viridis_c() +
  coord_map()


# Compare -----------------------------------------------------------------

oisst_same <- oisst %>% 
  slice(match_index) %>% 
  mutate(nav_lon = mask_long$nav_lon,
         nav_lat = mask_long$nav_lat) %>% 
  dplyr::rename(temp_oisst = temp) %>% 
  select(nav_lon, nav_lat, t, temp_oisst)

sst_combined <- sst %>% 
  left_join(oisst_same) %>% 
  na.omit() %>% 
  mutate(diff = temp-temp_oisst) %>% 
  group_by(t) %>% 
  summarise(mean = mean(diff),
            median = median(diff),
            sum = sum(diff),
            sd = sd(diff))

