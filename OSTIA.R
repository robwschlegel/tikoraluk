# The purpose of this script is to load and visualise OSTIA data.
# And then compare it against OISST data.


# Libraries ---------------------------------------------------------------

source("MHW_func.R")
library(FNN)
library(lubridate)
library(ncdf4)
doParallel::registerDoParallel(cores = 50)


# Data --------------------------------------------------------------------

OSTIA_files <- dir("../data", pattern = "MET", full.names = T)

OISST_files <- dir("../data/MHW", pattern = "MHW.calc", full.names = T)

pretty_palette <- c("#fefefe", "#f963fa", "#020135", "#00efe1", "#057400", "#fcfd00", "#ed0000", "#3d0000")

# map_base <- ggplot2::fortify(maps::map(fill = TRUE, plot = FALSE)) %>% 
#   dplyr::rename(lon = long) %>% 
#   mutate(lon = ifelse(lon < 0, lon + 360, lon))


# Functions ---------------------------------------------------------------

## testers...
# file_name <- OSTIA_files[1]
# chosen_date <- as.Date("2007-12-31")
load_OSTIA_sub <- function(file_name, chosen_date){
  if(!is.Date(chosen_date)) stop("Ensure that 'chosen_date' is in the date format.")
  nc <- nc_open(as.character(file_name))
  # Seconds since 1981-01-01 00:00:00
  nc_dates <- as.Date(as.POSIXct(nc$dim$time$vals, origin = "1981-01-01 00:00:00"))
  date_index <- which(nc_dates == chosen_date)
  sst <- ncvar_get(nc, varid = "analysed_sst")[ , ,date_index]
  dimnames(sst) <- list(lon = nc$dim$lon$vals,
                        lat = nc$dim$lat$vals)
  res <- as.data.frame(reshape2::melt(sst, value.name = "sst"), row.names = NULL) %>% 
    mutate(t = chosen_date,
           sst = sst-273.15) %>%
    select(lon, lat, t, sst)
  nc_close(nc)
  return(res)
}

## testers...
# file_name <- OISST_files[1]
# chosen_date <- as.Date("2007-12-31")
load_OISST_sub <- function(file_name, chosen_date){
  if(!is.Date(chosen_date)) stop("Ensure that 'chosen_date' is in the date format.")
  load(as.character(file_name))
  res <- MHW_clim(MHW_res) %>% 
    filter(t == chosen_date) %>% 
    select(lon, lat, t, temp)
  rm(MHW_res)
  return(res)
}

# This function finds the nearest OISST pixels for OSTIA data
# Then calculates means before determining the difference
## testers...
# df_OISST <- OISST_sub_corrected
# df_OSTIA <- OSTIA_sub
OSTIA_OISST_diff <- function(df_OSTIA, df_OISST){
  
  match_index <- as.vector(knnx.index(data = as.matrix(df_OISST[,1:2]),
                            query = as.matrix(df_OSTIA[,1:2]), k = 1))
  
  match_OSTIA <- df_OSTIA %>% 
    mutate(lon_match = df_OISST$lon[match_index],
           lat_match = df_OISST$lat[match_index]) %>% 
    group_by(lon_match, lat_match, t) %>% 
    summarise(sst = mean(sst, na.rm = T)) %>% 
    dplyr::rename(lon = lon_match, lat = lat_match) %>% 
    mutate(sst = ifelse(is.na(sst), NA, sst))
  
  match_diff <- left_join(match_OSTIA, df_OISST, by = c("lon", "lat", "t")) %>% 
    mutate(sst_diff = sst.x - sst.y) %>% 
    select(lon, lat, t, sst_diff)
  
  return(match_diff)
}


# Calculations ------------------------------------------------------------

OSTIA_sub <- load_OSTIA_sub(OSTIA_files[1], as.Date("2007-12-31"))

# Load OISST in stages as it is too RAM intensive in one shot
# system.time(
#   OISST_sub_1 <- plyr::ldply(OISST_files[1:480], load_OISST_sub, 
#                            .parallel = T, chosen_date = as.Date("2007-12-31"))
# ) # 249 seconds at 50 cores
# system.time(
#   OISST_sub_2 <- plyr::ldply(OISST_files[481:960], load_OISST_sub, 
#                              .parallel = T, chosen_date = as.Date("2007-12-31"))
# ) # 331 seconds at 50 cores
# system.time(
#   OISST_sub_3 <- plyr::ldply(OISST_files[961:1440], load_OISST_sub, 
#                              .parallel = T, chosen_date = as.Date("2007-12-31"))
# ) # xxx seconds at 50 cores
# OISST_sub <- rbind(OISST_sub_1, OISST_sub_2, OISST_sub_3)
# rm(OISST_sub_1, OISST_sub_2, OISST_sub_3)
# save(OISST_sub, file = "../data/OISST_sub_2007_12_31.RData")
load("../data/OISST_sub_2007_12_31.RData")

OISST_sub_corrected <- OISST_sub %>% 
  mutate(lon = ifelse(lon > 180, lon - 360, lon)) %>% 
  dplyr::rename(sst = temp)
  

# Visualise ---------------------------------------------------------------

global_sst <- function(df, product){
  sst_world <- ggplot(df, aes(x = lon, y = lat, fill = sst)) +
    geom_raster() +
    borders(fill = "grey80", colour = NA) +
    scale_fill_gradientn(colours = pretty_palette, limits = c(-2, 32)) +
    coord_cartesian(expand = F, 
                    xlim = c(-180, 180),
                    ylim = c(-89.875, 89.875)) +
    labs(x = "", y = "", title = paste0(product,": 2007-12-31"))
  return(sst_world)
}

OSTIA_world <- global_sst(OSTIA_sub, "OSTIA")
# OSTIA_world
ggsave(OSTIA_world, filename = "graph/OSTIA_world.png", height = 10, width = 14)

OISST_world <- global_sst(OISST_sub_corrected, "OISST")
# OISST_world
ggsave(OISST_world, filename = "graph/OISST_world.png", height = 10, width = 14)


# Difference --------------------------------------------------------------

OO_diff <- OSTIA_OISST_diff(OSTIA_sub, OISST_sub_corrected)

diff_world <- ggplot(OO_diff, aes(x = lon, y = lat, fill = sst_diff)) +
  geom_raster() +
  borders(fill = "grey80", colour = NA) +
  # scale_fill_gradientn(colours = pretty_palette) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  coord_cartesian(expand = F, 
                  xlim = c(-180, 180),
                  ylim = c(-89.875, 89.875)) +
  labs(x = "", y = "", title = paste0("OSTIA-OISST: 2007-12-31"))
# diff_world
ggsave(diff_world, filename = "graph/OSTIA_OISST_world_diff.png", height = 10, width = 14)

