# EMME
# This script houses the code used for the EMME-CCI analyses


# Setup -------------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(sf)
library(tidync)
library(doParallel); registerDoParallel(cores = 50)

# OISST ocean only pixels 
load("metadata/OISST_ocean_coords.Rdata")

# OISST lon values only
lon_OISST <- c(seq(0.125, 179.875, by = 0.25), seq(-179.875, -0.125, by = 0.25))

# OISST SST files
OISST_files <- dir("../data/OISST", pattern = "avhrr-only", full.names = T)

# The full MHW results by lon step
MHW_files <- dir("../data/MHW", full.names = T, recursive = T) # NB these only go to 2017

# The MHW cat results by daily global step
MHW_cat_files <- dir("../data/cat_clim", full.names = T, recursive = T)

# Global OISST MHW cat annual averages
OISST_cat_global_annual <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1982-2011_total.Rds") %>% 
  filter(t <= 2020) %>% 
  group_by(t) %>% 
  mutate(cat_n_prop_stack = cumsum(cat_n_prop),
         first_n_cum_prop_stack = cumsum(first_n_cum_prop)) %>% 
  ungroup() %>% 
  filter(category == "IV Extreme")

# Global OISST monthly SST average
OISST_SST_global_monthly <- read_csv("extracts/OISST_global_monthly.csv")

# Global OISST annual SST average
OISST_SST_global_annual <- OISST_SST_global_monthly %>% 
  mutate(t = lubridate::year(t)) %>% 
  filter(t <= 2020) %>% 
  group_by(t) %>% 
  summarise(temp = mean(temp), .groups = "drop") %>% 
  mutate(Ecoregion = "Global")

# Base map for plotting
load("metadata/map_base.Rdata")

# The MHW colour palette
# The MHW colour palette
MHW_colours <- c(
  "I Moderate" = "#ffc866",
  "II Strong" = "#ff6900",
  "III Severe" = "#9e0000",
  "IV Extreme" = "#2d0000"
)

# Function that loads a MHW cat file, includes the date, and subsets by desired pixels
readRDS_sub <- function(file_name, sub_pixels){
  res <- readRDS(file_name)
  res_sub <- res %>% 
    right_join(sub_pixels, by = c("lon", "lat")) %>% 
    filter(!is.na(t)) %>% 
    mutate(year = lubridate::year(t)) %>% 
    dplyr::select(Ecoregion, lon, lat, year, t, everything())
  rm(res); gc()
  return(res_sub)
}

# Function for loading SST from NetCDF
load_sst_sub <- function(lon_step, sub_pixels){
  
  # Establish lon row number
  lon_row <- which(lon_OISST == lon_step)

  # OISST data
  tidync_OISST <- tidync(OISST_files[lon_row]) %>% 
    hyper_tibble() %>% 
    mutate(time = as.Date(time, origin = "1970-01-01"),
           year = lubridate::year(time)) %>% 
    right_join(sub_pixels, by = c("lon", "lat")) %>% 
    dplyr::rename(t = time, temp = sst) %>%
    filter(!is.na(t)) %>%  
    dplyr::select(Ecoregion, lon, lat, year, t, everything())
  gc()
  return(tidync_OISST)
}

# Function for finding the first date of the highest category MHW per pixel
max_event_date <- function(df){
  df %>% 
    group_by(lat) %>% 
    filter(as.integer(category) == max(as.integer(category))) %>% 
    filter(t == min(t)) %>% 
    ungroup()
}

# Function for finding and cleaning up points within a given region polygon
points_in_region <- function(region_in){
  region_sub <- MEOW %>% 
    filter(ECOREGION == region_in) %>% 
    dplyr::select(geometry)
  region_sub <- as.data.frame(region_sub$geometry[[1]][[1]]) %>%
    `colnames<-`(c("lon", "lat"))
  # unnest()
  coords_in <- OISST_ocean_coords %>% 
    mutate(in_grid = sp::point.in.polygon(point.x = OISST_ocean_coords[["lon"]], point.y = OISST_ocean_coords[["lat"]], 
                                          pol.x = region_sub[["lon"]], pol.y = region_sub[["lat"]])) %>% 
    filter(in_grid >= 1) %>% 
    mutate(Ecoregion = region_in) %>% 
    dplyr::select(lon, lat, Ecoregion)
  return(coords_in)
}


# Establish regions -------------------------------------------------------

# Load MEOW
MEOW <- read_sf("metadata/MEOW/meow_ecos.shp") %>% 
  # filter(PROVINCE %in% c("Mediterranean Sea", "Red Sea and Gulf of Aden", "Somali/Arabian"),
         # !ECOREGION %in% c("Alboran Sea", "Western Mediterranean"))
  filter(ECOREGION %in% c("Arabian (Persian) Gulf", "Northern and Central Red Sea", "Southern Red Sea",  "Adriatic Sea",
                          "Levantine Sea", "Tunisian Plateau/Gulf of Sidra", "Ionian Sea", "Aegean Sea"))

# Find SST pixels within Med MEOW
registerDoParallel(cores = 15)
ecoregion_pixels <- plyr::ldply(unique(MEOW$ECOREGION), points_in_region, .parallel = T)

# Get ecoregion pixel count for category stats later on
ecoregion_pixel_count <- ecoregion_pixels %>% 
  group_by(Ecoregion) %>% 
  summarise(pixel_count = n(), .groups = "drop")

# Subset MHW lon files accordingly
MHW_files_sub <- MHW_files[seq(which(lon_OISST == min(ecoregion_pixels$lon)),
                               which(lon_OISST == max(ecoregion_pixels$lon)))]

# Plot pixels
ecoregion_pixels %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = Ecoregion)) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_quickmap(xlim = c(9, 60), ylim = c(10, 50), expand = F) +
  labs(x = "Longitude (°E)", y = "Latitude (°N)") +
  # guides(fill = guide_legend(nrow = 3)) +
  theme(legend.position = "bottom")
ggsave("EMME_pixels.png", height = 8, width = 8)


# Load data by region -----------------------------------------------------

# Load rough bounding box
rough_bbox <- c(range(ecoregion_pixels$lon), range(ecoregion_pixels$lat))

# Load cat files
registerDoParallel(cores = 30)
system.time(
MHW_cat_crop <- plyr::ldply(MHW_cat_files, readRDS_sub, .parallel = T, sub_pixels = ecoregion_pixels)
) # 352 seconds on 10 cores, 172 seconds on 30 cores
gc()

# Load lon files
registerDoParallel(cores = 30)
system.time(
SST_crop <- plyr::ldply(unique(ecoregion_pixels$lon), load_sst_sub, .parallel = T, sub_pixels = ecoregion_pixels)
) # 66 seconds on 30 cores
gc()

# Test visual map
MHW_cat_crop %>%
# SST_crop %>%
  dplyr::select(lon, lat, Ecoregion) %>% 
  distinct() %>%  
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = Ecoregion)) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_quickmap(xlim = c(9, 60), ylim = c(10, 50))
ggsave("EMME_pixels.png")


# SST stats ---------------------------------------------------------------

# Function for calculating mean and decadal trend per TS
# This works with per pixel calcs and per ecoregion
# testers...
# df <- SST_pixel_annual %>%
#   filter(lon == SST_pixel_annual$lon[1],
#          lat == SST_pixel_annual$lat[1])
mean_trend_calc <- function(df){

  # Decadal trend
  dec_trend <- broom::tidy(lm(temp_annual ~ year, df)) %>% 
    slice(2) %>% 
    mutate(dec_trend = round(estimate*10, 3)) %>% 
    dplyr::select(dec_trend, p.value)
  
  # Total means
  mean_total <- df %>% 
    summarise(temp_total = mean(temp_annual, na.rm = T), .groups = "drop")
  
  # Combine and exit
  res <- cbind(dec_trend, mean_total)
  rm(df, dec_trend, mean_total); gc()
  return(res)
}

## Per pixel stats
# Annual average per pixel
SST_pixel_annual <- SST_crop %>% 
  filter(year <= 2020) %>% # 2021 is not yet complete
  group_by(Ecoregion, lon, lat, year) %>%
  summarise(temp_annual = mean(temp, na.rm = T), .groups = "drop")

# Per pixel stats
system.time(
SST_pixel_stats <- plyr::ddply(SST_pixel_annual, c("lon", "lat"), mean_trend_calc, .parallel = T)
) # 143 seconds on 10 cores

## Per ecoregion stats
# Annual average per ecoregion
SST_ecoregion_annual <- SST_pixel_annual %>% 
  group_by(Ecoregion, year) %>%
  summarise(temp_annual = mean(temp_annual, na.rm = T), .groups = "drop")

# Per ecoregion stats
system.time(
SST_ecoregion_stats <- plyr::ddply(SST_ecoregion_annual, c("Ecoregion"), mean_trend_calc, .parallel = T)
) # 2 seconds on 10 cores

# Global mean and trend
OISST_global_stats <- mean_trend_calc(rename(OISST_SST_global_annual, temp_annual = temp, year = t)) %>% 
  mutate(dec_trend = round(dec_trend, 2))


# SST figure --------------------------------------------------------------

# Map of SST mean per pixel SST 
map_SST_total <- SST_pixel_stats %>%
  ggplot() +
  geom_tile(aes(fill = temp_total, x = lon, y = lat)) +
  geom_polygon(data = map_base, aes(group = group, x = lon, y = lat)) +
  geom_sf(data = MEOW, aes(colour = ECOREGION), fill = NA, show.legend = F) +
  scale_fill_viridis_c(breaks = c(17, 21, 25, 29)) +
  scale_colour_brewer(palette = "Set1") +
  labs(x = NULL, y = NULL, fill = "Temp. (°C)") +
  coord_sf(xlim = c(9, 60), ylim = c(10, 50)) +
  theme(legend.position = "top", 
        panel.background = element_rect(colour = "black", fill = NULL))
# ggsave("EMME_test.png")

# Map of decadal trend per pixel
map_SST_trend <- SST_pixel_stats %>%
  filter(p.value <= 0.05) %>% 
  ggplot() +
  geom_tile(aes(fill = dec_trend, x = lon, y = lat)) +
  geom_polygon(data = map_base, aes(group = group, x = lon, y = lat)) +
  geom_sf(data = MEOW, aes(colour = ECOREGION), fill = NA, show.legend = F) +
  scale_fill_viridis_c(option = "A") +
  scale_colour_brewer(palette = "Set1") +
  labs(x = NULL, y = NULL, 
       fill = "Temp. trend\n(°C/decade)", colour = "Ecoregion") +
  coord_sf(xlim = c(9, 60), ylim = c(10, 50)) +
  theme(legend.position = "top", 
        panel.background = element_rect(colour = "black", fill = NULL))
# ggsave("EMME_test.png")

# Prep labels for plotting
sst_eco_labels <- SST_ecoregion_stats %>% 
  arrange(-temp_total) %>% 
  mutate(dec_trend = round(dec_trend, 2),
         y_point = rev(seq(16, 30, 2)))

# Time series plots with decadal trends
ts_eco <- SST_ecoregion_annual %>% 
  ggplot(aes(x = year, y = temp_annual)) +
  # Ecoregion values
  geom_point(aes(colour = Ecoregion), show.legend = F) +
  geom_line(aes(colour = Ecoregion), key_glyph = "abline") +
  geom_smooth(aes(colour = Ecoregion), method = "lm", se = F, show.legend = F) +
  geom_label(data = sst_eco_labels, show.legend = F, label.size = 5,
             aes(label = paste0(dec_trend,"°C/dec."), x = 1978.5, y = y_point, colour = Ecoregion)) +
  geom_label(data = sst_eco_labels, show.legend = F, label.size = 0,
             aes(label = paste0(dec_trend,"°C/dec."), x = 1978.5, y = y_point), colour = "black") +
  # Global values
  geom_point(data = OISST_SST_global_annual, aes(x = t, y = temp), colour = "grey") +
  geom_line(data = OISST_SST_global_annual, aes(x = t, y = temp), colour = "grey") +
  geom_smooth(data = OISST_SST_global_annual, aes(x = t, y = temp), colour = "grey",
              method = "lm", se = F, show.legend = F) +
  geom_label(data = OISST_global_stats, show.legend = F, label.size = 5,
             aes(label = paste0(dec_trend,"°C/dec."), x = 1978.5, y = 14), colour = "grey") +
  geom_label(data = OISST_global_stats, show.legend = F, label.size = 0,
             aes(label = paste0(dec_trend,"°C/dec."), x = 1978.5, y = 14), colour = "black") +
  # Other bits
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  scale_colour_brewer(palette = "Set1") +
  scale_y_continuous(breaks = c(15, 19, 23, 27)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1975, 2020),
                     breaks = c(1982, 1987, 1992, 1997, 2002, 2007, 2012, 2017)) +
  labs(x = NULL, y = "Temperature (°C)", colour = "Ecoregion") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(colour = "black", fill = NULL))
# ggsave("EMME_test.png")

# Combine SST figures
fig_SST_maps <- ggpubr::ggarrange(map_SST_total, map_SST_trend, 
                                  ncol = 2, nrow = 1, align = "hv", 
                                  labels = c("A)", "B)"))
# ggsave("EMME_test.png", height = 4, width = 8)
fig_SST_ALL <- ggpubr::ggarrange(fig_SST_maps, ts_eco, 
                                 ncol = 1, #align = "hv", 
                                 labels = c(NA, "C)"), heights = c(1, 1))
ggsave("EMME_SST_fig.png", fig_SST_ALL, height = 10, width = 10)


# MHW cat stats -----------------------------------------------------------

# Function for calculating mean and decadal trend per TS
# This works with per pixel calcs and per ecoregion
# testers...
# df <- SST_pixel_annual %>%
#   filter(lon == SST_pixel_annual$lon[1],
#          lat == SST_pixel_annual$lat[1])
dur_trend_calc <- function(df){
  
  # Decadal trend
  dec_trend <- broom::tidy(lm(dur_annual ~ year, df)) %>% 
    slice(2) %>% 
    mutate(dec_trend = round(estimate*10, 3)) %>% 
    dplyr::select(dec_trend, p.value)
  
  # Total means
  dur_total <- df %>% 
    summarise(dur_total = mean(dur_annual, na.rm = T), .groups = "drop")
  
  # Combine and exit
  res <- cbind(dec_trend, dur_total)
  rm(df, dec_trend, dur_total); gc()
  return(res)
}

## Per pixel stats
# Annual average per pixel
dur_pixel_annual <- MHW_cat_crop %>% 
  filter(year <= 2020) %>% # 2021 is not yet complete
  group_by(Ecoregion, lon, lat, year) %>%
  summarise(dur_annual = n(), .groups = "drop")

# Per pixel stats
system.time(
  dur_pixel_stats <- plyr::ddply(dur_pixel_annual, c("lon", "lat"), dur_trend_calc, .parallel = T)
) # 100 seconds on 30 cores

## Per ecoregion stats
# Annual average per ecoregion
dur_ecoregion_annual <- dur_pixel_annual %>% 
  group_by(Ecoregion, year) %>%
  summarise(dur_annual = round(mean(dur_annual, na.rm = T)), .groups = "drop")

# Per ecoregion stats
dur_ecoregion_stats <- plyr::ddply(dur_ecoregion_annual, c("Ecoregion"), dur_trend_calc, .parallel = T)

# Global mean and trend
OISST_cat_global_stats <- dur_trend_calc(rename(OISST_cat_global_annual, dur_annual = cat_n_prop_stack, year = t)) %>% 
  mutate(dec_trend = round(dec_trend))


# MHW cat figure ----------------------------------------------------------

# Map of SST mean per pixel SST 
map_dur_total <- dur_pixel_stats %>%
  ggplot() +
  geom_tile(aes(fill = dur_total, x = lon, y = lat)) +
  geom_polygon(data = map_base, aes(group = group, x = lon, y = lat)) +
  geom_sf(data = MEOW, aes(colour = ECOREGION), fill = NA, show.legend = F) +
  scale_fill_viridis_c(option = "E") +
  scale_colour_brewer(palette = "Set1") +
  labs(x = NULL, y = NULL, fill = "MHW days (n)") +
  coord_sf(xlim = c(9, 60), ylim = c(10, 50)) +
  theme(legend.position = "top", 
        panel.background = element_rect(colour = "black", fill = NULL))
# ggsave("EMME_test.png")

# Map of decadal trend per pixel
map_dur_trend <- dur_pixel_stats %>%
  filter(p.value <= 0.05) %>% 
  ggplot() +
  geom_tile(aes(fill = dec_trend, x = lon, y = lat)) +
  geom_polygon(data = map_base, aes(group = group, x = lon, y = lat)) +
  geom_sf(data = MEOW, aes(colour = ECOREGION), fill = NA, show.legend = F) +
  scale_fill_viridis_c(option = "B") +
  scale_colour_brewer(palette = "Set1") +
  labs(x = NULL, y = NULL, 
       fill = "MHW trend\n(days/decade)", colour = "Ecoregion") +
  coord_sf(xlim = c(9, 60), ylim = c(10, 50)) +
  theme(legend.position = "top", 
        panel.background = element_rect(colour = "black", fill = NULL))
# ggsave("EMME_test.png")

# Prep labels for plotting
dur_eco_labels <- dur_ecoregion_stats %>% 
  arrange(-dur_total) %>% 
  mutate(dec_trend = round(dec_trend),
         y_point = rev(seq(20, 220, length.out = 8)))

# Time series plots with decadal trends
ts_dur_eco <- dur_ecoregion_annual %>% 
  ggplot(aes(x = year, y = dur_annual)) +
  # Ecoregion values
  geom_point(aes(colour = Ecoregion), show.legend = F) +
  geom_line(aes(colour = Ecoregion), key_glyph = "abline") +
  geom_smooth(aes(colour = Ecoregion), method = "lm", se = F, show.legend = F) +
  geom_label(data = dur_eco_labels, show.legend = F, label.size = 5,
             aes(label = paste0(dec_trend," days/dec."), x = 1978.5, y = y_point, colour = Ecoregion)) +
  geom_label(data = dur_eco_labels, show.legend = F, label.size = 0,
             aes(label = paste0(dec_trend," days/dec."), x = 1978.5, y = y_point), colour = "black") +
  # Global values
  geom_point(data = OISST_cat_global_annual, aes(x = t, y = cat_n_prop_stack), colour = "grey") +
  geom_line(data = OISST_cat_global_annual, aes(x = t, y = cat_n_prop_stack), colour = "grey") +
  geom_smooth(data = OISST_cat_global_annual, aes(x = t, y = cat_n_prop_stack), colour = "grey",
              method = "lm", se = F, show.legend = F) +
  geom_label(data = OISST_cat_global_stats, show.legend = F, label.size = 5,
             aes(label = paste0(dec_trend," days/dec."), x = 1978.5, y = -10), colour = "grey") +
  geom_label(data = OISST_cat_global_stats, show.legend = F, label.size = 0,
             aes(label = paste0(dec_trend," days/dec."), x = 1978.5, y = -10), colour = "black") +
  # Other bits
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  scale_colour_brewer(palette = "Set1") +
  # scale_y_continuous(breaks = c(15, 19, 23, 27)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1975, 2020),
                     breaks = c(1982, 1987, 1992, 1997, 2002, 2007, 2012, 2017)) +
  labs(x = NULL, y = "MHW days", colour = "Ecoregion") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(colour = "black", fill = NULL))
# ggsave("EMME_test.png")

# Combine SST figures
fig_dur_maps <- ggpubr::ggarrange(map_dur_total, map_dur_trend, 
                                  ncol = 2, nrow = 1, align = "hv", 
                                  labels = c("A)", "B)"))
# ggsave("EMME_test.png", height = 4, width = 8)
fig_SST_ALL <- ggpubr::ggarrange(fig_dur_maps, ts_dur_eco, 
                                 ncol = 1, #align = "hv", 
                                 labels = c(NA, "C)"), heights = c(1, 1))
ggsave("EMME_dur_fig.png", fig_SST_ALL, height = 10, width = 10)

