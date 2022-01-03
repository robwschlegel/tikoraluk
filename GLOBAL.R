# GLOBAL.R
# This script contains code used for global analyses
# This may be SST and/or MHW focussed
# MEOW is also used


# Setup -------------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(gridExtra) # For tables that work with ggplot
library(grid)
library(gtable)
library(gt) # For pretty tables. Not useful because can't be combined with ggplot objects
# library(ggplotify) # For converting pretty tables to ggplot objects
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

# MHW cat results by clim lon slices
MHW_cat_lon_files <- dir("../data/cat_lon", pattern = "MHW", full.names = T)

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
    right_join(sub_pixels, by = c("lon", "lat")) %>% 
    mutate(time = as.Date(time, origin = "1970-01-01"),
           year = lubridate::year(time)) %>% 
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
  region_in_sub <- MEOW %>% 
    filter(ECOREGION == region_in) %>% 
    dplyr::select(geometry)
  lidx <- length(region_in_sub$geometry[[1]])
  coords_in <- data.frame()
  for(i in 1:lidx){
    region_sub <- as.data.frame(region_in_sub$geometry[[1]][[i]]) %>%
      `colnames<-`(c("lon", "lat"))
    coords_in_i <- OISST_ocean_coords %>% 
      mutate(in_grid = sp::point.in.polygon(point.x = OISST_ocean_coords[["lon"]], point.y = OISST_ocean_coords[["lat"]], 
                                            pol.x = region_sub[["lon"]], pol.y = region_sub[["lat"]])) %>% 
      filter(in_grid >= 1) %>% 
      mutate(Ecoregion = region_in) %>% 
      dplyr::select(lon, lat, Ecoregion)
    coords_in <- rbind(coords_in, coords_in_i)
  }
  return(coords_in)
}


# Establish regions -------------------------------------------------------

# Load MEOW
MEOW <- read_sf("metadata/MEOW/meow_ecos.shp")

# Find SST pixels within Med MEOW
registerDoParallel(cores = 50)
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
  geom_tile(aes(fill = Ecoregion), show.legend = F) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_quickmap(expand = F) +
  labs(x = "Longitude (°E)", y = "Latitude (°N)") +
  theme(legend.position = "bottom")


# SST analysis ------------------------------------------------------------

# Function for SST trend calculations
mean_trend_calc <- function(df){
  
  # Decadal trend
  dec_trend <- broom::tidy(lm(temp_annual ~ year, df)) %>% 
    slice(2) %>% 
    mutate(dec_trend = round(estimate*10, 3),
           p.value = round(p.value, 4)) %>% 
    dplyr::select(dec_trend, p.value)
  
  # Total means
  mean_total <- df %>% 
    summarise(temp_total = round(mean(temp_annual, na.rm = T), 2), .groups = "drop")
  
  # Combine and exit
  res <- cbind(dec_trend, mean_total)
  rm(df, dec_trend, mean_total); gc()
  return(res)
}

# Function for performing analysis on SST pixels
sst_analysis <- function(lon, sub_pixels){
  
  # Base data
  SST_sub <- load_sst_sub(lon, sub_pixels)
  
  # Annual average per pixel
  SST_pixel_annual <- SST_sub %>% 
    filter(year <= 2020) %>% # 2021 is not yet complete
    group_by(Ecoregion, lon, lat, year) %>%
    summarise(temp_annual = mean(temp, na.rm = T), .groups = "drop")
  
  # Decadal trend
  SST_pixel_stats <- plyr::ddply(SST_pixel_annual, c("Ecoregion", "lon", "lat"), mean_trend_calc, .parallel = F)
  
  # CLean and exit
  rm(SST_sub, SST_pixel_annual); gc()
  return(SST_pixel_stats)
}

# Load lon files
registerDoParallel(cores = 50)
# system.time(
#   SST_trends <- plyr::ldply(unique(ecoregion_pixels$lon), sst_analysis, .parallel = T, sub_pixels = ecoregion_pixels)
# ) # 49 minutes on 50 cores
# gc()
# save(SST_trends, file = "data/SST_trends.RData")
load("data/SST_trends.RData")

## Per ecoregion stats
# Average per ecoregion
SST_trends_ecoregion <- SST_trends %>% 
  group_by(Ecoregion) %>%
  summarise(dec_trend_mean = round(mean(dec_trend, na.rm = T), 2),
            dec_trend_range = max(dec_trend, na.rm = T) - min(dec_trend, na.rm = T),
            dec_trend_sd = round(sd(dec_trend, na.rm = T), 2),
            temp_total_mean = round(mean(temp_total, na.rm = T), 2),
            temp_total_range = max(temp_total, na.rm = T) - min(temp_total, na.rm = T),
            temp_total_sd = round(sd(temp_total, na.rm = T), 2), .groups = "drop") %>% 
  arrange(-dec_trend_mean) %>% 
  mutate(rank = 1:n())

# Average per ecoregion
SST_trends_province <- SST_trends %>% 
  left_join(MEOW[,c("ECOREGION", "PROVINCE")], by = c("Ecoregion" = "ECOREGION")) %>% 
  group_by(PROVINCE) %>%
  summarise(dec_trend_mean = round(mean(dec_trend, na.rm = T), 2),
            dec_trend_range = max(dec_trend, na.rm = T) - min(dec_trend, na.rm = T),
            dec_trend_sd = round(sd(dec_trend, na.rm = T), 2),
            temp_total_mean = round(mean(temp_total, na.rm = T), 2),
            temp_total_range = max(temp_total, na.rm = T) - min(temp_total, na.rm = T),
            temp_total_sd = round(sd(temp_total, na.rm = T), 2), .groups = "drop") %>% 
  arrange(-dec_trend_mean) %>% 
  mutate(rank = 1:n())

# Average per ecoregion
SST_trends_realm <- SST_trends %>% 
  left_join(MEOW[,c("ECOREGION", "REALM")], by = c("Ecoregion" = "ECOREGION")) %>% 
  group_by(REALM) %>%
  summarise(dec_trend_mean = round(mean(dec_trend, na.rm = T), 2),
            dec_trend_range = max(dec_trend, na.rm = T) - min(dec_trend, na.rm = T),
            dec_trend_sd = round(sd(dec_trend, na.rm = T), 2),
            temp_total_mean = round(mean(temp_total, na.rm = T), 2),
            temp_total_range = max(temp_total, na.rm = T) - min(temp_total, na.rm = T),
            temp_total_sd = round(sd(temp_total, na.rm = T), 2), .groups = "drop") %>% 
  arrange(-dec_trend_mean) %>% 
  mutate(rank = 1:n())

# Global mean and trend
SST_trends_global <- SST_trends %>% 
  summarise(dec_trend_mean = round(mean(dec_trend, na.rm = T), 2),
            dec_trend_range = max(dec_trend, na.rm = T) - min(dec_trend, na.rm = T),
            dec_trend_sd = round(sd(dec_trend, na.rm = T), 2),
            temp_total_mean = round(mean(temp_total, na.rm = T), 2),
            temp_total_range = max(temp_total, na.rm = T) - min(temp_total, na.rm = T),
            temp_total_sd = round(sd(temp_total, na.rm = T), 2), .groups = "drop")

# Create table for plotting
SST_trends_ecoregion_Med <- SST_trends_ecoregion %>% 
  left_join(MEOW[,c("ECOREGION", "PROVINCE", "REALM")], by = c("Ecoregion" = "ECOREGION")) %>% 
  dplyr::select(-geometry) %>% 
  filter(PROVINCE == "Mediterranean Sea") %>% 
  dplyr::select(REALM, PROVINCE, Ecoregion, rank, dec_trend_mean, dec_trend_sd) %>% 
  mutate(grouping = "Ecoregion")
SST_trends_province_Med <- SST_trends_province %>% 
  left_join(MEOW[,c("PROVINCE", "REALM")], by = c("PROVINCE")) %>% 
  dplyr::select(-geometry) %>% 
  distinct() %>% 
  slice(1:10) %>% 
  dplyr::select(REALM, PROVINCE, rank, dec_trend_mean, dec_trend_sd)  %>% 
  mutate(grouping = "Province")
SST_trends_ALL <- bind_rows(SST_trends_ecoregion_Med, SST_trends_province_Med,
                            mutate(SST_trends_realm, grouping = "Realm")) %>% 
  dplyr::select(grouping, rank, REALM:dec_trend_sd)


# SST figure --------------------------------------------------------------

# Map of SST mean per pixel SST 
map_SST_total <- SST_trends %>%
  ## NB: THis causes plotting issues
  # na.omit() %>% 
  # filter(p.value <= 0.05) %>% 
  # mutate(temp_total_05 = quantile(temp_total, probs = 0.05),
  #        temp_total_95 = quantile(temp_total, probs = 0.95),
  #        temp_total = case_when(temp_total > temp_total_95 ~ temp_total_95,
  #                               temp_total < temp_total_05 ~ temp_total_05,
  #                               TRUE ~ temp_total)) %>% 
  ggplot() +
  geom_tile(aes(fill = temp_total, x = lon, y = lat)) +
  geom_sf(data = MEOW, aes(colour = ECOREGION), fill = NA, show.legend = F) +
  geom_polygon(data = map_base, aes(group = group, x = lon, y = lat)) +
  scale_fill_viridis_c() +
  # scale_colour_brewer(palette = "Set1") +
  labs(x = NULL, y = NULL, fill = "Annual average\ntemperature (°C)") +
  coord_sf(expand = F) +
  theme(legend.position = "top", 
        panel.background = element_rect(colour = "black", fill = NULL))
# map_SST_total

# Map of decadal trend per pixel
map_SST_trend <- SST_trends %>%
  na.omit() %>% 
  filter(p.value <= 0.05) %>% 
  mutate(dec_trend_05 = quantile(dec_trend, probs = 0.05),
         dec_trend_95 = quantile(dec_trend, probs = 0.95),
         dec_trend = case_when(dec_trend > dec_trend_95 ~ dec_trend_95,
                               dec_trend < dec_trend_05 ~ dec_trend_05,
                               TRUE ~ dec_trend)) %>% 
  ggplot() +
  geom_tile(aes(fill = dec_trend, x = lon, y = lat)) +
  # geom_sf(data = MEOW, aes(colour = ECOREGION), fill = NA, show.legend = F) +
  geom_polygon(data = map_base, aes(group = group, x = lon, y = lat)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  # scale_fill_viridis_c(option = "A") +
  # scale_colour_brewer(palette = "Set1") +
  labs(x = NULL, y = NULL, 
       fill = "Temp. trend\n(°C/decade)", colour = "Ecoregion") +
  coord_sf(expand = F) +
  theme(legend.position = "top", 
        panel.background = element_rect(colour = "black", fill = NULL))
# map_SST_trend

# Rank tables of ecoregions
## NB: This is pretty, but can't be combined with ggplot objects
med_table <- SST_trends_ALL %>% 
  dplyr::rename(Realm = REALM, Province = PROVINCE, Rank = rank, Trend = dec_trend_mean, SD = dec_trend_sd) %>% 
  gt(groupname_col = "grouping") %>% 
  tab_header(title = md("Mediterannean Sea ecoregions ranked by warming")) %>% 
  tab_spanner(label = "Decadal warming (°C)", columns = matches("Trend|SD"))
# med_table

# Table theme
t1 <- ttheme_default(core = list(bg_params = list(fill = c(rep(c("grey90", "grey95"), length.out = 7),
                                                           rep(c("grey100", "grey95"), length.out = 10),
                                                           rep(c("grey90", "grey95"), length.out = 12)))))

# Table
med_table <- SST_trends_ALL %>% 
  dplyr::rename(Realm = REALM, Province = PROVINCE, Rank = rank, Trend = dec_trend_mean, SD = dec_trend_sd) %>% 
  dplyr::select(-grouping) %>% 
  replace(is.na(.), "") %>% 
  tableGrob(rows = NULL, theme = t1) %>% 
  gtable_add_grob(grobs = segmentsGrob(
                    x0 = unit(0,"npc"),
                    y0 = unit(0,"npc"),
                    x1 = unit(1,"npc"),
                    y1 = unit(0,"npc"),
                    gp = gpar(lwd = 2.0)),
                  t = 8, b = 8, l = 1, r = 6) %>% 
  gtable_add_grob(grobs = segmentsGrob(
    x0 = unit(0,"npc"),
    y0 = unit(0,"npc"),
    x1 = unit(1,"npc"),
    y1 = unit(0,"npc"),
    gp = gpar(lwd = 2.0)),
    t = 18, b = 18, l = 1, r = 6)
# plot(med_table)

# Combine SST figures
fig_SST_maps <- ggpubr::ggarrange(map_SST_total, map_SST_trend, 
                                  ncol = 1, nrow = 2, align = "hv", 
                                  labels = c("A)", "B)"))
fig_SST_all <- ggpubr::ggarrange(fig_SST_maps, med_table, ncol = 2, nrow = 1, 
                                 labels = c(NA, "C)"), widths = c(1, 1))
ggsave("graph/GLOBAL_SST_trends.png", fig_SST_all, height = 11, width = 19)


# MHW analysis ------------------------------------------------------------

# Function for calculating mean and decadal trend per TS
dur_trend_calc <- function(df){
  
  # Decadal trend
  dec_trend <- broom::tidy(lm(dur_annual ~ year, df)) %>% 
    slice(2) %>% 
    mutate(dec_trend = round(estimate*10, 3),
           p.value = round(p.value, 4)) %>% 
    dplyr::select(dec_trend, p.value)
  
  # Total means
  dur_total <- df %>% 
    summarise(dur_total = round(mean(dur_annual, na.rm = T), 1),.groups = "drop")
  
  # Combine and exit
  res <- cbind(dec_trend, dur_total)
  rm(df, dec_trend, dur_total); gc()
  return(res)
}

# Analysis of trends in MHW days
MHW_analysis <- function(file_name, sub_pixels){
  
  # Prepare data
  MHW_sub <- readRDS_sub(file_name, sub_pixels = ecoregion_pixels) %>% 
    mutate(year = lubridate::year(t)) %>% 
    filter(year <= 2020) %>% # 2021 is not yet complete
    group_by(Ecoregion, lon, lat, year) %>% 
    summarise(category_max = max(category, na.rm = T),
              dur_annual = n(), .groups = "drop")
  
  # Decadal trend
  dur_pixel_stats <- plyr::ddply(MHW_sub, c("Ecoregion", "lon", "lat"), dur_trend_calc, .parallel = F)
 
  # CLean and exit
  rm(MHW_sub); gc()
  return(dur_pixel_stats) 
}

# Run global MEOW MHW duration analysis
# registerDoParallel(cores = 50)
# system.time(
#   MHW_dur_trends <- plyr::ldply(MHW_cat_lon_files, MHW_analysis, .parallel = T, sub_pixels = ecoregion_pixels)
# ) # xxx minutes on 50 cores
# gc()
# save(MHW_dur_trends, file = "data/MHW_dur_trends.RData")
load("data/MHW_dur_trends.RData")

## Per ecoregion stats
# Average per ecoregion
MHW_dur_trends_ecoregion <- MHW_dur_trends %>% 
  group_by(Ecoregion) %>%
  summarise(dec_trend_mean = round(mean(dec_trend, na.rm = T), 2),
            dec_trend_range = max(dec_trend, na.rm = T) - min(dec_trend, na.rm = T),
            dec_trend_sd = round(sd(dec_trend, na.rm = T), 2),
            dur_total_mean = round(mean(dur_total, na.rm = T), 2),
            dur_total_range = max(dur_total, na.rm = T) - min(dur_total, na.rm = T),
            dur_total_sd = round(sd(dur_total, na.rm = T), 2), .groups = "drop") %>% 
  arrange(-dec_trend_mean) %>% 
  mutate(rank = 1:n())

# Average per province
MHW_dur_trends_province <- MHW_dur_trends %>% 
  left_join(MEOW[,c("ECOREGION", "PROVINCE")], by = c("Ecoregion" = "ECOREGION")) %>% 
  group_by(PROVINCE) %>%
  summarise(dec_trend_mean = round(mean(dec_trend, na.rm = T), 2),
            dec_trend_range = max(dec_trend, na.rm = T) - min(dec_trend, na.rm = T),
            dec_trend_sd = round(sd(dec_trend, na.rm = T), 2),
            dur_total_mean = round(mean(dur_total, na.rm = T), 2),
            dur_total_range = max(dur_total, na.rm = T) - min(dur_total, na.rm = T),
            dur_total_sd = round(sd(dur_total, na.rm = T), 2), .groups = "drop") %>% 
  arrange(-dec_trend_mean) %>% 
  mutate(rank = 1:n())

# Average per realm
MHW_dur_trends_realm <- MHW_dur_trends %>% 
  left_join(MEOW[,c("ECOREGION", "REALM")], by = c("Ecoregion" = "ECOREGION")) %>% 
  group_by(REALM) %>%
  summarise(dec_trend_mean = round(mean(dec_trend, na.rm = T), 2),
            dec_trend_range = max(dec_trend, na.rm = T) - min(dec_trend, na.rm = T),
            dec_trend_sd = round(sd(dec_trend, na.rm = T), 2),
            dur_total_mean = round(mean(dur_total, na.rm = T), 2),
            dur_total_range = max(dur_total, na.rm = T) - min(dur_total, na.rm = T),
            dur_total_sd = round(sd(dur_total, na.rm = T), 2), .groups = "drop") %>% 
  arrange(-dec_trend_mean) %>% 
  mutate(rank = 1:n())

# Global mean and trend
MHW_dur_trends_global <- MHW_dur_trends %>% 
  summarise(dec_trend_mean = round(mean(dec_trend, na.rm = T), 2),
            dec_trend_range = max(dec_trend, na.rm = T) - min(dec_trend, na.rm = T),
            dec_trend_sd = round(sd(dec_trend, na.rm = T), 2),
            dur_total_mean = round(mean(dur_total, na.rm = T), 2),
            dur_total_range = max(dur_total, na.rm = T) - min(dur_total, na.rm = T),
            dur_total_sd = round(sd(dur_total, na.rm = T), 2), .groups = "drop")

# Create table for plotting
MHW_dur_trends_ecoregion_Med <- MHW_dur_trends_ecoregion %>% 
  left_join(MEOW[,c("ECOREGION", "PROVINCE", "REALM")], by = c("Ecoregion" = "ECOREGION")) %>% 
  dplyr::select(-geometry) %>% 
  filter(PROVINCE == "Mediterranean Sea") %>% 
  dplyr::select(REALM, PROVINCE, Ecoregion, rank, dec_trend_mean, dec_trend_sd) %>% 
  mutate(grouping = "Ecoregion")
MHW_dur_trends_province_Med <- MHW_dur_trends_province %>% 
  left_join(MEOW[,c("PROVINCE", "REALM")], by = c("PROVINCE")) %>% 
  dplyr::select(-geometry) %>% 
  distinct() %>% 
  slice(1:10) %>% 
  dplyr::select(REALM, PROVINCE, rank, dec_trend_mean, dec_trend_sd)  %>% 
  mutate(grouping = "Province")
MHW_dur_trends_ALL <- bind_rows(MHW_dur_trends_ecoregion_Med, MHW_dur_trends_province_Med,
                            mutate(MHW_dur_trends_realm, grouping = "Realm")) %>% 
  dplyr::select(grouping, rank, REALM:dec_trend_sd)


# MHW figures -------------------------------------------------------------

# Map of SST mean per pixel SST 
map_MHW_dur_total <- MHW_dur_trends %>%
  na.omit() %>% 
  mutate(dur_total_05 = quantile(dur_total, probs = 0.05),
         dur_total_95 = quantile(dur_total, probs = 0.95),
         dur_total = case_when(dur_total > dur_total_95 ~ dur_total_95,
                               dur_total < dur_total_05 ~ dur_total_05,
                               TRUE ~ dur_total)) %>% 
  ggplot() +
  geom_tile(aes(fill = dur_total, x = lon, y = lat)) +
  geom_sf(data = MEOW, aes(colour = ECOREGION), fill = NA, show.legend = F) +
  geom_polygon(data = map_base, aes(group = group, x = lon, y = lat)) +
  scale_fill_viridis_c(option = "A") +
  # scale_colour_brewer(palette = "Set1") +
  labs(x = NULL, y = NULL, fill = "MHW days\n(annual average)") +
  coord_sf(expand = F) +
  theme(legend.position = "top", 
        panel.background = element_rect(colour = "black", fill = NULL))
# map_MHW_dur_total

# Map of decadal trend per pixel
map_MHW_dur_trend <- MHW_dur_trends %>%
  na.omit() %>% 
  filter(p.value <= 0.05) %>% 
  mutate(dec_trend_05 = quantile(dec_trend, probs = 0.05),
         dec_trend_95 = quantile(dec_trend, probs = 0.95),
         dec_trend = case_when(dec_trend > dec_trend_95 ~ dec_trend_95,
                               dec_trend < dec_trend_05 ~ dec_trend_05,
                               TRUE ~ dec_trend)) %>% 
  ggplot() +
  geom_tile(aes(fill = dec_trend, x = lon, y = lat)) +
  # geom_sf(data = MEOW, aes(colour = ECOREGION), fill = NA, show.legend = F) +
  geom_polygon(data = map_base, aes(group = group, x = lon, y = lat)) +
  scale_fill_gradient2(low = "yellow4", mid = "white", high = "green4") +
  # scale_fill_viridis_c(option = "A") +
  # scale_colour_brewer(palette = "Set1") +
  labs(x = NULL, y = NULL, 
       fill = "Annual MHW\ndays (n/decade)", colour = "Ecoregion") +
  coord_sf(expand = F) +
  theme(legend.position = "top", 
        panel.background = element_rect(colour = "black", fill = NULL))
# map_MHW_dur_trend

# Rank tables of ecoregions
## NB: This is pretty, but can't be combined with ggplot objects
med_table <- MHW_dur_trends_ALL %>% 
  dplyr::rename(Realm = REALM, Province = PROVINCE, Rank = rank, Trend = dec_trend_mean, SD = dec_trend_sd) %>% 
  gt(groupname_col = "grouping") %>% 
  tab_header(title = md("Mediterannean Sea ecoregions ranked by increasing MHW days")) %>% 
  tab_spanner(label = "MHW duration (days/decade)", columns = matches("Trend|SD"))
# med_table

# Table theme
t1 <- ttheme_default(core = list(bg_params = list(fill = c(rep(c("grey90", "grey95"), length.out = 7),
                                                           rep(c("grey100", "grey95"), length.out = 10),
                                                           rep(c("grey90", "grey95"), length.out = 12)))))

# Table
med_table <- MHW_dur_trends_ALL %>% 
  dplyr::rename(Realm = REALM, Province = PROVINCE, Rank = rank, Trend = dec_trend_mean, SD = dec_trend_sd) %>% 
  dplyr::select(-grouping) %>% 
  replace(is.na(.), "") %>% 
  tableGrob(rows = NULL, theme = t1) %>%
  gtable_add_grob(grobs = segmentsGrob(
    x0 = unit(0,"npc"),
    y0 = unit(0,"npc"),
    x1 = unit(1,"npc"),
    y1 = unit(0,"npc"),
    gp = gpar(lwd = 2.0)),
    t = 8, b = 8, l = 1, r = 6) %>%
  gtable_add_grob(grobs = segmentsGrob(
    x0 = unit(0,"npc"),
    y0 = unit(0,"npc"),
    x1 = unit(1,"npc"),
    y1 = unit(0,"npc"),
    gp = gpar(lwd = 2.0)),
    t = 18, b = 18, l = 1, r = 6)
# plot(med_table)

# Combine SST figures
fig_MHW_dur_maps <- ggpubr::ggarrange(map_MHW_dur_total, map_MHW_dur_trend, 
                                  ncol = 1, nrow = 2, align = "hv", 
                                  labels = c("A)", "B)"))
fig_MHW_dur_all <- ggpubr::ggarrange(fig_MHW_dur_maps, med_table, ncol = 2, nrow = 1, 
                                     labels = c(NA, "C)"), widths = c(1, 1))
ggsave("graph/GLOBAL_MHW_duration.png", fig_MHW_dur_all, height = 11, width = 19)

