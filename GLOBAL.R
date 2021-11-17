# GLOBAL.R
# This script contains code used for global analyses
# THis may be SST and/or MHW focussed
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
}

# Load lon files
registerDoParallel(cores = 50)
system.time(
  SST_trends <- plyr::ldply(unique(ecoregion_pixels$lon), sst_analysis, .parallel = T, sub_pixels = ecoregion_pixels)
) # 49 minutes on 50 cores
gc
save(SST_trends, file = "data/SST_trends.RData")
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
  ggplot() +
  geom_tile(aes(fill = temp_total, x = lon, y = lat)) +
  geom_sf(data = MEOW, aes(colour = ECOREGION), fill = NA, show.legend = F) +
  geom_polygon(data = map_base, aes(group = group, x = lon, y = lat)) +
  scale_fill_viridis_c() +
  # scale_colour_brewer(palette = "Set1") +
  labs(x = NULL, y = NULL, fill = "Temp. (°C)") +
  coord_sf(expand = F) +
  theme(legend.position = "top", 
        panel.background = element_rect(colour = "black", fill = NULL))
map_SST_total

# Map of decadal trend per pixel
map_SST_trend <- SST_trends %>%
  filter(p.value <= 0.05) %>% 
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
map_SST_trend

# Rank tables of ecoregions
## NB: THis is pretty, but can't be combined with ggplot objects
med_table <- SST_trends_ALL %>% 
  dplyr::rename(Realm = REALM, Province = PROVINCE, Rank = rank, Trend = dec_trend_mean, SD = dec_trend_sd) %>% 
  gt(groupname_col = "grouping") %>% 
  tab_header(title = md("Mediterannean Sea ecoregions ranked by warming")) %>% 
  tab_spanner(label = "Decadal warming (°C)", columns = matches("Trend|SD"))
med_table

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
plot(med_table)

# Combine SST figures
fig_SST_maps <- ggpubr::ggarrange(map_SST_total, map_SST_trend, 
                                  ncol = 1, nrow = 2, align = "hv", 
                                  labels = c("A)", "B)"))
fig_SST_all <- ggpubr::ggarrange(fig_SST_maps, med_table, ncol = 2, nrow = 1, 
                                 labels = c(NA, "C)"), widths = c(1, 1))
ggsave("graph/GLOBAL_SST.png", fig_SST_all, height = 11, width = 19)


# MHW analysis ------------------------------------------------------------


# Load cat files
registerDoParallel(cores = 50)
system.time(
  MHW_cat_crop <- plyr::ldply(MHW_cat_files, readRDS_sub, .parallel = T, sub_pixels = ecoregion_pixels)
) # 749 seconds on 50 cores
gc()

# Test visual map
MHW_cat_crop %>%
  # SST_crop %>%
  dplyr::select(lon, lat, Ecoregion) %>% 
  distinct() %>%  
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = Ecoregion)) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_quickmap()


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
registerDoParallel(cores = 50)
system.time(
  dur_pixel_stats <- plyr::ddply(dur_pixel_annual, c("lon", "lat"), dur_trend_calc, .parallel = T)
) # xxx seconds on 30 cores

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

# Map of MHW duration mean per pixel
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
map_dur_total

# Map of decadal MHW duration trend per pixel
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
map_dur_trend

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
ts_dur_eco

# Combine SST figures
fig_dur_maps <- ggpubr::ggarrange(map_dur_total, map_dur_trend, 
                                  ncol = 2, nrow = 1, align = "hv", 
                                  labels = c("A)", "B)"))
fig_SST_ALL <- ggpubr::ggarrange(fig_dur_maps, ts_dur_eco, 
                                 ncol = 1, #align = "hv", 
                                 labels = c(NA, "C)"), heights = c(1, 1))
ggsave("GLOBAL_MHW_duration.png", fig_SST_ALL, height = 10, width = 10)


