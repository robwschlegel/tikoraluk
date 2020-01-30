# reviews/JMS_2020.R
# This script tests the MHW caluclations made in a paper that was
# sumitted to the Journal of Marine Science.
# They provide shockingly little w.r.t. a methods section,
# which may be enough to reject the paper there.
# But I also suspect that the MHW caluclations have been done incorrectly,
# so I need to check for myself first.


# Setup -------------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))

library(tidyverse)
library(heatwaveR)
library(tidync)

# Study area from the paper
bbox <- c(30, 50, -28, -8)

# File locations
OISST_files <- dir("../data/OISST", pattern = "avhrr-only", full.names = T)

# Lon coords
load("metadata/lon_OISST.RData")
OISST_lon_rows <- which(lon_OISST >= bbox[1] & lon_OISST <= bbox[2])

# register cores
doParallel::registerDoParallel(cores = 50)


# Load data ---------------------------------------------------------------

# Function for loading data
load_OISST <- function(lon_row){
  tidync_OISST <- tidync(OISST_files[lon_row]) %>% 
    hyper_filter(lat = between(lat, as.integer(bbox[3]), as.integer(bbox[4]))) %>%
    hyper_tibble() %>% 
    mutate(time = as.Date(time, origin = "1970-01-01")) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    dplyr::select(lon, lat, t, temp) %>% 
    filter(t <= "2018-12-31")
}

# Load the data
OISST_data <- plyr::ldply(OISST_lon_rows, load_OISST, .parallel = T)


# Detect MHWs -------------------------------------------------------------

# In the paper the authors used the full time series period for the clim base period
clim_data <- plyr::ddply(OISST_data, c("lon", "lat"), ts2clm, .parallel = T,
                         climatologyPeriod = c("1982-01-01", "2018-12-31"))

# Function to extract just MHW event data
detect_event_event <- function(df){
  res <- detect_event(df)$event %>% 
    dplyr::select(event_no:intensity_cumulative)
}

# MHW results
MHW_data <- plyr::ddply(clim_data, c("lon", "lat"), detect_event_event, .parallel = T)


# Monthly means -----------------------------------------------------------

# The two most likely monthly aggregates the authors used
MHW_monthly <- MHW_data %>% 
  mutate(month_start = lubridate::month(date_start, label = T),
         month_peak = lubridate::month(date_peak, label = T))

# Month start means
MHW_monthly_start_mean <- MHW_monthly %>% 
  dplyr::select(lon, lat, month_start, duration, intensity_mean, intensity_cumulative) %>% 
  group_by(lon, lat, month_start) %>% 
  mutate(event_count = n()) %>% 
  summarise_all(mean, na.rm = T)

# Month peak means
MHW_monthly_peak_mean <- MHW_monthly %>% 
  dplyr::select(lon, lat, month_peak, duration, intensity_mean, intensity_cumulative) %>% 
  group_by(lon, lat, month_peak) %>% 
  mutate(event_count = n()) %>% 
  summarise_all(mean, na.rm = T)


# Annual means ------------------------------------------------------------

MHW_year_mean <- MHW_data %>% 
  mutate(year = lubridate::year(date_start)) %>% 
  dplyr::select(year, duration, intensity_mean, intensity_cumulative) %>% 
  group_by(year) %>% 
  mutate(event_count = n()) %>% 
  summarise_all(mean, na.rm = T) %>% 
  pivot_longer(-year)
  

# Visualise ---------------------------------------------------------------

# NB: Some lines below provide a filtering option.
# This is done to allow for a more direct comparison for the magnitudes
# detected in the paper as they differ from those found here

# Mean mean intensity by month start date
# ggplot(filter(MHW_monthly_start_mean, intensity_mean <= 1.5), aes(x = lon, y = lat)) +
ggplot(MHW_monthly_start_mean, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = intensity_mean)) +
  coord_cartesian(expand = F, xlim = c(30, 48), ylim = c(-10, -27)) +
  facet_wrap(~month_start) +
  scale_fill_gradientn(colours = c("white", "dodgerblue", "yellow", "red")) +
  labs(x = NULL, y = NULL)

# Mean mean intensity by month peak date
ggplot(MHW_monthly_peak_mean, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = intensity_mean)) +
  coord_cartesian(expand = F, xlim = c(30, 48), ylim = c(-10, -27)) +
  facet_wrap(~month_peak) +
  scale_fill_gradientn(colours = c("white", "dodgerblue", "yellow", "red")) +
  labs(x = NULL, y = NULL)

# The month start and month peak values are very similar.
# We'll just go with month start from here out

# Mean count by month start date
ggplot(MHW_monthly_start_mean, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = event_count)) +
  coord_cartesian(expand = F, xlim = c(30, 48), ylim = c(-10, -27)) +
  facet_wrap(~month_start) +
  scale_fill_gradientn(colours = c("white", "dodgerblue", "yellow", "red")) +
  labs(x = NULL, y = NULL)

# Mean duration by month start date
# ggplot(filter(MHW_monthly_start_mean, duration <= 30), aes(x = lon, y = lat)) +
ggplot(MHW_monthly_start_mean, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = duration)) +
  coord_cartesian(expand = F, xlim = c(30, 48), ylim = c(-10, -27)) +
  facet_wrap(~month_start) +
  scale_fill_gradientn(colours = c("white", "dodgerblue", "yellow", "red")) +
  labs(x = NULL, y = NULL)

# Mean cumulative intensity by month start date
ggplot(filter(MHW_monthly_start_mean, intensity_cumulative <= 30), aes(x = lon, y = lat)) +
# ggplot(MHW_monthly_start_mean, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = intensity_cumulative)) +
  coord_cartesian(expand = F, xlim = c(30, 48), ylim = c(-10, -27)) +
  facet_wrap(~month_start) +
  scale_fill_gradientn(colours = c("white", "white", "dodgerblue", "yellow", "red"),
                       values = c(0, 0.2, 0.5, 0.75, 1)) +
  labs(x = NULL, y = NULL)

# Count per year
ggplot(MHW_year_mean, aes(x = year, y = value)) +
  geom_line() +
  # geom_hline(aes(yintercept = mean(value))) +
  facet_wrap(~name, scales = "free_y")
