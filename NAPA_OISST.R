# The purpose of this script is to house the functions and 
# analysis that compares the NAPA model against OISST


# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(lubridate)
library(doMC); doMC::registerDoMC(cores = 50)
library(stringr)
library(FNN)
library(ggpubr)
library(gridExtra)

source("MHW_prep.R")


# Meta-data ---------------------------------------------------------------

load("metadata/lon_lat_NAPA_OISST.RData")
load("metadata/lon_OISST.RData")


# Functions ---------------------------------------------------------------

# Function for loading the matching OISST and NAPA lon row
## tester...
# lon_row <- 1
load_OISST_NAPA <- function(lon_row){
  # Load data
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  load(paste0("../data/NAPA_sst_sub_",lon_row_pad,".RData"))
  load(paste0("../data/MHW.calc.",lon_row_pad,".RData"))
  
  # Prep for use
  OISST_long <- MHW_clim(MHW_res) %>% 
    select(lon, lat, t, temp)
  NAPA_long <- NAPA_sst_sub %>% 
    dplyr::rename(lon = nav_lon,
                  lat = nav_lat) %>% 
    select(lon, lat, t, temp)
  ALL_long <- rbind(OISST_long, NAPA_long)
  return(ALL_long)
}

# Function for running the chosen analyses on each pixel
# Summary stats
# - Get the summary of each pixel over time for both datasets
# -- Mean, median, quartiles, 10/90th, sd, min, max, decadal trend
# - Do this for the months, too
# - Do this for the sea ice concentration value, too (ice_ts)
summarise_OISST_NAPA <- function(df){
  # Run summary calculations
  ALL_res_1 <- df %>%
    na.omit() %>% 
    filter(temp != 0) %>% 
    # Constrain the OISST date range to match NAPA
    # ensuring the same period for calculations
    filter(t >= as.Date("1993-10-01"),
           t <= as.Date("2015-12-29")) %>% 
    # select(lon, lat, temp) %>% 
    group_by(lon, lat) %>% 
    summarise(min = min(temp, na.rm = T),
              quant_10 = quantile(temp, na.rm = T, probs = 0.10),
              quant_25 = quantile(temp, na.rm = T, probs = 0.25),
              quant_50 = quantile(temp, na.rm = T, probs = 0.50),
              quant_75 = quantile(temp, na.rm = T, probs = 0.75),
              quant_90 = quantile(temp, na.rm = T, probs = 0.90),
              max = max(temp, na.rm = T),
              mean = mean(temp, na.rm = T),
              sd = sd(temp, na.rm = T)) %>% 
    mutate(month = "overall") %>% 
    select(lon, lat, month, everything())
  
  ALL_res_2 <- df %>% 
    na.omit() %>% 
    filter(temp != 0) %>% 
    mutate(month = lubridate::month(t, label = T)) %>% 
    group_by(lon, lat, month) %>% 
    summarise(min = min(temp, na.rm = T),
              quant_10 = quantile(temp, na.rm = T, probs = 0.10),
              quant_25 = quantile(temp, na.rm = T, probs = 0.25),
              quant_50 = quantile(temp, na.rm = T, probs = 0.50),
              quant_75 = quantile(temp, na.rm = T, probs = 0.75),
              quant_90 = quantile(temp, na.rm = T, probs = 0.90),
              max = max(temp, na.rm = T),
              mean = mean(temp, na.rm = T),
              sd = sd(temp, na.rm = T)) %>% 
    select(lon, lat, month, everything())
  
  # Create monthly values and run linear models
  ALL_monthly <- df %>% 
    na.omit() %>% 
    filter(temp != 0) %>% 
    mutate(monthly = floor_date(t, unit = "month")) %>% 
    # select(lon, lat, monthly, temp) %>% 
    group_by(lon, lat, monthly) %>%
    summarise(temp = mean(temp, na.rm = T)) %>% 
    ungroup() %>% 
    na.omit()
  
  suppressWarnings(
  ALL_res_3 <- ALL_monthly %>% 
    group_by(lon, lat) %>% 
    do(dt = round(as.numeric(coef(lm(temp ~ monthly, data = .))[2]) * 120, 4)) %>% 
    mutate(dt = as.numeric(dt)) %>% 
    mutate(month = "overall") %>% 
    select(lon, lat, month, everything())
  )
  
  suppressWarnings(
  ALL_res_4 <- ALL_monthly %>% 
    mutate(month = lubridate::month(monthly, label = T)) %>% 
    group_by(lon, lat, month) %>% 
    do(dt = round(as.numeric(coef(lm(temp ~ monthly, data = .))[2]) * 120, 4)) %>% 
    mutate(dt = as.numeric(dt))
  )
  
  # Combine and exit
  suppressWarnings(
    ALL_res_5 <- rbind(ALL_res_1, ALL_res_2)
    )
  suppressWarnings(
    ALL_res_6 <- rbind(ALL_res_3, ALL_res_4)
    )
  suppressWarnings(
    ALL_res <- left_join(ALL_res_5, ALL_res_6, by = c("lon", "lat", "month"))
    )
  return(ALL_res)
}

# Function for loading, analysing, and saving the summary data
analyse_OISST_NAPA <- function(df){
  # Load
  lon_row <- df$lon_row
  ALL_long <- load_OISST_NAPA(lon_row)
  # Summarise
  OISST_NAPA_summary <- summarise_OISST_NAPA(ALL_long)
  # Save
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  save(OISST_NAPA_summary, file = paste0("../data/OISST_NAPA_summary_",lon_row_pad,".RData"))
}

correlate_OISST_NAPA <- function(df){
  # Load
  lon_row <- df$lon_row
  ALL_trim <- load_OISST_NAPA(lon_row) %>%
    # Constrain the OISST date range to match NAPA
    # ensuring the same period for calculations
    filter(t >= as.Date("1993-10-01"),
           t <= as.Date("2015-12-29"))
  # Match up
  ALL_match <- inner_join(lon_lat_NAPA_OISST, ALL_trim,
                          by = c("nav_lon" = "lon", "nav_lat" = "lat")) %>%
    left_join(ALL_trim, by = c("lon_O" = "lon", "lat_O" = "lat", "t")) %>% 
    mutate(time = "day") %>% 
    filter(temp.x != 0) %>% 
    na.omit()
  # Correlate
  cor_day <- ALL_match %>% 
    mutate(time = "day") %>% 
    group_by(lon, lat, time) %>% 
    summarise(cor = cor(temp.x, temp.y, 
                        use = "pairwise.complete.obs", 
                        method = "pearson"))
  cor_monthly <- ALL_match %>% 
    mutate(time = as.character(lubridate::month(t, label = T))) %>% 
    group_by(lon, lat, time) %>% 
    summarise(cor = cor(temp.x, temp.y, 
                        use = "pairwise.complete.obs", 
                        method = "pearson"))
  cor_month <- ALL_match %>% 
    mutate(t = floor_date(t, "month"),
           time = "month") %>% 
    group_by(lon, lat, t, time) %>% 
    summarise(temp.x = mean(temp.x, na.rm = T),
              temp.y = mean(temp.y, na.rm = T)) %>% 
    group_by(lon, lat, time) %>% 
    summarise(cor = cor(temp.x, temp.y,
                        use = "pairwise.complete.obs", 
                        method = "pearson"))
  cor_year <- ALL_match %>% 
    mutate(t = floor_date(t, "year")) %>% 
    group_by(lon, lat, t, time) %>% 
    summarise(temp.x = mean(temp.x, na.rm = T),
              temp.y = mean(temp.y, na.rm = T)) %>% 
    group_by(lon, lat, time) %>% 
    summarise(cor = cor(temp.x, temp.y, 
                        use = "pairwise.complete.obs", 
                        method = "pearson"))
  cor_all <- rbind(cor_day, cor_month, cor_year, cor_monthly) %>% 
    mutate(time = factor(time, levels = c("day", "month", "year", 
                                          "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
  return(cor_all)
}


# Summarise ---------------------------------------------------------------

lon_row_multi <- data.frame(lon_row = 1:1440,
                            x = 1:1440)

# Re-run on Friday, October 12th, 2018
# system.time(
#   plyr::ddply(lon_row_multi, .variables = "x",
#               .fun = analyse_OISST_NAPA, .parallel = T)
# ) # 1969 seconds at 50 cores

# Difference --------------------------------------------------------------

# Load all of the summary data run above
OISST_NAPA_saves <- dir("../data", pattern = "OISST_NAPA", full.names = T)

load_sub_diff_ON <- function(file_name){
  load(file = file_name)
  data_sub <- inner_join(lon_lat_NAPA_OISST, OISST_NAPA_summary,
                         by = c("nav_lon" = "lon", "nav_lat" = "lat")) %>%
    left_join(OISST_NAPA_summary, by = c("lon_O" = "lon", "lat_O" = "lat", "month")) %>% 
    mutate(min.z = min.x - min.y,
           quant_10.z = quant_10.x - quant_10.y,
           quant_25.z = quant_25.x - quant_25.y,
           quant_50.z = quant_50.x - quant_50.y,
           quant_75.z = quant_75.x - quant_75.y,
           quant_90.z = quant_90.x - quant_90.y,
           max.z = max.x - max.y,
           mean.z = mean.x - mean.y,
           sd.z = sd.x - sd.y,
           dt.z = dt.x - dt.y) %>% 
    rename_all(~str_replace_all(., "max", "banana")) %>%
    rename_all(~str_replace_all(., ".x", "_NAPA")) %>%
    rename_all(~str_replace_all(., ".y", "_OISST")) %>% 
    rename_all(~str_replace_all(., ".z", "_diff")) %>% 
    rename_all(~str_replace_all(., "banana", "max"))
  return(data_sub)
}

# system.time(
#   OISST_NAPA_difference <- plyr::ldply(OISST_NAPA_saves, .parallel = T,
#                                        .fun = load_sub_diff_ON)
# ) # 15 seconds at 50 cores
# save(OISST_NAPA_difference, file = "../data/OISST_NAPA_difference.RData")


# Correlation -------------------------------------------------------------

lon_row_multi <- data.frame(lon_row = 1:1440,
                            x = 1:1440)

# system.time(
#   OISST_NAPA_correlation_1 <- plyr::ddply(lon_row_multi[1:720, ], .variables = "x",
#                                         .fun = correlate_OISST_NAPA, .parallel = T)
# ) # 516 seconds at 50 cores
# system.time(
#   OISST_NAPA_correlation_2 <- plyr::ddply(lon_row_multi[721:1440, ], .variables = "x",
#                                           .fun = correlate_OISST_NAPA, .parallel = T)
# ) # 485 seconds at 50 cores
# OISST_NAPA_correlation <- rbind(OISST_NAPA_correlation_1, OISST_NAPA_correlation_2) %>% 
#   select(-x)
# save(OISST_NAPA_correlation, file = "../data/OISST_NAPA_correlation.RData")

