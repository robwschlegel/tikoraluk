# The purpose of this script is to house the functions and 
# analysis that compares the NAPA model against OISST


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(doMC); doMC::registerDoMC(cores = 50)
library(stringr)
library(FNN)

source("MHW_prep.R")


# Meta-data ---------------------------------------------------------------

# Need to quantify the distance (KMs) between FNN pixels

# Create static lon/lat index values for ease of comparison between the products


# Data --------------------------------------------------------------------


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
## tester...
# OISST <- MHW_res
# NAPA <- NAPA_sst_sub
summarise_OISST_NAPA <- function(df){
  # Run summary calculations
  ALL_res_1 <- df %>%
    select(lon, lat, temp) %>% 
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
    mutate(monthly = floor_date(t, unit = "month")) %>% 
    select(lon, lat, monthly, temp) %>% 
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
  # Analyse
  OISST_NAPA_summary <- summarise_OISST_NAPA(ALL_long)
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  save(OISST_NAPA_summary, file = paste0("../data/OISST_NAPA_summary_",lon_row_pad,".RData"))
}


# Analysis ----------------------------------------------------------------

lon_row_multi <- data.frame(lon_row = 1:1440,
                            x = 1:1440)


# Run on Wednesday, October 10th, 2018
# system.time(
#   plyr::ddply(lon_row_multi[1,], .variables = "x",
#               .fun = analyse_OISST_NAPA, .parallel = T)
# ) # 61 seconds at 50 cores
# system.time(
#   plyr::ddply(lon_row_multi[2:1440,], .variables = "x", 
#               .fun = analyse_OISST_NAPA, .parallel = T)
# ) # 2073 seconds at 50 cores



# Difference --------------------------------------------------------------

# Meta-data
load("metadata/lon_lat_NAPA_OISST.RData")
load("metadata/lon_OISST.RData")

# Load all of the data run above
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
    rename_all(~str_replace_all(., ".x", "_NAPA")) %>%
    rename_all(~str_replace_all(., ".y", "_OISST")) %>% 
    rename_all(~str_replace_all(., ".z", "_diff"))
  return(data_sub)
}

system.time(
  diff_res <- plyr::ldply(OISST_NAPA_saves, .parallel = T,
                          .fun = load_sub_diff_ON)
) # 13 seconds at 50 cores


# Visualise ---------------------------------------------------------------

ggplot(data = filter(diff_res, month == "overall"), 
       aes(x = nav_lon, y = nav_lat, colour = quant_50_NAPA)) +
  geom_point(size = 0.001) +
  scale_colour_viridis_c() +
  # coord_polar() +
  labs(x = "", y = "")

ggplot(data = filter(diff_res, month == "overall"), 
       aes(x = lon_O, y = lat_O, fill = quant_50_OISST)) +
  geom_raster() +
  scale_fill_viridis_c() +
  labs(x = "", y = "") 

ggplot(data = filter(diff_res, month == "overall"), 
       aes(x = nav_lon, y = nav_lat, colour = quant_50_diff)) +
  geom_point(size = 0.001) +
  scale_colour_gradient2() +
  # coord_polar() +
  labs(x = "", y = "") 
