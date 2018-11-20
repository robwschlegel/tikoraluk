# The purpose of this script is to house the functions and 
# analysis that compares the NAPA model against OISST


# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(lubridate)
library(doMC); doMC::registerDoMC(cores = 50)
library(stringr)
library(FNN)

source("MHW_prep.R")


# Meta-data ---------------------------------------------------------------

# The NAPA to OISST lon/lat mask
load("metadata/lon_lat_NAPA_OISST.RData")

# The OISST lon values for subsetting
load("metadata/lon_OISST.RData")


# Functions ---------------------------------------------------------------

# Function for creating day, month, year means for sst
# testers...
# df <- NAPA_sst
# product <- "NAPA"
sst_DMY <- function(df, product){
  
  # Daily values
  df_daily <- df %>% 
    mutate(month = "daily")
  
  # Monthly values
  df_monthly <- df %>% 
    mutate(t = floor_date(t, "month")) %>% 
    group_by(nav_lon, nav_lat, t) %>% 
    summarise(temp = mean(temp, na.rm = T)) %>% 
    mutate(month = "monthly") %>% 
    ungroup()
  
  # Yearly values
  df_yearly <- df %>% 
    mutate(t = floor_date(t, "year")) %>% 
    group_by(nav_lon, nav_lat, t) %>% 
    summarise(temp = mean(temp, na.rm = T)) %>% 
    mutate(month = "yearly") %>% 
    ungroup()
  
  # daily values by month
  df_month <- df %>% 
    mutate(month = lubridate::month(t, label = T))
  
  # All together now...
  df_ALL <- rbind(df_daily, df_monthly, df_yearly, df_month) %>% 
    mutate(product = product,
           month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
                                            "daily", "monthly", "yearly"))) %>% 
    group_by(nav_lon, nav_lat, month)
  return(df_ALL)
}


# Function for calculating simple decadal trends
dt <- function(df){
  if(nrow(na.omit(df)) == 0) return(NA)
  date_sub <- seq(as.Date("1994-01-01"), as.Date("2015-12-29"), by = "month")
  res <- df %>% 
    mutate(monthly = floor_date(t, unit = "month")) %>% 
    group_by(monthly) %>% 
    summarise_if(is.numeric, .funs = c("mean"), na.rm = T) %>%
    filter(monthly %in% date_sub) %>% 
    replace(is.na(.), 0) %>% 
    do(dt = round(as.numeric(coef(lm(temp ~ monthly, data = .))[2]) * 120, 4)) %>% 
    mutate(dt = as.numeric(dt))
  return(as.numeric(res))
}


# Function for finding mmm diff between products
sst_diff <- function(df){
  
  # Decadal trends
  sst_dt <- df %>% 
    group_by(nav_lon, nav_lat, month, product) %>% 
    nest() %>% 
    mutate(dt = map(data, dt)) %>% 
    select(-data) %>% 
    unnest()
  
  # Summary stats
  sst_sum <- df %>% 
    group_by(nav_lon, nav_lat, month, product) %>%
    summarise(min = min(temp, na.rm = T),
              quant_10 = quantile(temp, na.rm = T, probs = 0.10),
              quant_25 = quantile(temp, na.rm = T, probs = 0.25),
              quant_50 = quantile(temp, na.rm = T, probs = 0.50),
              quant_75 = quantile(temp, na.rm = T, probs = 0.75),
              quant_90 = quantile(temp, na.rm = T, probs = 0.90),
              max = max(temp, na.rm = T),
              mean = mean(temp, na.rm = T),
              sd = sd(temp, na.rm = T)) %>% 
    select(nav_lon, nav_lat, month, everything()) %>% 
    left_join(sst_dt, by = c("nav_lon", "nav_lat", "month", "product"))
  
  # Differences
  sst_sum_diff <- left_join(filter(sst_sum, product == "NAPA"), 
                           filter(sst_sum, product == "OISST"),
                           by = c("nav_lon", "nav_lat", "month")) %>%
    mutate(min = min.x - min.y,
           quant_10 = quant_10.x - quant_10.y,
           quant_25 = quant_25.x - quant_25.y,
           quant_50 = quant_50.x - quant_50.y,
           quant_75 = quant_75.x - quant_75.y,
           quant_90 = quant_90.x - quant_90.y,
           max = max.x - max.y, 
           mean = mean.x - mean.y,
           sd = sd.x - sd.y, 
           dt = dt.x - dt.y, 
           product = "difference") %>% 
    select(nav_lon, nav_lat, product, month, min:dt) %>% 
    rbind(sst_sum)
  return(sst_sum_diff)
}


# Function for creating daily decadal trend steps from a known decadal trend value
dt_steps <- function(dt_val){
  date_seq <- seq(as.Date("1994-01-01"), as.Date("2015-12-29"), by = "day")
  dt_seq <- seq(0, dt_val, length.out = length(date_seq))
  dt_res <- data.frame(t = date_seq, dt = dt_seq)
  return(dt_res)
}


# Function for finding correlations between data
sst_cor <- function(ALL_df, NAPA_df, OISST_df, dt_df){
  
  ### Prep
  dt_daily <- dt_df %>% 
    ungroup() %>% 
    filter(product != "difference") %>% 
    select(nav_lon, nav_lat, product, dt) %>% 
    group_by(nav_lon, nav_lat, product) %>% 
    do(dt_steps())
  
  ### Flat NAPA
  NAPA_flat <- MHW_clim(NAPA_df) %>% 
    select(nav_lon, nav_lat, t, temp, seas) %>% 
    filter(t >= "1994-01-01", t <= "2015-12-29") %>% 
    mutate(product = "NAPA") %>% 
    left_join(dt_daily, by = c("nav_lon", "nav_lat", "t", "product")) %>% 
    mutate(temp = temp-seas-dt) %>% 
    select(nav_lon, nav_lat, t, temp) %>% 
    sst_DMY(., "NAPA")
  
  ### Flat OISST
  OISST_flat <- MHW_clim(OISST_df) %>% 
    left_join(lon_lat_NAPA_OISST, by = c("lon" = "lon_O", "lat" = "lat_O")) %>% 
    right_join(unique(select(MHW_clim(NAPA_df), nav_lon, nav_lat)), by = c("nav_lon", "nav_lat")) %>% 
    filter(t >= "1994-01-01", t <= "2015-12-29") %>%
    mutate(product = "OISST") %>% 
    left_join(dt_daily, by = c("nav_lon", "nav_lat", "t", "product")) %>% 
    mutate(temp = temp-seas-dt) %>%
    select(nav_lon, nav_lat, t, temp) %>% 
    sst_DMY(., "OISST")
  

  ### Normal correlation - no change to data
  ALL_norm_cor <- left_join(filter(ALL_df, product == "NAPA"), 
                            filter(ALL_df, product == "OISST"),
                            by = c("nav_lon", "nav_lat", "t", "month")) %>% 
    group_by(nav_lon, nav_lat, month) %>% 
    summarise(cor_norm = cor(temp.x, temp.y, 
                             use = "pairwise.complete.obs", 
                             method = "pearson"))
  
  
  ### Flat correlation - seas. sig. and DT removed
  ALL_flat_cor <- left_join(NAPA_flat, OISST_flat,
                            by = c("nav_lon", "nav_lat", "t", "month")) %>% 
    group_by(nav_lon, nav_lat, month) %>% 
    summarise(cor_flat = cor(temp.x, temp.y, 
                             use = "pairwise.complete.obs", 
                             method = "pearson"))
  
  
  ### Finish
  ALL_cor <- left_join(ALL_norm_cor, ALL_flat_cor, by = c("nav_lon", "nav_lat", "month")) %>% 
    mutate(product = "difference")
  return(ALL_cor)
}


# Function for running the numbers on sst from NAPA and OISST
## tester...
# lon_row <- 1
sst_ON <- function(lon_row){
  print(paste("Began run", lon_row, "at", Sys.time()))
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  
  ### Load NAPA data
  load(paste0("../data/MHW.NAPA.calc.",lon_row_pad,".RData"))
  NAPA_sst <- MHW_clim(MHW_res) %>% 
    select(nav_lon, nav_lat, t, temp) %>% 
    sst_DMY(., "NAPA")
  
  ### Load OISST data
  load(paste0("../data/MHW.match.calc.",lon_row_pad,".RData"))
  OISST_sst <- MHW_clim(MHW_match_res) %>% 
    left_join(lon_lat_NAPA_OISST, by = c("lon" = "lon_O", "lat" = "lat_O")) %>% 
    right_join(unique(select(MHW_clim(MHW_res), nav_lon, nav_lat)), by = c("nav_lon", "nav_lat")) %>% 
    select(nav_lon, nav_lat, t, temp) %>% 
      sst_DMY(., "OISST")
  
  ### Combine
  ALL_sst <- rbind(NAPA_sst, OISST_sst)
  
  ### difference
  # system.time(
  ALL_sst_diff <- sst_diff(ALL_sst)
  # ) # 29 seconds
  
  ### correlation
  # system.time(
  ALL_sst_cor <- sst_cor(ALL_sst, MHW_res, MHW_match_res, ALL_sst_diff)
  # ) # 33 seconds
  rm(ALL_sst, MHW_res, MHW_match_res)
  
  ### Finish
  ALL_sst_res <- left_join(ALL_sst_diff, ALL_sst_cor, 
                           by = c("nav_lon", "nav_lat", "month", "product"))
  print(paste("Completed run",lon_row,"at",Sys.time()))
  return(ALL_sst_res)
}


# Summarise ---------------------------------------------------------------

# system.time(
#   test <- sst_ON(1)
# ) # 70 seconds

# Re-run on Friday, October 12th, 2018
system.time(
  OISST_NAPA_SST_summary_1 <- plyr::ldply(1:720, .fun = sst_ON, .parallel = T)
) # xxx seconds at 50 cores
system.time(
  OISST_NAPA_SST_summary_2 <- plyr::ldply(721:1440, .fun = sst_ON, .parallel = T)
) # xxx seconds at 50 cores
OISST_NAPA_SST_summary <- rbind(OISST_NAPA_SST_summary_1, OISST_NAPA_SST_summary_2)
rm(OISST_NAPA_SST_summary_1, OISST_NAPA_SST_summary_2)
save(OISST_NAPA_SST_summary, file = "../data/OISST_NAPA_SST_summary.RData")

