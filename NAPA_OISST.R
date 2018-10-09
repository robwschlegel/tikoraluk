# The purpose of this script is to house the functions and 
# analysis that compares the NAPA model against OISST


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(doMC); doMC::registerDoMC(cores = 50)
library(stringr)

source("MHW_prep.R")


# Meta-data ---------------------------------------------------------------

# Need to quantify the distance (KMs) between FNN pixels

# Create static lon/lat index values for ease of comparison between the products


# Data --------------------------------------------------------------------


# Functions ---------------------------------------------------------------

# Function for loading the matching OISST and NAPA lon row
## tester...
lon_row <- 1
load_OISST_NAPA <- function(lon_row){
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  load(paste0("../data/NAPA_sst_sub_",lon_row_pad,".RData"))
  load(paste0("../data/MHW.calc.",lon_row_pad,".RData"))
}

# Function for running the chosen analyses on each pixel
# Summary stats
# - Get the summary of each pixel over time for both datasets
# -- Mean, median, quartiles, 10/90th, sd, min, max, decadal trend
# - Do this for the months, too
# - Do this for the sea ice concentration value, too (ice_ts)
## tester...
OISST <- MHW_res
NAPA <- NAPA_sst_sub
summarise_OISST_NAPA <- function(OISST, NAPA){
  OISST_long <- MHW_clim(OISST)
  
  OISST_res_1 <- OISST_long %>%
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
  
  OISST_res_2 <- OISST_long %>% 
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
              sd = sd(temp, na.rm = T))
  
  OISST_res_3 <- OISST_long %>% 
    mutate(monthly = floor_date(t, unit = "month")) %>% 
    select(lon, lat, monthly, temp) %>% 
    group_by(lon, lat, monthly) %>%
    summarise(temp = mean(temp, na.rm = T)) %>% 
    ungroup() %>% 
    group_by(lon, lat) %>% 
    map2(monthly, temp, ~ {
      lm(as.formula(paste(.y, "~", paste(.x, collapse = " + "))), data = dataset) %>%
        summary() %>%
        `$`("coefficients")
    })
    
    
    summarise(dt = round(as.numeric(coef(lm(temp ~ monthly, data = .))[2]) * 120, 4))
  
  # OISST_res_4 <- OISST_long %>% 
  #   mutate(monthly = floor_date(t, unit = "month"),
  #          month = lubridate::month(t, label = T)) %>% 
  #   select(lon, lat, monthly, month, temp) %>% 
  #   group_by(lon, lat, month) %>%
  #   summarise(temp = mean(temp, na.rm = T)) %>% 
  #   ungroup() %>% 
  #   group_by(lon, lat) %>% 
  #   map2(x_var, y_var, ~ {
  #     lm(as.formula(paste(.y, "~", paste(.x, collapse = " + "))), data = dataset) %>%
  #       summary() %>%
  #       `$`("coefficients")
  #   })
  #   
  #   nest() %>% 
  #   mutate(dt = map(data, lm, formula = temp ~ month, data = .))
  #   summarise(dt = round(as.numeric(coef(lm(temp ~ month, data = .))[2]) * 120, 4))
    
test <- OISST_overall_trend %>% 
  filter(lon == 0.125, lat == 38.125) %>% 
  summarise(dt = round(as.numeric(coef(lm(temp ~ month, data = .))[2]) * 120, 4))
test2 <- round(as.numeric(coef(lm(temp ~ month, data = test))[2]) * 120, 3)
round(as.numeric(coef(model)[2]) * 120, 3)
  }


# Analysis ----------------------------------------------------------------


