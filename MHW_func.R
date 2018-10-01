# The purpose of this script is to provide one central script
# that can be source() called to put all of the necessary 
# functions for loading OISST data and calculating MHWs from them.

# The script 'MHW_calc.R' is where the code that iteratively 
# calculates MHWs from the .mat files is kept.


# Load libraries ----------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(heatwaveR, lib.loc = "~/R-packages")
if(packageVersion("heatwaveR") != "0.3.4"){
  devtools::install_github("robwschlegel/heatwaveR")
}


# Source scripts ----------------------------------------------------------

source("load_mat.R")


# MHW functions -----------------------------------------------------------

# Function for calculating clims only per pixel
clim_only_nested <- function(df){
  res <- df %>% 
    group_by(lon, lat) %>%
    nest() %>% 
    mutate(clim_only = map(data, ts2clm, clmOnly = TRUE, robust = FALSE,
                       climatologyPeriod = c("1982-01-01", "2011-12-31"))) %>% 
    select(-data) %>%
    unnest()
}

# Run MHW algorithm on nested data
detect_event_event <- function(df){
  res <- detect_event(ts2clm(df, climatologyPeriod = c("1982-01-01", "2011-12-31")))
}

detect_event_nested <- function(df){
  res <- df %>% 
    na.omit() %>% 
    group_by(lon, lat) %>% 
    dplyr::rename(t = date) %>% 
    nest() %>% 
    mutate(event = map(data, detect_event_event)) %>% 
    select(-data) %>%
    unnest()
}

MHW_slice <- function(run = seq(1, 2, 1)){
  file_list_sub <- file_list %>% 
    slice(run) %>% 
    mutate(x = file_num) %>% 
    nest(-x) %>% 
    mutate(res = map(data, MHW_calc))
}

for(i in 1:2){
  file_list_sub <- file_list[i,]
  OISST <- load_OISST_mat(file_list_sub)
  MHW_res <- OISST %>%
    group_by(lon, lat) %>%
    nest() %>% 
    mutate(clim = map(data, ts2clm, robust = FALSE,
                      climatologyPeriod = c("1982-01-01", "2011-12-31")),
           event = map(clim, detect_event),
           cat = map(event, category, climatology = TRUE)) %>% 
    select(-data)
}


test <- MHW_slice(run = c(1:2))
test <- MHW_calc(test)

MHW_save <- function(){
  save(df, file = paste0("../data/MHW.calc.", df$file,".RData"))
}

# Example -----------------------------------------------------------------


# Load file 17... just for fun
file_list_sub <- file_list[17:18,]

system.time(
OISST <- file_list_sub %>%
  nest() %>%
  mutate(sst = map(data, load_OISST_mat)) %>%
  select(-data) %>%
  unnest()
) # ~11 seconds

# Calculate the climatologies only

system.time(
  MHW_res <- OISST %>%
    group_by(lon, lat) %>%
    nest() %>% 
    mutate(clim = map(data, ts2clm, robust = FALSE,
                      climatologyPeriod = c("1982-01-01", "2011-12-31")),
           event = map(clim, detect_event),
           cat = map(event, category, climatology = TRUE)) %>% 
    select(-data) #%>%
    # unnest()
) # 93 seconds
