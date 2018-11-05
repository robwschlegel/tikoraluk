# The purpose of this script is to compare the MHW results
# From the OISST and NAPA data


# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(heatwaveR)
if(packageVersion("heatwaveR") != "0.3.5"){
  devtools::install_github("robwschlegel/heatwaveR")
  library(heatwaveR)
}
library(doMC); doMC::registerDoMC(cores = 50)

source("MHW_func.R")
rm(file_list, NAPA_RData, OISST_RData)
options(scipen=999)


# Data --------------------------------------------------------------------

MHW_NAPA <- dir("../data", pattern = "MHW.NAPA.", full.names = T)

MHW_OISST <- dir("../data", pattern = "MHW.match.", full.names = T)


# Functions ---------------------------------------------------------------

# Function for calculating t-tests on multiple columns
# df <- ALL_clim %>%
#   ungroup() %>%
#   filter(nav_lon == ALL_clim$nav_lon[1],
#          nav_lat == ALL_clim$nav_lat[1]) %>%
#   select(-doy, -nav_lon, - nav_lat) %>%
#   mutate(product = as.factor(product))
# df <- ALL_event %>%
#   ungroup() %>%
#   filter(nav_lon == ALL_event$nav_lon[1],
#          nav_lat == ALL_event$nav_lat[1],
#          month == "Jan") %>%
#   select(-date_peak, -nav_lon, - nav_lat, -month) %>%
#   mutate(product = as.factor(product))
# test <- plyr::ddply(ALL_clim, .variables = c("nav_lon", "nav_lat", "month"), .fun = t_test_p)
# test <- ALL_event %>% 
#   select(-date_peak) %>%
#   nest() %>% 
#   slice(292) %>% 
#   unnest()
#   # mutate(t_tests = map(data, t_test_p))
# df <- test %>% 
#   select(-(nav_lon:month))
t_test_p <- function(df){
  NAPA_row <- nrow(filter(df, product == "NAPA"))
  NAPA_unique <- nrow(unique(filter(df, product == "NAPA")[, 1]))
  OISST_row <- nrow(filter(df, product == "OISST"))
  OISST_unique <- nrow(unique(filter(df, product == "OISST")[, 1]))
  if(NAPA_row >= 3 & OISST_row >= 3 & NAPA_unique >= 3 & OISST_unique >= 3){
    t_tests <- df[ , -grep("product", names(df))] %>%
      map(~ t.test(.x ~ df$product)) %>% 
      map_dfr(~ broom::tidy(.), .id = 'metric') %>%
      mutate(p.value = round(p.value, 4),
             test = "t-test") %>%
      select(test, metric, p.value) %>% 
      spread(key = "metric", value = "p.value")
  } else {
    t_tests <- data.frame(test = "t-test", y = NA, z = NA, stringsAsFactors = F)
    colnames(t_tests) <- c("test", names(df)[1:2])
  }
  return(t_tests)
}

# Function for caluclating Kolmogorov Smirnof tests
ks_test_p <- function(df){
  df_1 <- df %>% 
    filter(product == "OISST")
  df_2 <- df %>% 
    filter(product == "NAPA")
  suppressWarnings(
  res <- data.frame(test = "KS-test",
                    seas = round(ks.test(df_1$seas, df_2$seas)$p.value, 4),
                    thresh = round(ks.test(df_1$thresh, df_2$thresh)$p.value, 4), 
                    stringsAsFactors = F)
  )
  return(res)
}

# Function for calculating mmm differences for seas and thresh
## tester...
# df <- ALL_clim
clim_diff <- function(df){
  clim_mmm <- df %>% 
    group_by(nav_lon, nav_lat, month, product) %>% 
    select(-doy) %>%
    summarise_if(is.numeric, .funs = c("min", "mean", "max"), na.rm = T)
  clim_mmm_dif <- left_join(filter(clim_mmm, product == "NAPA"), 
                            filter(clim_mmm, product == "OISST"),
                            by = c("nav_lon", "nav_lat", "month")) %>% 
    mutate(seas_min = seas_min.x - seas_min.y,
           seas_mean = seas_mean.x - seas_mean.y,
           seas_max = seas_max.x - seas_max.y, 
           thresh_min = thresh_min.x - thresh_min.y,
           thresh_mean = thresh_mean.x - thresh_mean.y,
           thresh_max = thresh_max.x - thresh_max.y,
           product = "difference") %>% 
    select(nav_lon, nav_lat, product, month, seas_min:thresh_max) %>% 
    rbind(clim_mmm)
  return(clim_mmm_dif)
}

# Function for calculating mmm differences for duration and max intensity
## tester...
# df <- ALL_event
event_diff <- function(df){
  event_mmm <- df %>% 
    group_by(nav_lon, nav_lat, month, product) %>% 
    summarise_if(is.numeric, .funs = c("min", "mean", "max"), na.rm = T)
  event_mmm_dif <- left_join(filter(event_mmm, product == "NAPA"),
                             filter(event_mmm, product == "OISST"),
                             by = c("nav_lon", "nav_lat", "month")) %>% 
    mutate(duration_min = duration_min.x - duration_min.y,
           duration_mean = duration_mean.x - duration_mean.y,
           duration_max = duration_max.x - duration_max.y, 
           intensity_max_min = intensity_max_min.x - intensity_max_min.y,
           intensity_max_mean = intensity_max_mean.x - intensity_max_mean.y,
           intensity_max_max = intensity_max_max.x - intensity_max_max.y,
           product = "difference") %>% 
    select(nav_lon, nav_lat, product, month, duration_min:intensity_max_max) %>% 
    rbind(event_mmm)
  return(event_mmm_dif)
}

# Function for loading the matching OISST and NAPA lon row
# Then making all of the necessary calculations
## tester...
# lon_row <- 163
MHW_ON <- function(lon_row){
  print(paste("Began run", lon_row, "at", Sys.time()))
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  
  if(file.exists(paste0("../data/OISST_NAPA_MHW_summary_",lon_row_pad,".RData"))){
    print(paste0("OISST_NAPA_MHW_summary_",lon_row_pad,
                 ".RData already exists. Moving on to next file."))
  } else {
    ### Load data
    load(paste0("../data/MHW.NAPA.calc.",lon_row_pad,".RData"))
    load(paste0("../data/MHW.match.calc.",lon_row_pad,".RData"))
    
    
    ### Climatology comparisons
    
    ## Extract clims
    NAPA_clim <- MHW_clim(MHW_res) %>% 
      mutate(month = lubridate::month(t, label = T)) %>%
      select(nav_lon, nav_lat, month, doy, seas, thresh) %>%
      mutate(product = "NAPA") %>% 
      group_by(nav_lon, nav_lat, month) %>% 
      distinct()
    OISST_clim <- MHW_clim(MHW_match_res) %>% 
      left_join(lon_lat_NAPA_OISST, by = c("lon" = "lon_O", "lat" = "lat_O")) %>% 
      mutate(month = lubridate::month(t, label = T)) %>% 
      select(nav_lon, nav_lat, month, doy, seas, thresh) %>% 
      mutate(product = "OISST") %>% 
      group_by(nav_lon, nav_lat, month) %>% 
      distinct() %>% 
      filter(nav_lon %in% NAPA_clim$nav_lon,
             nav_lat %in% NAPA_clim$nav_lat)
    
    ## Combine clims
    ALL_clim_month <- rbind(OISST_clim, NAPA_clim)
    ALL_clim_overall <- rbind(OISST_clim, NAPA_clim) %>% 
      ungroup() %>% 
      mutate(month = "overall") %>% 
      group_by(nav_lon, nav_lat, month)
    suppressWarnings(
      ALL_clim <- rbind(ALL_clim_month, ALL_clim_overall) %>% 
        ungroup() %>% 
        mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
                                                "overall"))) %>% 
        group_by(nav_lon, nav_lat, month)
    )
    
    ## Min/mean/max clim values
    ALL_clim_mmm <- clim_diff(ALL_clim)
    
    ## clim t-tests and KS-tests
    ALL_clim_test <- ALL_clim %>% 
      # group_by(nav_lon, nav_lat, month) %>% 
      select(-doy) %>% 
      nest() %>% 
      mutate(t_tests = map(data, t_test_p),
             ks_tests = map(data, ks_test_p)) %>% 
      select(-data)
    ALL_clim_p <- rbind(unnest(ALL_clim_test, ALL_clim_test$t_tests),
                        unnest(ALL_clim_test, ALL_clim_test$ks_tests)) %>% 
      select(nav_lon, nav_lat, month, test, seas, thresh)
    
    
    ### Event comparisons
    
    ## Extract events
    NAPA_event <- MHW_event(MHW_res) %>% 
      mutate(month = lubridate::month(date_peak, label = T)) %>%
      select(nav_lon, nav_lat, month, date_peak, duration, intensity_max) %>%
      mutate(product = "NAPA") %>% 
      group_by(nav_lon, nav_lat, month)
    OISST_event <- MHW_event(MHW_match_res) %>% 
      left_join(lon_lat_NAPA_OISST, by = c("lon" = "lon_O", "lat" = "lat_O")) %>% 
      mutate(month = lubridate::month(date_peak, label = T)) %>% 
      select(nav_lon, nav_lat, month, date_peak, duration, intensity_max) %>% 
      mutate(product = "OISST") %>% 
      group_by(nav_lon, nav_lat, month) %>% 
      filter(nav_lon %in% NAPA_event$nav_lon,
             nav_lat %in% NAPA_event$nav_lat)
    
    ## Combine events
    ALL_event_month <- rbind(OISST_event, NAPA_event)
    ALL_event_overall <- rbind(OISST_event, NAPA_event) %>% 
      ungroup() %>% 
      mutate(month = "overall") %>% 
      group_by(nav_lon, nav_lat, month)
    suppressWarnings(
      ALL_event <- rbind(ALL_event_month, ALL_event_overall) %>% 
        ungroup() %>% 
        mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
                                                "overall"))) %>% 
        group_by(nav_lon, nav_lat, month)
    )
    
    ## Count events per lon/lat/month
    ALL_event_count <- ALL_event %>% 
      group_by(nav_lon, nav_lat, month,product) %>% 
      summarise(count = n())
    
    ## Min/Mean/Max differences in metrics
    # Max intensity and duration
    ALL_event_mmm <- event_diff(ALL_event)
    
    ## t-test for primary two metrics
    ALL_event_p <- ALL_event %>% 
      select(-date_peak) %>%
      nest() %>% 
      mutate(t_tests = map(data, t_test_p)) %>% 
      select(-data) %>% 
      unnest() %>% 
      select(nav_lon, nav_lat, month, test, duration, intensity_max)
    
    ## Extract categories
    # OISST_cat <- MHW_cat_event(MHW_match_res)
    # NAPA_cat <- MHW_cat_event(MHW_res)
    
    ## Combine categories
    
    ## Test categories
    
    ## Combine and save
    ALL_res <- list(ALL_clim_mmm, ALL_clim_p, 
                    ALL_event_count, ALL_event_mmm, ALL_event_p)
    save(ALL_res, file = paste0("../data/OISST_NAPA_MHW_summary_",lon_row_pad,".RData"))
  }
  
  # return(ALL_res)
  print(paste("Completed run",lon_row,"at",Sys.time()))
}


# Comparisons -------------------------------------------------------------

# system.time(
#   plyr::ldply(1, .fun = MHW_ON, .parallel = T)
# ) # 71 seconds at 50 cores

# Run on Monday, November 5th, 2018
system.time(
  plyr::ldply(seq(1:1440), .fun = MHW_ON, .parallel = T)
) # 1895 seconds at 50 cores

# Task 163 failed:
# "Evaluation error: data are essentially constant."
# MHW_ON(163)
# 18 seconds at 50 cores