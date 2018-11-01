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
t_test_p <- function(df){
  t_tests <- df[ , -grep("product", names(df))] %>%
    map(~ t.test(.x ~ df$product)) %>% 
    map_dfr(~ broom::tidy(.), .id = 'metric') %>%
    mutate(p.value = round(p.value, 4),
           test = "t-test") %>%
    select(test, metric, p.value) %>% 
    spread(key = "metric", value = "p.value")
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
clim_diff <- function(df){
  clim_mmm <- df %>% 
    group_by(nav_lon, nav_lat, product) %>% 
    select(-doy) %>% 
    summarise_if(is.numeric, .funs = c("min", "mean", "max"), na.rm = T)
  clim_mmm_dif <- left_join(filter(clim_mmm, product == "NAPA"), 
                            filter(clim_mmm, product == "OISST"),
                            by = c("nav_lon", "nav_lat")) %>% 
    mutate(seas_min = seas_min.x - seas_min.y,
           seas_mean = seas_mean.x - seas_mean.y,
           seas_max = seas_max.x - seas_max.y, 
           thresh_min = thresh_min.x - thresh_min.y,
           thresh_mean = thresh_mean.x - thresh_mean.y,
           thresh_max = thresh_max.x - thresh_max.y,
           product = "difference") %>% 
    select(nav_lon, nav_lat, product, seas_min, seas_mean, seas_max, thresh_min, thresh_mean, thresh_max) %>% 
    rbind(clim_mmm)
  return(clim_mmm_dif)
}


# Function for loading the matching OISST and NAPA lon row
# Then making all of the necessary calculations
## tester...
# lon_row <- 1
MHW_ON <- function(lon_row){
  # Load data
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  load(paste0("../data/MHW.NAPA.calc.",lon_row_pad,".RData"))
  load(paste0("../data/MHW.match.calc.",lon_row_pad,".RData"))
  
  
  ### Climatology comparisons
  
  # Extract clims
  NAPA_clim <- MHW_clim(MHW_res) %>% 
    select(nav_lon, nav_lat, doy, seas, thresh) %>%
    mutate(product = "NAPA") %>% 
    group_by(nav_lon, nav_lat) %>% 
    distinct()
  # lon_lat_index <- right_join(lon_lat_NAPA_OISST, NAPA_clim,
  #                             by = c("nav_lon", "nav_lat")) %>% 
  #   select("nav_lon", "nav_lat")
  OISST_clim <- MHW_clim(MHW_match_res) %>% 
    left_join(lon_lat_NAPA_OISST, by = c("lon" = "lon_O", "lat" = "lat_O")) %>% 
    mutate(lubridate::month(t, label = T)) %>% 
    select(nav_lon, nav_lat, doy, seas, thresh) %>% 
    mutate(product = "OISST") %>% 
    group_by(nav_lon, nav_lat) %>% 
    distinct() %>% 
    filter(nav_lon %in% NAPA_clim$nav_lon,
           nav_lat %in% NAPA_clim$nav_lat)
  ALL_clim <- rbind(OISST_clim, NAPA_clim)
  
  # Min/mean/max clim values
  ALL_clim_mmm <- clim_diff(ALL_clim)
  ALL_clim_month_mmm <- ALL_clim %>% 
    mutate(month = lubridate::month())
  
  ALL_clim_mmm <- ALL_clim %>% 
    group_by(nav_lon, nav_lat, product) %>% 
    select(-doy) %>% 
    summarise_if(is.numeric, .funs = c("min", "mean", "max"), na.rm = T)
  ALL_clim_mmm_dif <- left_join(filter(ALL_clim_mmm, product == "NAPA"), 
                                 filter(ALL_clim_mmm, product == "OISST"),
                                 by = c("nav_lon", "nav_lat")) %>% 
    mutate(seas_min = seas_min.x - seas_min.y,
           seas_mean = seas_mean.x - seas_mean.y,
           seas_max = seas_max.x - seas_max.y, 
           thresh_min = thresh_min.x - thresh_min.y,
           thresh_mean = thresh_mean.x - thresh_mean.y,
           thresh_max = thresh_max.x - thresh_max.y,
           product = "difference") %>% 
    select(nav_lon, nav_lat, product, seas_min, seas_mean, seas_max, thresh_min, thresh_mean, thresh_max) %>% 
    rbind(ALL_clim_mmm)
  
  # clim t-tests and KS-tests
  ALL_clim_test <- ALL_clim %>% 
    group_by(nav_lon, nav_lat) %>% 
    select(-doy) %>% 
    nest() %>% 
    mutate(t_tests = map(data, t_test_p),
           ks_tests = map(data, ks_test_p)) %>% 
    select(-data)
  ALL_clim_p <- rbind(unnest(ALL_clim_test, ALL_clim_test$t_tests),
                         unnest(ALL_clim_test, ALL_clim_test$ks_tests)) %>% 
    select(nav_lon, nav_lat, test, seas, thresh)
  
  
  
  
  ### Event comparisons
  
  # Extract event stats
  OISST_event <- MHW_event(MHW_match_res)
  NAPA_event <- MHW_event(MHW_res)
  
  # Count events per lon/lat
  OISST_count <- OISST_event %>% 
    group_by(lon, lat) %>% 
    summarise(count = n())
  NAPA_count <- NAPA_event %>% 
    group_by(nav_lon, nav_lat) %>% 
    summarise(count = n())
  
  # t-test for primary four metrics
  
  
  # Min/Mean/Max differences in metrics
  # Max intensity and duration
  
  
  # Extract category stats
  OISST_cat <- MHW_cat_event(MHW_match_res)
  NAPA_cat <- MHW_cat_event(MHW_res)


  # Then do this for all month groupings
  
  
  
  
  ALL_res <- list(ALL_clim_mmm_dif, ALL_clim_p)
  return(ALL_res)
}




# Function for running comparison stats

# Events
## Count
## ANOVA for primary four metrics
## Difference in start/peak/end dates
### Base this on FNN for NAPA peak date

# Categories
## Same category?
## i_max difference
## proportion differences (4)


# Comparisons -------------------------------------------------------------


