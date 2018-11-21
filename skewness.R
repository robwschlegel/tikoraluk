# The purpose of this function is to load AVISO and NAPA data
# and to then calculate the skewness of the flow of the data based on SSH/SLA


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ncdf4)
library(lubridate)
library(doMC); doMC::registerDoMC(cores = 50)


# Files -------------------------------------------------------------------

# The NAPA to OISST lon/lat mask
load("metadata/lon_lat_NAPA_OISST.RData")

# The OISST lon values for subsetting
load("metadata/lon_OISST.RData")


# Functions ---------------------------------------------------------------

# Function for creating day, month, year means for height
height_DMY <- function(df, product){
  
  # Daily values
  df_daily <- df %>% 
    mutate(month = "daily")
  
  # Monthly values
  df_monthly <- df %>% 
    mutate(t = floor_date(t, "month")) %>% 
    group_by(nav_lon, nav_lat, t) %>% 
    summarise(height = mean(height, na.rm = T)) %>% 
    mutate(month = "monthly") %>% 
    ungroup()
  
  # Yearly values
  df_yearly <- df %>% 
    mutate(t = floor_date(t, "year")) %>% 
    group_by(nav_lon, nav_lat, t) %>% 
    summarise(height = mean(height, na.rm = T)) %>% 
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


# Calculate the skewness of SSH/SLA for a given pixel
# lon_row <- 1
skewness_AN <- function(lon_row){
  
  ### Begin
  print(paste("Began run", lon_row, "at", Sys.time()))
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  
  ### Extract NAPA height
  load(paste0("../data/NAPA_ssh_sub_",lon_row_pad,".RData"))
  NAPA_height <- height_DMY(na.omit(NAPA_ssh_sub), "NAPA")
    # na.omit() %>% 
    # mutate(month = lubridate::month(t, label = T)) %>%
    # select(nav_lon, nav_lat, t, height) %>%
    
    # mutate(product = "NAPA")
  
  ### Extract AVISO height
  load(paste0("../data/AVISO_anom_sub_",lon_row_pad,".RData"))
  AVISO_height <- AVISO_anom_sub %>% 
    filter(t >= as.Date("1993-10-01"),
           t <= as.Date("2015-12-29")) %>% 
    left_join(lon_lat_NAPA_OISST, by = c("lon" = "lon_O", "lat" = "lat_O")) %>% 
    # mutate(month = lubridate::month(t, label = T)) %>%
    dplyr::rename(height = sla) %>% 
    select(nav_lon, nav_lat, t, height) %>% 
    # mutate(product = "AVISO") %>% 
    # na.omit() %>%
    right_join(distinct(select(NAPA_ssh_sub, nav_lon, nav_lat)),
               by = c("nav_lon", "nav_lat")) %>%
    na.omit() %>%
    height_DMY(., "AVISO")
    # filter(nav_lon %in% NAPA_height$nav_lon,
    #        nav_lat %in% NAPA_height$nav_lat)
  rm(NAPA_ssh_sub)
  rm(AVISO_anom_sub)
  
  ### Combine heights
  ALL_height <- rbind(AVISO_height, NAPA_height)
  rm(AVISO_height, NAPA_height)
  # ALL_height_month <- rbind(AVISO_height, NAPA_height)
  # ALL_height_overall <- rbind(AVISO_height, NAPA_height) %>% 
  #   mutate(month = "overall")
  # suppressWarnings(
  #   ALL_height <- rbind(ALL_height_month, ALL_height_overall) %>% 
  #     mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
  #                                             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
  #                                             "overall"))) %>% 
  #     group_by(nav_lon, nav_lat, product, month)
  # )
  
  ### Calculate skewness
  skew_res <- ALL_height %>% 
    group_by(nav_lon, nav_lat, product, month) %>%
    summarise(skewness = e1071::skewness(height, na.rm = T)) %>% 
    mutate(skewness = ifelse(is.na(skewness), NA, skewness)) %>% 
    ungroup() %>% 
    na.omit()
  
  ### Calculate differences
  skew_diff_res <- left_join(filter(skew_res, product == "NAPA"), 
                             filter(skew_res, product == "AVISO"),
                             by = c("nav_lon", "nav_lat", "month")) %>%
    ungroup() %>% 
    mutate(skewness = skewness.x - skewness.y,
           product = "difference") %>% 
    select(nav_lon, nav_lat, product, month, skewness) %>% 
    rbind(skew_res)
  
  ### Finish
  print(paste("Completed run",lon_row,"at",Sys.time()))
  return(skew_diff_res)
}


# Skewness ----------------------------------------------------------------

# system.time(
#   test <- skewness_AN(1)
# ) # 33 seconds

# Re-run on Wednesday, November 21st, 2018
# AVISO_NAPA_skewness_summary <- plyr::ldply(1:1440, .fun = skewness_AN, .parallel = T)
# save(AVISO_NAPA_skewness_summary, file = "../data/AVISO_NAPA_skewness_summary.RData")

