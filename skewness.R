# The purpose of this function is to load AVISO and NAPA data
# and to then calculate the skewness of the flow of the data based on SSH/SLA


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ncdf4)
library(doMC); doMC::registerDoMC(cores = 50)


# Files -------------------------------------------------------------------

# AVISO_files <- dir(path = "../data", pattern = "AVISO_anom", full.names = T)

# NAPA_files <- dir(path = "../data", pattern = "NAPA_ssh", full.names = T)

# The NAPA to OISST lon/lat mask
load("metadata/lon_lat_NAPA_OISST.RData")

# The OISST lon values for subsetting
# load("metadata/lon_OISST.RData")


# Functions ---------------------------------------------------------------

# Calculate the skewness of SSH/SLA for a given pixel
# lon_row <- 1
skewness_AN <- function(lon_row){
  
  ## Begin
  print(paste("Began run", lon_row, "at", Sys.time()))
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  
  ### Load data
  load(paste0("../data/AVISO_anom_sub_",lon_row_pad,".RData"))
  load(paste0("../data/NAPA_ssh_sub_",lon_row_pad,".RData"))
  
  ## Extract height
  NAPA_height <- NAPA_ssh_sub %>% 
    na.omit() %>% 
    mutate(month = lubridate::month(t, label = T)) %>%
    select(nav_lon, nav_lat, month, height) %>%
    mutate(product = "NAPA")
  AVISO_height <- AVISO_anom_sub %>% 
    # na.omit() %>%
    filter(t >= as.Date("1993-10-01"),
           t <= as.Date("2015-12-29")) %>% 
    left_join(lon_lat_NAPA_OISST, by = c("lon" = "lon_O", "lat" = "lat_O")) %>% 
    mutate(month = lubridate::month(t, label = T)) %>%
    select(nav_lon, nav_lat, month, sla) %>% 
    mutate(product = "AVISO") %>% 
    dplyr::rename(height = sla) %>% 
    # na.omit() %>%
    right_join(unique(select(NAPA_height, nav_lon, nav_lat)), by = c("nav_lon", "nav_lat"))
    # filter(nav_lon %in% NAPA_height$nav_lon,
    #        nav_lat %in% NAPA_height$nav_lat)
  
  ## Combine heights
  ALL_height_month <- rbind(AVISO_height, NAPA_height)
  ALL_height_overall <- rbind(AVISO_height, NAPA_height) %>% 
    mutate(month = "overall")
  suppressWarnings(
    ALL_height <- rbind(ALL_height_month, ALL_height_overall) %>% 
      mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
                                              "overall"))) %>% 
      group_by(nav_lon, nav_lat, product, month)
  )
  
  ## Calculate skewness
  skew_res <- ALL_height %>% 
    # group_by(nav_lon, nav_lat, product, month) %>% 
    summarise(skewness = e1071::skewness(height, na.rm = T)) %>% 
    mutate(skewness = ifelse(is.na(skewness), NA, skewness)) %>% 
    ungroup()
  
  ## Calculate differences
  skew_diff_res <- left_join(filter(skew_res, product == "NAPA"), 
                             filter(skew_res, product == "AVISO"),
                             by = c("nav_lon", "nav_lat", "month")) %>%
    ungroup() %>% 
    mutate(skewness = skewness.x - skewness.y,
           product = "difference") %>% 
    select(nav_lon, nav_lat, product, month, skewness) %>% 
    rbind(skew_res)
  
  ## Finish
  print(paste("Completed run",lon_row,"at",Sys.time()))
  return(skew_diff_res)
}


# Skewness ----------------------------------------------------------------

# system.time(
#   test <- skewness_AN(1)
# ) # 63 seconds

# Run on Tuesday, November 6th, 2018
system.time(
  AVISO_NAPA_skewness_diff <- plyr::ldply(seq(1:1440), .fun = skewness_AN, .parallel = T)
) # xxx seconds at 50 cores
save(AVISO_NAPA_skewness_diff, file = "../data/AVISO_NAPA_skewness_diff.RData")
