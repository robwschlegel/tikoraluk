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
MHW_data <- plyr::dlply(clim_data, c("lon", "lat"), detect_event, .parallel = T)


