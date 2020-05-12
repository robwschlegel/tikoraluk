# CMC_download.R
# The purpose of this script is to download CMC data
# The HTTPS access for the o.2 degree data from 1991 to 2017:
# https://data.nodc.noaa.gov/ghrsst/L4/GLOB/CMC/CMC0.2deg/
# The HTTPS access for the 0.1 degree data from 2016 onwards:
# https://data.nodc.noaa.gov/ghrsst/L4/GLOB/CMC/CMC0.1deg/
# This script should be source() run so as to be run in its entirety
# It searches first to ensure it is not downloading files that already exist
# NB: For some reason there are gaps in the daily CMC data after 2016
# This is found in both the 0.1 and 0.2 degree products


# Setup -------------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(tidync)
# library(ncdf4)
library(lubridate)
# library(raster)
library(doParallel); registerDoParallel(cores = 25)


# Prep --------------------------------------------------------------------

# The date ranges that are housed therein
# NB: These are historic repos and therefore the dates are static
# Links to the active portals: https://www.ghrsst.org/ghrsst-data-services/services/
date_range_old <- seq(as.Date("1991-09-01"), as.Date("2016-12-31"), by = "day")
date_range_new <- seq(as.Date("2017-01-01"), as.Date("2018-12-31"), by = "day")


# Download function -------------------------------------------------------

# testers...
# date_choice <- as.Date("1991-09-02")
# date_choice <- as.Date("2017-01-01")
download_CCI <- function(date_choice){
  
  # Prep the necessary URL pieces
  date_slash <- str_replace_all(date_choice, "-", "/")
  date_nogap <- str_replace_all(date_choice, "-", "")
  date_doy <- str_pad(yday(date_choice), width = 3, pad = "0", side = "left")
  date_year <- lubridate::year(date_choice)
  front_chunk <- "https://data.nodc.noaa.gov/ghrsst/L4/GLOB/CMC"
  
  if(date_choice %in% date_range_old){
    mid_chunk <- "CMC0.2deg"
    tail_chunk <- "120000-CMC-L4_GHRSST-SSTfnd-CMC0.2deg-GLOB-v02.0-fv02.0.nc"
  } else if(date_choice %in% date_range_new){
    mid_chunk <- "CMC0.1deg"
    tail_chunk <- "120000-CMC-L4_GHRSST-SSTfnd-CMC0.1deg-GLOB-v02.0-fv03.0.nc"
  } else{
    stop("The URL structure has changed.")
  }
  
  complete_URL <- paste0(front_chunk,"/",mid_chunk,"/",date_year,"/",date_doy,"/",date_nogap,tail_chunk)
  file_name <- paste0("../data/CMC/",date_nogap,tail_chunk)
  
  # Download and save the file if needed
  if(file.exists(file_name)){
    return()
  } else{
    download.file(url = complete_URL, method = "libcurl", destfile = file_name)
  }
  Sys.sleep(2) # Give the server a quick breather
}


# Download data -----------------------------------------------------------

# Test run
# download_CCI(date_range_old[300], base_URL_old)
# download_CCI(date_range_new[3], base_URL_new)

# Run in parallel
doParallel::registerDoParallel(cores = 25)

# Download all data from 1991 to 2016
  # NB: 2016-03-24 is missing
plyr::l_ply(date_range_old, .fun = download_CCI, .parallel = T)


# Check file sizes and remove failed downloads ----------------------------

# Find files that are under a certain size
# file_sizes <- file.info(dir("../data/CMC", full.names = T)) %>%
#   mutate(file_name = row.names(.)) %>%
#   filter(size < 15000000)

# file.remove(file_sizes$file_name)

# NB: If files are removed, run the above code again


# Test visuals ------------------------------------------------------------

# test_dat <- tidync("../data/CCI/19820620120000-ESACCI-L4_GHRSST-SSTdepth-OSTIA-GLOB_CDR2.1-v02.0-fv01.0.nc") %>%
# hyper_tibble()

# NB: This takes a minute or two to render
# ggplot(test_dat, aes(x = lon, y = lat)) +
#   geom_raster(aes(fill = analysed_sst)) +
#   # borders() +
#   scale_fill_viridis_c() +
#   coord_cartesian(expand = F) +
#   theme_void()