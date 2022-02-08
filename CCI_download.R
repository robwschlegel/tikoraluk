# tikoraluk/CCI_download.R
# The purpose of this script is to download CCI data
# These data are available in a number of places, but direct FTP access is at:
# http://data.ceda.ac.uk/neodc/esacci/sst/data/CDR_v2/Analysis/L4/v2.1
# The data for 2017 and 2018 are found here:
# http://data.ceda.ac.uk/neodc/c3s_sst/data/ICDR_v2/Analysis/L4/v2.0
# This script should be source() run so as to be run in its entirety
# It searches first to ensure it is not downloading files that already exist


# Setup -------------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(tidync)
library(ncdf4)
library(lubridate)
library(raster)
library(doParallel); registerDoParallel(cores = 50)


# Prep --------------------------------------------------------------------

# The URLs where the data are housed for direct download
  # NB: Note that the versions are different; v2.1 vs. v2.0
  # NB: It looks like going straight through the thredds server is a more stable option
base_URL_old <- "http://dap.ceda.ac.uk/thredds/fileServer/neodc/esacci/sst/data/CDR_v2/Analysis/L4/v2.1"
base_URL_new <- "http://dap.ceda.ac.uk/thredds/fileServer/neodc/c3s_sst/data/ICDR_v2/Analysis/L4/v2.0"

# The date ranges that are housed therein
  # NB: These are historic repos and therefore the dates are static
  # I assume that the 'new' data will be updated through 2019 by the end of 2020
date_range_old <- seq(as.Date("1981-09-01"), as.Date("2016-12-31"), by = "day")
date_range_new <- seq(as.Date("2017-01-01"), as.Date("2020-12-31"), by = "day")


# Download function -------------------------------------------------------

download_CCI <- function(date_choice, base_URL){
  
  # Prep the necessary URL pieces
  date_slash <- str_replace_all(date_choice, "-", "/")
  date_nogap <- str_replace_all(date_choice, "-", "")
  
  if(str_detect(base_URL, "esacci")){
    tail_chunk <- "120000-ESACCI-L4_GHRSST-SSTdepth-OSTIA-GLOB_CDR2.1-v02.0-fv01.0.nc"
  } else if(str_detect(base_URL, "c3s_sst")){
    tail_chunk <- "120000-C3S-L4_GHRSST-SSTdepth-OSTIA-GLOB_ICDR2.0-v02.0-fv01.0.nc"
  } else{
    stop("The URL structure has changed.")
  }
  
  complete_URL <- paste0(base_URL,"/",date_slash,"/",date_nogap,tail_chunk)
  file_name <- paste0("../data/CCI/",date_nogap,tail_chunk)
  
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
doParallel::registerDoParallel(cores = 50)

# Download all old data: 1981-09-01 to 2016-12-31
plyr::l_ply(date_range_old, .fun = download_CCI, base_URL = base_URL_old, .parallel = T)

# Download all new data: 2016-01-01 to 2020-12-31
plyr::l_ply(date_range_new, .fun = download_CCI, base_URL = base_URL_new, .parallel = T)


# Check file sizes and remove failed downloads ----------------------------

# Find files that are under a certain size
# file_sizes <- file.info(dir("../data/CCI", full.names = T)) %>%
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


# Load data ---------------------------------------------------------------

# Files
CCI_files <- dir("../data/CCI", full.names = T)
tidync("../data/CCI/19820620120000-ESACCI-L4_GHRSST-SSTdepth-OSTIA-GLOB_CDR2.1-v02.0-fv01.0.nc")

# Load function
load_CCI_region <- function(file_name, lon_min, lon_max, lat_min, lat_max){
  res <- tidync(file_name) %>%
    hyper_filter(lat = dplyr::between(lat, lat_min, lat_max),
                 lon = dplyr::between(lon, lon_min, lon_max)) %>%
    hyper_tibble() %>%
    dplyr::rename(t = time, temp = analysed_sst) %>%
    mutate(t = as.Date(as.POSIXct(t, origin = '1981-01-01', tz = "GMT")),
           temp = round(temp-273.15, 2)) %>%
    dplyr::select(lon, lat, t, temp)
  return(res)
}

# Shelburne harbour and Jordan Bay. Late feb / early march 2015 (approx. 24th Feb – march 3rd). 
# Top left = -65.5843, 43.568. Top right = -65.1896, 43.8499. Bottom right = -64.8205, 43.567. Bottom left = -65.2871, 43.3256. 
# shelbourne_harbour <- plyr::ldply(CCI_files[12175:12325], load_CCI_region, .parallel = T,
#                                   lon_min = -65.5843, lon_max = -64.8205,
#                                   lat_min = 43.3256, lat_max = 43.8499)
# write_csv(shelbourne_harbour, "extracts/shelbourne_harbour.csv")

# Test visual
# ggplot(data = filter(shelbourne_harbour, t == "2015-02-22"), aes(x = lon, y = lat)) +
#   geom_tile(aes(fill = temp)) +
#   borders(colour = "black") +
#   coord_quickmap(xlim = c(-65.5843, -64.8205),
#                  ylim = c(43.3256, 43.8499), expand = F)

