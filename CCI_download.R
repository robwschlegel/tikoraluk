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


# Prep --------------------------------------------------------------------

# Example complete old URL
# "http://data.ceda.ac.uk/neodc/esacci/sst/data/CDR_v2/Analysis/L4/v2.1/1981/09/01/19810901120000-ESACCI-L4_GHRSST-SSTdepth-OSTIA-GLOB_CDR2.1-v02.0-fv01.0.nc"

# Example complete new URL
# "http://data.ceda.ac.uk/neodc/c3s_sst/data/ICDR_v2/Analysis/L4/v2.0/2017/01/01/20170101120000-C3S-L4_GHRSST-SSTdepth-OSTIA-GLOB_ICDR2.0-v02.0-fv01.0.nc"

# The URLs where the data are housed for FTP
  # NB: Note that the versions are different; v2.1 vs. v2.0
  # NB: It looks like going straight through the thredds server is a more stable option
# base_URL_old <- "http://data.ceda.ac.uk/neodc/esacci/sst/data/CDR_v2/Analysis/L4/v2.1"
base_URL_old <- "http://dap.ceda.ac.uk/thredds/fileServer/neodc/esacci/sst/data/CDR_v2/Analysis/L4/v2.1"
# base_URL_new <- "http://data.ceda.ac.uk/neodc/c3s_sst/data/ICDR_v2/Analysis/L4/v2.0"
base_URL_new <- "http://dap.ceda.ac.uk/thredds/fileServer/neodc/c3s_sst/data/ICDR_v2/Analysis/L4/v2.0"

# The date ranges that are housed therein
  # NB: These are historic repos and therefore the dates are static
  # I assume that the 'new' data will be updated through 2019 by the end of 2020
date_range_old <- seq(as.Date("1981-09-01"), as.Date("2016-12-31"), by = "day")
date_range_new <- seq(as.Date("2017-01-01"), as.Date("2018-12-31"), by = "day")


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
  # NB: 1985/04/10 will not download. 504 timeout every time...
  # It won't download manually either
plyr::l_ply(date_range_old, .fun = download_CCI, base_URL = base_URL_old, .parallel = T)

# Download all new data: 2016-01-01 to 2018-12-31
plyr::l_ply(date_range_new, .fun = download_CCI, base_URL = base_URL_new, .parallel = T)


# Check file sizes and remove failed downloads ----------------------------

# file.info("../data/CCI/19850410120000-ESACCI-L4_GHRSST-SSTdepth-OSTIA-GLOB_CDR2.1-v02.0-fv01.0.nc")$size
# file.remove("../data/CCI/19850410120000-ESACCI-L4_GHRSST-SSTdepth-OSTIA-GLOB_CDR2.1-v02.0-fv01.0.nc")


# Download GLORYS data ----------------------------------------------------

# library(reticulate)

# py_run_file("../GLORYS_quarter_degree_loop_on_date.py")


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

