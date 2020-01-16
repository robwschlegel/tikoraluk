# tikoraluk/OSTIA_download.R
# The purpose of this script is to download OSTIA data from the CMEMS FTP site


# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))

# devtools::install_github("skgrange/threadr")

library(tidyverse)
library(tidync)
library(ncdf4)
library(lubridate)
library(RCurl)
library(RCMEMS)
library(threadr)

doParallel::registerDoParallel(cores = 50)


# CMEMS option ------------------------------------------------------------

# This is a copy of the script that CMEMS provides for downloading data
# OSTIA_script <- 'python ~/motuclient-python/motuclient.py --motu http://nrt.cmems-du.eu/motu-web/Motu --service-id SST_GLO_SST_L4_NRT_OBSERVATIONS_010_001-TDS --product-id METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2 --longitude-min -179.97500610351562 --longitude-max 179.97500610351562 --latitude-min -89.9749984741211 --latitude-max 89.9749984741211 --date-min "2020-01-15 12:00:00" --date-max "2020-01-15 12:00:00" --depth-min 0 --depth-max 1 --variable analysed_sst --variable sea_ice_fraction --variable analysis_error --variable mask --out-dir Downloads --out-name test.nc --user rschlegel1 --pwd RobertCMEMS2018'

# We then feed that string into the following function where it parses it into it's important bits
# cfg <- parse.CMEMS.script(OSTIA_script, parse.user = T)

# Here we can look at the results
# If there is anything wonky here it needs to be addressed before downloading may commence
# cfg

# If everything looks correct this function will download the desired data
# Note that ne may not download over 1024MB in one shot
# A single day of data is ~128MB
# CMEMS.download(cfg)


# FTP option --------------------------------------------------------------

# OSTIA_URL <- "ftp://nrt.cmems-du.eu/Core/SST_GLO_SST_L4_NRT_OBSERVATIONS_010_001/METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2/2020/01/20200101120000-UKMO-L4_GHRSST-SSTfnd-OSTIA-GLOB-v02.0-fv02.0.nc"
# download_ftp_file(OSTIA_URL, "~/data/OSTIA/test.nc", verbose = TRUE, credentials = "rschlegel1:RobertCMEMS2018")


# Prep --------------------------------------------------------------------

# The URL where the data are housed for FTP
base_URL <- "ftp://nrt.cmems-du.eu/Core/SST_GLO_SST_L4_NRT_OBSERVATIONS_010_001/METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2"

# The date ranges that are housed therein
# NB: These are historic repos and therefore the dates are static
# I assume that the 'new' data will be updated through 2019 by the end of 2020
date_range <- seq(as.Date("2019-01-01"), as.Date(Sys.time())-1, by = "day")


# Download function -------------------------------------------------------

# Check URL for new data
OSTIA_files_year <- list_files_ftp(paste0(base_URL), credentials = "rschlegel1:RobertCMEMS2018")
OSTIA_files_month <- plyr::llply(OSTIA_files_year[-length(OSTIA_files_year)], list_files_ftp)
OSTIA_files_day <- lapply(OSTIA_files_month[-length(OSTIA_files_month)], list_files_ftp)

# date_choice <- date_range[1]
# date_choice <- "2021-02-01"
download_OSTIA <- function(date_choice){
  
  # Prep the necessary URL pieces
  date_slash <- strtrim(str_replace_all(date_choice, "-", "/"), width = 7)
  date_nogap_day <- str_replace_all(date_choice, "-", "")
  # date_nogap <- strtrim(date_nogap_day, width = 6)
  
  tail_chunk <- "120000-UKMO-L4_GHRSST-SSTfnd-OSTIA-GLOB-v02.0-fv02.0.nc"

  complete_URL <- paste0(base_URL,"/",date_slash,"/",date_nogap_day,tail_chunk)
  file_name <- paste0("~/data/OSTIA/",date_nogap_day,tail_chunk)
  # file_name <- paste0(date_nogap,tail_chunk)
  
  # Download and save the file if needed
  if(file.exists(file_name)){
    return()
  } else{
    download_ftp_file(complete_URL, file_name, verbose = TRUE, credentials = "rschlegel1:RobertCMEMS2018")
  }
  Sys.sleep(2) # Give the server a quick breather
}


# Download data -----------------------------------------------------------

# Test run
# download_OSTIA(date_range[1])

# Run in parallel
doParallel::registerDoParallel(cores = 50)

# Download data from 2019-01-01 to present day
  # NB: Some files won't download when run in parallel
  # I think the FTP server may be more aware of multiple requests than an HTTPS server
  # It may also have been due to high tikoraluk use, too
plyr::l_ply(date_range, .fun = download_OSTIA, .parallel = T)


# Check file sizes and remove failed downloads ----------------------------

# file.info("../data/CCI/19850410120000-ESACCI-L4_GHRSST-SSTdepth-OSTIA-GLOB_CDR2.1-v02.0-fv01.0.nc")$size
# file.remove("../data/CCI/19850410120000-ESACCI-L4_GHRSST-SSTdepth-OSTIA-GLOB_CDR2.1-v02.0-fv01.0.nc")


# Test visuals ------------------------------------------------------------

# test_dat <- tidync("~/data/OSTIA/20190101120000-UKMO-L4_GHRSST-SSTfnd-OSTIA-GLOB-v02.0-fv02.0.nc") %>%
#   hyper_tibble() %>% 
#   na.omit()

# NB: This takes a minute or two to render
# ggplot(test_dat, aes(x = lon, y = lat)) +
#   # geom_raster(aes(fill = analysed_sst)) +
#   geom_point(aes(colour = analysed_sst)) +
#   # borders() +
#   scale_fill_viridis_c() +
#   scale_colour_viridis_c() +
#   coord_cartesian(expand = F) +
#   theme_void()

