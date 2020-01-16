# tikoraluk/GLORYS_download.R
# The purpose of this script is to download GLORYS data from the CMEMS FTP site
# The file structure differs slightly from OSTIA so it's not possible to have
# a central CMEMS FTP download function

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
GLORYS_script <- 'python ~/motuclient-python/motuclient.py --motu http://my.cmems-du.eu/motu-web/Motu --service-id GLOBAL_REANALYSIS_PHY_001_030-TDS --product-id global-reanalysis-phy-001-030-daily --longitude-min -180 --longitude-max 179.9166717529297 --latitude-min -80 --latitude-max 90 --date-min "2018-12-25 12:00:00" --date-max "2018-12-25 12:00:00" --depth-min 0.493 --depth-max 0.4942 --variable thetao --variable bottomT --variable so --variable zos --variable uo --variable vo --variable mlotst --variable siconc --variable sithick --variable usi --variable vsi --out-dir ../data/GLORYS --out-name test.nc --user rschlegel1 --pwd RobertCMEMS2018'

# We then feed that string into the following function where it parses it into it's important bits
cfg <- parse.CMEMS.script(GLORYS_script, parse.user = T)

# Here we can look at the results
# If there is anything wonky here it needs to be addressed before downloading may commence
cfg

# NB: This only grabs the first variable
# We must manually trick it as so
cfg <- RCMEMS::update(cfg, variable = "thetao --variable uo")
cfg

# If everything looks correct this function will download the desired data
# Note that ne may not download over 1024MB in one shot
  # NB: Before running this, ensure that the terminal is either pointed at a Python v2.7.15 or greater
  # pyhton --version
CMEMS.download(cfg)

# NetCDF contents
tidync("../data/GLORYS/test.nc")

# Test visuals
test_dat <- tidync("../data/GLORYS/test.nc") %>% 
  hyper_tibble()

ggplot(test_dat, aes(x = longitude, y = latitude)) +
  geom_raster(aes(fill = thetao)) +
  scale_fill_viridis_c() +
  theme_void()


# FTP option --------------------------------------------------------------

# Currently not using this as FTP for a full depth file is 1.3GB/day
# At ~25 years that's ~11T of data

# Prep --------------------------------------------------------------------

# The URL where the data are housed for FTP
# base_URL <- "ftp://nrt.cmems-du.eu/Core/SST_GLO_SST_L4_NRT_OBSERVATIONS_010_001/METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2"

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

