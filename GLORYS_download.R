# tikoraluk/GLORYS_download.R
# The purpose of this script is to download GLORYS data from the CMEMS FTP site
# The file structure differs slightly from OSTIA so it's not possible to have
# a central CMEMS FTP download function
# Also the full GLORYS files are massive so subsetting is ideal


# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))

# devtools::install_github("skgrange/threadr")

library(tidyverse)
library(tidync)
library(ncdf4)
library(lubridate)
# library(RCurl)
library(RCMEMS)
library(threadr)

doParallel::registerDoParallel(cores = 50)


# FTP option --------------------------------------------------------------

# Currently not using this as FTP for a full depth file is 1.3GB/day
# At ~25 years that's ~11T of data

# Prep --------------------------------------------------------------------

# NB: This repo is static
date_range <- base::expand.grid(1993:2018, 1:12) %>% 
  dplyr::rename(year = Var1, month = Var2) %>% 
  arrange(year, month) %>% 
  mutate(year_mon = paste0(year,"-",month)) %>% 
  dplyr::select(year_mon)


# Download function -------------------------------------------------------

# NB: This function is currently designed to subset data to the MHWflux projects domain
# date_choice <- date_range$year_mon[312]
download_GLORYS <- function(date_choice){
  
  # The GLORYS script
  GLORYS_script <- 'python ~/motuclient-python/motuclient.py --motu http://my.cmems-du.eu/motu-web/Motu --service-id GLOBAL_REANALYSIS_PHY_001_030-TDS --product-id global-reanalysis-phy-001-030-daily --longitude-min -180 --longitude-max 179.9166717529297 --latitude-min -80 --latitude-max 90 --date-min "2018-12-25 12:00:00" --date-max "2018-12-25 12:00:00" --depth-min 0.493 --depth-max 0.4942 --variable thetao --variable bottomT --variable so --variable zos --variable uo --variable vo --variable mlotst --variable siconc --variable sithick --variable usi --variable vsi --out-dir ../data/GLORYS --out-name test.nc --user rschlegel1 --pwd RobertCMEMS2018'
  
  # Prep the necessary URL pieces
  date_start <- parse_date(date_choice, format = "%Y-%m")
  date_end <- date_start %m+% months(1) - 1
  
  if(date_end == as.Date("2018-12-31")) date_end <- as.Date("2018-12-25")
  
  file_name <- paste0("MHWflux_GLORYS_",date_choice,".nc")
  
  cfg <- parse.CMEMS.script(GLORYS_script, parse.user = T)
  cfg_update <- RCMEMS::update(cfg, variable = "thetao --variable bottomT --variable so --variable zos --variable uo --variable vo --variable mlotst --variable siconc --variable sithick --variable usi --variable vsi",
                               longitude.min = "-80.5",
                               longitude.max = "-40.5",
                               latitude.min = "31.5",
                               latitude.max = "63.5",
                               date.min = as.character(date_start),
                               date.max = as.character(date_end),
                               out.name = file_name)
  
  # Download and save the file if needed
  if(file.exists(paste0("~/data/GLORYS/",file_name))){
    return()
  } else{
    CMEMS.download(cfg_update)
  }
  Sys.sleep(2) # Give the server a quick breather
}


# Download data -----------------------------------------------------------

# Test run
download_GLORYS(date_range$year_mon[1])

# Run in parallel
doParallel::registerDoParallel(cores = 50)

# Download data from 1993-01-01 to 2018-12-25
  #NB: The CMEMS server is a little wonky, rather not try to multicore this
plyr::l_ply(date_range$year_mon, .fun = download_GLORYS, .parallel = F)


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

