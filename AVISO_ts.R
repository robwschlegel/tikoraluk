# The purpose of this script is to load the monthly AVISO NetCDF files
# and then save them as one slice per longitude (1:1440)

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ncdf4)
library(doMC); doMC::registerDoMC(cores = 3)


# Files -------------------------------------------------------------------

# The NetCDF files
# AVISO_files <- dir(path = "../data", pattern = "CMEMS", full.names = T)
AVISO_files <- c("~/Downloads/CMEMS_dataset-duacs-rep-global-merged-allsat-phy-l4_1993-01.nc",
                 "~/Downloads/CMEMS_dataset-duacs-rep-global-merged-allsat-phy-l4_1993-01.nc")

# The NAPA to OISST lon/lat mask
load("metadata/lon_lat_NAPA_OISST.RData")

# The OISST lon values for subsetting
load("metadata/lon_OISST.RData")


# Functions ---------------------------------------------------------------

# Ease function for extracting AVISO variables
AVISO_var <- function(file_name, var_id, coords){
  nc <- nc_open(as.character(file_name))
  coord_sub <- which(nc$dim$longitude$vals == df$lon)
  res <- ncvar_get(nc, varid = var_id)[coord_sub, , ]
  dimnames(res) <- list(#lon = nc$dim$longitude$vals,
                        lat = nc$dim$latitude$vals,
                        t = nc$dim$time$vals)
  res <- as.data.frame(reshape2::melt(res, value.name = var_id), row.names = NULL) %>% 
    mutate(t = as.Date(t, origin = "1950-01-01"),
           lon = df$lon) %>%
    select(lon, everything())
    # filter(lon == coords)
  nc_close(nc)
  return(res)
}

# Load AVSIO anomaly data and subset accordingly
load_AVISO_anom_sub <- function(file_name, coords){
  # nc <- nc_open(as.character(file_name))
  # correct_dates <- as.Date(nc$dim$time$vals, origin = "1950-01-01")
  sla <- AVISO_var(file_name, "sla", coords)
  ugosa <- AVISO_var(file_name, "ugosa", coords)
  vgosa <- AVISO_var(file_name, "vgosa", coords)
  res <- left_join(sla, ugosa, by = c("lon", "lat", "t")) %>% 
    left_join(vgosa, by = c("lon", "lat", "t"))
  return(res)
}

# Function for combining and saving the subsetted NAPA SST data
save_AVISO_anom_sub <- function(df){
  
  # coords <- lon_lat_NAPA_OISST %>% 
  #   filter(lon_O == df$lon)
  
  # coords <- df$lon
  
  # system.time(
  AVISO_anom_sub <- plyr::ldply(AVISO_files,
                                .fun = load_AVISO_anom_sub, 
                                .parallel = TRUE, 
                                coords = df$lon)
  # )
  
  lon_sub_label <- str_pad(which(lon_OISST == df$lon), width = 4, pad = "0", side = "left")
  
  if(nrow(AVISO_anom_sub) > 0){
    save(AVISO_anom_sub, file = paste0("../data/AVISO_anom_sub_",lon_sub_label,".RData"))
  }
}


# AVISO anom --------------------------------------------------------------

lon_OISST_multi <- data.frame(lon = lon_OISST,
                              x = 1:length(lon_OISST))

# Run on Tuesday, October 30th, 2018
system.time(
  plyr::ddply(lon_OISST_multi[1,], .variables = "x",
              .fun = save_AVISO_anom_sub)
) # xxx seconds at 50 cores