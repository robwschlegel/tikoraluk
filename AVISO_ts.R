# The purpose of this script is to test why RStudio is throwing errors
# when one tries to run the AVISO load/save code in a for loop


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ncdf4)
library(doMC); doMC::registerDoMC(cores = 50)


# Files -------------------------------------------------------------------

# The NetCDF files
AVISO_files <- dir(path = "../data", pattern = "CMEMS", full.names = T)

# The NAPA to OISST lon/lat mask
load("metadata/lon_lat_NAPA_OISST.RData")

# The OISST lon values for subsetting
load("metadata/lon_OISST.RData")


# Functions ---------------------------------------------------------------

# Ease function for extracting AVISO variables
AVISO_var <- function(file_name, var_id, coords){
  nc <- nc_open(as.character(file_name))
  coord_sub <- which(nc$dim$longitude$vals == coords)
  res <- ncvar_get(nc, varid = var_id)[coord_sub, , ]
  dimnames(res) <- list(lat = nc$dim$latitude$vals,
                        t = nc$dim$time$vals)
  res <- as.data.frame(reshape2::melt(res, value.name = var_id), row.names = NULL) %>% 
    mutate(t = as.Date(t, origin = "1950-01-01"),
           lon = coords) %>%
    select(lon, everything())
  nc_close(nc)
  return(res)
}

# Load AVSIO anomaly data and subset accordingly
load_AVISO_anom_sub <- function(file_name, coords){
  sla <- AVISO_var(file_name, "sla", coords)
  ugosa <- AVISO_var(file_name, "ugosa", coords)
  vgosa <- AVISO_var(file_name, "vgosa", coords)
  res <- left_join(sla, ugosa, by = c("lon", "lat", "t")) %>% 
    left_join(vgosa, by = c("lon", "lat", "t"))
  return(res)
}

# Function for combining and saving the subsetted AVISO data
save_AVISO_anom_sub <- function(coords){
  AVISO_anom_sub <- plyr::ldply(AVISO_files,
                                .fun = load_AVISO_anom_sub, 
                                .parallel = TRUE, 
                                coords = coords)
  lon_sub_label <- str_pad(which(lon_OISST == coords), width = 4, pad = "0", side = "left")
  save(AVISO_anom_sub, file = paste0("../data/AVISO_anom_sub_",lon_sub_label,".RData"))
}


# AVISO anom --------------------------------------------------------------

system.time(
  for(i in 1:1440){
    print(paste0("Began run ",i,", lon = ",lon_OISST[i],", at ",Sys.time()))
    save_AVISO_anom_sub(lon_OISST[i])
    print(paste("Completed run",i,"at",Sys.time()))
  }
)
