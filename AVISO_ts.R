# The purpose of this script is to load the monthly AVISO NetCDF files
# and then save them as one slice per longitude (1:1440)

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

# Function for combining and saving the subsetted NAPA SST data
save_AVISO_anom_sub <- function(coords){
  
  # coords <- lon_lat_NAPA_OISST %>% 
  #   filter(lon_O == df$lon)
  
  # coords <- df$lon
  
  # system.time(
  AVISO_anom_sub <- plyr::ldply(AVISO_files,
                                .fun = load_AVISO_anom_sub, 
                                .parallel = TRUE, 
                                coords = coords)
  # )
  
  lon_sub_label <- str_pad(which(lon_OISST == coords), width = 4, pad = "0", side = "left")
  
  # if(nrow(AVISO_anom_sub) > 0){
    save(AVISO_anom_sub, file = paste0("../data/AVISO_anom_sub_",lon_sub_label,".RData"))
  # }
}


# AVISO anom --------------------------------------------------------------

# Run on Tuesday, October 30th, 2018
system.time(
  plyr::ldply(lon_OISST[1], .fun = save_AVISO_anom_sub, .progress = "text")
) # 230 seconds at 50 cores
system.time(
  plyr::ldply(lon_OISST[2:10], .fun = save_AVISO_anom_sub, .progress = "text")
) # 2097 seconds at 50 cores
system.time(
  plyr::ldply(lon_OISST[11:52], .fun = save_AVISO_anom_sub, .progress = "text")
) # stopped at 52 and wouldn't continue
system.time(
  plyr::ldply(lon_OISST[53:55], .fun = save_AVISO_anom_sub, .progress = "text")
) # hung after 55
# Switching over to for loop to see if this addresses the hanging issue
system.time(
  for(i in 56:58){
    save_AVISO_anom_sub(lon_OISST[i])
    print(paste("Completed run",i,"at",system.time()))
  }
) # stops after three runs
system.time(
  for(i in 59:62){
    print(paste("Began run",i,"at",Sys.time()))
    save_AVISO_anom_sub(lon_OISST[i])
    print(paste("Completed run",i,"at",Sys.time()))
  }
) # 1015 seconds at 50 cores
system.time(
  for(i in 63){
    print(paste("Began run",i,"at",Sys.time()))
    save_AVISO_anom_sub(lon_OISST[i])
    print(paste("Completed run",i,"at",Sys.time()))
  }
) # stops after one run


system.time(
  plyr::ldply(lon_OISST[101:400], .fun = save_AVISO_anom_sub, .progress = "text")
) # xxx seconds at 50 cores
system.time(
  plyr::ldply(lon_OISST[401:700], .fun = save_AVISO_anom_sub, .progress = "text")
) # xxx seconds at 50 cores
system.time(
  plyr::ldply(lon_OISST[701:1000], .fun = save_AVISO_anom_sub, .progress = "text")
) # xxx seconds at 50 cores
system.time(
  plyr::ldply(lon_OISST[1001:1440], .fun = save_AVISO_anom_sub, .progress = "text")
) # xxx seconds at 50 cores

