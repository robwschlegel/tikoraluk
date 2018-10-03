# The purpose of this script is to open and extract
# from every NAPA NetCDF file a specific series of pixels
# in order to create time series that are then useful for
# any range of other applicatios.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ncdf4)


# Data --------------------------------------------------------------------



# The file location
NAPA_files <- dir("../../data/NAPA025/1d_grid_T_2D/", full.names = T)

# The NAPA lon/lat values
load("NAPA/mask_long.RData")


# Functions ---------------------------------------------------------------


## Testing...
# file_name <- NAPA_files[1]

load_NAPA_sst_sub <- function(df, lon_sub = 1){
  nc <- nc_open(df$file_name)
  date_start <- ymd(str_sub(basename(file_name), start = 29, end = 36))
  date_end <- ymd(str_sub(basename(file_name), start = 38, end = 45))
  date_seq <- seq(date_start, date_end, by = "day")
  sst <- as.data.frame(ncvar_get(nc, varid = "sst")[lon_sub,,]) %>% 
    mutate(lon = as.numeric(nc$dim$x$vals)[lon_sub]) %>% 
    gather(-lon, key = lat, value = temp) %>% 
    mutate(lat = rep(as.numeric(nc$dim$y$vals), times = 5),
           temp = ifelse(temp == 0, NA, temp),
           t = rep(seq(date_start, date_end, by = "day"), each = 735)) %>%
    select(lon, lat, t, temp) %>% 
    na.omit() %>% 
    filter(lon == lon_sub) %>% 
    left_join(mask_long, by = c("lon", "lat"))
  nc_close(nc)
  return(sst)
}

# length(unique(mask_long$lon))

save_NAPA_sst_sub <- function(lon_sub){
  test <- NAPA_files[1:20]
  NAPA_sst_sub <- data.frame(file_name = test,
                             x = 1:length(test)) %>% 
    # mutate(y = 1:n()) %>% 
    group_by(x) %>%
    nest() %>% 
    mutate(sst = purrr::map(data, load_NAPA_sst_sub, 
                            lon_sub = lon_sub)) %>% 
    select(sst) %>% 
    unnest()
  
  
  NAPA_sst_sub <- data.frame()
  for(i in 1:length(NAPA_files)){
    res <- load_NAPA_sst_sub(NAPA_files[i], lon_sub = lon_sub)
    if(nrow(res) != 0){
      NAPA_sst_sub <- rbind(NAPA_sst_sub, res)
    } else {
      NAPA_sst_sub <- NAPA_sst_sub
    }
  }
  save(NAPA_sst_sub, file = paste0("NAPA/NAPA_sst_sub_",lon_sub,".RData"))
}

# Process data ------------------------------------------------------------

# Run Wednesday, October 3rd, 2018
system.time(
save_NAPA_sst_sub(lon_sub = 20)
) # xxx seconds

NAPA_files_multi <- data.frame(file = NAPA_files,
                               x = 1:length(NAPA_files)) #%>% 
  # slice(1:10)

system.time(
  plyr::ddply(file_list_multi, .variables = "x", 
              .fun = MHW_calc, .parallel = TRUE)
) 
