# The purpose of this script is to provide some space were specific MCS visualisations may be made


# Libraries ---------------------------------------------------------------

library(tidyverse)
doMC::registerDoMC(cores = 50)
source("MHW_prep.R")


# Meta-data ---------------------------------------------------------------

# The MCS results
MCS_RData <-  c(file = dir(path = "../data", pattern = "MCS.calc.*.RData", full.names = T))

# The lon coords for the OISST data
load("metadata/lon_OISST.RData")
lon_OISST <- ifelse(lon_OISST > 180, lon_OISST-360, lon_OISST)


# Functions ---------------------------------------------------------------

load_MCS_event_sub <- function(file_name, date_range,
                         lon_range = NA, lat_range){
  load(file_name)
  res <- MHW_event(MCS_res) %>% 
    filter(date_start >= date_range[1], date_start <= date_range[2],
           # lon >= lon_range[1], lon <= lon_range[2],
           lat >= lat_range[1], lat <= lat_range[2]) #%>%
    # select(lon, lat, t, temp)
  rm(MCS_res)
  return(res)
}

load_MCS_clim_sub <- function(file_name, date_range,
                               lon_range = NA, lat_range){
  load(file_name)
  res <- MHW_clim(MCS_res) %>% 
    filter(t >= date_range[1], t <= date_range[2],
           # lon >= lon_range[1], lon <= lon_range[2],
           lat >= lat_range[1], lat <= lat_range[2]) #%>%
  # select(lon, lat, t, temp)
  rm(MCS_res)
  return(res)
}


# Data --------------------------------------------------------------------

# The base map
map_base <- ggplot2::fortify(maps::map(fill = TRUE, plot = FALSE)) %>% 
  dplyr::rename(lon = long)

# Currently interested in the 2016 winter MCS that happened just outside of the Bay of Fundy
# Gulf Stream boundary from 'gulf_stream.R'
# GS_bound <- c(25, 50, -85, -40)
YHZ_bound <- c(42, 47, -75, -55)

MCS_event_YHZ <- plyr::ldply(MCS_RData[which(lon_OISST >= YHZ_bound[3] & lon_OISST <= YHZ_bound[4])], 
                             .fun = load_MCS_event_sub, .parallel = T, 
                             date_range = c("2015-01-01", "2017-01-01"),
                             lat_range = c(YHZ_bound[1], YHZ_bound[2]))

MCS_event_YHZ_sub <- MCS_event_YHZ %>% 
  mutate(lon = ifelse(lon > 180, lon-360, lon)) %>% 
  filter(date_start >= "2016-12-10", date_start <= "2016-12-20")#,
         # lat >= YHZ_bound[1], lat <= YHZ_bound[2],
         # lon >= YHZ_bound[3], lon <= YHZ_bound[4])




# Visuals -----------------------------------------------------------------

# Maximum intensity
plot_max <- ggplot(MCS_event_YHZ_sub, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = intensity_max)) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_cartesian(xlim = YHZ_bound[3:4], ylim = YHZ_bound[1:2], expand = F) +
  labs(x = "", y = "", fill = "Max. Intensity (°C)") +
  scale_fill_gradient(low = "grey", high = "navy") +
  theme(legend.position = "bottom")
plot_max

# Cumulative intensity
plot_cum <- ggplot(MCS_event_YHZ_sub, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = intensity_max)) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_cartesian(xlim = YHZ_bound[3:4], ylim = YHZ_bound[1:2], expand = F) +
  labs(x = "", y = "", fill = "Cum. Intensity (°C x days)") +
  scale_fill_gradient(low = "grey", high = "deepskyblue") +
  theme(legend.position = "bottom")
plot_cum

# Rate of onset
plot_onset <- ggplot(MCS_event_YHZ_sub, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = rate_onset)) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_cartesian(xlim = YHZ_bound[3:4], ylim = YHZ_bound[1:2], expand = F) +
  labs(x = "", y = "", fill = "Max.Intensity (°C/days)") +
  scale_fill_gradient(low = "grey", high = "darkorchid") +
  theme(legend.position = "bottom")
plot_onset

# Duration
plot_duration <- ggplot(MCS_event_YHZ_sub, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = duration)) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_cartesian(xlim = YHZ_bound[3:4], ylim = YHZ_bound[1:2], expand = F) +
  labs(x = "", y = "", fill = "Duration (days)") +
  scale_fill_gradient(low = "grey", high = "purple") +
  theme(legend.position = "bottom")
plot_duration

# Combine
plot_all <- ggpubr::ggarrange(plot_max, plot_cum, plot_onset, plot_duration)
plot_all
