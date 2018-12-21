# The purpose of this script is to provide some space were specific MCS visualisations may be made


# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(heatwaveR)
library(gganimate)
doMC::registerDoMC(cores = 50)
source("MHW_prep.R")


# Meta-data ---------------------------------------------------------------

# The MCS results
MCS_RData <-  c(file = dir(path = "../data", pattern = "MCS.calc.*.RData", full.names = T))

# The lon coords for the OISST data
load("metadata/lon_OISST.RData")
lon_OISST <- ifelse(lon_OISST > 180, lon_OISST-360, lon_OISST)


# The base map
map_base <- ggplot2::fortify(maps::map(fill = TRUE, plot = FALSE)) %>% 
  dplyr::rename(lon = long)

# Currently interested in the 2016 winter MCS that happened just outside of the Bay of Fundy
# Gulf Stream boundary from 'gulf_stream.R'
# GS_bound <- c(25, 50, -85, -40)
YHZ_bound <- c(42, 47, -75, -55)


# Functions ---------------------------------------------------------------

# Subset event metric files
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

# Subset climatology files
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


# Event data --------------------------------------------------------------

# Extract event metrics
MCS_event_YHZ <- plyr::ldply(MCS_RData[which(lon_OISST >= YHZ_bound[3] & lon_OISST <= YHZ_bound[4])], 
                             .fun = load_MCS_event_sub, .parallel = T, 
                             date_range = c("2016-06-01", "2017-06-01"),
                             lat_range = c(YHZ_bound[1], YHZ_bound[2]))

# edit lat/lon
MCS_event_YHZ <- MCS_event_YHZ %>% 
  mutate(lon = ifelse(lon > 180, lon-360, lon),
         lon = round(lon, 3), lat = round(lat, 3))

# Subset for plotting
MCS_event_YHZ_sub <- MCS_event_YHZ %>% 
  filter(date_start >= "2016-12-10", date_start <= "2016-12-20")

# One pixel for time series example
MCS_event_YHZ_one <- MCS_event_YHZ %>% 
  filter(lon == -66.875, lat == 42.875)

# Event visuals -----------------------------------------------------------

# Maximum intensity
event_max <- ggplot(MCS_event_YHZ_sub, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = intensity_max)) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_cartesian(xlim = YHZ_bound[3:4], ylim = YHZ_bound[1:2], expand = F) +
  labs(x = "", y = "", fill = "Max. Intensity (°C)") +
  scale_fill_gradient(low = "grey", high = "navy") +
  theme(legend.position = "bottom")
event_max

# Cumulative intensity
event_cum <- ggplot(MCS_event_YHZ_sub, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = intensity_max)) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_cartesian(xlim = YHZ_bound[3:4], ylim = YHZ_bound[1:2], expand = F) +
  labs(x = "", y = "", fill = "Cum. Intensity (°C x days)") +
  scale_fill_gradient(low = "grey", high = "deepskyblue") +
  theme(legend.position = "bottom")
# event_cum

# Rate of onset
event_onset <- ggplot(MCS_event_YHZ_sub, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = rate_onset)) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_cartesian(xlim = YHZ_bound[3:4], ylim = YHZ_bound[1:2], expand = F) +
  labs(x = "", y = "", fill = "Rate onset (°C/days)") +
  scale_fill_gradient(low = "grey", high = "darkorchid") +
  theme(legend.position = "bottom")
# event_onset

# Duration
event_duration <- ggplot(MCS_event_YHZ_sub, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = duration)) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_cartesian(xlim = YHZ_bound[3:4], ylim = YHZ_bound[1:2], expand = F) +
  labs(x = "", y = "", fill = "Duration (days)") +
  scale_fill_gradient(low = "grey", high = "steelblue") +
  theme(legend.position = "bottom")
# event_duration

# Combine
event_all <- ggpubr::ggarrange(event_max, event_cum, event_onset, event_duration)
event_all
ggsave(event_all, filename = "graph/MCS/YHZ_2016_12.png", width = 18, height = 12)

ggplot(MCS_event_YHZ_one, aes(x = date_start, y = intensity_max)) +
  geom_lolli(colour = "steelblue3", colour_n = "navy", n = 0) +
  labs(x = "Start Date",
       y = expression(paste("Max. intensity [", degree, "C]")))


# Clim data ---------------------------------------------------------------

# Extract event metrics
MCS_clim_YHZ <- plyr::ldply(MCS_RData[which(lon_OISST >= YHZ_bound[3] & lon_OISST <= YHZ_bound[4])], 
                            .fun = load_MCS_clim_sub, .parallel = T, 
                            date_range = c("2016-06-01", "2017-06-01"),
                            lat_range = c(YHZ_bound[1], YHZ_bound[2]))

# Subset for plotting
MCS_clim_YHZ_sub <- MCS_clim_YHZ %>% 
  mutate(lon = ifelse(lon > 180, lon-360, lon),
         intensity = thresh-temp) %>% 
  filter(t >= "2016-12-01", t <= "2016-12-31")

# One pixel for time series example
MCS_clim_YHZ_one <- MCS_clim_YHZ %>% 
  mutate(lon = round(lon, 3), lat = round(lat, 3),
         lon = ifelse(lon > 180, lon-360, lon)) %>% 
  filter(lon == -56.875, lat == 42.875)

# Top event from above dataframe
MCS_clim_YHZ_top <- MCS_clim_YHZ_one %>% 
  slice(223:239)


# Clim visuals ------------------------------------------------------------

# Time series
ggplot(MCS_clim_YHZ_one, aes(x = t, y = thresh, y2 = temp)) +
  geom_flame(aes(y = thresh, y2 = temp, fill = "all"), show.legend = T) +
  geom_flame(data = MCS_clim_YHZ_top, aes(y = thresh, y2 = temp, fill = "top"), show.legend = T) +
  geom_line(aes(y = temp, colour = "temp")) +
  geom_line(aes(y = thresh, colour = "thresh"), size = 1.0) +
  geom_line(aes(y = seas, colour = "seas"), size = 1.2) +
  scale_colour_manual(name = "Line Colour",
                      values = c("temp" = "black", "thresh" =  "forestgreen", "seas" = "grey80")) +
  scale_fill_manual(name = "Event Colour", values = c("all" = "steelblue3", "top" = "navy")) +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  scale_y_continuous(limits = c(2, 25))

# A map
clim_temp <- ggplot(filter(MCS_clim_YHZ_sub, t == "2016-12-18"), aes(x = lon, y = lat)) +
  geom_raster(aes(fill = intensity)) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_cartesian(xlim = YHZ_bound[3:4], ylim = YHZ_bound[1:2], expand = F) +
  scale_fill_gradient(low = "grey", high = "steelblue") +
  theme(legend.position = "bottom")
clim_temp

# Base map
yhz_base <- ggplot(map_base, aes(x = lon, y = lat)) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_cartesian(xlim = YHZ_bound[3:4], ylim = YHZ_bound[1:2], expand = F) +
  theme(legend.position = "bottom")
yhz_base

# Animation for December 2016
yhz_base + geom_raster(data = MCS_clim_YHZ_sub, aes(fill = intensity)) +
  scale_fill_gradient(low = "navy", high = "grey") +
  labs(title = 'Date: {frame_time}', x = '', y = '', fill = "°C below threshold") +
  transition_time(t)
anim_save("graph/MCS/YHZ_2016_12.gif")
