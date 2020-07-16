# The purpose of this script is to provide some space were specific MCS visualisations may be made


# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(heatwaveR)
library(gganimate)
library(ggpubr)
library(XML)
library(doParallel); registerDoParallel(cores = 50)
source("MHW_prep.R")


# Meta-data ---------------------------------------------------------------

# The MCS results
MCS_RData <-  c(file = dir(path = "../data/MCS", pattern = "MCS.calc.*.RData", full.names = T))

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

## Potential colour palettes
# w3schools.com/colors/colors_groups.asp
# sciviscolor.org/home/environmental-palettes/

#474E73

#1cb5e0, #000046

#0575e6, #021b79 

#B0E0E6, #6495ED, #0000CD, #191970

#1fa2ff #12d8fa #a6ffcb 

#1a2980 #26d0ce 

#5433ff #20bdff #a5fecb 

## RcolorBrewer palettes
# Blues
# PuBu
# RdBu

# Consider ROYGBIV
# Because MHWs use ROY it could be good to use GBIV for MCSs
# Maybe don't worry about perceptual symetry
# Just pick the best colours from a colour wheel


# Load an XML file containing weird rgb vlaues and convert to hex
rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255)

# BlueSpectrum colour palette from sciviscolor.org/home/environmental-palettes/
BlueSpectrum <- t(data.frame(xmlToList(xmlParse("metadata/BlueSpectrum.xml"))$ColorMap[1:27], stringsAsFactors = F)) %>% 
  data.frame(., stringsAsFactors = F) %>% 
  remove_rownames(.) %>% 
  select(r, g, b) %>% 
  mutate(r = round(as.numeric(r)*255), g = round(as.numeric(g)*255), b = round(as.numeric(b)*255),
         hex = rgb2hex(r, g, b))
write_csv(BlueSpectrum, "metadata/BlueSpecturm.csv")

# BlueWater colour palette from sciviscolor.org/home/environmental-palettes/
BlueWater <- t(data.frame(xmlToList(xmlParse("metadata/BlueWater.xml"))$ColorMap[1:12], stringsAsFactors = F)) %>% 
  data.frame(., stringsAsFactors = F) %>% 
  remove_rownames(.) %>% 
  select(r, g, b) %>% 
  mutate(r = round(as.numeric(r)*255), g = round(as.numeric(g)*255), b = round(as.numeric(b)*255),
         hex = rgb2hex(r, g, b))
write_csv(BlueWater, "metadata/BlueWater.csv")

# This is negotiable...
# MCS_palette <- c(BlueSpectrum$hex[6], BlueSpectrum$hex[13], BlueSpectrum$hex[20], BlueSpectrum$hex[27])
MCS_palette <- c(BlueWater$hex[10], BlueWater$hex[7], BlueWater$hex[4], BlueWater$hex[2])

# Set line colours
lineCol <- c(
  "Temperature" = "black",
  "Climatology" = "black",
  "Threshold" = "black",
  "2x Threshold" = "black",
  "3x Threshold" = "black",
  "4x Threshold" = "black"
)

# Set category fill colours
fillCol <- c(
  "Moderate" = MCS_palette[1],
  "Strong" = MCS_palette[2],
  "Severe" = MCS_palette[3],
  "Extreme" = MCS_palette[4]
)

# The MHW colour palette
MHW_colours <- c(
  "Moderate" = "#ffc866",
  "Strong" = "#ff6900",
  "Severe" = "#9e0000",
  "Extreme" = "#2d0000"
)


# Colour palette comparison figure ----------------------------------------

# Find extreme MCSs
  # NB: No extreme MCS in Med, NWA, WA
  # NB: 2 severe days in Med, 0 in the others
sst_MCS <- detect_event(ts2clm(sst_Med, climatologyPeriod = c("1982-01-01", "2011-12-31"), pctile = 10), coldSpells = T)$climatology %>% 
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff,
                temp = temp/1.2) # Cook the books to get the desired colour range 
# filter(sst_MCS, temp < thresh_4x)

# Centre a line plot on 1994-01-09 
MCS_test_palette <- ggplot(data = sst_MCS, aes(x = t)) +
  geom_flame(aes(y = thresh, y2 = temp, fill = "Moderate"), n = 5, n_gap = 2, show.legend = F) +
  geom_flame(aes(y = thresh_2x, y2 = temp, fill = "Strong"), show.legend = F) +
  geom_flame(aes(y = thresh_3x, y2 = temp, fill = "Severe"), show.legend = F) +
  geom_flame(aes(y = thresh_4x, y2 = temp, fill = "Extreme"), show.legend = F) +
  geom_line(aes(y = thresh_2x, col = "2x Threshold"), size = 0.2, linetype = "dashed") +
  geom_line(aes(y = thresh_3x, col = "3x Threshold"), size = 0.2, linetype = "dotdash") +
  geom_line(aes(y = thresh_4x, col = "4x Threshold"), size = 0.2, linetype = "dotted") +
  geom_line(aes(y = seas, col = "Climatology"), size = 0.6) +
  geom_line(aes(y = thresh, col = "Threshold"), size = 0.6) +
  geom_line(aes(y = temp, col = "Temperature"), size = 0.4) +
  scale_colour_manual(name = NULL, values = lineCol,
                      breaks = c("Temperature", "Climatology", "Threshold",
                                 "2x Threshold", "3x Threshold", "4x Threshold")) +
  scale_fill_manual(name = "Event colour", values = fillCol) +
  scale_x_date(date_labels = "%b %Y", expand = c(0, 0),
               limits = c(as.Date("1993-10-02"), as.Date("1994-03-30"))) +
  # scale_y_continuous(limits = c(18, 32), expand = c(0, 0),
                     # breaks = seq(20, 30, by = 5)) +
  guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid",
                                                                "dashed", "dotdash", "dotted"),
                                                   size = c(1, 1, 1, 1, 1, 1)))) +
  labs(y = "Temp. [°C]", x = NULL) #+
  # formatting for multi-panel figure
  # labs(y = NULL) +
  # theme(axis.text.y = element_blank(),
        # axis.ticks.y = element_blank())
# ggsave("graph/MCS_test_palette.png", MCS_test_palette, width = 6, height = 4)
# ggsave("graph/MCS_test_palette.pdf", MCS_test_palette, width = 6, height = 4)

# Get Oz event
MHW <- ts <- ts2clm(sst_WA, climatologyPeriod = c("1982-01-01", "2011-12-31")) %>% 
  detect_event()
clim_cat <- MHW$clim %>% 
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff) %>% 
  dplyr::slice(10580:10690)

# Create MHW for demo
MHW_demo <- ggplot(data = clim_cat, aes(x = t, y = temp)) +
  geom_flame(aes(y2 = thresh, fill = "Moderate")) +
  geom_flame(aes(y2 = thresh_2x, fill = "Strong")) +
  geom_flame(aes(y2 = thresh_3x, fill = "Severe")) +
  geom_flame(aes(y2 = thresh_4x, fill = "Extreme")) +
  geom_line(aes(y = thresh_2x, col = "2x Threshold"), size = 0.2, linetype = "dashed") +
  geom_line(aes(y = thresh_3x, col = "3x Threshold"), size = 0.2, linetype = "dotdash") +
  geom_line(aes(y = thresh_4x, col = "4x Threshold"), size = 0.2, linetype = "dotted") +
  geom_line(aes(y = seas, col = "Climatology"), size = 0.5) +
  geom_line(aes(y = thresh, col = "Threshold"), size = 0.5) +
  geom_line(aes(y = temp, col = "Temperature"), size = 0.4) +
  scale_colour_manual(name = NULL, values = lineCol,
                      breaks = c("Temperature", "Climatology", "Threshold",
                                 "2x Threshold", "3x Threshold", "4x Threshold")) +
  scale_fill_manual(name = NULL, values = MHW_colours, guide = FALSE) +
  scale_x_date(date_labels = "%b %Y", expand = c(0, 0),) +
  guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid",
                                                                "dashed", "dotdash", "dotted"),
                                                   size = c(0.6, 0.7, 0.7, 0.7, 0.7, 0.7)))) +
  labs(y = "Temp. [°C]", x = NULL)
# MHW_demo

# Side by side events
event_compare <- ggarrange(MHW_demo, MCS_test_palette, ncol = 1, nrow = 2, align = "hv", common.legend = TRUE)
ggsave("graph/event_compare.png", event_compare, height = 4, width = 6)

# Create the colour palette for plotting by itself
colour_palette <- data.frame(category = factor(c("I Moderate", "II Strong", "III Severe", "IV Extreme"),
                                               levels = c("I Moderate", "II Strong", "III Severe", "IV Extreme")),
                             MHW = c(MHW_colours[1], MHW_colours[2], MHW_colours[3], MHW_colours[4]),
                             MCS = c(MCS_palette[1], MCS_palette[2], MCS_palette[3], MCS_palette[4])) %>% 
  pivot_longer(cols = c(MHW, MCS), names_to = "event", values_to = "colour")

# Show the palettes side-by-side
palette_compare <- ggplot(data = colour_palette, aes(x = category, y = event)) +
  geom_tile(fill = colour_palette$colour) +
  coord_cartesian(expand = F) +
  labs(x = NULL, y = NULL)
ggsave("graph/palette_compare.png", palette_compare, height = 3, width = 6)


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
