# MCS_visuals.R
# The purpose of this script is to provide some space were specific MCS visualisations may be made


# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(heatwaveR)
library(gganimate)
library(ggpubr)
library(XML)
library(ggridges)
library(doParallel); registerDoParallel(cores = 50)
source("MCS_prep.R")


# Meta-data ---------------------------------------------------------------

# Currently interested in the 2016 winter MCS that happened just outside of the Bay of Fundy
YHZ_bound <- c(42, 47, -75, -55)

## Potential colour palettes
# w3schools.com/colors/colors_groups.asp
# sciviscolor.org/home/environmental-palettes/

# Consider ROYGBIV
# Because MHWs use ROY it could be good to use GBIV for MCSs
# Maybe don't worry about perceptual symmetry
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
  "Climatology" = "grey20",
  "Threshold" = "darkorchid",
  "2x Threshold" = "darkorchid",
  "3x Threshold" = "darkorchid",
  "4x Threshold" = "darkorchid"
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

# The MCS colour palette
MCS_colours <- c(
  "I Moderate" = "#A4D4E0",
  "II Strong" = "#5B80A6",
  "III Severe" = "#2A3C66",
  "IV Extreme" = "#111433"
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
  labs(y = "Temp. [°C]", x = NULL)


# Event data --------------------------------------------------------------

## Halifax
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
  filter(date_start >= "2016-12-10", date_start <= "2017-01-15")

# One pixel for time series example
MCS_event_YHZ_one <- MCS_event_YHZ %>% 
  filter(lon == -64.125, lat == 46.375)


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
event_cum

# Rate of onset
event_onset <- ggplot(MCS_event_YHZ_sub, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = rate_onset)) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_cartesian(xlim = YHZ_bound[3:4], ylim = YHZ_bound[1:2], expand = F) +
  labs(x = "", y = "", fill = "Rate onset (°C/days)") +
  scale_fill_gradient(low = "grey", high = "darkorchid") +
  theme(legend.position = "bottom")
event_onset

# Duration
event_duration <- ggplot(MCS_event_YHZ_sub, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = duration)) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_cartesian(xlim = YHZ_bound[3:4], ylim = YHZ_bound[1:2], expand = F) +
  labs(x = "", y = "", fill = "Duration (days)") +
  scale_fill_gradient(low = "grey", high = "steelblue") +
  theme(legend.position = "bottom")
event_duration

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




# Figure 1 ----------------------------------------------------------------

fig_1 <- ggplot(data = sst_MCS, aes(x = t)) +
  geom_flame(aes(y = thresh, y2 = temp, fill = "Moderate"), n = 5, n_gap = 2) +
  geom_flame(aes(y = thresh_2x, y2 = temp, fill = "Strong")) +
  geom_flame(aes(y = thresh_3x, y2 = temp, fill = "Severe")) +
  geom_flame(aes(y = thresh_4x, y2 = temp, fill = "Extreme")) +
  geom_line(aes(y = thresh_2x, col = "2x Threshold"), size = 0.2, linetype = "dashed") +
  geom_line(aes(y = thresh_3x, col = "3x Threshold"), size = 0.2, linetype = "dotdash") +
  geom_line(aes(y = thresh_4x, col = "4x Threshold"), size = 0.2, linetype = "dotted") +
  geom_line(aes(y = seas, col = "Climatology"), size = 0.6) +
  geom_line(aes(y = thresh, col = "Threshold"), size = 0.6) +
  geom_line(aes(y = temp, col = "Temperature"), size = 0.4) +
  scale_colour_manual(name = "Line colours", values = lineCol,
                      breaks = c("Temperature", "Climatology", "Threshold",
                                 "2x Threshold", "3x Threshold", "4x Threshold")) +
  scale_fill_manual(name = "Category", values = fillCol, breaks = c("Moderate", "Strong", "Severe", "Extreme")) +
  scale_x_date(date_labels = "%b %Y", expand = c(0, 0),
               limits = c(as.Date("1993-10-02"), as.Date("1994-03-30"))) +
  scale_y_continuous(limits = c(8, 22), expand = c(0, 0), breaks = seq(10, 20, by = 5)) +
  guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid",
                                                                "dashed", "dotdash", "dotted"),
                                                   size = c(1, 1, 1, 1, 1, 1)))) +
  labs(y = "Temp. [°C]", x = NULL)
# fig_1
ggsave("graph/MCS/fig_1.png", fig_1, width = 6)


# Figure 2 ----------------------------------------------------------------

# One of the most widely published MCS is that which occurred off Florida in 2003
FL_bound <- c(26, 36, -82, -72)

# Load the Florida region data
FL_data <- load_MCS_ALL(FL_bound)

# Notes from Monday 2020-08-10 meeting
# Map anomalies should be blue to red
  # But use different shades of blue etc. so they don't look like the MCS colours
  # Consider white or yellow
# Add labels in top left corners

# Create a schematic for figure 1

# testers...
# date_range <- c("2003-07-01", "2003-7-31")
Hobday_Fig_3_MCS <- function(MCS_data, date_range){
  
  # Find the most intense point
  centre_point <- MCS_data$clim_data %>% 
    mutate(anom = temp - seas) %>% 
    filter(t >= date_range[1],
           t <= date_range[2]) %>% 
    filter(anom == min(anom))
  
  # Find the date range of the event
  centre_dates <- MCS_data$event_data %>% 
    filter(lon == centre_point$lon[1],
           lat == centre_point$lat[1],
           event_no == centre_point$event_no[1])
  
  # Event name
  centre_name <- paste0(lubridate::year(centre_dates$date_peak), " event")
  
  # Extract the top event rows
  mcs_top <- MCS_data$clim_data %>% 
    filter(lon == centre_point$lon[1],
           lat == centre_point$lat[1],
           t >= centre_dates$date_start[1]-1,
           t <= centre_dates$date_end[1]+1)
  
  # Map figure
  mf <- MCS_data$clim_data %>% 
    filter(t == centre_point$t[1]) %>% 
    mutate(anom = temp - seas) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_tile(aes(fill = anom)) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
    geom_label(aes(x = -80, y = 35, label = centre_point$t[1]), size = 6) +
    geom_point(data = centre_point, aes(x = lon, y = lat), shape = 21, fill = "pink", size = 3) +
    coord_quickmap(expand = F, xlim = range(MCS_data$clim_data$lon), ylim = range(MCS_data$clim_data$lat)) +
    scale_fill_gradient2(low = "blue", high = "red") +
    labs(x = NULL, y = NULL, fill = "SSTa (°C)") +
    theme(panel.border = element_rect(colour = "black", fill = NA))
  # mf
  
  # Event line figure
  el <- MCS_data$clim_data %>% 
    filter(lon == centre_point$lon[1],
           lat == centre_point$lat[1],
           t >= centre_point$t-190,
           t <= centre_point$t+190) %>% 
    mutate(diff = thresh - seas,
           thresh_2x = thresh + diff,
           thresh_3x = thresh_2x + diff,
           thresh_4x = thresh_3x + diff) %>% 
    ggplot(aes(x = t)) +
    geom_flame(aes(y = thresh, y2 = temp, fill = "Moderate"), n = 5, n_gap = 2) +
    geom_flame(aes(y = thresh_2x, y2 = temp, fill = "Strong")) +
    geom_flame(aes(y = thresh_3x, y2 = temp, fill = "Severe")) +
    geom_flame(aes(y = thresh_4x, y2 = temp, fill = "Extreme")) +
    geom_line(aes(y = thresh_2x, col = "2x Threshold"), size = 0.2, linetype = "dashed") +
    geom_line(aes(y = thresh_3x, col = "3x Threshold"), size = 0.2, linetype = "dotdash") +
    geom_line(aes(y = thresh_4x, col = "4x Threshold"), size = 0.2, linetype = "dotted") +
    geom_line(aes(y = seas, col = "Climatology"), size = 0.6) +
    geom_line(aes(y = thresh, col = "Threshold"), size = 0.6) +
    geom_line(aes(y = temp, col = "Temperature"), size = 0.4) +
    scale_colour_manual(name = "Line colours", values = lineCol,
                        breaks = c("Temperature", "Climatology", "Threshold",
                                   "2x Threshold", "3x Threshold", "4x Threshold")) +
    scale_fill_manual(name = "Category", values = fillCol, breaks = c("Moderate", "Strong", "Severe", "Extreme")) +
    scale_x_date(date_labels = "%b %Y", expand = c(0, 0)) +
    guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid",
                                                                  "dashed", "dotdash", "dotted"),
                                                     size = c(1, 1, 1, 1, 1, 1)))) +
    labs(y = expression(paste("Temperature (°C)")), x = NULL) +
    theme(panel.border = element_rect(colour = "black", fill = NA))
  # el
  
  # Lolliplot figures
  ld <- MCS_data$event_data %>% 
    filter(lon == centre_point$lon[1],
           lat == centre_point$lat[1]) %>% 
    ggplot(aes(x = date_peak, y = duration)) +
    geom_lolli(colour = "steelblue3") +
    geom_lolli(data = centre_dates, colour = "navy") +
    labs(x = NULL, y = "Duration (days)", colour = "Events") +
    theme(axis.text.x = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA))
  # ld
  lim <- MCS_data$event_data %>% 
    filter(lon == centre_point$lon[1],
           lat == centre_point$lat[1]) %>% 
    ggplot(aes(x = date_peak, y = intensity_max)) +
    geom_lolli(colour = "steelblue3") +
    geom_lolli(data = centre_dates, colour = "navy") +
    labs(x = NULL, y = "Maximum Intensity (°C)", colour = "Events") +
    theme(axis.text.x = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA))
  # lim
  lic <- MCS_data$event_data %>% 
    filter(lon == centre_point$lon[1],
           lat == centre_point$lat[1]) %>% 
    ggplot(aes(x = date_peak, y = intensity_cumulative)) +
    geom_lolli(colour = "steelblue3") +
    geom_lolli(data = centre_dates, colour = "navy") +
    labs(x = "Peak date", y = "Cumulative Intensity (°C)", colour = "Events") +
    theme(panel.border = element_rect(colour = "black", fill = NA))
  # lic
  
  # Combine and save
  full_fig <- ggarrange(mf, el, ld, lim, lic, ncol = 1, nrow = 5, align = "h", 
                        heights = c(1.2, 0.7, 0.5, 0.5, 0.5))
  return(full_fig)
}

# The 2003 Florida summer event
FL_2003_summer <- Hobday_Fig_3_MCS(FL_data, c("2003-07-01", "2003-7-31"))
ggsave("graph/MCS/FL_2003_summer.png", FL_2003_summer, height = 14, width = 5)

# The 2002 winter event
FL_2002_winter <- Hobday_Fig_3_MCS(FL_data, c("2002-09-01", "2003-01-31"))
ggsave("graph/MCS/FL_2002_winter.png", FL_2002_winter, height = 14, width = 5)

# The biggest event
FL_max <- Hobday_Fig_3_MCS(FL_data, c("1982-01-01", "2020-12-31"))
ggsave("graph/MCS/FL_max.png", FL_max, height = 14, width = 5)

# Combine all three
FL_trio <- ggarrange(FL_2003_summer, FL_2002_winter, FL_max, ncol = 3, nrow = 1)
ggsave("graph/MCS/FL_trio.png", FL_trio, height = 14, width = 15)
ggsave("graph/MCS/fig_2.png", FL_trio, height = 14, width = 15)


# Figure 3 ----------------------------------------------------------------

MHW_v_MCS <- readRDS("data/MHW_v_MCS.Rds")

fig_3_func <- function(tile_val){
  ggplot(data = MHW_v_MCS, aes(x = lon, y = lat)) +
    geom_tile(aes_string(fill = tile_val)) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
    coord_quickmap(expand = F) +
    scale_fill_gradient2(low = "blue", high = "red") +
    labs(x = NULL, y = NULL, fill = tile_val) +
    theme_void() +
    theme(panel.border = element_rect(colour = "black", fill = NA))
}

fig_3a <- fig_3_func("dur")
fig_3b <- fig_3_func("i_mean")
fig_3c <- fig_3_func("i_max")
fig_3d <- fig_3_func("i_cum")

fig_3 <- ggpubr::ggarrange(fig_3a, fig_3b, fig_3c, fig_3d, ncol = 2, nrow = 2)
ggsave("graph/MCS/fig_3.png", fig_3, height = 8, width = 16)


# Figure 4 ----------------------------------------------------------------

SSTa_stats <- readRDS("data/SSTa_stats.Rds") %>% 
  dplyr::select(lon:anom_kurt) %>% 
  pivot_longer(c(anom_kurt, anom_skew)) %>% 
  mutate(name = case_when(name == "anom_kurt" ~ "kurtosis",
                          name == "anom_skew" ~ "skewness"))


# Show a ridegplot with the fill for kurtosis and the colour for skewness
fig_4 <- SSTa_stats %>% 
  mutate(lat_10 = factor(plyr::round_any(lat, 10))) %>% 
  dplyr::select(-lon, -lat) %>% 
  mutate(season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter", "Total"))) %>% 
  ggplot(aes(x = value, y = lat_10)) +
  geom_density_ridges(aes(fill = season), alpha = 0.5, size = 0.1) +
  # scale_x_continuous(limits = c(-2, 10), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-2, 6), expand = c(0, 0)) +
  labs(x = NULL, y = "Latitude" ) +
  facet_wrap(~name) +
  theme_ridges()
ggsave("graph/MCS/fig_4.png", fig_4, width = 12)

