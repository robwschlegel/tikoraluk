# MCS_visuals.R
# The purpose of this script is to provide some space were specific MCS visualisations may be made


# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
source("MCS_prep.R")
library(gganimate)
library(ggpubr)
library(ggridges)
library(ggpattern)
library(heatwaveR); packageVersion("heatwaveR")
library(doParallel); registerDoParallel(cores = 50)
#

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

# Currently interested in the 2016 winter MCS that happened just outside of the Bay of Fundy
YHZ_bound <- c(42, 47, -75, -55)

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

# Find a pixel that naturally experienced a Cat 4 event
AC_bound <- c(-35, 4-5, 20, 35)
AC_data <- load_MCS_ALL(AC_bound)

# Manually look through the events to find a good Cat 4
AC_data_cat <- AC_data$cat_data

# A 2018 event at 29.625 -31.625 looks like a good candidate
AC_data_event_sub <- AC_data$event_data %>% 
  filter(lon == 29.625, lat == -31.625)
AC_data_clim_sub <- AC_data$clim_data %>% 
  filter(lon == 29.625, lat == -31.625,
         t >= "2017-12-01", t <= "2018-04-30") %>% 
  mutate(diff = thresh - seas,
         thresh_2x = thresh + diff,
         thresh_3x = thresh_2x + diff,
         thresh_4x = thresh_3x + diff)

# Schematic of a MCS
fig_1 <- ggplot(data = AC_data_clim_sub, aes(x = t)) +
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
  # Cumulative intensity label
  geom_curve(colour = "steelblue1",
             aes(x = as.Date("2018-02-23"), xend = as.Date("2018-02-10"),
                 y = 24.7, yend = 19.39), curvature = -0.4) +
  geom_label(aes(label = "Cum. Intensity = -70.04°CxDays", x = as.Date("2018-02-26"), y = 22.0),
             colour = "steelblue1", label.size = 3) +
  geom_label(aes(label = "Cum. Intensity = -70.04°CxDays", x = as.Date("2018-02-26"), y = 22.0),
             colour = "black", label.size = 0) +
  # Max intensity label
  geom_segment(colour = "midnightblue",
               aes(x = as.Date("2018-02-10"), xend = as.Date("2018-02-10"),
                   y = 25.6323, yend = 19.0)) +
  geom_label(aes(label = "Max. Intensity = -6.24°C", x = as.Date("2018-02-10"), y = 19.0),
             colour = "midnightblue", label.size = 3) +
  geom_label(aes(label = "Max. Intensity = -6.24°C", x = as.Date("2018-02-10"), y = 19.0),
             colour = "black", label.size = 0) +
  # Duration label
  geom_segment(colour = "slateblue1",
               aes(x = as.Date("2018-01-28"), xend = as.Date("2018-01-28"),
                   y = 24.1698, yend = 26.0)) +
  geom_segment(colour = "slateblue1",
               aes(x = as.Date("2018-02-23"), xend = as.Date("2018-02-23"),
                   y = 24.7, yend = 26.0)) +
  geom_segment(colour = "slateblue1",
               aes(x = as.Date("2018-01-28"), xend = as.Date("2018-02-23"),
                   y = 26.0, yend = 26.0)) +
  geom_label(aes(label = "Duration = 25 days", x = as.Date("2018-02-10"), y = 26.0),
             colour = "slateblue1", label.size = 3) +
  geom_label(aes(label = "Duration = 25 days", x = as.Date("2018-02-10"), y = 26.0),
             colour = "black", label.size = 0) +
  # I Moderate
  geom_label(aes(label = "I Moderate = 44%", x = as.Date("2018-01-28"), y = 23.5),
                 colour = MCS_palette[1], label.size = 3) +
  geom_label(aes(label = "I Moderate = 44%", x = as.Date("2018-01-28"), y = 23.5),
             colour = "black", label.size = 0) +
  # II Strong
  geom_label(aes(label = "II Strong = 20%", x = as.Date("2018-02-03"), y = 22.5),
             colour = MCS_palette[2], label.size = 3) +
  geom_label(aes(label = "II Strong = 20%", x = as.Date("2018-02-03"), y = 22.5),
             colour = "black", label.size = 0) +
  # III Severe
  geom_label(aes(label = "III Severe = 8%", x = as.Date("2018-02-04"), y = 21.5),
             colour = MCS_palette[3], label.size = 3) +
  geom_label(aes(label = "III Severe = 8%", x = as.Date("2018-02-04"), y = 21.5),
             colour = "black", label.size = 0) +
  # IV Extreme
  geom_label(aes(label = "IV Extreme = 20%", x = as.Date("2018-02-04"), y = 20.5),
             colour = MCS_palette[4], label.size = 3) +
  geom_label(aes(label = "IV Extreme = 20%", x = as.Date("2018-02-04"), y = 20.5),
             colour = "black", label.size = 0) +
  # Other aesthetics
  scale_colour_manual(name = "Line colours", values = lineCol,
                      breaks = c("Temperature", "Climatology", "Threshold",
                                 "2x Threshold", "3x Threshold", "4x Threshold")) +
  scale_fill_manual(name = "Category", values = fillCol, breaks = c("Moderate", "Strong", "Severe", "Extreme")) +
  scale_x_date(expand = c(0, 0), date_labels = "%b %Y", 
               breaks = c(as.Date("2018-02-01"), as.Date("2018-03-01")),
               limits = c(as.Date("2018-01-10"), as.Date("2018-03-15"))) +
  scale_y_continuous(limits = c(18, 28), expand = c(0, 0), breaks = seq(20, 26, by = 2)) +
  guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid",
                                                                "dashed", "dotdash", "dotted"),
                                                   size = c(1, 1, 1, 1, 1, 1)))) +
  labs(y = "Temperature (°C)", x = NULL) +
  theme(panel.border = element_rect(colour = "black", fill = NA))
# fig_1
ggsave("graph/MCS/fig_1.png", fig_1, width = 12, height = 6)
ggsave("graph/MCS/fig_1.pdf", fig_1, width = 12, height = 6)


# Figure 2 ----------------------------------------------------------------

# One of the most widely published MCS is that which occurred off Florida in 2003
FL_bound <- c(26, 36, -84, -72)
FL_data <- load_MCS_ALL(FL_bound)

# Atlantic Ocean cold blob 2014 - 2016 under Greenland
AO_bound <- c(43, 65, -50, -7)
AO_data <- load_MCS_ALL(AO_bound)

# Australia southern reef
OZ_bound <- c(-26, -22, 150, 155)
OZ_data <- load_MCS_ALL(OZ_bound)

# Mediterranean
MD_bound <- c(0, 27, 31, 45)
MD_data <- load_MCS_ALL(MD_bound)

# California current
CC_bound <- c(38, 48, -132, -124)
CC_data <- load_MCS_ALL(CC_bound)

# Taiwan Strait
TS_bound <- c(22, 26, 116, 122)
TS_data <- load_MCS_ALL(TS_bound)

# TO DO: Consider searching for the day that has the hgighest total max intensity pixels

# testers...
# date_range <- c("2014-01-01", "2016-12-31")
# intensity_choice <- "cumulative"
Hobday_Fig_3_MCS <- function(MCS_data, date_range, intensity_choice = "max", line_legend = "none"){
  
  # Find the most intense point
  if(intensity_choice == "max"){
    centre_point <- MCS_data$clim_data %>% 
      mutate(anom = temp - seas) %>% 
      filter(t >= date_range[1],
             t <= date_range[2]) %>% 
      filter(anom == min(anom))
    # centre_date <- centre_point$t
  } else if(intensity_choice == "cumulative"){
    centre_point <- MCS_data$event_data %>% 
      filter(date_start >= date_range[1],
             date_end <= date_range[2]) %>% 
      filter(intensity_cumulative == min(intensity_cumulative))
    # centre_date <- centre_point$date_peak
  }
  
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
    filter(t == centre_dates$date_peak) %>% 
    mutate(anom = temp - seas) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_tile(aes(fill = anom)) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
    geom_label(aes(x = min(lon), y = max(lat), label = centre_dates$date_peak), hjust = 0, vjust = 1, size = 6) +
    geom_point(data = centre_point, aes(x = lon, y = lat), shape = 21, fill = "yellow", size = 3) +
    coord_quickmap(expand = F, xlim = range(MCS_data$clim_data$lon), ylim = range(MCS_data$clim_data$lat)) +
    scale_fill_gradient2(low = "blue", high = "red") +
    labs(x = NULL, y = NULL, fill = "SSTa (°C)") +
    theme(panel.border = element_rect(colour = "black", fill = NA))
  # mf
  
  # Event line figure
  el <- MCS_data$clim_data %>% 
    filter(lon == centre_point$lon[1],
           lat == centre_point$lat[1],
           t >= centre_dates$date_start-30,
           t <= centre_dates$date_end+30) %>% 
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
    guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid", "dashed", "dotdash", "dotted"),
                                                     size = c(1, 1, 1, 1, 1, 1)))) +
    labs(y = expression(paste("Temperature (°C)")), x = NULL) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          legend.position = line_legend)
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

# Atlantic Ocean cold blob of 2014 - 2016
AO_blob <- Hobday_Fig_3_MCS(AO_data, c("2014-01-01", "2016-12-31"), intensity_choice = "cumulative", line_legend = "right")
ggsave("graph/MCS/AO_blob.png", AO_blob, height = 14, width = 5)

# Australia event
OZ_reef <- Hobday_Fig_3_MCS(OZ_data, c("2003-01-01", "2003-12-31"), intensity_choice = "cumulative")
ggsave("graph/MCS/OZ_reef.png", OZ_reef, height = 14, width = 5)

# California current
CC_coast <- Hobday_Fig_3_MCS(CC_data, c("2003-01-01", "2003-12-31"), intensity_choice = "max")
ggsave("graph/MCS/CC_coast.png", CC_coast, height = 14, width = 5)

# Taiwan Strait
TS_coast <- Hobday_Fig_3_MCS(TS_data, c("2007-01-01", "2008-12-31"), intensity_choice = "max")
ggsave("graph/MCS/TS_coast.png", TS_coast, height = 14, width = 5)

# Combine the three notorious MCS multi-panel figures
fig_2 <- ggarrange(TS_coast, FL_2003_summer, AO_blob, ncol = 3, nrow = 1, labels = c("a)", "b)", "c)"))
ggsave("graph/MCS/fig_2.png", fig_2, height = 14, width = 15)
ggsave("graph/MCS/fig_2.pdf", fig_2, height = 14, width = 15)


# Figure 3 ----------------------------------------------------------------
# Maps of the mean metrics

# Load all results into one brick
MCS_count_trend <- plyr::ldply(MCS_count_trend_files, readRDS, .parallel = T)
unique(MCS_count_trend$name)

# Figures of trends and annual states
fig_3_func <- function(var_name, mean_plot = T){
  
  # Basic filter
  df <- MCS_count_trend %>% 
    filter(name == var_name,
           lat >= -70, lat <= 70)
  
  # Significant results
  df_p <- df %>% 
    filter(p.value <= 0.05)
  
  # Find 10th and 90th quantiles to round off tails for plotting
  value_q10 <- quantile(df$value, 0.1)
  value_q90 <- quantile(df$value, 0.9)
  slope_q10 <- quantile(df$slope, 0.1)
  slope_q90 <- quantile(df$slope, 0.9)
  
  if(var_name == "total_count"){
    viridis_choice <- "A"
  } else if(var_name == "dur_mean"){
    viridis_choice <- "B"
  } else{
    viridis_choice <- "D"
  }
  
  if(mean_plot){
    # The mean value map
    map_res <- df %>% 
      mutate(value = case_when(value <= value_q10 ~ value_q10,
                               value >= value_q90 ~ value_q90,
                               TRUE ~ value)) %>% 
      ggplot(aes(x = lon, y = lat)) +
      geom_raster(aes(fill = value)) +
      geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
      scale_fill_viridis_c(paste0(var_name,"\n(annual)"), option = viridis_choice) +
      coord_quickmap(expand = F, ylim = c(-70, 70)) +
      theme_void() +
      theme(panel.border = element_rect(colour = "black", fill = NA))
    # mean_map 
  } else{
    # The trend map
    map_res <- df %>% 
      mutate(slope = case_when(slope <= slope_q10 ~ slope_q10,
                               slope >= slope_q90 ~ slope_q90,
                               TRUE ~ slope)) %>% 
      ggplot(aes(x = lon, y = lat)) +
      geom_raster(aes(fill = slope)) +
      geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
      scale_fill_gradient2(paste0(var_name,"\n(annual)"), low = "blue", high = "red") +
      coord_quickmap(expand = F, ylim = c(-70, 70)) +
      theme_void() +
      theme(panel.border = element_rect(colour = "black", fill = NA))
    # trend_map 
  }
  map_res
}

fig_3a <- fig_3_func("total_count")
fig_3b <- fig_3_func("dur_mean")
fig_3c <- fig_3_func("i_max_mean")
fig_3d <- fig_3_func("i_cum_mean")

fig_3 <- ggpubr::ggarrange(fig_3a, fig_3b, fig_3c, fig_3d, ncol = 2, nrow = 2, 
                           align = "hv", labels = c("a)", "b)", "c)", "d)"))
ggsave("graph/MCS/fig_3.png", fig_3, height = 7, width = 16)
ggsave("graph/MCS/fig_3.pdf", fig_3, height = 7, width = 16)


# Figure 4 ----------------------------------------------------------------
# Maps of the trends in the metrics

fig_4a <- fig_3_func("total_count", mean_plot = F)
fig_4b <- fig_3_func("dur_mean", mean_plot = F)
fig_4c <- fig_3_func("i_max_mean", mean_plot = F)
fig_4d <- fig_3_func("i_cum_mean", mean_plot = F)

fig_4 <- ggpubr::ggarrange(fig_4a, fig_4b, fig_4c, fig_4d, ncol = 2, nrow = 2, 
                           align = "hv", labels = c("a)", "b)", "c)", "d)"))
ggsave("graph/MCS/fig_4.png", fig_4, height = 7, width = 16)
ggsave("graph/MCS/fig_4.pdf", fig_4, height = 7, width = 16)


# Figure 5 ----------------------------------------------------------------

MHW_v_MCS <- readRDS("data/MHW_v_MCS.Rds")

fig_5_func <- function(tile_val){
  ggplot(data = MHW_v_MCS, aes(x = lon, y = lat)) +
    geom_tile(aes_string(fill = tile_val)) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
    coord_quickmap(expand = F, ylim = c(-70, 70)) +
    scale_fill_gradient2(low = "blue", high = "red") +
    labs(x = NULL, y = NULL, fill = tile_val) +
    theme_void() +
    theme(panel.border = element_rect(colour = "black", fill = NA))
}

fig_5a <- fig_5_func("count")
fig_5b <- fig_5_func("dur")
fig_5c <- fig_5_func("i_max")
fig_5d <- fig_5_func("i_cum")

fig_5 <- ggpubr::ggarrange(fig_5a, fig_5b, fig_5c, fig_5d, ncol = 2, nrow = 2, labels = c("a)", "b)", "c)", "d)"))
ggsave("graph/MCS/fig_5.png", fig_5, height = 8, width = 16)
ggsave("graph/MCS/fig_5.pdf", fig_5, height = 8, width = 16)


# Figure 6 ----------------------------------------------------------------

# A figure that shows the skewness or kurtosis map of values per pixel
# This then could be spatially correlated with the difference between maximum intensities of MHW-MCS

# Prep SSTa stats
SSTa_stats <- readRDS("data/SSTa_stats.Rds") %>% 
  dplyr::select(lon:anom_kurt) %>% 
  pivot_longer(c(anom_kurt, anom_skew)) %>% 
  mutate(name = case_when(name == "anom_kurt" ~ "kurtosis",
                          name == "anom_skew" ~ "skewness"))

# Find upper skewness and kurtosis quantiles
skew_quants <- SSTa_stats %>% 
  filter(name == "skewness", season == "Total") %>% 
  summarise(q10 = quantile(value, 0.1),
            q90 = quantile(value, 0.9))
kurt_quants <- SSTa_stats %>% 
  filter(name == "kurtosis", season == "Total") %>% 
  summarise(q10 = quantile(value, 0.1),
            q90 = quantile(value, 0.9))

# Map of skewness per pixel
map_skew <- SSTa_stats %>% 
  filter(name == "skewness", season == "Total") %>% 
  mutate(value = case_when(value <= skew_quants$q10 ~ skew_quants$q10,
                           value >= skew_quants$q90 ~ skew_quants$q90,
                           TRUE ~ value)) %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_raster(aes(fill = value)) +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  scale_fill_gradient2("Skewness", low = "blue", high = "red") +
  coord_quickmap(expand = F, ylim = c(-70, 70)) +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill = NA))
map_skew

# Map of kurtosis per pixel
map_kurt <- SSTa_stats %>% 
  filter(name == "kurtosis", season == "Total") %>% 
  mutate(value = case_when(value <= kurt_quants$q10 ~ kurt_quants$q10,
                           value >= kurt_quants$q90 ~ kurt_quants$q90,
                           TRUE ~ value)) %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_raster(aes(fill = value)) +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  scale_fill_gradient2("Kurtosis", low = "blue", high = "red") +
  coord_quickmap(expand = F, ylim = c(-70, 70)) +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill = NA))
map_kurt

# Correlate with MHW-MCS stats
SSTa_stats %>% 
  filter(season == "Total") %>% 
  pivot_wider(names_from = "name", values_from = "value") %>% 
  left_join(MHW_v_MCS) %>% 
  na.omit() %>% 
  summarise(cor_i_max = cor(skewness, i_max))

# Line plot of correlation between skewness and i_max
plot_skew <- SSTa_stats %>% 
  filter(season == "Total") %>% 
  pivot_wider(names_from = "name", values_from = "value") %>% 
  left_join(MHW_v_MCS) %>% 
  na.omit() %>% 
  ggplot(aes(x = skewness, y = i_max)) +
  geom_point(aes(colour = lat), alpha = 0.1) +
  geom_smooth(method = "lm") +
  scale_colour_gradient2(low = "purple", high = "green")
plot_skew

# Line plot of correlation between kurtosis and i_max
plot_kurt <- SSTa_stats %>% 
  filter(season == "Total") %>% 
  pivot_wider(names_from = "name", values_from = "value") %>% 
  left_join(MHW_v_MCS) %>% 
  na.omit() %>% 
  ggplot(aes(x = kurtosis, y = i_max)) +
  geom_point(aes(colour = lat), alpha = 0.1) +
  geom_smooth(method = "lm") +
  scale_colour_gradient2(low = "purple", high = "green")
plot_kurt


fig_6 <- ggpubr::ggarrange(map_skew, map_kurt, plot_skew, plot_kurt, ncol = 2, nrow = 2, labels = c("a)", "b)", "c)", "d)"))
ggsave("graph/MCS/fig_6.png", fig_6, height = 8, width = 16)
ggsave("graph/MCS/fig_6.pdf", fig_6, height = 8, width = 16)


# Figure 7 ----------------------------------------------------------------
# Figures showing what the temperature threshold must be per pixel to reach the four categories

# Function for loading and extracting the lowest MCS threshold per pixel
# lon_step <- 1
MCS_thresh_func <- function(lon_step){
  MCS_df <- readRDS(MCS_lon_files[lon_step])
  MCS_thresh <- MCS_df %>% 
    dplyr::select(-cat) %>% 
    unnest(event) %>% 
    filter(row_number() %% 2 == 1) %>% 
    unnest(event) %>% 
    mutate(diff = thresh - seas,
           thresh_2x = thresh + diff,
           thresh_3x = thresh_2x + diff,
           thresh_4x = thresh_3x + diff) %>% 
    group_by(lon, lat) %>% 
    summarise(thresh_4x = min(thresh_4x, na.rm = T), .groups = "drop")
  rm(MCS_df); gc() 
  return(MCS_thresh)
}

# Load all of the max Cat 4 thresholds
# doParallel::registerDoParallel(cores = 20)
# MCS_thresh <- plyr::ldply(1:1440, MCS_thresh_func, .parallel = T, .paropts = c(.inorder = FALSE))
# saveRDS(MCS_thresh, "data/MCS_thresh.Rds")
MCS_thresh <- readRDS("data/MCS_thresh.Rds")

# Do the same for MHWs
MHW_thresh_func <- function(lon_step){
  MHW_thresh <- tidync::tidync(dir("../data/thresh/", full.names = T)[lon_step]) %>% 
    tidync::hyper_tibble() %>% 
    mutate(diff = thresh - seas,
           thresh_2x = thresh + diff,
           thresh_3x = thresh_2x + diff,
           thresh_4x = thresh_3x + diff) %>% 
    group_by(lon, lat) %>% 
    summarise(thresh_4x = max(thresh_4x, na.rm = T), .groups = "drop")
}

# Load all of the max Cat 4 thresholds
doParallel::registerDoParallel(cores = 50)
MHW_thresh <- plyr::ldply(1:1440, MHW_thresh_func, .parallel = T, .paropts = c(.inorder = FALSE))

# Map of the Cat 4 thresholds for the MCSs
map_MCS_thresh <- MCS_thresh %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_raster(aes(fill = thresh_4x)) +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  geom_contour(aes(z = thresh_4x), colour = "black", breaks = c(-1.8, 35)) +
  scale_fill_gradient2("IV threshold", low = "blue", high = "red") +
  coord_quickmap(expand = F, ylim = c(-70, 70)) +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill = NA))

# Map of the Cat 4 thresholds for the MCSs
map_MHW_thresh <- MHW_thresh %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_raster(aes(fill = thresh_4x)) +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  geom_contour(aes(z = thresh_4x), colour = "black", breaks = c(-1.8, 35)) +
  scale_fill_gradient2("IV threshold", low = "blue", high = "red") +
  coord_quickmap(expand = F, ylim = c(-70, 70)) +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill = NA))

# Combine maps
fig_7 <- ggpubr::ggarrange(map_MCS_thresh, map_MHW_thresh, ncol = 2, nrow = 1, labels = c("a)", "b)"))
ggsave("graph/MCS/fig_7.png", fig_7, height = 4, width = 16)
ggsave("graph/MCS/fig_7.pdf", fig_7, height = 4, width = 16)


# Figure 8 ----------------------------------------------------------------

# The difference between the standard category definition and one corrected for -1.8C

# Extract the barrier island time series from the Florida data
BI_coords <- FL_data$clim_data %>% 
  mutate(anom = temp - seas) %>% 
  filter(anom == min(anom))
BI_data <- FL_data$clim_data %>% 
  filter(lon == BI_coords$lon,
         lat == BI_coords$lat,
         t >= BI_coords$t-100,
         t <= BI_coords$t+100) %>% 
  mutate(diff = thresh - seas,
         thresh_2x = thresh + diff,
         thresh_3x = thresh_2x + diff,
         thresh_4x = thresh_3x + diff) %>% 
  mutate(diff_new = case_when(thresh_4x <= -1.8 ~ -(thresh+1.8)/3, TRUE ~ diff),
         thresh_2x_new = thresh + diff_new,
         thresh_3x_new = thresh_2x_new + diff_new,
         thresh_4x_new = thresh_3x_new + diff_new) 
  

# The top panel: original categories
fig8a <- BI_data  %>% 
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
  scale_y_continuous(limits = c(-10, 30), expand = c(0, 0)) +
  guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid", "dashed", "dotdash", "dotted"),
                                                   size = c(1, 1, 1, 1, 1, 1)))) +
  labs(y = expression(paste("Temperature (°C)")), x = NULL) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "right")
# fig8a

# Bottom panel: the categories corrected for -1.8C
fig8b <- BI_data  %>% 
  ggplot(aes(x = t)) +
  geom_flame(aes(y = thresh, y2 = temp, fill = "Moderate"), n = 5, n_gap = 2) +
  geom_flame(aes(y = thresh_2x_new, y2 = temp, fill = "Strong")) +
  geom_flame(aes(y = thresh_3x_new, y2 = temp, fill = "Severe")) +
  geom_flame(aes(y = thresh_4x_new, y2 = temp, fill = "Extreme")) +
  geom_line(aes(y = thresh_2x_new, col = "2x Threshold"), size = 0.2, linetype = "dashed") +
  geom_line(aes(y = thresh_3x_new, col = "3x Threshold"), size = 0.2, linetype = "dotdash") +
  geom_line(aes(y = thresh_4x_new, col = "4x Threshold"), size = 0.2, linetype = "dotted") +
  geom_line(aes(y = seas, col = "Climatology"), size = 0.6) +
  geom_line(aes(y = thresh, col = "Threshold"), size = 0.6) +
  geom_line(aes(y = temp, col = "Temperature"), size = 0.4) +
  scale_colour_manual(name = "Line colours", values = lineCol,
                      breaks = c("Temperature", "Climatology", "Threshold",
                                 "2x Threshold", "3x Threshold", "4x Threshold")) +
  scale_fill_manual(name = "Category", values = fillCol, breaks = c("Moderate", "Strong", "Severe", "Extreme")) +
  scale_x_date(date_labels = "%b %Y", expand = c(0, 0)) +
  scale_y_continuous(limits = c(-10, 30), expand = c(0, 0)) +
  guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid", "dashed", "dotdash", "dotted"),
                                                   size = c(1, 1, 1, 1, 1, 1)))) +
  labs(y = expression(paste("Temperature (°C)")), x = NULL) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "right")
# fig8b

# Combine and save
fig_8 <- ggpubr::ggarrange(fig8a, fig8b, ncol = 2, nrow = 1, labels = c("a)", "b)"), legend = "bottom", common.legend = T)
ggsave("graph/MCS/fig_8.png", fig_8, height = 4, width = 10)
ggsave("graph/MCS/fig_8.pdf", fig_8, height = 4, width = 10)

