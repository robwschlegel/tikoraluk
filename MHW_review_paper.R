# MHW_review_paper.R
# The purpose of this script is to house the code used in the ARMS MHW review paper


# Setup -------------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(heatwaveR)

# Set line colours
lineCol <- c(
  "Temperature" = "black",
  "Climatology" = "darkblue",
  "Threshold" = "darkgreen",
  "2x Threshold" = "darkgreen",
  "3x Threshold" = "darkgreen",
  "4x Threshold" = "darkgreen"
)

# Set category fill colours
fillCol <- c(
  "All" = "salmon",
  "Top" = "red",
  "Moderate" = "#ffc866",
  "Strong" = "#ff6900",
  "Severe" = "#9e0000",
  "Extreme" = "#2d0000"
)


# Figure 2 ----------------------------------------------------------------

# This figure shows multiple panels side by side to allow the reader to visually
# compare the different extreme value detection methods

# Absolute threshold
fig2A <- ggplot(data = sst_WA, aes(x = t)) +
  geom_flame(aes(y = temp, y2 = quantile(temp, 0.9)), fill = "salmon", n = 1, n_gap = 2, show.legend = F) +
  geom_flame(aes(y = temp, y2 = 25), fill = "red", n = 1, n_gap = 2, show.legend = F) +
  # geom_flame(data = mhw_top, aes(y = temp, y2 = 25, fill = "top"),  show.legend = T) +
  geom_line(aes(y = temp, colour = "Temperature")) +
  geom_line(aes(y = 25, colour = "Threshold"), size = 1.0, linetype = "dashed") +
  geom_line(aes(y = quantile(temp, 0.9), colour = "Threshold"), size = 1.0) +
  geom_line(aes(y = mean(temp), colour = "Climatology"), size = 1.2) +
  geom_point(data = filter(sst_WA, temp == max(sst_WA$temp)), aes(y = temp),
             size = 2, colour = "darkmagenta") +
  scale_colour_manual(name = "Line Colour", values = lineCol) +
  scale_fill_manual(name = "Event Colour", values = fillCol) +
  scale_x_date(date_labels = "%b %Y", expand = c(0, 0),
               limits = c(as.Date("2010-10-02"), as.Date("2011-06-30"))) +
  scale_y_continuous(limits = c(18, 32), expand = c(0, 0),
                     breaks = seq(20, 30, by = 5)) +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL)
fig2A

# Hobday definition
ts_Hobday <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-01-01"))
ts_Hobday_top <- ts_Hobday %>% 
  slice(10585:10696)

fig2B <- ggplot(data = ts_Hobday, aes(x = t)) +
  geom_flame(aes(y = temp, y2 = thresh, fill = "All"), n = 5, n_gap = 2, show.legend = F) +
  geom_flame(data = ts_Hobday_top, aes(y = temp, y2 = thresh, fill = "Top"),  show.legend = F) +
  geom_line(aes(y = temp, colour = "Temperature")) +
  geom_line(aes(y = thresh, colour = "Threshold"), size = 1.0) +
  geom_line(aes(y = seas, colour = "Climatology"), size = 1.2) +
  geom_point(data = filter(sst_WA, temp == max(sst_WA$temp)), aes(y = temp),
             size = 2, colour = "darkmagenta") +
  scale_colour_manual(name = "Line Colour", values = lineCol) +
  scale_fill_manual(name = "Event Colour", values = fillCol) +
  scale_x_date(date_labels = "%b %Y", expand = c(0, 0),
               limits = c(as.Date("2010-10-02"), as.Date("2011-06-30"))) +
  scale_y_continuous(limits = c(18, 32), expand = c(0, 0),
                     breaks = seq(20, 30, by = 5)) +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL) +
  # formatting for multi-panel figure
  labs(y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
fig2B

# Froehlicher definition
ts_Froehlicher <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-01-01"), pctile = 99)

fig2C <- ggplot(data = ts_Froehlicher, aes(x = t)) +
  geom_flame(aes(y = temp, y2 = thresh, fill = "All"), show.legend = F) +
  # geom_flame(data = ts_Hobday_top, aes(y = temp, y2 = thresh, fill = "top"),  show.legend = T) +
  geom_line(aes(y = temp, colour = "Temperature")) +
  geom_line(aes(y = thresh, colour = "Threshold"), size = 1.0) +
  geom_line(aes(y = seas, colour = "Climatology"), size = 1.2) +
  geom_point(data = filter(sst_WA, temp == max(sst_WA$temp)), aes(y = temp),
             size = 2, colour = "darkmagenta") +
  scale_colour_manual(name = "Line Colour", values = lineCol) +
  scale_fill_manual(name = "Event Colour", values = fillCol) +
  scale_x_date(date_labels = "%b %Y", expand = c(0, 0),
               limits = c(as.Date("2010-10-02"), as.Date("2011-06-30"))) +
  scale_y_continuous(limits = c(18, 32), expand = c(0, 0),
                     breaks = seq(20, 30, by = 5)) +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL) +
  # formatting for multi-panel figure
  labs(y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
fig2C

# Hobday categories
ts_Hobday_cat <- ts_Hobday %>%
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff) #%>% 
  # dplyr::slice(10580:10690)

fig2D <- ggplot(data = ts_Hobday_cat, aes(x = t, y = temp)) +
  geom_flame(aes(y2 = thresh, fill = "Moderate"), n = 5, n_gap = 2, show.legend = F) +
  geom_flame(aes(y2 = thresh_2x, fill = "Strong"), show.legend = F) +
  geom_flame(aes(y2 = thresh_3x, fill = "Severe"), show.legend = F) +
  geom_flame(aes(y2 = thresh_4x, fill = "Extreme"), show.legend = F) +
  geom_line(aes(y = thresh_2x, col = "2x Threshold"), size = 1.0, linetype = "dashed") +
  geom_line(aes(y = thresh_3x, col = "3x Threshold"), size = 1.0, linetype = "dotdash") +
  geom_line(aes(y = thresh_4x, col = "4x Threshold"), size = 1.0, linetype = "dotted") +
  geom_line(aes(y = seas, col = "Climatology"), size = 1.2) +
  geom_line(aes(y = thresh, col = "Threshold"), size = 1.0) +
  geom_line(aes(y = temp, col = "Temperature")) +
  geom_point(data = filter(sst_WA, temp == max(sst_WA$temp)), aes(y = temp),
             size = 2, colour = "darkmagenta") +
  scale_colour_manual(name = "Line colour", values = lineCol,
                      breaks = c("Temperature", "Climatology", "Threshold",
                                 "2x Threshold", "3x Threshold", "4x Threshold")) +
  scale_fill_manual(name = "Event colour", values = fillCol) +
  scale_x_date(date_labels = "%b %Y", expand = c(0, 0),
               limits = c(as.Date("2010-10-02"), as.Date("2011-06-30"))) +
  scale_y_continuous(limits = c(18, 32), expand = c(0, 0),
                     breaks = seq(20, 30, by = 5)) +
  guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid",
                                                                "dashed", "dotdash", "dotted")))) +
  labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL) +
  # formatting for multi-panel figure
  labs(y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
fig2D

# Combine all panels
fig2 <- ggpubr::ggarrange(fig2A, fig2B, fig2C, fig2D,
                          labels = c("A)", "B)", "C)", "D)"),
                          ncol = 4, nrow = 1, common.legend = T, legend = "bottom", align = "v")
fig2
ggsave("graph/MHW_review_fig2.png", fig2, height = 4, width = 16)
ggsave("graph/MHW_review_fig2.pdf", fig2, height = 4, width = 16)


# Figure 2 winter version -------------------------------------------------

# Find the categories of events as this also provides the season of occurrence
sst_WA_cats <- category(detect_event(ts_Hobday))
sst_Med_cats <- category(detect_event(ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31"))), S = F)
sst_NWA_cats <- category(detect_event(ts2clm(sst_NW_Atl, climatologyPeriod = c("1983-01-01", "2012-12-31"))), S = F)

# It looks like we want event 50 from the NWA
sst_NWA_clim <- detect_event(ts2clm(sst_NW_Atl, climatologyPeriod = c("1983-01-01", "2012-12-31")))$climatology
sst_NWA_events <- detect_event(ts2clm(sst_NW_Atl, climatologyPeriod = c("1983-01-01", "2012-12-31")))$event

# Absolute threshold
figW2A <- ggplot(data = sst_NWA_clim, aes(x = t)) +
  geom_flame(aes(y = temp, y2 = quantile(temp, 0.9)), fill = "salmon", n = 1, n_gap = 2, show.legend = F) +
  geom_flame(aes(y = temp, y2 = 17), fill = "red", n = 1, n_gap = 2, show.legend = F) +
  geom_line(aes(y = temp, colour = "Temperature")) +
  geom_line(aes(y = 17, colour = "Threshold"), size = 1.0, linetype = "dashed") +
  geom_line(aes(y = quantile(temp, 0.9), colour = "Threshold"), size = 1.0) +
  geom_line(aes(y = mean(temp), colour = "Climatology"), size = 1.2) +
  geom_point(data = filter(sst_NWA_clim, t == sst_NWA_cats$peak_date[sst_NWA_cats$event_no == 50]),
             aes(y = temp), size = 2, colour = "darkmagenta") +
  scale_colour_manual(name = "Line Colour", values = lineCol) +
  scale_fill_manual(name = "Event Colour", values = fillCol) +
  scale_x_date(date_labels = "%b %Y", expand = c(0, 0),
               limits = c(as.Date("2011-12-02"), as.Date("2012-08-30"))) +
  scale_y_continuous(limits = c(0, 20), expand = c(0, 0),
                     breaks = seq(2, 18, by = 4)) +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL)
figW2A

# Hobday definition
ts_Hobday_top <- sst_NWA_clim[11057:11114]

figW2B <- ggplot(data = sst_NWA_clim, aes(x = t)) +
  geom_flame(aes(y = temp, y2 = thresh), fill = "salmon", n = 5, n_gap = 2, show.legend = F) +
  geom_flame(data = ts_Hobday_top, aes(y = temp, y2 = thresh, fill = "Top"),  show.legend = F) +
  geom_line(aes(y = temp, colour = "Temperature")) +
  geom_line(aes(y = thresh, colour = "Threshold"), size = 1.0) +
  geom_line(aes(y = seas, colour = "Climatology"), size = 1.2) +
  geom_point(data = filter(sst_NWA_clim, t == sst_NWA_cats$peak_date[sst_NWA_cats$event_no == 50]),
             aes(y = temp), size = 2, colour = "darkmagenta") +
  scale_colour_manual(name = "Line Colour", values = lineCol) +
  scale_fill_manual(name = "Event Colour", values = fillCol) +
  scale_x_date(date_labels = "%b %Y", expand = c(0, 0),
               limits = c(as.Date("2011-12-02"), as.Date("2012-08-30"))) +
  scale_y_continuous(limits = c(0, 20), expand = c(0, 0),
                     breaks = seq(2, 18, by = 4)) +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL) +
  # formatting for multi-panel figure
  labs(y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
figW2B

# Froehlicher definition
ts_Froehlicher <- ts2clm(sst_NW_Atl, climatologyPeriod = c("1983-01-01", "2012-01-01"), pctile = 99)

figW2C <- ggplot(data = ts_Froehlicher, aes(x = t)) +
  geom_flame(aes(y = temp, y2 = thresh, fill = "All"), show.legend = F) +
  # geom_flame(data = ts_Hobday_top, aes(y = temp, y2 = thresh, fill = "top"),  show.legend = T) +
  geom_line(aes(y = temp, colour = "Temperature")) +
  geom_line(aes(y = thresh, colour = "Threshold"), size = 1.0) +
  geom_line(aes(y = seas, colour = "Climatology"), size = 1.2) +
  geom_point(data = filter(sst_NWA_clim, t == sst_NWA_cats$peak_date[sst_NWA_cats$event_no == 50]),
             aes(y = temp), size = 2, colour = "darkmagenta") +
  scale_colour_manual(name = "Line Colour", values = lineCol) +
  scale_fill_manual(name = "Event Colour", values = fillCol) +
  scale_x_date(date_labels = "%b %Y", expand = c(0, 0),
               limits = c(as.Date("2011-12-02"), as.Date("2012-08-30"))) +
  scale_y_continuous(limits = c(0, 20), expand = c(0, 0),
                     breaks = seq(2, 18, by = 4)) +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL) +
  # formatting for multi-panel figure
  labs(y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
figW2C

# Hobday categories
ts_Hobday_cat <- sst_NWA_clim %>%
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff) #%>% 
# dplyr::slice(10580:10690)

figW2D <- ggplot(data = ts_Hobday_cat, aes(x = t, y = temp)) +
  geom_flame(aes(y2 = thresh, fill = "Moderate"), n = 5, n_gap = 2, show.legend = F) +
  geom_flame(aes(y2 = thresh_2x, fill = "Strong"), show.legend = F) +
  geom_flame(aes(y2 = thresh_3x, fill = "Severe"), show.legend = F) +
  geom_flame(aes(y2 = thresh_4x, fill = "Extreme"), show.legend = F) +
  geom_line(aes(y = thresh_2x, col = "2x Threshold"), size = 1.0, linetype = "dashed") +
  geom_line(aes(y = thresh_3x, col = "3x Threshold"), size = 1.0, linetype = "dotdash") +
  geom_line(aes(y = thresh_4x, col = "4x Threshold"), size = 1.0, linetype = "dotted") +
  geom_line(aes(y = seas, col = "Climatology"), size = 1.2) +
  geom_line(aes(y = thresh, col = "Threshold"), size = 1.0) +
  geom_line(aes(y = temp, col = "Temperature")) +
  geom_point(data = filter(sst_NWA_clim, t == sst_NWA_cats$peak_date[sst_NWA_cats$event_no == 50]),
             aes(y = temp), size = 2, colour = "darkmagenta") +
  scale_colour_manual(name = "Line colour", values = lineCol,
                      breaks = c("Temperature", "Climatology", "Threshold",
                                 "2x Threshold", "3x Threshold", "4x Threshold")) +
  scale_fill_manual(name = "Event colour", values = fillCol) +
  scale_x_date(date_labels = "%b %Y", expand = c(0, 0),
               limits = c(as.Date("2011-12-02"), as.Date("2012-08-30"))) +
  scale_y_continuous(limits = c(0, 20), expand = c(0, 0),
                     breaks = seq(2, 18, by = 4)) +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid",
                                                                "dashed", "dotdash", "dotted")))) +
  labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL) +
  # formatting for multi-panel figure
  labs(y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
figW2D

# Combine all panels
figW2 <- ggpubr::ggarrange(figW2A, figW2B, figW2C, figW2D,
                          labels = c("A)", "B)", "C)", "D)"),
                          ncol = 4, nrow = 1, common.legend = T, legend = "bottom", align = "v")
figW2
ggsave("graph/MHW_review_figW2.png", figW2, height = 4, width = 16)
ggsave("graph/MHW_review_figW2.pdf", figW2, height = 4, width = 16)

