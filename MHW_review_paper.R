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
  geom_flame(aes(y = temp, y2 = 25, fill = "All"), show.legend = F) +
  # geom_flame(data = mhw_top, aes(y = temp, y2 = 25, fill = "top"),  show.legend = T) +
  geom_line(aes(y = temp, colour = "Temperature")) +
  geom_line(aes(y = 25, colour = "Threshold"), size = 1.0) +
  geom_line(aes(y = mean(temp), colour = "Climatology"), size = 1.2) +
  scale_colour_manual(name = "Line Colour", values = lineCol) +
  scale_fill_manual(name = "Event Colour", values = fillCol) +
  scale_x_date(date_labels = "%b %Y", expand = c(0, 0),
               limits = c(as.Date("2010-10-02"), as.Date("2011-06-30"))) +
  scale_y_continuous(limits = c(19, 30.5), expand = c(0, 0)) +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL) #+
  # formatting for multi-panel figure
  # theme(axis.text.x = element_blank(),
  #       axis.ticks.x = element_blank())
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
  scale_colour_manual(name = "Line Colour", values = lineCol) +
  scale_fill_manual(name = "Event Colour", values = fillCol) +
  scale_x_date(date_labels = "%b %Y", expand = c(0, 0),
               limits = c(as.Date("2010-10-02"), as.Date("2011-06-30"))) +
  scale_y_continuous(limits = c(19, 30.5), expand = c(0, 0)) +
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
  scale_colour_manual(name = "Line Colour", values = lineCol) +
  scale_fill_manual(name = "Event Colour", values = fillCol) +
  scale_x_date(date_labels = "%b %Y", expand = c(0, 0),
               limits = c(as.Date("2010-10-02"), as.Date("2011-06-30"))) +
  scale_y_continuous(limits = c(19, 30.5), expand = c(0, 0)) +
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
  geom_line(aes(y = thresh_2x, col = "2x Threshold"), size = 0.7, linetype = "dashed") +
  geom_line(aes(y = thresh_3x, col = "3x Threshold"), size = 0.7, linetype = "dotdash") +
  geom_line(aes(y = thresh_4x, col = "4x Threshold"), size = 0.7, linetype = "dotted") +
  geom_line(aes(y = seas, col = "Climatology"), size = 0.7) +
  geom_line(aes(y = thresh, col = "Threshold"), size = 0.7) +
  geom_line(aes(y = temp, col = "Temperature"), size = 0.6) +
  scale_colour_manual(name = "Line colour", values = lineCol,
                      breaks = c("Temperature", "Climatology", "Threshold",
                                 "2x Threshold", "3x Threshold", "4x Threshold")) +
  scale_fill_manual(name = "Event colour", values = fillCol) +
  scale_x_date(date_labels = "%b %Y", expand = c(0, 0),
               limits = c(as.Date("2010-10-02"), as.Date("2011-06-30"))) +
  scale_y_continuous(limits = c(19, 30.5), expand = c(0, 0)) +
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
                          ncol = 4, nrow = 1, common.legend = T, align = "v")
fig2
ggsave("graph/MHW_review_fig2.png", height = 4, width = 16)
