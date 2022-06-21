# WMO
# This script contains the code used to get the numbers for the annual WMO contribution
# Also for the decadal 2011-2020 WMO report


# Setup -------------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(heatwaveR)


# Years of interest -------------------------------------------------------

# Load 1982, the first year
MHW_cat_daily_1982 <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1982-2011_1982.Rds")
MCS_cat_daily_1982 <- readRDS("../MHWapp/data/annual_summary/OISST_MCS_cat_daily_1982-2011_1982.Rds")

# Load the most intense year
MHW_cat_daily_2016 <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1982-2011_2016.Rds")
# 1982 is the most intense MCS year

# Load the most coverage year
MCS_cat_daily_1985 <- readRDS("../MHWapp/data/annual_summary/OISST_MCS_cat_daily_1982-2011_1985.Rds")

# Load the most recent low coverage year
MHW_cat_daily_2012 <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1982-2011_2012.Rds")

# Load 2019
MHW_cat_daily_2019 <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1982-2011_2019.Rds")
MCS_cat_daily_2019 <- readRDS("../MHWapp/data/annual_summary/OISST_MCS_cat_daily_1982-2011_2019.Rds")

# Load 2020
MHW_cat_daily_2020 <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1982-2011_2020.Rds")
MCS_cat_daily_2020 <- readRDS("../MHWapp/data/annual_summary/OISST_MCS_cat_daily_1982-2011_2020.Rds")

# Load 2021
MHW_cat_daily_2021 <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1982-2011_2021.Rds")
MCS_cat_daily_2021 <- readRDS("../MHWapp/data/annual_summary/OISST_MCS_cat_daily_1982-2011_2021.Rds")

# Total history
MHW_total_summary <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1982-2011_total.Rds")
MCS_total_summary <- readRDS("../MHWapp/data/annual_summary/OISST_MCS_cat_daily_1982-2011_total.Rds")


# Panel A) ----------------------------------------------------------------
# Map of the highest MHWs per pixel for the year
# This is visual so no stats are needed

# Panel B) ----------------------------------------------------------------
# Daily count of MHWs by category
# cat_area_prop

cat_prop_stats <- function(df){
  df %>% 
    group_by(category) %>% 
    summarise(mean = mean(cat_area_prop), .groups = "drop") %>% 
    mutate(sum = sum(mean))
}

# Get MHW stats
cat_prop_stats(MHW_cat_daily_2016)
cat_prop_stats(MHW_cat_daily_2020)
cat_prop_stats(MHW_cat_daily_2021)

# Get MCS stats
cat_prop_stats(MCS_cat_daily_1982)
cat_prop_stats(MCS_cat_daily_2020)
cat_prop_stats(MCS_cat_daily_2021)

# Historic record
total_daily <- MHW_total_summary %>% 
  group_by(t) %>% 
  summarise(sum(cat_area_prop_mean))


# Panel C) ----------------------------------------------------------------
# Overall percent of ocean affected by MHWs
# first_area_cum_prop

first_cum_prop_stats <- function(df){
  df %>% 
    group_by(category) %>%
    filter(t == max(t)) %>% 
    ungroup() %>% 
    dplyr::select(t, category, first_area_cum_prop) %>% 
    mutate(sum = sum(first_area_cum_prop))
}

# Get MHW stats
first_cum_prop_stats(MHW_cat_daily_2012)
first_cum_prop_stats(MHW_cat_daily_2016)
first_cum_prop_stats(MHW_cat_daily_2020)
first_cum_prop_stats(MHW_cat_daily_2021)

# Get MCS stats
first_cum_prop_stats(MCS_cat_daily_1982)
first_cum_prop_stats(MCS_cat_daily_1985)
first_cum_prop_stats(MCS_cat_daily_2020)
first_cum_prop_stats(MCS_cat_daily_2021)

# Years when there were more cat II than I events
cat_II_over_I <- MHW_total_summary %>% 
  group_by(t) %>% 
  dplyr::select(t, first_area_cum_prop, category) %>% 
  pivot_wider(names_from = category, values_from = first_area_cum_prop) %>% 
  mutate(total_cover = `I Moderate`+`II Strong`+`III Severe`+`IV Extreme`,
         cat_II_over_I = case_when(`II Strong` > `I Moderate` ~ TRUE, TRUE ~ FALSE))

cat_I_over_II <- MCS_total_summary %>% 
  group_by(t) %>% 
  dplyr::select(t, first_area_cum_prop, category) %>% 
  pivot_wider(names_from = category, values_from = first_area_cum_prop) %>% 
  mutate(total_cover = `I Moderate`+`II Strong`+`III Severe`+`IV Extreme`,
         cat_I_over_II = case_when(`I Moderate` > `II Strong` ~ TRUE, TRUE ~ FALSE))


# Panel D) ----------------------------------------------------------------
# Average days of MHWs per pixel
# cat_area_prop

cat_prop_stats <- function(df){
  df %>% 
    group_by(category) %>%
    filter(t == max(t)) %>% 
    ungroup() %>% 
    dplyr::select(t, category, cat_area_cum_prop) %>% 
    mutate(sum = sum(cat_area_cum_prop))
}

# Get MHW stats
cat_prop_stats(MHW_cat_daily_1982)
cat_prop_stats(MHW_cat_daily_2016)
cat_prop_stats(MHW_cat_daily_2019)
cat_prop_stats(MHW_cat_daily_2020)
cat_prop_stats(MHW_cat_daily_2021)

# Get MCS stats
cat_prop_stats(MCS_cat_daily_1982)
cat_prop_stats(MCS_cat_daily_1985)
cat_prop_stats(MCS_cat_daily_2019)
cat_prop_stats(MCS_cat_daily_2020)
cat_prop_stats(MCS_cat_daily_2021)

# Historic summary
total_days <- MHW_total_summary %>% 
  group_by(t) %>% 
  summarise(sum(cat_area_prop))


# WMO text ----------------------------------------------------------------

# 2020
"Much of the ocean experienced at least a 'Strong' MHW at some point throughout 2020 (Figure 7A).
Conspicuously absent are MHWs in the Atlantic Ocean south of Greenland, and in the eastern equatorial Pacific.
The Laptev Sea experienced a particularly intense MHW from June to December. 
Anecdotal reports claim that this was due to an atmospheric heatwave that may have prevented the seasonal formation of ice later in the year.
Another important MHW to note in 2020 was the return of the semi-persistent 'blob v2.0' in the eastern Pacific.
This event is similar in scale to the original 'blob' that has been rigorously studied.24,25
An average of 21% of the ocean was experiencing a MHW on any given day in 2020 (Figure 7B). 
This is an increase from the 20% average of 2019, but less than the 2016 peak of 23%.
Much more of the ocean experienced MHWs classified as 'strong' (45%) than 'moderate' (28%).
In total, 84% of the ocean experienced at least one MHW (Figure 7C), matching the record for 2019, but less than the 2016 peak (88%).
On average, each ocean pixel experienced a total of 77 MHW days (Figure 7D). 
This is greater than 2019 (74) but less than the 2016 peak (83)."

# 2021 text
"Marine heatwaves and cold-spells
In analogy with heatwaves and cold spells on land, marine heatwaves (MHW) and marine cold-spells (MCS) are prolonged periods of 
extreme heat or cold that affect the near-surface layer of the ocean. 
They can have a range of consequences for marine life and dependent communities and MHWs have become more frequent over the 20th Century. 
Satellite retrievals of sea-surface temperature are used to monitor MHWs and MCSs, categorized here as moderate, strong, severe, or
extreme (for definitions, seeMarine heatwave and marine cold spell data).

Much of the ocean experienced at least one 'strong' MHW at some point in 2021 (Figure 7).
Due to the below average sea-surface temperatures associated with a strong La Niña year, MHWs were conspicuously absent in the 
eastern equatorial Pacific Ocean, which was also one of the only regions of the global ocean to see broad MCS coverage.
The Southern Ocean is one of the only areas in which MCSs are increasing in duration.
The Laptev and Beaufort Seas experienced 'severe' and 'extreme' MHWs from January to April 2021.
The ice-edge regions to the east of Greenland (August), north of Svalbard (October), and east of the Ross Sea (December) experienced notable 'extreme' MHWs.
In 2021, almost all MCSs were 'moderate', except in areas of high variability such as the poleward extension of the Gulf Stream.

MHWs in 2021 showed an average daily coverage of 13%, which is less than the record of 17% in 2016 and 16% in 2020.
The most common category of MHW in 2021 was ‘strong’ (28%) for the eighth consecutive year.
Overall 57% of the ocean surface experienced at least one MHW during 2021 (Figure 7c) – less than the record of 65% in 2016 
and the lowest annual coverage since 2012 (57%).

The average daily coverage of the global ocean by MCSs in 2021 was 4% (Figure 8b) –
a lower value than the record high in 1982 (7%) and comparable to 2020 (4%). 
In total, 25% of the ocean surface experienced at least one MCS during 2021 (Figure 8c), 
which is comparable to 2020 (25%), but much less than the 1985 record (63%). 
Of the 25% coverage, most MCS were classified as either ‘moderate’ (20%) or ‘strong’ (4%)."

# MHW figure caption
"Figure 7: (a) Global map showing the highest MHW category (for definitions, see Marine heatwave and marine cold-spell data) 
experienced at each pixel over 2021 (reference period 1982–2011). 
Light grey indicates that no MHW occurred in a pixel over the entire year; 
(b) Stacked bar plot showing the percentage of the surface of the ocean experiencing an MHW on any given day of the year; 
(c) Stacked bar plot showing the cumulative percentage of the surface of the ocean that experienced an MHW over the year. 
Note: These values are based on when in the year a pixel first experienced its highest MHW category, so no pixel is counted twice.
Horizontal lines in this figure show the final percentages for each category of MHW; 
d) Stacked bar plot showing the cumulative number of MHW days averaged over the surface of the ocean. 
Note: This average is calculated by dividing the cumulative sum of MHW days per pixel weighted by the surface area of those pixels.
Data are from NOAA OISST. Source: Robert Schlegel"

# MCS figure caption
"Figure 8: as for Figure 7 but showing marine cold-spells (MCSs) rather than marine heatwaves (MHWs). Data are from NOAA OISST.
Source: Robert Schlegel"

# Addendum
"Marine heatwave and marine cold-spell data
MHWs are categorized as moderate when the sea-surface temperature (SST) is above the 90th
percentile of the climatological distribution for five days or longer; the subsequent categories are
defined with respect to the difference between the SST and the climatological distribution average:
strong, severe, or extreme, if that difference is, respectively, more than two, three or four times the
difference between the 90th percentile and the climatological distribution average (Hobday et al.,
2018). MCS categories are analogous but counting days below the 10th percentile.

The baseline used for MHWs and MCSs is 1982–2011, which is shifted by one year from the
standard normal period of 1981–2010 because the first full year of the satellite SST series on which
it is based is 1982.

Hobday, A.J. et al., 2018: Categorizing and Naming Marine Heatwaves. Oceanography, 31(2):
1–13. doi: https://eprints.utas.edu.au/27875/.

NOAA OISST v2: Optimum Interpolation Sea Surface Temperature (OISST):

Banzon, V. et al., 2016: A Long-Term Record of Blended Satellite and in Situ Sea-Surface
Temperature for Climate Monitoring, Modeling and Environmental Studies. Earth System
Science Data, 8(1): 165–176. doi: https://essd.copernicus.org/articles/8/165/2016/."

# References
"Hobday, A. J., Alexander, L. V., Perkins, S. E., Smale, D. A., Straub, S. C., Oliver, E. C., ... & Wernberg, T. (2016). 
A hierarchical approach to defining marine heatwaves. Progress in Oceanography, 141, 227-238."

"Hobday, A. J., Oliver, E. C., Gupta, A. S., Benthuysen, J. A., Burrows, M. T., Donat, M. G., ... & Smale, D. A. (2018). 
Categorizing and naming marine heatwaves. Oceanography, 31(2), 162-173."

"Huang, B., Liu, C., Banzon, V., Freeman, E., Graham, G., Hankins, B., ... & Zhang, H. M. (2021). 
Improvements of the daily optimum interpolation sea surface temperature (DOISST) version 2.1. Journal of Climate, 34(8), 2923-2939."


# Decadal report ----------------------------------------------------------

# Total annual data
MHW_total_summary <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1982-2011_total.Rds") %>% mutate(type = "MHW")
MCS_total_summary <- readRDS("../MHWapp/data/annual_summary/OISST_MCS_cat_daily_1982-2011_total.Rds") %>% mutate(type = "MCS")
total_summary <- rbind(MHW_total_summary, MCS_total_summary) %>% filter(t <= 2020) 

# Historic records
total_daily_cover <- total_summary %>% group_by(type,t) %>% 
  summarise(average_days = sum(cat_area_cum_prop), .groups = "drop")
total_percent_daily_cover <- total_summary %>% group_by(type, t) %>% 
  summarise(average_percent_cover = sum(cat_area_prop_mean), .groups = "drop")
total_percent_cover <- total_summary %>% group_by(type, t) %>% 
  summarise(average_percent_cover = sum(first_area_cum_prop), .groups = "drop")

# Years when there were more cat II than I events
cat_II_over_I <- MHW_total_summary %>% group_by(t) %>% 
  dplyr::select(t, first_area_cum_prop, category) %>% 
  pivot_wider(names_from = category, values_from = first_area_cum_prop) %>% 
  mutate(total_cover = `I Moderate`+`II Strong`+`III Severe`+`IV Extreme`,
         cat_II_over_I = case_when(`II Strong` > `I Moderate` ~ TRUE, TRUE ~ FALSE))
cat_I_over_II <- MCS_total_summary %>% group_by(t) %>% 
  dplyr::select(t, first_area_cum_prop, category) %>% 
  pivot_wider(names_from = category, values_from = first_area_cum_prop) %>% 
  mutate(total_cover = `I Moderate`+`II Strong`+`III Severe`+`IV Extreme`,
         cat_I_over_II = case_when(`I Moderate` > `II Strong` ~ TRUE, TRUE ~ FALSE))

# Base decadal df
decadal_base <- total_summary %>% 
  mutate(dec = case_when(t %in% 1982:1990 ~ 1, t %in% 1991:2000 ~ 2,
                         t %in% 2001:2010 ~ 3, t %in% 2011:2020 ~ 4)) %>% 
  dplyr::rename(daily_cover = cat_area_cum_prop, percent_daily_cover = cat_area_prop_mean, percent_cover = first_area_cum_prop) %>% 
  dplyr::select(type, dec, t, category, daily_cover, percent_daily_cover, percent_cover) %>% 
  mutate(percent_daily_cover = round(percent_daily_cover*100, 2),
         percent_cover = percent_cover*100) %>% 
  pivot_longer(cols = c(daily_cover, percent_daily_cover, percent_cover), names_to = "var_name")

# Decadal min, mean, max by category and total
decadal_cat <- decadal_base %>% group_by(type, dec, category, var_name) %>% 
  summarise(min = min(value), mean = mean(value), max = max(value), .groups = "drop")

# Decadal min, mean, max total
decadal_sum <- decadal_base %>% group_by(type, dec, t, var_name) %>% 
  summarise(value = sum(value), .groups = "drop") %>% 
  group_by(type, dec, var_name) %>% 
  summarise(min = min(value), mean = mean(value), max = max(value), .groups = "drop")

# Bar plots of change in time of the usual stats, but a bar for each decade
  # See Figure 1 in previous WMO decadal report
  # Custom legend with text in the middle and the squares of colour to the left and right
  # Use different cat names to allow stacked bar plots with MHW and MCS next to each other
  # Or rather use positive and negative y-axis to show MCS below MHW on the same years
  # Put years on 0 axis as labels

# The MHW+MCS category colour palettes
MHW_colours <- c("I Moderate" = "#ffc866", "II Strong" = "#ff6900",  "III Severe" = "#9e0000", "IV Extreme" = "#2d0000")
MCS_colours <- c("I Moderate" = "#C7ECF2", "II Strong" = "#85B7CC", "III Severe" = "#4A6A94", "IV Extreme" = "#111433")
event_colours <- c("MHW - I Moderate" = "#ffc866", "MHW - II Strong" = "#ff6900",
                     "MHW - III Severe" = "#9e0000", "MHW - IV Extreme" = "#2d0000",
                     "MCS - I Moderate" = "#C7ECF2", "MCS - II Strong" = "#85B7CC",
                     "MCS - III Severe" = "#4A6A94", "MCS - IV Extreme" = "#111433")

# Prep dfs for plotting
df_bar_cat <- decadal_cat %>% 
  dplyr::select(-min, -max) %>% 
  pivot_wider(values_from = mean, names_from = var_name) %>% 
  mutate(cat_type = paste0(type," - ", category),
         daily_cover = case_when(type == "MCS" ~ -daily_cover, TRUE ~ daily_cover),
         percent_cover = case_when(type == "MCS" ~ -percent_cover, TRUE ~ percent_cover))
df_bar_sum <- decadal_sum %>% 
  pivot_longer(cols = c(min, mean, max)) %>% 
  unite(var_stat, var_name, name) %>% 
  mutate(value = case_when(type == "MCS" ~ -value, TRUE ~ value)) %>% 
  pivot_wider(values_from = value, names_from = var_stat)
df_labs <- data.frame(dec = 1:4, labs = c("1982-1990", "1991-2000", "2001-2010", "2011-2020"))
df_colour_palette <- data.frame(category = factor(c("I Moderate", "II Strong", "III Severe", "IV Extreme"),
                                               levels = c("I Moderate", "II Strong", "III Severe", "IV Extreme")),
                                x = 1:4,
                                MHW = c(MHW_colours[1], MHW_colours[2], MHW_colours[3], MHW_colours[4]),
                                MCS = c(MCS_colours[1], MCS_colours[2], MCS_colours[3], MCS_colours[4])) %>% 
  pivot_longer(cols = c(MHW, MCS), names_to = "event", values_to = "colour") %>% 
  mutate(y = case_when(event == "MHW" ~ 1, TRUE ~ 0))

# Create a combo legend
fig_legend <- ggplot(data = df_colour_palette, aes(x = y, y = rev(category))) +
  geom_tile(fill = df_colour_palette$colour) +
  # geom_hline(aes(yintercept = 0.5)) +
  geom_label(aes(x = 0.5, y = rev(x), label = category), size = 6) +
  coord_cartesian(expand = F) +
  labs(x = NULL, y = NULL) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black"))
fig_legend
fig_blank <- ggplot() + geom_blank() + theme(panel.background = element_rect(fill = "white", colour = NA))
fig_blank

# Daily average and daily percent figure
fig_day <- ggplot(data = df_bar_cat, aes(x = dec)) +
  geom_bar(aes(fill = cat_type, y = daily_cover), stat = "identity", show.legend = F,
           position = position_stack(reverse = TRUE), width = 1) +
  geom_errorbar(data = df_bar_sum, width = 0.2,
                aes(group = type, ymin = daily_cover_min, ymax = daily_cover_max)) +
  geom_hline(aes(yintercept = 0)) +
  geom_label(data = df_labs, aes(x = dec, y = 0, label = labs)) + 
  ggpmisc::geom_grob(aes(x = 0, y = 0, label = list(cowplot::as_grob(fig_1_a))), vp.width = 0.7, vp.height = 0.7, 
                     nudge_x = -0.033, add.segments = F)
  # geom_point(data = df_bar_sum, aes(y = daily_cover_mean)) + # The top of the bars
  scale_fill_manual("Category", values = event_colours) +
  scale_y_continuous(limits = c(-27, 65),
                     breaks = c(-20, 20, 40, 60),
                     labels = c("20", "20", "40", "60"),
                     sec.axis = sec_axis(name = paste0("Daily ocean coverage"), 
                                         trans = ~ . + 0,
                                         breaks = c(-21.9, -14.6, -7.3, 7.3, 14.6, 21.9, 29.2, 36.5, 43.8, 51.1, 58.4),
                                         labels = c("6%", "4%", "2%", "2%", "4%", "6%", "8%", "10%", "12%", "14%", "16%"))) +
  # scale_x_continuous(expand = c(0, 0)) +
  # guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  labs(y = paste0("Ocean days"), x = NULL) +
  coord_cartesian(expand = F) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
fig_day

# Stacked barplot of cumulative percent of ocean affected by MHWs
fig_percent <- ggplot(data = df_bar_cat, aes(x = dec)) +
  geom_bar(aes(fill = cat_type, y = percent_cover), stat = "identity", show.legend = F,
           position = position_stack(reverse = TRUE), width = 1) +
  geom_errorbar(data = df_bar_sum, width = 0.2,
                aes(group = type, ymin = percent_cover_min, ymax = percent_cover_max)) +
  geom_hline(aes(yintercept = 0)) +
  geom_label(data = df_labs, aes(x = dec, y = 0, label = labs)) +
  scale_fill_manual("Category", values = event_colours) +
  scale_y_continuous(limits = c(-70, 70),
                     breaks = c(-60, -40, -20, 20, 40, 60),
                     labels = c("60%", "40%", "20%", "20%", "40%", "60%")) +
  # scale_x_continuous(breaks = seq(1984, 2019, 7)) +
  labs(y = paste0("Total ocean coverage"), x = NULL) +
  coord_cartesian(expand = F) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
fig_percent

# Combine and save
fig_panels <- ggpubr::ggarrange(fig_day, fig_percent, align = "hv")
fig_legend_white <- ggpubr::ggarrange(fig_blank, fig_legend, fig_blank, ncol = 1, heights = c(1, 1, 1))
fig_final <- ggpubr::ggarrange(fig_panels, fig_legend_white, nrow = 1, widths = c(6, 1))
fig_final
ggsave("graph/WMO_dec_2020.png", width = 13, height = 7)

# List some noteworthy events:
  # Med - 2003; Western Australia - 2012; NW Atlantic - 2013; Blob - 2015-18; NZ - 20??
"It was during this decade that one may say that the focussed study of MHW and MCS began. 
There was of course a wealth of literature focussing on these extreme events going back at least as far as 
19XX (e.g. XXX et al., XXX), but it was with Hobday et al. (2016, 2018) that a numeric definition for these events
that could produce both locally and globally comparible results started to see widespread use. 
There is some important criticism of this methodology however that should be considered (see Jacox, 20XX). 
Of specific importance to note is that with the Hobday definition of MHW/MCS, the warming signal in the temperature time series is 
not first removed (generally via linear interpolation, but see Wang et al. (2022) for why this is problematic). 
This is one of the primary reasons why one wll note a general increase in all of the following MHW results, and a decrease for MCS.
That being said, there was a series of high profile MHW over 2011-2020. Indeed, the only well documented MHW to have occurred 
in a previous decade was in the Mediterranean in 2003 (Garrabou et al., 20XX), which was driven primarily by the atmospheric heatwave 
(Ortega et al., 20XX) that killed 66,000 people across Europe (WMO deacadal report 2010).
The 2011 Western Austalia MHW can be accredited as having finally clarified the need to create a globally
consistent MHW definition. This event destroyed hundreds of kilometres of coastal kelp forests (Wernberg et al., 20XX),
most of which have remained as a much less productive scrub turf (Wernberg et al., 20XX). 
The 2012 Northwest Atlantic MHW occurred at just the right time of year to drive the centre of the North American lobster 
fishery just far enough north as to cross over an international border,
making it the first ocean climate event on record to cause political tension between two high income nations (XXX et al., XXX). 
Without a doubt the largest MHW to have occurred since record keeping beagn was the 2015-2018 event, appropriately nicknamed 'Th Blob'.
This event covered much of the northeast Pacific and persisted for years, affecting every level in the tophic web (XXX et al., XXX).
More recently the waters around Tasmania have been expriencing regularly occurring MHW and much of the natural ranges of local species
are at risk (XXX et al. XXX). As thw orld continues to warm, it is almost a certain that the following decade will be host to a cast
of newsworthy MHW that far outpaces those of this closing deacde.
Very little can be said of noteworthy MCS from 2011-2020, with the exception of the semi-persistent 'cold blob' 
found in the Altantic ocean below Greenland. It has been posited that this may be a sign of the slowing of the AMOC (XXX et al., XXX)."

# List top ten warmest year stats
"The top three years with the highest average daily count of MHWs were 2016, 2020, and 2019 respectively.
The last eight years of the decade (2013-2020) were all in the top ten highest years of recorded MHW days,
with 2010 (#8) and 1998 (#9) also being noteworthy. A similar pattern was seen for the overall surface area 
per year that experienced a MHW.
Roughly the opposite has been seen for MCS, with only 2011 (#9) and 2010 (#10) being in the top ten of years with highest daily averages, 
which is otherwise populated with years from the beginning of the data record. The curious uptick in global MCS days from roughly 2007 
is due to the ucrrently unstudy increase to the duraiton of MCS in the southern ocean (Schlegel et al., 2022). 
Therefore, while 2010 and 2011 may be in the top ten years with the highest average MCS days, 
the top ten for highest percent cover of the ocean is almost entirely those years from the beginning of recorded data."

# Cat II over cat I observations
"A defining characteristic of MHWs in this decade (2011-2020) has been the emergence of Category II events over Category I,
which has remained a consistent feature from 2014-2020. This had occurred only twice before in the historic record (1998, 2010).
The same cannot  be said for MCSs, for which this has occurred only in the first three years of the available data (1982-1984),
and at no other point in this or the preceeding decade."

# Years with extreme days
"The occurrence of Category IV (Extreme) events was so uncommon in the past that they could hardly be measured on a global scale.
Now they occur frequently enough that in the current decade the ocean experienced an average of 0.5 extreme days per year (25 times that of MCS), 
with a record of 1 full day in 2016. While this may sound like a small value, consider that generally for a MHW to experience 
even one day at Category IV requires a mountain of anomalous temperature, and the occurrence of Category IV events is known
to be able to change entire ecosystems (e.g. Wernberg et al., 20XX). That the ocean is now experiencing so many Category IV MHW days 
that when averaged over the entire surface of the ocean they no longer amount to a miniscule fraction should be a worrying sign.
Indeed, this is exactly what is warned of in Oliver et al. (20XX) when they show in Figure XX the difference in the average daily
MHW categories for RCP 4.5 vs RCP 8.5. Depending on the emmissions scenario that we humans manage to hold ourselves to into the future, 
we will either be seeing very few more Category 4 days (RCP4.5), or they will become dominant (RCP8.5).
In which case it is likely that most ecosystems of the ocean will be forced to change."

# An interesting metric would be the surface area that experienced 100+ MHW days in a year

# References
"Hobday, A. J., Alexander, L. V., Perkins, S. E., Smale, D. A., Straub, S. C., Oliver, E. C., ... & Wernberg, T. (2016). 
A hierarchical approach to defining marine heatwaves. Progress in Oceanography, 141, 227-238."

"Hobday, A. J., Oliver, E. C., Gupta, A. S., Benthuysen, J. A., Burrows, M. T., Donat, M. G., ... & Smale, D. A. (2018). 
Categorizing and naming marine heatwaves. Oceanography, 31(2), 162-173."

"Huang, B., Liu, C., Banzon, V., Freeman, E., Graham, G., Hankins, B., ... & Zhang, H. M. (2021). 
Improvements of the daily optimum interpolation sea surface temperature (DOISST) version 2.1. Journal of Climate, 34(8), 2923-2939."


# BAMS text ---------------------------------------------------------------

# Search through old e-mails to retrieve text from correspondence.

