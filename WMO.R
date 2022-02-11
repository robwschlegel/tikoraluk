# WMO
# This script contains the code used to get the numbers for the annual WMO contribution


# Setup -------------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(heatwaveR)

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
# cat_prop

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
# first_n_cum_prop

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


# Panel D) ----------------------------------------------------------------
# Average days of MHWs per pixel
# cat_n_prop

cat_prop_stats <- function(df){
  df %>% 
    group_by(category) %>%
    filter(t == max(t)) %>% 
    ungroup() %>% 
    dplyr::select(t, category, cat_n_prop) %>% 
    mutate(sum = sum(cat_n_prop))
}

# Get MHW stats
cat_prop_stats(MHW_cat_daily_1982)
cat_prop_stats(MHW_cat_daily_2016)
cat_prop_stats(MHW_cat_daily_2020)
cat_prop_stats(MHW_cat_daily_2021)

# Historic summary
total_days <- MHW_total_summary %>% 
  group_by(t) %>% 
  summarise(sum(cat_n_prop))


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


# BAMS text ---------------------------------------------------------------

# Search through old e-mails to retrieve text from correspondence.

