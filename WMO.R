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
    dplyr::select(t, category, cat_area_prop) %>% 
    mutate(sum = sum(cat_area_prop))
}

# Get MHW stats
cat_prop_stats(MHW_cat_daily_1982)
cat_prop_stats(MHW_cat_daily_2016)
cat_prop_stats(MHW_cat_daily_2019)
cat_prop_stats(MHW_cat_daily_2020)
cat_prop_stats(MHW_cat_daily_2021)

# Historic summary
total_days <- MHW_total_summary %>% 
  group_by(t) %>% 
  summarise(sum(cat_n_prop))


# Text --------------------------------------------------------------------

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

# 2021 MHW text
"MARINE HEATWAVES
As with heatwaves on land, extreme heat can affect the near-surface layer of the oceans. 
This situation is called a marine heatwave (MHW), and it can cause a range of consequences for marine life and dependent communities. 
Satellite retrievals of sea-surface temperature can be used to monitor MHWs.  
An MHW is categorized here as moderate, strong, severe or extreme (for definitions, see Marine heatwave data).

Much of the ocean experienced at least one ‘strong’ MHW at some point in 2021 (Figure XXa). 
Conspicuously absent are MHWs in the eastern equatorial Pacific Ocean and much of the Southern Ocean. 
The period of sea-ice cover in the Southern Ocean has been extending later into the year.
This is one of the very few parts of the global ocean where marine cold-spells (MCSs) are increasing in duration.
A continuation of MHWs occurring at the end of 2020, the Laptev to Beaufort Sea's experienced particularly intense MHWs from January to April. 
The average daily coverage of the global ocean by MHWs in 2020 was 13% (Figure  XXb).
This percentage is less that of 2020 (16%) and the 2016 peak of 17%. 
The ocean experienced a similar amount of MHWs classified as ‘strong’ (25%) and ‘moderate’ (25%).
In total, 55% of the surface ocean experienced at least one MHW during 2021 (Figure  XXc);
this is less than the percentage of the ocean that experienced MHWs in 2020 (63%) and the 2016 peak (65%).
Note that many of these numbers differ slightly from previous reports because the calculations are now weighted on the surface area of the SST pixels.
Previous reports weighted all pixels the same, which allowed the polar regions to have an outsized weight in the results."

# 2021 MHW text
"MARINE COLD-SPELLS
As with cold-spells on land, extreme cold can affect the near-surface layer of the oceans. 
This situation is called a marine cold-spell (MCS), and it can cause a range of consequences for marine life and dependent communities. 
Satellite retrievals of sea-surface temperature can be used to monitor MCSs.  
An MCS is categorized here the same as MHWs, e.g. moderate, strong, severe or extreme (for definitions, see Marine heatwave data).

The occurrence of MCSs in the global ocean was much less common than MHWs for 2021 (Figure XYa, Figure XXa). 
Almost all MCSs that did occur in 2021 were 'moderate', with the one consistent exception being the eddies that spin off of 
the poleward extensions of western boundary currents (WBC), which are generally classified as 'extreme' events.
The eastern equatorial Pacific Ocean, which experienced almost no MHWs in 2021, was one of the few non-polar regions of the global ocean to see broad MCS coverage.
The average daily coverage of the global ocean by MCSs in 2020 was 3% (Figure  XYb).
This percentage is less that of 2020 (4%) and half that of the 1982 peak of 7%. 
The ocean experienced MCSs predominantly classified as ‘moderate’ (17%) with ‘strong’ (3%) the second highest by some distance.
In total, 22% of the surface of the ocean experienced at least one MCS during 2021 (Figure  XYc);
this is less than percentage of the ocean that experienced MCSs in 2020 (25%) and the 1985 peak (63%).
Note that because MCSs now occur almost exclusively in the polar regions of the ocean, 
the decision to calculate global statistics based on pixels weighted by surface area can have a large effect because polar pixels 
are relatively much smaller than their equatorial counterparts."

# MHW figure caption
"Figure 8. (a) Global map showing the highest MHW category (for definitions, see Marine heatwave data) experienced at each pixel 
over the course of the year (reference period 1982–2011). Light grey indicates that no MHW occurred in a pixel over the entire year; 
(b) Stacked bar plot showing the percentage of the surface of the ocean experiencing an MHW on any given day of the year; 
(c) Stacked bar plot showing the cumulative percentage of the surface of the ocean that experienced an MHW over the year. 
Note: These values are based on when in the year a pixel first experienced its highest MHW category, so no pixel is counted twice.  
Horizontal lines in this figure show the final percentages for each category of MHW; 
(d) Stacked bar plot showing the cumulative number of MHW days averaged over the surface of the ocean.  
Note: This average is calculated by dividing the cumulative sum of MHW days per pixel weighted by the surface area of those pixels.
Source: Robert Schlegel"

# MCS figure caption
## Same as MHW caption

# Addendum
"MARINE HEATWAVE DATA
MHWs are categorized as moderate when the sea-surface temperature (SST) is above the
90th percentile of the climatological distribution for five days or longer; the subsequent
categories are defined with respect to the difference between the SST and the climatological
distribution average: strong, severe, or extreme, if that difference is, respectively, more than
two, three or four times the difference between the 90th percentile and the climatological
distribution average (Hobday et al., 2018).

The baseline used for MHWs is 1982–2011, which is shifted by one year from the standard
normal period of 1981–2010 because the satellite SST series on which it is based starts in
1981.

Hobday, A.J. et al., 2018: Categorizing and Naming Marine Heatwaves. Oceanography, 31(2):
1–13. doi: https://eprints.utas.edu.au/27875/.

NOAA OISST v2: Optimum Interpolation Sea Surface Temperature (OISST):
Banzon, V. et al., 2016: A Long-Term Record of Blended Satellite and in Situ Sea-Surface
Temperature for Climate Monitoring, Modeling and Environmental Studies. Earth System
Science Data, 8(1): 165–176. doi: https://essd.copernicus.org/articles/8/165/2016/."

