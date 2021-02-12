# WMO
# This script contains the code used to get the numbers for the annual WMO contribution


# Setup -------------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(heatwaveR)

# Load ocean coordinates for finding proportions
load("../MHWapp/metadata/OISST_ocean_coords.Rdata")

# Load 2016, the most intense year
MHW_cat_daily_2016 <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1982-2011_2016.Rds") %>% 
  mutate(first_n_cum_prop = round(first_n_cum/nrow(OISST_ocean_coords), 4),
         cat_prop = round(cat_n/nrow(OISST_ocean_coords), 4))

# Load 2019
MHW_cat_daily_2019 <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1982-2011_2019.Rds") %>% 
  mutate(first_n_cum_prop = round(first_n_cum/nrow(OISST_ocean_coords), 4),
         cat_prop = round(cat_n/nrow(OISST_ocean_coords), 4))

# Load 2020
MHW_cat_daily_2020 <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1982-2011_2020.Rds") %>% 
  mutate(first_n_cum_prop = round(first_n_cum/nrow(OISST_ocean_coords), 4),
         cat_prop = round(cat_n/nrow(OISST_ocean_coords), 4))


# Panel A) ----------------------------------------------------------------
# Map of the highest MHWs per pixel for the year
# This is visual so no stats are needed

# Panel B) ----------------------------------------------------------------
# Daily count of MHWs by category
# cat_prop

cat_prop_stats <- function(df){
  df %>% 
    group_by(category) %>% 
    summarise(mean = mean(cat_prop), .groups = "drop") %>% 
    mutate(sum = sum(mean))
}

# Get stats
cat_prop_stats(MHW_cat_daily_2016)
cat_prop_stats(MHW_cat_daily_2019)
cat_prop_stats(MHW_cat_daily_2020)


# Panel C) ----------------------------------------------------------------
# Overall percent of ocean affected by MHWs
# first_n_cum_prop

first_n_cum_prop_stats <- function(df){
  df %>% 
    group_by(category) %>%
    filter(t == max(t)) %>% 
    ungroup() %>% 
    dplyr::select(t, category, first_n_cum_prop) %>% 
    mutate(sum = sum(first_n_cum_prop))
}

# Get stats
first_n_cum_prop_stats(MHW_cat_daily_2016)
first_n_cum_prop_stats(MHW_cat_daily_2019)
first_n_cum_prop_stats(MHW_cat_daily_2020)


# Panel D) ----------------------------------------------------------------
# Average days of MHWs per pixel
# cat_n_prop

cat_n_prop_stats <- function(df){
  df %>% 
    group_by(category) %>%
    filter(t == max(t)) %>% 
    ungroup() %>% 
    dplyr::select(t, category, cat_n_prop) %>% 
    mutate(sum = sum(cat_n_prop))
}

# Get stats
cat_n_prop_stats(MHW_cat_daily_2016)
cat_n_prop_stats(MHW_cat_daily_2019)
cat_n_prop_stats(MHW_cat_daily_2020)


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

