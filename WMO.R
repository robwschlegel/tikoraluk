# WMO
# This script contains the code used to get the numbers for the annual WMO contribution
# Also for the decadal 2011-2020 WMO report


# Setup -------------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(heatwaveR)
library(ggpmisc)
library(doParallel)


# Download data -----------------------------------------------------------

# Options to help download large chunks of MHW/MCS data

# MHW files
mhw_files <- dir("../data/cat_clim", pattern = "1982-2011.Rda", full.names = TRUE, recursive = TRUE)

# MCS files
mcs_files <- dir("../data/cat_clim/MCS", pattern = "1982-2011.Rds", full.names = TRUE, recursive = TRUE)

# Function that concatenates files into annuals and saves to "extract/"
save_cat_clim_year <- function(year_choice, files, overwrite = FALSE){
  # Subset files by year
  year_files <- files[grepl(paste0("/",year_choice,"/"), files)]
  # Load
  year_res <- plyr::ldply(year_files, read_rds, .parallel = FALSE)
  # Get file segment
  if(grepl("MCS", year_files[1])){
    event_type <- "cat.clim.MCS."
  } else {
    event_type <- "cat.clim.MHW."
  }
  # Save and clear RAM
  if(!file.exists(paste0("extracts/",event_type,year_choice,".Rds")) | overwrite)
    saveRDS(year_res, file = paste0("extracts/",event_type,year_choice,".Rds"))
  rm(year_files, year_res, event_type); gc()
}

# Save so they can be downloaded locally
# NB: Don't run this in parallel
registerDoParallel(cores = 10)
plyr::l_ply(1982:2024, save_cat_clim_year, mhw_files, .parallel = TRUE)
plyr::l_ply(1982:2024, save_cat_clim_year, mcs_files, .parallel = TRUE)


# Years of interest -------------------------------------------------------

# Load 1982, the first year
MHW_cat_daily_1982 <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1982-2011_1982.Rds")
MCS_cat_daily_1982 <- readRDS("../MHWapp/data/annual_summary/OISST_MCS_cat_daily_1982-2011_1982.Rds")

# Load the most intense year
MHW_cat_daily_2016 <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1982-2011_2016.Rds")
# 1982 is the most intense MCS year

# Load the most daily coverage year
MHW_cat_daily_2023 <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1982-2011_2023.Rds")

# Load the most spatial coverage year
MCS_cat_daily_1985 <- readRDS("../MHWapp/data/annual_summary/OISST_MCS_cat_daily_1982-2011_1985.Rds")

# Load the most recent low coverage year
MHW_cat_daily_2012 <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1982-2011_2012.Rds")

# Load previous year
MHW_cat_daily_2022 <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1982-2011_2022.Rds")
MCS_cat_daily_2022 <- readRDS("../MHWapp/data/annual_summary/OISST_MCS_cat_daily_1982-2011_2022.Rds")

# Load current year
MHW_cat_daily_2023 <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1982-2011_2023.Rds")
MCS_cat_daily_2023 <- readRDS("../MHWapp/data/annual_summary/OISST_MCS_cat_daily_1982-2011_2023.Rds")

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
cat_prop_stats(MHW_cat_daily_2022)
cat_prop_stats(MHW_cat_daily_2023)

# Get MCS stats
cat_prop_stats(MCS_cat_daily_1982)
cat_prop_stats(MCS_cat_daily_2022)
cat_prop_stats(MCS_cat_daily_2023)

# Historic record
total_daily_MHW <- MHW_total_summary %>% 
  group_by(t) %>% 
  summarise(daily_average_cover = sum(cat_area_prop_mean))
total_daily_MCS <- MCS_total_summary %>% 
  group_by(t) %>% 
  summarise(daily_average_cover = sum(cat_area_prop_mean))


# Panel C) ----------------------------------------------------------------
# Average days of MHWs per pixel
# cat_area_prop

cat_days_stats <- function(df){
  df %>% 
    group_by(category) %>%
    filter(t == max(t)) %>% 
    ungroup() %>% 
    dplyr::select(t, category, cat_area_cum_prop) %>% 
    mutate(sum = sum(cat_area_cum_prop))
}

# Get MHW stats
cat_days_stats(MHW_cat_daily_1982)
cat_days_stats(MHW_cat_daily_2016)
cat_days_stats(MHW_cat_daily_2022)
cat_days_stats(MHW_cat_daily_2023)

# Get MCS stats
cat_days_stats(MCS_cat_daily_1982)
cat_days_stats(MCS_cat_daily_2022)
cat_days_stats(MCS_cat_daily_2023)

# Historic summary
total_days_MHW <- MHW_total_summary %>% 
  group_by(t) %>% 
  summarise(sum(cat_area_prop))
total_days_MCS <- MCS_total_summary %>% 
  group_by(t) %>% 
  summarise(sum(cat_area_prop))


# Panel D) ----------------------------------------------------------------
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
first_cum_prop_stats(MHW_cat_daily_1982)
first_cum_prop_stats(MHW_cat_daily_2012)
first_cum_prop_stats(MHW_cat_daily_2016)
first_cum_prop_stats(MHW_cat_daily_2022)
first_cum_prop_stats(MHW_cat_daily_2023)

# Get MCS stats
first_cum_prop_stats(MCS_cat_daily_1982)
first_cum_prop_stats(MCS_cat_daily_1985)
first_cum_prop_stats(MCS_cat_daily_2022)
first_cum_prop_stats(MCS_cat_daily_2023)

# Years when there were more cat II than I MHW
cat_II_over_I <- MHW_total_summary %>% 
  group_by(t) %>% 
  dplyr::select(t, first_area_cum_prop, category) %>% 
  pivot_wider(names_from = category, values_from = first_area_cum_prop) %>% 
  mutate(total_cover = `I Moderate`+`II Strong`+`III Severe`+`IV Extreme`,
         cat_II_over_I = case_when(`II Strong` > `I Moderate` ~ TRUE, TRUE ~ FALSE))

# Years when there were more cat I than II MCS
cat_I_over_II <- MCS_total_summary %>% 
  group_by(t) %>% 
  dplyr::select(t, first_area_cum_prop, category) %>% 
  pivot_wider(names_from = category, values_from = first_area_cum_prop) %>% 
  mutate(total_cover = `I Moderate`+`II Strong`+`III Severe`+`IV Extreme`,
         cat_I_over_II = case_when(`I Moderate` > `II Strong` ~ TRUE, TRUE ~ FALSE))

# Historic record
total_area_MHW <- MHW_total_summary %>% 
  group_by(t) %>% 
  summarise(sum(first_area_cum_prop))
total_area_MCS <- MCS_total_summary %>% 
  group_by(t) %>% 
  summarise(sum(first_area_cum_prop))


# WMO text ----------------------------------------------------------------

## 2020 text ---------------------------------------------------------------

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


## 2021 text ---------------------------------------------------------------

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


## 2022 text ---------------------------------------------------------------

"Marine heatwaves and cold-spells
As with heatwaves and cold spells on land, marine heatwaves (MHW) and marine cold-spells (MCS) are prolonged periods of 
extreme heat or cold in seas and oceans. MHWs have generally become more frequent over the 20th Century while MCS have been decreasing.
These events can have a range of consequences for marine life and dependent communities. 
Satellite retrievals of sea-surface temperature are used to operationally monitor MHWs and MCSs globally, 
categorised here as moderate, strong, severe, or extreme (for definitions, seeMarine heatwave and marine cold spell data).

Now the third La Niña year in a row, persistently low sea-surface temperatures in the equatorial Pacific mean an absence of MHW there (Figure 7), 
while simultaneously being one of the only regions of the global ocean to see wide-spread 'strong' MCS coverage (Figure 8).
The Southern Ocean is however the only region in which MCSs are increasing in duration.
This is due to a complex process that is the focus of ongoing research.
The Laptev and Beaufort Seas experienced 'severe' and 'extreme' MHWs over Spring to Autumn of 2022.
This is now a persistent annual phenomenon and is likely a sign that the global tipping point of sea ice melt in the Arctic has been passed.
The ice-edge regions to the north of Svalbard and east of the Ross Sea experienced notable 'extreme' MHW for the second consecutive year.

The global ocean experienced an average daily MHW coverage of 16%, 
which is less than the record of 17% in 2016 but higher than the 2021 average of 13%.
The most common category of MHW in 2022 was ‘moderate’ (27%). 
This is the first time in nine years when the most common MHW category was not 'strong'.
Overall 55% of the ocean surface experienced at least one MHW during 2022 (Figure 7c) – less than the record of 65% in 2016 
and the lowest annual coverage since 2012 (57%).

The average daily coverage of the global ocean by MCSs in 2021 was 5% (Figure 8b).
While this is lower than the record high in 1982 (7%), it is the highest value since 2011 (6%). 
In total, 22% of the ocean surface experienced at least one MCS during 2022 (Figure 8c), 
less than 2021 (25%), and much less than the 1985 record (63%). 
Of the 22% coverage, almost all MCS were classified as either ‘moderate’ (18%) or ‘strong’ (4%)."


## 2023 text ---------------------------------------------------------------

"Marine heatwaves and cold-spells

As with heatwaves and cold-spells on land, marine heatwaves (MHW) and marine cold-spells (MCS) are prolonged periods of 
extreme heat or cold in seas and oceans that can have a range of consequences for marine life and dependent communities. 
MHWs have become more frequent, intense, and longer lasting over the 20th Century, while MCS have been decreasing. 
Satellite retrievals of sea-surface temperature are used to operationally monitor MHWs and MCSs globally, 
categorised here as moderate, strong, severe, extreme, or ice (for definitions, see Marine heatwave and marine cold-spell data).

Coming out of a triple dip La Niña, 2023 saw the rise of the first El Niño in several years. 
While it had been projected to become a strong event, [at the time of writing] by most measures it stopped short of that. 
The occurrence of El Niño events is important for global MHW occurrence as these tend to cause wide-spread events throughout 
the Eastern Tropical Pacific. 
While this region did experience 'strong' MHWs (Figure XX A), they were not as expansive as they have been during previous El Niño years. 
Of particular interest in 2023 was the persistent and wide-spread MHW coverage of the North Atlantic Ocean 
throughout summer and autumn [at time of writing]. 
This has been a curious event because this regions tends not to be driven by ENSO variability, 
meaning that the understanding of why this occurred will likely be an area of focussed research in the future. 
Other noteworthy regions were the Mediterranean Sea, which experienced near complete coverage of 'strong' and 'severe' 
MHWs for the 12th consecutive year, and the waters surrounding New Zealand, which [at the time of writing] 
have remained at a +1-2°C temperature anomaly since January. Leading to a practically unheard of cumulative temperature anomaly of +500°C. 
The occurrence of MCS within +-60° of the equator was practically unheard of (Figure YY A).

The global ocean experienced an average daily MHW coverage of 20% [so far] (Figure XX B), which has shattered the previous record of 17% in 2016. 
When averaged across the surface of the ocean, the global daily total of MHWs [so far] in 2023 was  47 (Figure XX C). 
This is less than 2022 (58 days), and the 2016 record (61 days). 
Overall 63% of the ocean surface [so far] experienced at least one MHW during 2023 (Figure XX D), 
less than the record of 65% in 2016, but much higher than 2022 (58%). 
The most common category of MHW in 2023 was ‘moderate’ (30%), 
making this the second year in a row when the most common MHW category was not 'strong' (2014-2021). 

The average daily coverage of the global ocean by MCSs in 2023 was 2% [so far] (Figure YY B). 
This is much lower than 2022 (5%), and the record high in 1982 (7%). 
The global daily total of MCS in 2023 was 5 days (Figure YY C), far below 2022 (17 days) and the record in 1982 (24 days). 
In total, 14% of the ocean surface experienced at least one MCS during 2023 [so far] (Figure YY D), less than 2022 (25%), 
and much less than the 1985 record (63%). 
Of the 14% coverage, most MCS were classified as either ‘moderate’ (8%) or ‘ice’ (3%)."


## Figure captions ---------------------------------------------------------

"Figure 7: (a) Global map showing the highest MHW category (for definitions, see Marine heatwave and marine cold-spell data) 
experienced at each pixel over 2023 (reference period 1982–2011). Light grey indicates that no MHW occurred in a pixel over the entire year; 
(b) Stacked bar plot showing the percentage of the surface of the ocean experiencing an MHW on any given day of the year; 
(c) Stacked bar plot showing the cumulative number of MHW days averaged over the surface of the ocean. 
Note: This average is calculated by dividing the cumulative sum of MHW days per pixel weighted by the surface area of those pixels. 
(d) Stacked bar plot showing the total percentage of the surface of the ocean that experienced an MHW from 1982 to present.
Data are from NOAA OISST. 
Source: Robert Schlegel"

"Figure 8: as for Figure 7 but showing marine cold-spells (MCSs) rather than marine heatwaves (MHWs). 
Data are from NOAA OISST.
Source: Robert Schlegel"


## Addendum ----------------------------------------------------------------

"Marine heatwave and marine cold-spell data
MHWs are categorized as moderate when the sea-surface temperature (SST) is above the 90th
percentile of the climatological distribution for five days or longer; the subsequent categories are
defined with respect to the difference between the SST and the climatological distribution average:
strong, severe, or extreme, if that difference is, respectively, more than two, three or four times the
difference between the 90th percentile and the climatological distribution average (Hobday et al.,
2018). MCS categories are analogous but counting days below the 10th percentile, with the exception of the ‘ice’ category. 
This category is given to any MCS when the threshold for the occurrence on any given day of the event is below -1.7°C (Schlegel et la., 2021). 
These are therefore considered to be conditions caused by sea-ice properties, and not extreme temperature fluctuations.

The baseline used for MHWs and MCSs is 1982–2011, which is shifted by one year from the
standard normal period of 1981–2010 because the first full year of the satellite SST series on which
it is based is 1982. This period has not been updated to the current standard normal period of 1991-2020 because the shifting 
of the baseline has a significant effect on the results, and would not allow for comparison of MHW/MCS statistics with previous 
versions of this report.

All MHWs and MCSs are detected using the NOAA daily Optimum Interpolation Sea Surface Temperature (OISST) v2.1 dataset (Huang et al. 2021)."


## References --------------------------------------------------------------

"Hobday, A. J., Alexander, L. V., Perkins, S. E., Smale, D. A., Straub, S. C., Oliver, E. C., ... & Wernberg, T. (2016). 
A hierarchical approach to defining marine heatwaves. Progress in Oceanography, 141, 227-238. https://doi.org/10.1016/j.pocean.2015.12.014

Hobday, A. J., Oliver, E. C., Gupta, A. S., Benthuysen, J. A., Burrows, M. T., Donat, M. G., ... & Smale, D. A. (2018). 
Categorizing and naming marine heatwaves. Oceanography, 31(2), 162-173. https://www.jstor.org/stable/26542662

Huang, B., Liu, C., Banzon, V., Freeman, E., Graham, G., Hankins, B., ... & Zhang, H. M. (2021). 
Improvements of the daily optimum interpolation sea surface temperature (DOISST) version 2.1. 
Journal of Climate, 34(8), 2923-2939. https://doi.org/10.1175/JCLI-D-20-0166.1

Schlegel, R. W., Darmaraki, S., Benthuysen, J. A., Filbee-Dexter, K., & Oliver, E. C. (2021). 
Marine cold-spells. Progress in Oceanography, 198, 102684."



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
         percent_cover = case_when(type == "MCS" ~ -percent_cover, TRUE ~ percent_cover)) |> 
  filter(cat_type != "MCS - V Ice")
df_bar_sum <- decadal_sum %>% 
  pivot_longer(cols = c(min, mean, max)) %>% 
  unite(var_stat, var_name, name) %>% 
  mutate(value = case_when(type == "MCS" ~ -value, TRUE ~ value)) %>% 
  pivot_wider(values_from = value, names_from = var_stat)
df_labs <- data.frame(dec = 1:4, labs = c("1982 - 1990", "1991 - 2000", "2001 - 2010", "2011 - 2020"))
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
  geom_label(aes(x = 0.5, y = rev(x), label = category)) +
  coord_cartesian(expand = F) +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(panel.border = element_rect(fill = NA, colour = "black"))
# fig_legend

# Daily average and daily percent figure
fig_day <- ggplot(data = df_bar_cat, aes(x = dec)) +
  geom_bar(aes(fill = cat_type, y = daily_cover), stat = "identity", show.legend = F,
           position = position_stack(reverse = TRUE), width = 1) +
  geom_errorbar(data = df_bar_sum, width = 0.2,
                aes(group = type, ymin = daily_cover_min, ymax = daily_cover_max)) +
  geom_hline(aes(yintercept = 0)) +
  geom_label(data = df_labs, aes(x = dec, y = 0, label = labs)) + 
  geom_grob(aes(x = 1.2, y = 60, label = list(cowplot::as_grob(fig_legend))), vp.width = 0.3, vp.height = 0.3) +
  # geom_point(data = df_bar_sum, aes(y = daily_cover_mean)) + # The top of the bars
  scale_fill_manual("Category", values = event_colours) +
  scale_y_continuous(limits = c(-40, 90),
                     breaks = c(-20, 20, 40, 60, 80),
                     labels = c("20", "20", "40", "60", "80"),
                     sec.axis = sec_axis(name = paste0("Average ocean coverage"), 
                                         trans = ~ . + 0,
                                         breaks = c(seq(-10, 30, 5)/0.365)[c(1,2,4,5,6,7,8,9)],
                                         labels = c("10%", "5%", "5%", "10%", "15%", "20%", "25%", "30%"))) +
  # scale_x_continuous(expand = c(0, 0)) +
  # guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  labs(y = paste0("Average ocean days"), x = NULL) +
  coord_cartesian(expand = F) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
# fig_day

# Stacked barplot of cumulative percent of ocean affected by MHWs
fig_percent <- ggplot(data = df_bar_cat, aes(x = dec)) +
  geom_bar(aes(fill = cat_type, y = percent_cover), stat = "identity", show.legend = F,
           position = position_stack(reverse = TRUE), width = 1) +
  geom_errorbar(data = df_bar_sum, width = 0.2,
                aes(group = type, ymin = percent_cover_min, ymax = percent_cover_max)) +
  geom_hline(aes(yintercept = 0)) +
  geom_label(data = df_labs, aes(x = dec, y = 0, label = labs)) +
  scale_fill_manual("Category", values = event_colours) +
  scale_y_continuous(limits = c(-100, 100),
                     breaks = c(-80, -60, -40, -20, 20, 40, 60, 80),
                     labels = c("80%", "60%", "40%", "20%", "20%", "40%", "60%", "80%")) +
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
# fig_percent

# Combine and save
fig_final <- ggpubr::ggarrange(fig_percent, fig_day, align = "hv", labels = c("a)", "b)"))
# fig_final
ggsave("graph/WMO_dec_2020.png", fig_final, width = 12, height = 6)
ggsave("graph/WMO_dec_2020.eps", fig_final, width = 12, height = 6)

# Figure caption
"Figure XX: The decadal occurrence of marine heatwaves (MHW) and marine cold-spells (MCS) over the global ocean. 
a) The average annual percent of the surface of the global ocean that experienced at least one MHW or MCS. 
b) The globally averaged annual days that the ocean experienced a MHW or MCS (left Y-axis). 
Also expressed as the average percent of the ocean that this could cover for the entire year (right Y-axis). 
All bars show the annual values averaged over the indicated decade, with whiskers showing the highest and lowest annual values. 
The colours in the bars show the proportion of the category of the MHW/MCS for the total cover (a) or global days (b). 
Note that both the MHW (red) and MCS (blue) values are in positive units; MCS values are shown below the X-axis for ease of comparison."

# List some noteworthy events:
"It can be argued that it was during the current decade (2001 - 2010) when the focussed study of marine heatwaves (MHW) and marine cold-spells (MCS) began. 
There was of course a wealth of literature focussing on these extreme events going back at least as far as 1936 (e.g. Storey & Gudger, 1936), 
but it was the publication of Hobday et al. (2016, 2018) when a quantitative and qualitative definition for these events that could produce both 
locally and globally comparable results started to see widespread use. 
There is some important criticism of this methodology that should be considered (see opinion in Jacox, 2019), 
with the most conspicuous being that with the Hobday definition of MHW/MCS, 
the warming signal in the temperature time series is not first removed (generally via linear interpolation, but see Wang et al. (2022)). 
This is one of the primary reasons why one will note a general increase in all of the following MHW results, and a decrease for MCS. 
That being said, there was a series of high profile MHW this decade. 
Indeed, the only well documented MHW to have occurred in a previous decade was in the Mediterranean in 2003 (Garrabou et al., 2009), 
which was driven primarily by the atmospheric heatwave (Olita et al., 2007) that killed 66,000 people across Europe that summer (WMO, 2013). 
The 2011 Western Australia MHW can be credited as having finally clarified the need to create a globally consistent MHW definition. 
This event wreaked havoc on hundreds of kilometres of coastal kelp forests (Wernberg et al., 2013), 
most of which have remained as a much less productive scrub turf since (Wernberg et al., 2016). 
The 2012 Northwest Atlantic MHW occurred at just the right time of year to drive the centre of the North American lobster fishery 
just far enough north as to cross over an international border,
making it the first ocean climate event on record to cause political tension between two high income nations (Mills et al., 2013). 
Without a doubt the largest MHW to have occurred since record keeping began in 1982 was the 2014-2016 event, appropriately nicknamed 'The Blob'. 
This MHW covered much of the northeast Pacific and persisted for years, affecting every level in the trophic web (Cavole et al., 2016). 
More recently the waters around Tasmania have been experiencing regularly recurring MHW, 
putting much of the local flora and fauna at risk (Perkins-Kirkpatrick et al., 2019). 
For a more complete list of high impact MHW, and further insight into the global risks of MHW to biodiversity and ecosystem services, please see Smale et al. (2019). 
As the world continues to warm, it is almost a certainty that the following decade will be host to a cast of newsworthy MHW that far outpaces those of this closing decade. 
Very little can be said of noteworthy MCS from 2011 - 2020, with the exception of the semi-persistent 'cold blob' found in the Atlantic ocean below Greenland. 
It has been posited that this may be a sign of the slowing of the AMOC (Yeager et al., 2016)."

# List top ten warmest year stats
"Over 2011-2020, 60% of the surface of the ocean experienced a MHW on average, with the highest being 65% in 2016 (Figure XXa). 
The top ten years of MHW coverage contain seven years from this decade, and three from the previous (2000 - 2010). 
For MCS, the present decadal average cover is 31.5%, and no years from this or the previous decade are found in the top ten. 
The top three years with the highest average MHW days were in 2016 (61 days), 2020 (58 days), and 2019 (54 days) (Figure XXb). 
The last eight years of the decade (2013-2020) were all in the top ten highest years of average MHW days, in addition to 2010 (36 days) and 1998 (29 days). 
Roughly the opposite has been seen for MCS, with only 2011 (21 days) and 2010 (19 days) being in the top ten of years of highest daily averages, 
which is otherwise populated by years from the beginning of the data record. 
The curious uptick in global MCS days for this decade started in 2007 and is due to an increase in the 
duration (but not intensity) of MCS in the Southern Ocean (Schlegel et al., 2022). 
Therefore, while 2010 and 2011 may be in the top ten years with the highest average MCS days, 
this is why the top ten years for the highest percent cover of the ocean is almost entirely those years from the beginning of recorded data, 
with 1985 (63%) having the most coverage."

# Cat II over cat I observations
"A defining characteristic of MHWs in this decade (2011-2020) has been the emergence of Category II (Strong) events over Category I (Moderate), 
which has remained a consistent feature from 2014-2020. 
This had occurred only twice before in the historic record (1998, 2010). 
The same cannot be said for MCSs, for which this has occurred only in the first three years of the available data (1982-1984), 
and at no other point in this or the preceding decades."

# Years with extreme days
"The occurrence of Category IV (Extreme) events was so uncommon in the past that they could hardly be measured on a global scale. 
Now Category IV MHW occur frequently enough that in the current decade the ocean experienced an average of 0.5 extreme MHW days per year (25 times that of MCS), 
with a record of 1 full day in 2016. While this may sound like a small value, 
consider that generally for a MHW to experience even 1 day at Category IV requires a mountain of anomalous temperature underneath. 
It is also known that the occurrence of Category IV events may be able to change entire ecosystems (e.g. Wernberg et al., 2016; Smale et al., 2019). 
That so many Category IV MHW days are occurring that when averaged over the entire surface of the ocean they no longer amount to a miniscule fraction is a worrying sign. 
Indeed, this is exactly what is warned of in Oliver et al. (2019) when they show in Figure 3 the difference in the average daily MHW categories for RCP 4.5 vs RCP 8.5. 
Depending on the emissions scenario that we humans manage to hold ourselves to, we will either see relatively few more Category IV days (RCP4.5), 
or they will begin their march towards dominance from 2050 onwards (RCP8.5). In which case it is likely that most ecosystems throughout the ocean will be forced to change."

# An interesting metric would be the surface area that experienced 100+ MHW days in a year

# References

"Cavole, L. M., Demko, A. M., Diner, R. E., Giddings, A., Koester, I., Pagniello, C. M., ... & Franks, P. J. (2016). 
Biological impacts of the 2013–2015 warm-water anomaly in the Northeast Pacific: winners, losers, and the future. Oceanography, 29(2), 273-285."

"Garrabou, J., Coma, R., Bensoussan, N., Bally, M., Chevaldonné, P., Cigliano, M., ... & Cerrano, C. (2009). 
Mass mortality in Northwestern Mediterranean rocky benthic communities: effects of the 2003 heat wave. Global change biology, 15(5), 1090-1103."

"Hobday, A. J., Alexander, L. V., Perkins, S. E., Smale, D. A., Straub, S. C., Oliver, E. C., ... & Wernberg, T. (2016). 
A hierarchical approach to defining marine heatwaves. Progress in Oceanography, 141, 227-238."

"Hobday, A. J., Oliver, E. C., Gupta, A. S., Benthuysen, J. A., Burrows, M. T., Donat, M. G., ... & Smale, D. A. (2018). 
Categorizing and naming marine heatwaves. Oceanography, 31(2), 162-173."

"Huang, B., Liu, C., Banzon, V., Freeman, E., Graham, G., Hankins, B., ... & Zhang, H. M. (2021). 
Improvements of the daily optimum interpolation sea surface temperature (DOISST) version 2.1. Journal of Climate, 34(8), 2923-2939."

"Jacox, M. G. (2019). 
Marine heatwaves in a changing climate."

"Mills, K. E., Pershing, A. J., Brown, C. J., Chen, Y., Chiang, F. S., Holland, D. S., ... & Wahle, R. A. (2013). 
Fisheries management in a changing climate: lessons from the 2012 ocean heat wave in the Northwest Atlantic. Oceanography, 26(2), 191-195."

"Olita, A., Sorgente, R., Natale, S., Gaberšek, S., Ribotti, A., Bonanno, A., & Patti, B. (2007). 
Effects of the 2003 European heatwave on the Central Mediterranean Sea: surface fluxes and the dynamical response. Ocean Science, 3(2), 273-289."

"Oliver, E. C., Burrows, M. T., Donat, M. G., Sen Gupta, A., Alexander, L. V., Perkins-Kirkpatrick, S. E., ... & Smale, D. A. (2019). 
Projected marine heatwaves in the 21st century and the potential for ecological impact. Frontiers in Marine Science, 6, 734."

"Perkins-Kirkpatrick, S., King, A. D., Cougnon, E. A., Grose, M. R., Oliver, E. C. J., Holbrook, N., ... & Pourasghar, F. (2019). 
The role of natural variability and anthropogenic climate change in the 2017/18 Tasman Sea marine heatwave."

"Schlegel, R. W., Darmaraki, S., Benthuysen, J. A., Filbee-Dexter, K., & Oliver, E. C. (2021). 
Marine cold-spells. Progress in Oceanography, 198, 102684."

"Smale, D. A., Wernberg, T., Oliver, E. C., Thomsen, M., Harvey, B. P., Straub, S. C., ... & Moore, P. J. (2019). 
Marine heatwaves threaten global biodiversity and the provision of ecosystem services. Nature Climate Change, 9(4), 306-312."

"Storey, M., & Gudger, E. W. (1936). 
Mortality of fishes due to cold at Sanibel Island, Florida, 1886-1936. Ecology, 17(4), 640-648."

"Wang, S., Jing, Z., Sun, D., Shi, J., & Wu, L. (2022). 
A new model for isolating the marine heatwave changes under warming scenarios. Journal of Atmospheric and Oceanic Technology."

"Wernberg, T., Smale, D. A., Tuya, F., Thomsen, M. S., Langlois, T. J., De Bettignies, T., ... & Rousseaux, C. S. (2013). 
An extreme climatic event alters marine ecosystem structure in a global biodiversity hotspot. Nature Climate Change, 3(1), 78-82."

"Wernberg, T., Bennett, S., Babcock, R. C., De Bettignies, T., Cure, K., Depczynski, M., ... & Wilson, S. (2016). 
Climate-driven regime shift of a temperate marine ecosystem. Science, 353(6295), 169-172."

"WMO (2013). 
The Global Climate 2001-2010: a decade of climate extremes. WMO- No. 1103."

"Yeager, S. G., Kim, W. M., & Robson, J. (2016). 
What caused the Atlantic cold blob of 2015. US CLIVAR Variations, 14(2), 24-31."


# BAMS text ---------------------------------------------------------------

## 2023 text----------------------------------------------------------------

"The 2023 analysis of NOAA OISST daily v2.1 (Huang et al. 2021) revealed an ocean where 94% of the surface experienced at least one marine heatwave (MHW; Hobday et al. 2016; Figs. x.y.z.a,b), 
and 27% experienced at least one marine cold-spell (MCS; Fig. x.y.z.c,d). 
The most common MHW category (Hobday et al. 2018) in 2023 was Category 2 Strong (49%), with the coverage of Category 3 Severe events reaching 10%. 
Category 1 Moderate MCSs have remained the most common (16%) cool events in all years since 1987. 
The ocean experienced a global average of 116 MHW days (13 MCS days) in 2023. 
This is far greater than the 2016 MHW record of 86 days (MCS record of 37 days in 1982; Figs. x.y.za,c). 
This equates to a daily average MHW coverage of 32% (4% MCS; Figs. x.y.za,c)."


## 2024 text ---------------------------------------------------------------

"The 2024 analysis of NOAA OISST daily v2.1 (Huang et al. 2021) revealed an ocean where 91% of the surface experienced at least one 
marine heatwave (MHW; Hobday et al. 2016; Figs. x.y. a,b), and 26% experienced at least one marine cold-spell (MCS; Fig. x.y. c,d). 
The most common MHW category (Hobday et al. 2018) in 2024 was Category 2 Strong (46%), with the coverage of Category 3 Severe events reaching 8%. 
Category 1 Moderate MCSs have remained the most common (16%) cool events in all years since 1987. 
The ocean experienced a global average of 100 MHW days (9 MCS days) in 2024. 
This is far greater than the 2016 MHW record of 58 days (MCS record of 55 days in 1982; Figs. x.y. a,c). 
This equates to a daily average MHW coverage of 27% (2% MCS; Figs. x.y. a,c)."

"2024 started off where 2023 ended, hot. With most of the Atlantic Ocean, and the majority of all oceans within ±20° of the equator in a MHW state.
Large patches of the Southern Ocean to the south of Africa and Australia as well.
The MHW in the Atlantic, north of the equator, was a continuation of the basin-scale event that started in January of 2023, 
and has continued nearly unabated for two years through to the end of 2024.
It seems only a matter of time until this event gains a unique name for itself, a la 'The Blob' in the Northeast Pacific.
Similar in size to this North Atlantic event was another, loosely connected just to the south of the equator.
A holdover from 2023, the basin-scale event picked up steam going into March, but finally dissipated in June, 
just as a MHW larger than Western Europe began to develop in the North Atlantic.
This event persisted nearly to the end of the year, though did break up twice, 
potentially signifying different key drivers of the anomalous heat.
From August to late November, the Barents and Kara Sea experienced what has become a perennially re-occurring Category 4 MHW.
The Gulf af Aden, and much of the surrounding waters, experienced a noteworthy MHW in February.
With the Mediterranean Sea reserving its place for special mention, for another year in a row, 
due to the exceptionally intense regional MHWs it experienced from July to September.

Notable MCSs within ±60° of the equator have nearly vanished since the late 90s. With 2024 being no exception.
Some high category events did take place throughout the year, 
though almost all of them were either an expression of seasonal changes in the ice edge of the polar regions, 
or particularly pronounced anti-cyclonic eddies within the western boundary currents (WBC). 
The ice edge events in particular cannot be ruled out as artefacts of the data assimilation of the OISST product.
That being said, the Russian coastline of the Barents Sea exhibited intense cooling over May to June, 
with the Karas sea experiencing a wide-spread Category 4 MCS from June to July."


## Figure caption ----------------------------------------------------------

"Fig. x.y. Annual global marine heatwave (MHW; [a],[b]) and marine cold-spell (MCS; [c],[d]) occurrence from NOAA OISST
v2.1 using a climatology base period of 1991–2020. (a),(c) The average count of MHW/MCS days experienced over the
surface of the ocean each year (left y-axis), also expressed as the percent of the surface of the ocean experiencing a 
MHW/MCS on any given day (right y-axis) of that year. (b),(d) Total percent of the surface area of the ocean that experienced
an MHW/MCS at some point during the year. The values shown are for the highest category of MHW/MCS experienced at any point."


## Technical definition ----------------------------------------------------

"A MHW is detected when five or more consecutive days of temperature are above a 90th percentile daily climatology (Hobday et al. 2016). 
MHWs are categorized as moderate when the greatest temperature anomaly during the event is less than double the difference between 90th percentile and seasonal climatology. 
When this value is more than double, triple, or quadruple the distance, the MHW is categorized as strong, severe, or extreme, respectively (Hobday et al. 2018). 
The direct inverse is used to detect and categorize MCSs (i.e., days below the 10th percentile). 
The baseline period used to detect events in this report is 1991–2020, matching the current advise from the WMO for defining climate normals.
Note that this years report is the first to shift to the new baseline, while all previous reports used the 1982-2011 baseline.
This means that the values for MHWs will appear lower in this report than previous years, even though, in absolute terms, the ocean continues to warm."


## References --------------------------------------------------------------

"Hobday, A. J., Alexander, L. V., Perkins, S. E., Smale, D. A., Straub, S. C., Oliver, E. C., ... & Wernberg, T. (2016). 
A hierarchical approach to defining marine heatwaves. Progress in Oceanography, 141, 227-238. https://doi.org/10.1016/j.pocean.2015.12.014

Hobday, A. J., Oliver, E. C., Gupta, A. S., Benthuysen, J. A., Burrows, M. T., Donat, M. G., ... & Smale, D. A. (2018). 
Categorizing and naming marine heatwaves. Oceanography, 31(2), 162-173. https://www.jstor.org/stable/26542662

Huang, B., Liu, C., Banzon, V., Freeman, E., Graham, G., Hankins, B., ... & Zhang, H. M. (2021). 
Improvements of the daily optimum interpolation sea surface temperature (DOISST) version 2.1. Journal of Climate, 34(8), 2923-2939. https://doi.org/10.1175/JCLI-D-20-0166.1"


## Load data ---------------------------------------------------------------
# NB: Now using 1991-2020 clim for BAMS reports

# Load 1982, the first year
MHW_cat_daily_1982 <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1991-2020_1982.Rds")
MCS_cat_daily_1982 <- readRDS("../MHWapp/data/annual_summary/OISST_MCS_cat_daily_1991-2020_1982.Rds")

# Load the most intense year
MHW_cat_daily_2016 <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1991-2020_2016.Rds")
# 1982 is the most intense MCS year

# Load the most daily coverage year
MHW_cat_daily_2023 <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1991-2020_2023.Rds")

# Load the most spatial coverage year
MCS_cat_daily_1985 <- readRDS("../MHWapp/data/annual_summary/OISST_MCS_cat_daily_1991-2020_1985.Rds")

# Load the most recent low coverage year
MHW_cat_daily_2012 <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1991-2020_2012.Rds")

# Load previous year
MHW_cat_daily_2023 <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1991-2020_2023.Rds")
MCS_cat_daily_2023 <- readRDS("../MHWapp/data/annual_summary/OISST_MCS_cat_daily_1991-2020_2023.Rds")

# Load current year
MHW_cat_daily_2024 <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1991-2020_2024.Rds")
MCS_cat_daily_2024 <- readRDS("../MHWapp/data/annual_summary/OISST_MCS_cat_daily_1991-2020_2024.Rds")

# Total history
MHW_total_summary <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1991-2020_total.Rds")
MCS_total_summary <- readRDS("../MHWapp/data/annual_summary/OISST_MCS_cat_daily_1991-2020_total.Rds")


## Panel A+C ----------------------------------------------------------------
## Annual MHW/MCS days

# Daily percent coverage
cat_prop_stats <- function(df){
  df %>% 
    group_by(category) %>% 
    summarise(mean = mean(cat_area_prop), .groups = "drop") %>% 
    mutate(sum = sum(mean))
}
cat_prop_stats(MHW_cat_daily_2024)
cat_prop_stats(MCS_cat_daily_2024)

# Total days per year
cat_days_stats <- function(df){
  df %>% 
    group_by(category) %>%
    filter(t == max(t)) %>% 
    ungroup() %>% 
    dplyr::select(t, category, cat_area_cum_prop) %>% 
    mutate(sum = sum(cat_area_cum_prop))
}

cat_days_stats(MHW_cat_daily_2024)
cat_days_stats(MHW_cat_daily_2016)
cat_days_stats(MCS_cat_daily_2024)
cat_days_stats(MCS_cat_daily_1982)


## Panel B+D ----------------------------------------------------------------
## Annual total MHW+MCS cover

first_cum_prop_stats <- function(df){
  df %>% 
    group_by(category) %>%
    filter(t == max(t)) %>% 
    ungroup() %>% 
    dplyr::select(t, category, first_area_cum_prop) %>% 
    mutate(sum = sum(first_area_cum_prop))
}

first_cum_prop_stats(MHW_cat_daily_2024)
first_cum_prop_stats(MHW_cat_daily_2016)
first_cum_prop_stats(MCS_cat_daily_2024)
first_cum_prop_stats(MCS_cat_daily_1982)


# Annual video ------------------------------------------------------------

# See: "../MHWapp/MHW_annual_summary.R"

