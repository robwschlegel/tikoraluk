# The purpose of this script is to provide an easy source for 
# calculating all of the historic MCSs in one go
# 1: Setup
# 2: Full calculations
# 3: Daily categories
# 4: Annual summaries
# 5: Total summaries 
# 6: Trends 
# 7: MHWs minus MCSs
# 8: SSTa skewness and kurtosis


# 1: Setup ----------------------------------------------------------------

# Libraries
.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(lubridate)
library(dtplyr)
library(tidync)
library(broom)
library(e1071)
library(ggridges)
# remotes::install_github("robwschlegel/heatwaveR")
library(heatwaveR); packageVersion("heatwaveR")
library(doParallel); registerDoParallel(cores = 50)
source("MCS_prep.R")

# File locations
OISST_files <- dir("../data/OISST", pattern = "avhrr-only", full.names = T)
MCS_lon_files <- dir("../data/MCS", full.names = T)
MCS_cat_files <- dir("../data/cat_clim_MCS", full.names = T)
MCS_count_trend_files <- dir("annual_summary_MCS", pattern = "count_trend", full.names = T)
seas_thresh_files <- dir("../data/thresh", pattern = "MHW.seas.thresh.", full.names = T)

# Metadata
load("../MHWapp/metadata/OISST_ocean_coords.Rdata")

# The MCS colour palette
MCS_colours <- c(
  "I Moderate" = "#A4D4E0",
  "II Strong" = "#5B80A6",
  "III Severe" = "#2A3C66",
  "IV Extreme" = "#111433"
)

# The base map
load("../MHWapp/metadata/map_base.Rdata")

# Disable scientific notation
options(scipen = 9999)

# The MCS results
MCS_RData <- c(file = dir(path = "../data/MCS", pattern = "MCS.calc.*.RData", full.names = T))

# TO DO
# Also need to calculate the 1/(days from start to peak) and 1/(days from peak to end) and make maps


# 2: Full calculations  ---------------------------------------------------

# Function for loading OISST data, calculating MCSs, and saving the results
# lon_row <- 1203
MCS_calc <- function(lon_row){
  
  # Begin
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  print(paste("Began run", lon_row_pad, "at", Sys.time()))
  
  # Load data
  SST <- tidync(OISST_files[lon_row]) %>% 
    hyper_tibble() %>% 
    mutate(time = as.Date(time, origin = "1970-01-01")) %>% 
    dplyr::rename(t = time, temp = sst)
  
  # Make calculations
  MCS_res <- SST %>%
    group_by(lon, lat) %>%
    nest() %>% 
    mutate(clim = purrr::map(data, ts2clm, climatologyPeriod = c("1982-01-01", "2011-12-31"), pctile = 10),
           event = purrr::map(clim, detect_event, coldSpells = T), 
           cat = purrr::map(event, category, climatology = T, season = "peak")) %>%
    select(-data, -clim)
  
  # Finish
  # save(MCS_res, file = paste0("../data/MCS/MCS.calc", lon_row_pad,".RData"))
  saveRDS(MCS_res, paste0("../data/MCS/MCS.calc.", lon_row_pad,".Rds"))
  rm(SST, MCS_res); gc()
  print(paste("Completed run",lon_row_pad,"at",Sys.time()))
}

# system.time(
#   MCS_calc(1204)
# ) # 150 seconds

# Ran on Thursday, July 30th, 2020
plyr::l_ply(1:1440, .fun = MCS_calc, .parallel = T)
# Takes just over two hours


# 3: Daily categories -----------------------------------------------------

# Function for loading a cat_lon slice and extracting a single day of values
# testers...
# cat_lon_file <- MCS_lon_files[613]
# date_range <- c(as.Date("2019-11-01"), as.Date("2020-01-07"))
load_sub_cat_clim <- function(cat_lon_file, date_range){
  # cat_clim <- readRDS(cat_lon_file)
  load(cat_lon_file)
  # cat_clim_sub <- cat_clim %>%
  cat_clim_sub <- MCS_res %>%
    dplyr::select(-event) %>% 
    unnest(cols = cat) %>% 
    filter(row_number() %% 2 == 1) %>% 
    filter(nrow(cat$climatology) > 0) %>% 
    unnest(cols = cat) %>% 
    ungroup() %>% 
    filter(t >= date_range[1], t <= date_range[2])
  # rm(cat_clim); gc()
  rm(MCS_res); gc()
  return(cat_clim_sub)
}

# Function for saving daily global cat files
# df <- cat_clim_daily
# date_choice <- as.Date("1982-01-01")
save_sub_cat_clim <- function(date_choice, df){
  
  # Establish flie name and save location
  cat_clim_year <- lubridate::year(date_choice)
  cat_clim_dir <- paste0("../data/cat_clim_MCS/",cat_clim_year)
  dir.create(as.character(cat_clim_dir), showWarnings = F)
  cat_clim_name <- paste0("cat.clim.MCS.",date_choice,".Rds")
  
  # Extract data and save
  df_sub <- df %>% 
    filter(t == date_choice)
  saveRDS(df_sub, file = paste0(cat_clim_dir,"/",cat_clim_name))
  rm(df); gc()
}

# Function for loading, prepping, and saving the daily global category slices
# tester...
# date_range <- c(as.Date("1982-01-01"), as.Date("1982-01-31"))
cat_clim_global_daily <- function(date_range){
  cat_clim_daily <- plyr::ldply(MCS_lon_files, load_sub_cat_clim,
                                .parallel = T, date_range = date_range) %>% 
    ungroup() %>% 
    na.omit() %>% 
    mutate(category = factor(category, levels = c("I Moderate", "II Strong",
                                                  "III Severe", "IV Extreme")),
           intensity = round(intensity, 2)) %>% 
    data.frame()
  
  # NB: Running this on too many cores may cause RAM issues
  registerDoParallel(cores = 10)
  plyr::l_ply(seq(min(cat_clim_daily$t), max(cat_clim_daily$t), by = "day"), 
              save_sub_cat_clim, .parallel = T, df = cat_clim_daily)
  rm(cat_clim_daily); gc()
}

# NB: Better not to run the entire 30+ years at once
# registerDoParallel(cores = 50)
# cat_clim_global_daily(date_range = c(as.Date("1982-01-01"), as.Date("1990-12-31")))
# registerDoParallel(cores = 50)
# cat_clim_global_daily(date_range = c(as.Date("1991-01-01"), as.Date("2000-12-31")))
# registerDoParallel(cores = 50)
# cat_clim_global_daily(date_range = c(as.Date("2001-01-01"), as.Date("2010-12-31")))
# registerDoParallel(cores = 50)
# cat_clim_global_daily(date_range = c(as.Date("2011-01-01"), as.Date("2020-12-31")))


# 4: Annual summaries -----------------------------------------------------

# Function for finding the first date of the highest category MHW per pixel
max_event_date <- function(df){
  df %>% 
    group_by(lat) %>% 
    filter(as.integer(category) == max(as.integer(category))) %>% 
    filter(t == min(t)) %>% 
    ungroup()
}

# testers...
# chosen_year <- 1982
# product <- "OISST"
# chosen_clim <- "1982-2011"
# force_calc <- T
MCS_annual_state <- function(chosen_year, product, chosen_clim, force_calc = F){
  
  print(paste0("Started run on ",product, "(", 
               chosen_clim,"): ", chosen_year," at ",Sys.time()))
  
  ## Find file location
  MCS_cat_files <- dir(paste0("../data/cat_clim_MCS/", chosen_year), full.names = T)
  # MCS_cat_files <- dir(paste0("../data/",product,"_cat_MCS/", chosen_year), 
  #                      full.names = T, pattern = chosen_clim)
  
  ## Create figure title
  if(length(MCS_cat_files) < 365){
    extra_bit <- " (so far)"
  } else{
    extra_bit <- ""
  }
  product_name <- product
  if(product == "OISST") product_name <- "NOAA OISST"
  fig_title <- paste0("MCS categories of ",chosen_year, extra_bit,
                      "\n",product_name,"; Climatogy period: ",chosen_clim)
  
  ## Load data
  
  ## Load/Process data
  # Categories per pixel
  if(file.exists(paste0("annual_summary_MCS/cat_pixel_MCS_",chosen_year,".Rds")) & !force_calc){
    MCS_cat_pixel <- readRDS(paste0("annual_summary_MCS/cat_pixel_MCS_",chosen_year,".Rds"))
  } else{
    
    # system.time(
    MCS_cat <- plyr::ldply(MCS_cat_files, readRDS, .parallel = T) #%>% 
    # right_join(OISST_no_ice_coords, by = c("lon", "lat")) %>%  # Filter out ice if desired
    # na.omit()
    # ) # 12 seconds
    
    MCS_intensity <- MCS_cat %>% 
      group_by(lon, lat) %>% 
      summarise(intensity_sum = sum(intensity)) %>% 
      ungroup()
    
    # system.time(
    MCS_cat_pixel <- MCS_cat %>% 
      dplyr::select(-event_no) %>% 
      plyr::ddply(., c("lon"), max_event_date, 
                  .parallel = T, .paropts = c(.inorder = FALSE)) %>% 
      unique() %>%
      left_join(MCS_intensity, by = c("lon", "lat"))
    # ) # 14 seconds
    saveRDS(MCS_cat_pixel, file = paste0("annual_summary_MCS/MCS_cat_pixel_",chosen_year,".Rds"))
    
    ## Summarise
    # system.time(
    MCS_cat_count <- lazy_dt(MCS_cat) %>% 
      group_by(lon, lat, event_no) %>% 
      summarise(max_cat = max(as.integer(category))) %>% 
      data.frame() %>% 
      dplyr::select(-event_no) %>% 
      mutate(max_cat = factor(max_cat, levels = c(1:4),  labels = levels(MCS_cat$category))) %>% 
      group_by(lon, lat) %>% 
      table() %>% 
      as.data.frame() %>% 
      pivot_wider(values_from = Freq, names_from = max_cat) %>% 
      mutate(lon = as.numeric(as.character(lon)),
             lat = as.numeric(as.character(lat)))
    # ) # 16 seconds
    saveRDS(MCS_cat_count, paste0("annual_summary_MCS/MCS_cat_count_", chosen_year,".Rds"))
  }
  
  # Daily count and cumulative count per pixel
  if(file.exists(paste0("annual_summary_MCS/MCS_cat_daily_",chosen_year,".Rds")) & !force_calc){
    MCS_cat_daily <- readRDS(paste0("annual_summary_MCS/MCS_cat_daily_",chosen_year,".Rds"))
  } else{
    
    # Complete dates by categories data.frame
    full_grid <- expand_grid(t = seq(as.Date(paste0(chosen_year,"-01-01")), max(MCS_cat$t), by = "day"), 
                             category = as.factor(levels(MCS_cat$category))) %>% 
      mutate(category = factor(category, levels = levels(MCS_cat$category)))
    
    # system.time(
    MCS_cat_single <- MCS_cat_pixel %>%
      group_by(t) %>%
      count(category) %>%
      dplyr::rename(first_n = n) %>% 
      ungroup() %>% 
      right_join(full_grid, by = c("t", "category")) %>% 
      mutate(first_n = ifelse(is.na(first_n), 0, first_n)) %>% 
      arrange(t) %>% 
      group_by(category) %>%
      mutate(first_n_cum = cumsum(first_n)) %>% 
      ungroup() %>% 
      group_by(category) %>% 
      ungroup()
    # ) # 1 second
    
    # system.time(
    MCS_cat_daily <- MCS_cat %>% 
      group_by(t) %>% 
      count(category) %>% 
      ungroup() %>% 
      right_join(full_grid, by = c("t", "category")) %>% 
      dplyr::rename(cat_n = n) %>% 
      mutate(cat_n = ifelse(is.na(cat_n), 0, cat_n)) %>% 
      group_by(category) %>% 
      mutate(cat_n_cum = cumsum(cat_n),
             cat_n_prop = round(cat_n_cum/nrow(OISST_ocean_coords), 4)) %>% 
      ungroup() %>% 
      right_join(MCS_cat_single, by = c("t", "category"))
    # ) # 1 second
    saveRDS(MCS_cat_daily, file = paste0("annual_summary_MCS/MCS_cat_daily_",chosen_year,".Rds"))
  }
  
  # Add prop columns for more accurate plotting
  MCS_cat_daily <- MCS_cat_daily %>% 
    mutate(first_n_cum_prop = round(first_n_cum/nrow(OISST_ocean_coords), 4),
           cat_prop = round(cat_n/nrow(OISST_ocean_coords), 4))
  
  # Extract small data.frame for easier labelling
  MCS_cat_daily_labels <- MCS_cat_daily %>% 
    group_by(category) %>% 
    filter(t == max(t)) %>% 
    ungroup() %>% 
    mutate(label_first_n_cum = cumsum(first_n_cum_prop))
  
  ## Create figures
  # Global map of MHW occurrence
  fig_map <- ggplot(MCS_cat_pixel, aes(x = lon, y = lat)) +
    # geom_tile(data = OISST_ice_coords, fill = "powderblue", colour = NA, alpha = 0.5) +
    geom_tile(aes(fill = category), colour = NA) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
    scale_fill_manual("Category", values = MCS_colours) +
    coord_cartesian(expand = F, ylim = c(min(OISST_ocean_coords$lat),
                                         max(OISST_ocean_coords$lat))) +
    theme_void() +
    guides(fill = guide_legend(override.aes = list(size = 10))) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          panel.background = element_rect(fill = "grey90"))
  # fig_map
  
  # Stacked barplot of global daily count of MHWs by category
  fig_count <- ggplot(MCS_cat_daily, aes(x = t, y = cat_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MCS_colours) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0.2, 0.8, length.out = 4),
                       labels = paste0(seq(20, 80, by = 20), "%")) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
    labs(y = "Global MCS count\n(non-cumulative)", x = "Day of the year") +
    coord_cartesian(expand = F) +
    theme(axis.title = element_text(size = 15),
          axis.text = element_text(size = 13))
  # fig_count
  
  # Stacked barplot of cumulative percent of ocean affected by MHWs
  fig_cum <- ggplot(MCS_cat_daily, aes(x = t, y = first_n_cum_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1) +
    geom_hline(data = MCS_cat_daily_labels, show.legend = F,
               aes(yintercept = label_first_n_cum, colour = category)) +
    scale_fill_manual("Category", values = MCS_colours) +
    scale_colour_manual("Category", values = MCS_colours) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0.2, 0.8, length.out = 4),
                       labels = paste0(seq(20, 80, by = 20), "%")) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
    labs(y = "Top MCS category per pixel\n(cumulative)", x = "Day of first occurrence") +
    coord_cartesian(expand = F) +
    theme(axis.title = element_text(size = 15),
          axis.text = element_text(size = 13))
  # fig_cum
  
  # Stacked barplot of average cumulative MHW days per pixel
  fig_prop <- ggplot(MCS_cat_daily, aes(x = t, y = cat_n_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MCS_colours) +
    scale_y_continuous(breaks = round(seq(sum(MCS_cat_daily_labels$cat_n_prop)*0.25,
                                          sum(MCS_cat_daily_labels$cat_n_prop)*0.75, length.out = 3), 0)) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +  
    labs(y = "Average MCS days per pixel\n(cumulative)", x = "Day of the year") +
    coord_cartesian(expand = F) +
    theme(axis.title = element_text(size = 15),
          axis.text = element_text(size = 13))
  # fig_prop
  
  # print("Combining figures")
  fig_ALL_sub <- ggpubr::ggarrange(fig_count, fig_cum, fig_prop, ncol = 3, align = "hv",
                                   labels = c("B)", "C)", "D)"), font.label = list(size = 16))
  fig_ALL <- ggpubr::ggarrange(fig_map, fig_ALL_sub, ncol = 1, heights = c(1, 0.6),
                               labels = c("A)"), common.legend = T, legend = "bottom",
                               font.label = list(size = 16))
  
  # Standard caption technique
  fig_ALL_cap <- grid::textGrob(fig_title, x = 0.01, just = "left", gp = grid::gpar(fontsize = 20))
  fig_ALL_cap <- ggpubr::ggarrange(fig_ALL_cap, fig_ALL, heights = c(0.07, 1), nrow = 2)
  
  # print("Saving final figure")
  ggsave(fig_ALL_cap, height = 12, width = 18, 
         filename = paste0("graph/summary/",product,"_cat_summary_", chosen_clim,"_",chosen_year,".png"))
  # ggsave(fig_ALL_cap, height = 12, width = 18, 
  # filename = paste0("figures/",product,"_cat_summary_", chosen_clim,"_",chosen_year,".pdf")) # looks bad...
}

# Run ALL years
# NB: Running this in parallel will cause a proper stack overflow
# registerDoParallel(cores = 50)
# plyr::l_ply(1982:2020, MCS_annual_state, force_calc = T, .parallel = F,
#             product = "OISST", chosen_clim = "1982-2011") # ~50 seconds for one


# 5: Total summaries ------------------------------------------------------

# testers...
# product <- "OISST"
# chosen_clim <- "1982-2011"
MCS_total_state <- function(product, chosen_clim){
  
  # Create mean values of daily count
  cat_daily_mean <- map_dfr(dir("annual_summary_MCS", pattern = paste0("MCS_cat_daily"),
                                full.names = T), readRDS) %>%
    mutate(t = lubridate::year(t)) %>%
    group_by(t, category) %>%
    summarise(cat_n = mean(cat_n, na.rm = T)) %>%
    ungroup() %>%
    mutate(cat_prop_daily_mean = round(cat_n/nrow(OISST_ocean_coords), 4))
  
  # Extract only values from Decemer 31st
  cat_daily <- map_dfr(dir("annual_summary_MCS", pattern = paste0("MCS_cat_daily_"),
                           full.names = T), readRDS) %>%
    # cat_daily <- map_dfr(dir("data/annual_summary/v2.0", pattern = "cat_daily",
    # full.names = T), readRDS) %>% # The old v2.0 OISST data
    filter(lubridate::month(t) == 12, lubridate::day(t) == 31) %>%
    mutate(t = lubridate::year(t),
           first_n_cum_prop = round(first_n_cum/nrow(OISST_ocean_coords), 4)) %>% 
    left_join(cat_daily_mean, by = c("t", "category"))
  
  # Save and exit
  saveRDS(cat_daily, paste0("annual_summary_MCS/MCS_cat_daily_total.Rds"))
}

## Run them all
# MCS_total_state("OISST", "1982-2011")

MCS_total_state_fig <- function(df, product, chosen_clim){
  
  # Stacked barplot of global daily count of MHWs by category
  fig_count_historic <- ggplot(df, aes(x = t, y = cat_prop_daily_mean)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = T,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MCS_colours) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0.2, 0.8, length.out = 4),
                       labels = paste0(seq(20, 80, by = 20), "%")) +
    scale_x_continuous(breaks = seq(1982, 2019, 5)) +
    labs(y = "Daily MHW occurrence", x = NULL) +
    coord_cartesian(expand = F) +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16))
  # fig_count_historic
  
  # Stacked barplot of cumulative percent of ocean affected by MHWs
  fig_cum_historic <- ggplot(df, aes(x = t, y = first_n_cum_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = T,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MCS_colours) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0.2, 0.8, length.out = 4),
                       labels = paste0(seq(20, 80, by = 20), "%")) +
    scale_x_continuous(breaks = seq(1982, 2019, 5)) +
    labs(y = "Total MHW occurrence", x = NULL) +
    coord_cartesian(expand = F) +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16))
  # fig_cum_historic
  
  # Stacked barplot of average cumulative MHW days per pixel
  fig_prop_historic <- ggplot(df, aes(x = t, y = cat_n_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = T,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MCS_colours) +
    scale_y_continuous(limits = c(0, 50),
                       breaks = seq(10, 40, length.out = 3)) +
    scale_x_continuous(breaks = seq(1982, 2019, 5)) +
    labs(y = "MHW days/pixel", x = NULL) +
    coord_cartesian(expand = F) +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16))
  # fig_prop_historic
  
  # Create the figure title
  product_title <- product
  if(product == "OISST") product_title <- "NOAA OISST"
  min_year <- min(df$t)
  max_year <- max(df$t)
  clim_title <- gsub("-", " - ", chosen_clim)
  fig_title <- paste0("MHW category summaries: ",min_year," - ",max_year,
                      "\n",product_title,"; Climatogy period: ",clim_title)
  
  # Stick them together and save
  fig_ALL_historic <- ggpubr::ggarrange(fig_count_historic, fig_cum_historic, fig_prop_historic,
                                        ncol = 3, align = "hv", labels = c("(a)", "(b)", "(c)"), hjust = -0.1,
                                        font.label = list(size = 14), common.legend = T, legend = "bottom")
  ggsave(fig_ALL_historic, filename = paste0("graph/summary/",product,"_cat_historic_"
                                             ,chosen_clim,".png"), height = 4.25, width = 12)
  # ggsave(fig_ALL_full, filename = paste0("figures/",product,"_cat_historic_",chosen_clim,".eps"), height = 4.25, width = 12)
}

## Run them all
# OISST
# MCS_total <- readRDS("annual_summary_MCS/MCS_cat_daily_total.Rds")
# MCS_total_state_fig(MCS_total, "OISST", "1982-2011")


# 6: Trends ---------------------------------------------------------------

MCS_trend_calc <- function(lon_step){
  
  # Start
  lon_step_pad <- str_pad(lon_step, 4, pad = "0")
  print(paste0("Began run on ",lon_step," at ", Sys.time()))
  
  # Load chosen file
  load(MCS_lon_files[lon_step])
  
  # Unpack categories
  MCS_cat <- MCS_res %>%
    dplyr::select(-event) %>% 
    unnest(cols = cat) %>% 
    filter(row_number() %% 2 == 0) %>% 
    filter(nrow(cat$event) > 0) %>% 
    unnest(cols = cat) %>% 
    ungroup() %>% 
    mutate(category = factor(category, levels = c("I Moderate", "II Strong",
                                                  "III Severe", "IV Extreme")),
           season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter"))) %>% 
    data.frame()
  
  # Unpack event metrics and join
  MCS_event <- MCS_res %>%
    dplyr::select(-cat) %>% 
    unnest(cols = event) %>% 
    filter(row_number() %% 2 == 0) %>% 
    filter(nrow(event$event) > 0) %>%
    unnest(cols = event) %>%
    ungroup() %>% 
    left_join(MCS_cat, by = c("lon", "lat", "event_no", "duration")) %>% 
    ungroup() %>% 
    mutate(year = year(date_peak))
  rm(MCS_res); gc()
  
  # Annual metric summaries
  suppressWarnings(
  suppressMessages(
  MCS_metric <- MCS_event %>% 
    group_by(lon, lat, year) %>% 
    summarise(dur_mean = mean(duration, na.rm = T),
              dur_sum = sum(duration, na.rm = T),
              i_mean = mean(intensity_mean, na.rm = T),
              i_max_mean = mean(intensity_max, na.rm = T),
              i_max_min = min(intensity_max, na.rm = T),
              i_cum_mean = mean(intensity_cumulative, na.rm = T),
              i_cum_sum = sum(intensity_cumulative, na.rm = T),
              onset_mean = mean(rate_onset, na.rm = T),
              onset_min = min(rate_onset, na.rm = T),
              decline_mean = mean(rate_decline, na.rm = T),
              decline_min = min(rate_decline, na.rm = T),
              p_moderate = mean(p_moderate, na.rm = T),
              p_strong = mean(p_strong, na.rm = T),
              p_severe = mean(p_severe, na.rm = T),
              p_extreme = mean(p_extreme, na.rm = T)) %>% 
    filter(p_extreme >= 0) %>% 
    mutate_if(is.numeric, round, 4) %>% 
    pivot_longer(cols = c(-lon, -lat, -year))
  ))
  MCS_metric$value[is.na(MCS_metric$value)] <- NA
  MCS_metric$value[is.infinite(as.matrix(MCS_metric$value))] <- NA
  
  # Annual metric trends
  suppressWarnings(
  MCS_metric_trends <- MCS_metric %>% 
    group_by(lon, lat, name) %>% 
    nest() %>% 
    mutate(model = map(data, ~lm(value ~ year, data = .)),
           model_out = map(model, ~broom::tidy(.))) %>% 
    dplyr::select(-data, -model) %>% 
    unnest(cols = model_out) %>% 
    ungroup() %>% 
    filter(term == "year") %>% 
    dplyr::rename(slope = estimate) %>% 
    dplyr::select(lon, lat, name, slope, p.value) %>% 
    mutate(slope = round(slope, 4), 
           p.value = round(p.value, 4))
  )
  MCS_metric_trends[is.na(MCS_metric_trends)] <- 1
  
  # Annual category count summaries 
  MCS_cat_count <- MCS_event %>% 
    dplyr::select(lon, lat, year, category) %>% 
    group_by(lon, lat, year) %>% 
    table() %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    pivot_wider(values_from = Freq, names_from = category)
  
  # Annual peak of season count
  MCS_season_count <- MCS_event %>% 
    dplyr::select(lon, lat, year, season) %>% 
    group_by(lon, lat, year) %>% 
    table() %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    pivot_wider(values_from = Freq, names_from = season)
  
  # Join all count data.frames
  MCS_count <- left_join(MCS_cat_count, MCS_season_count,
                         by = c("lon", "lat", "year")) %>%
    mutate(total_count = Spring + Summer + Fall + Winter,
           lon = as.numeric(lon),
           lat = as.numeric(lat),
           year = as.numeric(year)) %>% 
    pivot_longer(cols = c(-lon, -lat, -year))
  
  # Trends in count values
  suppressWarnings(
  MCS_count_trends <- MCS_count %>% 
    group_by(lon, lat, name) %>% 
    nest() %>% 
    mutate(model = map(data, ~lm(value ~ year, data = .)),
           model_out = map(model, ~broom::tidy(.))) %>% 
    dplyr::select(-data, -model) %>% 
    unnest(cols = model_out) %>% 
    ungroup() %>% 
    filter(term == "year") %>% 
    dplyr::rename(slope = estimate) %>% 
    dplyr::select(lon, lat, name, slope, p.value) %>% 
    mutate(slope = round(slope, 4), 
           p.value = round(p.value, 4))
  )
  MCS_count_trends[is.na(MCS_count_trends)] <- 1
  
  # Final data.frame and save
  MCS_count_trend <- rbind(MCS_metric, MCS_count) %>% 
    dplyr::select(-year) %>% 
    group_by(lon, lat, name) %>%
    summarise_if(is.numeric, mean, na.rm = T) %>% 
    left_join(rbind(MCS_metric_trends, MCS_count_trends), by = c("lon", "lat", "name")) %>% 
    mutate(value = round(value, 4))
  saveRDS(MCS_count_trend, paste0("annual_summary_MCS/MCS_count_trend_",lon_step_pad,".Rds"))
}

# Run one
# system.time(
#   MCS_trend_calc(1172)
# ) # 29 seconds

# Run all
# registerDoParallel(cores = 50)
# plyr::l_ply(1:1440, MCS_trend_calc, .parallel = T)

# Load all results into one brick
MCS_count_trend <- plyr::ldply(MCS_count_trend_files, readRDS, .parallel = T)

# Figures of trends and annual states
var_mean_trend_fig <- function(var_name){
  
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
  
  # The mean value map
  mean_map <- df %>% 
    mutate(value = case_when(value <= value_q10 ~ value_q10,
                             value >= value_q90 ~ value_q90,
                             TRUE ~ value)) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_raster(aes(fill = value)) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
    scale_fill_viridis_c("Mean\n(annual)") +
    # coord_cartesian(expand = F, ylim = c(min(OISST_ocean_coords$lat),
    #                                      max(OISST_ocean_coords$lat))) +
    coord_cartesian(expand = F, ylim = c(-70, 70)) +
    theme_void() +
    # guides(fill = guide_legend(override.aes = list(size = 10))) +
    labs(title = var_name) +
    theme(legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          panel.background = element_rect(fill = "grey90"))
  # mean_map
  
  # The trend map
  trend_map <- df %>% 
    mutate(slope = case_when(slope <= slope_q10 ~ slope_q10,
                             slope >= slope_q90 ~ slope_q90,
                             TRUE ~ slope)) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_raster(aes(fill = slope)) +
    # geom_point(data = df_p, shape = 4, size = 0.1, alpha = 0.1) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
    scale_fill_gradient2("Slope\n(annual)", low = "blue", high = "red") +
    # coord_cartesian(expand = F, ylim = c(min(OISST_ocean_coords$lat),
    #                                      max(OISST_ocean_coords$lat))) +
    coord_cartesian(expand = F, ylim = c(-70, 70)) +
    theme_void() +
    # guides(fill = guide_legend(override.aes = list(size = 10))) +
    labs(title = paste0(var_name," trend")) +
    theme(legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          panel.background = element_rect(fill = "grey90"))
  # trend_map
  
  full_map <- ggpubr::ggarrange(mean_map, trend_map, ncol = 1, nrow = 2, align = "hv")
  ggsave(paste0("graph/summary/mean_trend_",var_name,".png"), full_map, width = 12, height = 12)
}

# plyr::l_ply(unique(MCS_count_trend$name), var_mean_trend_fig, .parallel = T)


# 7: MHWs minus MCSs ------------------------------------------------------


# 8: SSTa skewness and kurtosis -------------------------------------------

# testers...
# file_name_MCS <- MCS_RData[1]
# lon_step <- lon_OISST[1]
skew_kurt_calc <- function(lon_step){
  # Load the data
  df <- sst_seas_thresh_merge(lon_step, date_range = as.Date("1982-01-01"))
  
  # Add a season category
  df_season <- df %>% 
    mutate(month = month(t, label = T)) %>% 
    mutate(season = case_when(month %in% c("Jan", "Feb", "Mar") & lat > 0 ~ "Winter",
                              month %in% c("Apr", "May", "Jun") & lat > 0 ~ "Spring",
                              month %in% c("Jul", "Aug", "Sep") & lat > 0 ~ "Summer",
                              month %in% c("Oct", "Nov", "Dec") & lat > 0 ~ "Autumn",
                              month %in% c("Jan", "Feb", "Mar") & lat < 0 ~ "Summer",
                              month %in% c("Apr", "May", "Jun") & lat < 0 ~ "Autumn",
                              month %in% c("Jul", "Aug", "Sep") & lat < 0 ~ "Winter",
                              month %in% c("Oct", "Nov", "Dec") & lat < 0 ~ "Spring")) %>% 
    dplyr::select(-month)
  
  # Combine data frames and calculate skewness and kurtosis
  skew_kurt <- df %>%
    mutate(season = "Total") %>% 
    rbind(., df_season) %>% 
    mutate(season = factor(season, levels = c("Total", "Spring", "Summer", "Autumn", "Winter"))) %>% 
    group_by(lon, lat, season) %>% 
    summarise(anom_skew = round(skewness(anom), 2),
              anom_kurt = round(kurtosis(anom), 2),
              anom_min = min(anom),
              anom_mean = round(mean(anom), 2),
              anom_max = max(anom), .groups = "drop")
  return(skew_kurt)
}

# Load the global SSTa
registerDoParallel(cores = 50)
system.time(SSTa_stats <- plyr::ldply(lon_OISST, skew_kurt_calc, .parallel = T, .paropts = c(.inorder = F))) # 947 seconds
saveRDS(SSTa_stats, "data/SSTa_stats.Rds")

# Show a ridegplot with the fill for kurtosis and the colour for skewness
SSTa_ridge <- SSTa_stats %>% 
  mutate(lat_10 = factor(plyr::round_any(lat, 10))) %>% 
  dplyr::select(-lon, -lat) %>% 
  mutate(season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter", "Total"))) %>% 
  ggplot(aes(x = anom_skew, y = lat_10)) +
  geom_density_ridges(aes(fill = season), alpha = 0.5, size = 0.1) +
  # scale_x_continuous(limits = c(-2, 10), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-2, 5), expand = c(0, 0)) +
  theme_ridges()
ggsave("graph/kurt_skew_lon.png", SSTa_ridge, width = 12)
