# The purpose of this script is to provide an easy source for 
# calculating all of the historic MCSs in one go

# I haven't presently put in any failsafes for oversaving...

# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(lubridate)
library(tidync)
# remotes::install_github("robwschlegel/heatwaveR")
library(heatwaveR); packageVersion("heatwaveR")
source("MHW_prep.R")
library(doParallel); registerDoParallel(cores = 50)


# Data --------------------------------------------------------------------

# The OISST files
OISST_files <- dir("../data/OISST", pattern = "avhrr-only", full.names = T)

# The full MCS lon files
MCS_lon_files <- dir("../data/MCS", full.names = T)

# The MCS cat files
MCS_cat_files <- dir("../data/cat_clim_MCS", full.names = T)


# Functions ---------------------------------------------------------------

# Function for loading OISST data, calculating MCSs, and saving the results
# lon_row <- 613
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
  save(MCS_res, file = paste0("../data/MCS/MCS.calc", lon_row_pad,".RData"))
  rm(SST, MCS_res); gc()
  print(paste("Completed run",lon_row_pad,"at",Sys.time()))
}

# Function for loading a cat_lon slice and extracting a single day of values
# testers...
# cat_lon_file <- MCS_lon_files[613]
# date_range <- c(as.Date("2019-11-01"), as.Date("2020-01-07"))
load_sub_cat_clim <- function(cat_lon_file, date_range){
  
  # Prototype data.frame for unnesting
  proto_df <- data.frame(t = as.Date('1982-01-01'),
                         event_bo = as.integer(1),
                         intensity = as.numeric(-1.2),
                         category = as.character("I Moderate"))
  
  # cat_clim <- readRDS(cat_lon_file)
  load(cat_lon_file)
  # cat_clim_sub <- cat_clim %>%
  cat_clim_sub <- MCS_res %>%
    dplyr::select(-event) %>% 
    unnest(cols = cat) %>% 
    filter(row_number() %% 2 == 1) %>% 
    filter(nrow(cat$climatology) > 0) %>% 
    # map(~filter(.x, is.Date(t)))
    # filter(nrow(.) > 0)
    # na.omit() %>% 
    # slice(6:7) %>% 
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
  cat_clim_dir <- paste0("../data/cat_clim_MCS",cat_clim_year)
  dir.create(as.character(cat_clim_dir), showWarnings = F)
  cat_clim_name <- paste0("cat.clim.MCS.",date_choice,".Rda")
  
  # Extract data and save
  df_sub <- df %>% 
    filter(t == date_choice)
  saveRDS(df_sub, file = paste0(cat_clim_dir,"/",cat_clim_name))
}

# Function for loading, prepping, and saving the daily global category slices
# tester...
# date_range <- c(as.Date("1982-01-01"), as.Date("1982-01-31"))
cat_clim_global_daily <- function(date_range){
  cat_clim_daily <- plyr::ldply(MCS_lon_files[613], load_sub_cat_clim,
                                .parallel = T, date_range = date_range) %>% 
    mutate(category = factor(category, levels = c("I Moderate", "II Strong",
                                                  "III Severe", "IV Extreme"))) %>% 
    na.omit()
  
  # NB: Running this on too many cores may cause RAM issues
  registerDoParallel(cores = 20)
  plyr::l_ply(seq(min(cat_clim_daily$t), max(cat_clim_daily$t), by = "day"), 
              save_sub_cat_clim, .parallel = T, df = cat_clim_daily)
}


# Calculate lon files -----------------------------------------------------

# system.time(
#   MCS_calc(613)
# ) # 150 seconds

# Ran on Monday, June 15th, 2020
plyr::l_ply(1:1440, .fun = MCS_calc, .parallel = T)
# Takes just over two hours


# Calculate cat files -----------------------------------------------------

# NB: Better not to run the entire 30+ years at once
registerDoParallel(cores = 50)
cat_clim_global_daily(date_range = c(as.Date("1982-01-01"), as.Date("1990-12-31")))
cat_clim_global_daily(date_range = c(as.Date("1991-01-01"), as.Date("2000-12-31")))
cat_clim_global_daily(date_range = c(as.Date("2001-01-01"), as.Date("2010-12-31")))
cat_clim_global_daily(date_range = c(as.Date("2011-01-01"), as.Date("2020-12-31")))


# Annual summary ----------------------------------------------------------

# testers...
# chosen_year <- 2016
# product <- "OISST"
# chosen_clim <- "1982-2011"
# force_calc <- T
MCS_annual_state <- function(chosen_year, product, chosen_clim, force_calc = F){
  
  print(paste0("Started run on ",product, "(", 
               chosen_clim,"): ", chosen_year," at ",Sys.time()))
  
  ## Find file location
  MCS_cat_files <- dir(paste0("../data/cat_clim_MCS/", chosen_year), 
                       full.names = T, pattern = chosen_clim)
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
  if(force_calc){
    # system.time(
    MCS_cat <- plyr::ldply(MCS_cat_files, readRDS, .parallel = T) #%>% 
    # right_join(OISST_no_ice_coords, by = c("lon", "lat")) %>%  # Filter out ice if desired
    # na.omit()
    # ) # 12 seconds
  }
  
  ## Process data
  # Max category per pixel
  if(file.exists(paste0("data/annual_summary/",product,"_cat_pixel_",
                        chosen_clim,"_",chosen_year,".Rds")) & !force_calc){
    MHW_cat_pixel <- readRDS(paste0("data/annual_summary/",product,"_cat_pixel_",
                                    chosen_clim,"_",chosen_year,".Rds"))
  } else{
    # print(paste0("Filtering out the max category at each pixel and counting sum of intensity; ~14 seconds"))
    
    MHW_intensity <- MHW_cat %>% 
      group_by(lon, lat) %>% 
      summarise(intensity_sum = sum(intensity)) %>% 
      ungroup()
    
    # system.time(
    MHW_cat_pixel <- MHW_cat %>% 
      dplyr::select(-event_no) %>% 
      plyr::ddply(., c("lon"), max_event_date, 
                  .parallel = T, .paropts = c(.inorder = FALSE)) %>% 
      unique() %>%
      left_join(MHW_intensity, by = c("lon", "lat"))
    # ) # 14 seconds
    saveRDS(MHW_cat_pixel, file = paste0("data/annual_summary/",product,"_cat_pixel_",
                                         chosen_clim,"_",chosen_year,".Rds"))
  }
  
  # Daily count and cumulative count per pixel
  if(file.exists(paste0("data/annual_summary/",product,"_cat_daily_",
                        chosen_clim,"_",chosen_year,".Rds")) & !force_calc){
    MHW_cat_daily <- readRDS(paste0("data/annual_summary/",product,"_cat_daily_",
                                    chosen_clim,"_",chosen_year,".Rds"))
  } else{
    # print(paste0("Counting the daily + cumulative categories per day; ~3 seconds"))
    
    # Complete dates by categories data.frame
    full_grid <- expand_grid(t = seq(as.Date(paste0(chosen_year,"-01-01")), max(MHW_cat$t), by = "day"), 
                             category = as.factor(levels(MHW_cat$category))) %>% 
      mutate(category = factor(category, levels = levels(MHW_cat$category)))
    
    # system.time(
    MHW_cat_single <- MHW_cat_pixel %>%
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
    MHW_cat_daily <- MHW_cat %>% 
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
      right_join(MHW_cat_single, by = c("t", "category"))
    # ) # 1 second
    saveRDS(MHW_cat_daily, file = paste0("data/annual_summary/",product,"_cat_daily_",
                                         chosen_clim,"_",chosen_year,".Rds"))
  }
  
  # Add prop columns for more accurate plotting
  MHW_cat_daily <- MHW_cat_daily %>% 
    mutate(first_n_cum_prop = round(first_n_cum/nrow(OISST_ocean_coords), 4),
           cat_prop = round(cat_n/nrow(OISST_ocean_coords), 4))
  
  # Extract small data.frame for easier labelling
  MHW_cat_daily_labels <- MHW_cat_daily %>% 
    group_by(category) %>% 
    filter(t == max(t)) %>% 
    ungroup() %>% 
    mutate(label_first_n_cum = cumsum(first_n_cum_prop))
  
  ## Create figures
  # print("Creating figures")
  
  # Global map of MHW occurrence
  fig_map <- ggplot(MHW_cat_pixel, aes(x = lon, y = lat)) +
    # geom_tile(data = OISST_ice_coords, fill = "powderblue", colour = NA, alpha = 0.5) +
    geom_tile(aes(fill = category), colour = NA) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
    scale_fill_manual("Category", values = MHW_colours) +
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
  fig_count <- ggplot(MHW_cat_daily, aes(x = t, y = cat_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MHW_colours) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0.2, 0.8, length.out = 4),
                       labels = paste0(seq(20, 80, by = 20), "%")) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
    labs(y = "Global MHW count\n(non-cumulative)", x = "Day of the year") +
    coord_cartesian(expand = F) +
    theme(axis.title = element_text(size = 15),
          axis.text = element_text(size = 13))
  # fig_count
  
  # Stacked barplot of cumulative percent of ocean affected by MHWs
  fig_cum <- ggplot(MHW_cat_daily, aes(x = t, y = first_n_cum_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1) +
    geom_hline(data = MHW_cat_daily_labels, show.legend = F,
               aes(yintercept = label_first_n_cum, colour = category)) +
    scale_fill_manual("Category", values = MHW_colours) +
    scale_colour_manual("Category", values = MHW_colours) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0.2, 0.8, length.out = 4),
                       labels = paste0(seq(20, 80, by = 20), "%")) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
    labs(y = "Top MHW category per pixel\n(cumulative)", x = "Day of first occurrence") +
    coord_cartesian(expand = F) +
    theme(axis.title = element_text(size = 15),
          axis.text = element_text(size = 13))
  # fig_cum
  
  # Stacked barplot of average cumulative MHW days per pixel
  fig_prop <- ggplot(MHW_cat_daily, aes(x = t, y = cat_n_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MHW_colours) +
    scale_y_continuous(breaks = round(seq(sum(MHW_cat_daily_labels$cat_n_prop)*0.25,
                                          sum(MHW_cat_daily_labels$cat_n_prop)*0.75, length.out = 3), 0)) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +  
    labs(y = "Average MHW days per pixel\n(cumulative)", x = "Day of the year") +
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
         filename = paste0("figures/",product,"_cat_summary_", chosen_clim,"_",chosen_year,".png"))
  # ggsave(fig_ALL_cap, height = 12, width = 18, 
  # filename = paste0("figures/",product,"_cat_summary_", chosen_clim,"_",chosen_year,".pdf")) # looks bad...
  
  # print(paste0("Finished run on ",product, "(", 
  #              chosen_clim,"): ", chosen_year," at ",Sys.time()))
}


# Total summary -----------------------------------------------------------



# Trends ------------------------------------------------------------------

# Metrics

# Categories

