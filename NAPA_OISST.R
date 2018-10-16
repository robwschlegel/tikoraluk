# The purpose of this script is to house the functions and 
# analysis that compares the NAPA model against OISST


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(doMC); doMC::registerDoMC(cores = 50)
library(stringr)
library(FNN)
library(ggpubr)
library(gridExtra)

source("MHW_prep.R")


# Meta-data ---------------------------------------------------------------

load("metadata/lon_lat_NAPA_OISST.RData")
load("metadata/lon_OISST.RData")

# Need to quantify the distance (KMs) between FNN pixels


# Data --------------------------------------------------------------------


# Functions ---------------------------------------------------------------

# Function for loading the matching OISST and NAPA lon row
## tester...
# lon_row <- 1
load_OISST_NAPA <- function(lon_row){
  # Load data
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  load(paste0("../data/NAPA_sst_sub_",lon_row_pad,".RData"))
  load(paste0("../data/MHW.calc.",lon_row_pad,".RData"))
  
  # Prep for use
  OISST_long <- MHW_clim(MHW_res) %>% 
    select(lon, lat, t, temp)
  NAPA_long <- NAPA_sst_sub %>% 
    dplyr::rename(lon = nav_lon,
                  lat = nav_lat) %>% 
    select(lon, lat, t, temp)
  ALL_long <- rbind(OISST_long, NAPA_long)
  return(ALL_long)
}

# Function for running the chosen analyses on each pixel
# Summary stats
# - Get the summary of each pixel over time for both datasets
# -- Mean, median, quartiles, 10/90th, sd, min, max, decadal trend
# - Do this for the months, too
# - Do this for the sea ice concentration value, too (ice_ts)
summarise_OISST_NAPA <- function(df){
  # Run summary calculations
  ALL_res_1 <- df %>%
    # Constrain the OISST date range to match NAPA
    # ensuring the same period for calculations
    filter(t >= as.Date("1993-10-01"),
           t <= as.Date("2015-12-29")) %>% 
    # select(lon, lat, temp) %>% 
    group_by(lon, lat) %>% 
    summarise(min = min(temp, na.rm = T),
              quant_10 = quantile(temp, na.rm = T, probs = 0.10),
              quant_25 = quantile(temp, na.rm = T, probs = 0.25),
              quant_50 = quantile(temp, na.rm = T, probs = 0.50),
              quant_75 = quantile(temp, na.rm = T, probs = 0.75),
              quant_90 = quantile(temp, na.rm = T, probs = 0.90),
              max = max(temp, na.rm = T),
              mean = mean(temp, na.rm = T),
              sd = sd(temp, na.rm = T)) %>% 
    mutate(month = "overall") %>% 
    select(lon, lat, month, everything())
  
  ALL_res_2 <- df %>% 
    mutate(month = lubridate::month(t, label = T)) %>% 
    group_by(lon, lat, month) %>% 
    summarise(min = min(temp, na.rm = T),
              quant_10 = quantile(temp, na.rm = T, probs = 0.10),
              quant_25 = quantile(temp, na.rm = T, probs = 0.25),
              quant_50 = quantile(temp, na.rm = T, probs = 0.50),
              quant_75 = quantile(temp, na.rm = T, probs = 0.75),
              quant_90 = quantile(temp, na.rm = T, probs = 0.90),
              max = max(temp, na.rm = T),
              mean = mean(temp, na.rm = T),
              sd = sd(temp, na.rm = T)) %>% 
    select(lon, lat, month, everything())
  
  # Create monthly values and run linear models
  ALL_monthly <- df %>% 
    mutate(monthly = floor_date(t, unit = "month")) %>% 
    # select(lon, lat, monthly, temp) %>% 
    group_by(lon, lat, monthly) %>%
    summarise(temp = mean(temp, na.rm = T)) %>% 
    ungroup() %>% 
    na.omit()
  
  suppressWarnings(
  ALL_res_3 <- ALL_monthly %>% 
    group_by(lon, lat) %>% 
    do(dt = round(as.numeric(coef(lm(temp ~ monthly, data = .))[2]) * 120, 4)) %>% 
    mutate(dt = as.numeric(dt)) %>% 
    mutate(month = "overall") %>% 
    select(lon, lat, month, everything())
  )
  
  suppressWarnings(
  ALL_res_4 <- ALL_monthly %>% 
    mutate(month = lubridate::month(monthly, label = T)) %>% 
    group_by(lon, lat, month) %>% 
    do(dt = round(as.numeric(coef(lm(temp ~ monthly, data = .))[2]) * 120, 4)) %>% 
    mutate(dt = as.numeric(dt))
  )
  
  # Combine and exit
  suppressWarnings(
    ALL_res_5 <- rbind(ALL_res_1, ALL_res_2)
    )
  suppressWarnings(
    ALL_res_6 <- rbind(ALL_res_3, ALL_res_4)
    )
  suppressWarnings(
    ALL_res <- left_join(ALL_res_5, ALL_res_6, by = c("lon", "lat", "month"))
    )
  return(ALL_res)
}

# Function for loading, analysing, and saving the summary data
analyse_OISST_NAPA <- function(df){
  # Load
  lon_row <- df$lon_row
  ALL_long <- load_OISST_NAPA(lon_row)
  # Summarise
  OISST_NAPA_summary <- summarise_OISST_NAPA(ALL_long)
  # Save
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  save(OISST_NAPA_summary, file = paste0("../data/OISST_NAPA_summary_",lon_row_pad,".RData"))
}

correlate_OISST_NAPA <- function(df){
  # Load
  lon_row <- df$lon_row
  ALL_long <- load_OISST_NAPA(lon_row) %>%
    # Constrain the OISST date range to match NAPA
    # ensuring the same period for calculations
    filter(t >= as.Date("1993-10-01"),
           t <= as.Date("2015-12-29"))
  # Match up
  ALL_match <- inner_join(lon_lat_NAPA_OISST, ALL_trim,
                          by = c("nav_lon" = "lon", "nav_lat" = "lat")) %>%
    left_join(ALL_trim, by = c("lon_O" = "lon", "lat_O" = "lat", "t")) %>% 
    mutate(time = "day") %>% 
    filter(temp.x != 0) %>% 
    na.omit()
  # Correlate
  cor_day <- ALL_match %>% 
    group_by(lon, lat) %>% 
    summarise(cor_day = cor(temp.x, temp.y, 
                        use = "pairwise.complete.obs", 
                        method = "pearson"))
  cor_month <- ALL_match %>% 
    mutate(t = floor_date(t, "month")) %>% 
    group_by(lon, lat, t) %>% 
    summarise(temp.x = mean(temp.x, na.rm = T),
              temp.y = mean(temp.y, na.rm = T)) %>% 
    group_by(lon, lat) %>% 
    summarise(cor_month = cor(temp.x, temp.y, 
                            use = "pairwise.complete.obs", 
                            method = "pearson"))
  cor_year <- ALL_match %>% 
    mutate(t = floor_date(t, "year")) %>% 
    group_by(lon, lat, t) %>% 
    summarise(temp.x = mean(temp.x, na.rm = T),
              temp.y = mean(temp.y, na.rm = T)) %>% 
    group_by(lon, lat) %>% 
    summarise(cor_year = cor(temp.x, temp.y, 
                              use = "pairwise.complete.obs", 
                              method = "pearson"))
  cor_all <- left_join(cor_day, cor_month, by = c("lon", "lat")) %>% 
    left_join(cor_year, by = c("lon", "lat"))
  return(cor_all)
  # Save
  # lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  # save(OISST_NAPA_summary, file = paste0("../data/OISST_NAPA_summary_",lon_row_pad,".RData"))
}


# Summarise ---------------------------------------------------------------

lon_row_multi <- data.frame(lon_row = 1:1440,
                            x = 1:1440)

# Re-run on Friday, October 12th, 2018
# system.time(
#   plyr::ddply(lon_row_multi, .variables = "x",
#               .fun = analyse_OISST_NAPA, .parallel = T)
# ) # 1969 seconds at 50 cores

# Difference --------------------------------------------------------------

# Load all of the summary data run above
OISST_NAPA_saves <- dir("../data", pattern = "OISST_NAPA", full.names = T)

load_sub_diff_ON <- function(file_name){
  load(file = file_name)
  data_sub <- inner_join(lon_lat_NAPA_OISST, OISST_NAPA_summary,
                         by = c("nav_lon" = "lon", "nav_lat" = "lat")) %>%
    left_join(OISST_NAPA_summary, by = c("lon_O" = "lon", "lat_O" = "lat", "month")) %>% 
    mutate(min.z = min.x - min.y,
           quant_10.z = quant_10.x - quant_10.y,
           quant_25.z = quant_25.x - quant_25.y,
           quant_50.z = quant_50.x - quant_50.y,
           quant_75.z = quant_75.x - quant_75.y,
           quant_90.z = quant_90.x - quant_90.y,
           max.z = max.x - max.y,
           mean.z = mean.x - mean.y,
           sd.z = sd.x - sd.y,
           dt.z = dt.x - dt.y) %>% 
    rename_all(~str_replace_all(., ".x", "_NAPA")) %>%
    rename_all(~str_replace_all(., ".y", "_OISST")) %>% 
    rename_all(~str_replace_all(., ".z", "_diff"))
  return(data_sub)
}

# system.time(
#   OISST_NAPA_difference <- plyr::ldply(OISST_NAPA_saves, .parallel = T,
#                                        .fun = load_sub_diff_ON)
# ) # 15 seconds at 50 cores
# save(OISST_NAPA_difference, file = "../data/OISST_NAPA_difference.RData")


# Correlation -------------------------------------------------------------

lon_row_multi <- data.frame(lon_row = 1:1440,
                            x = 1:1440)


# NB: This falls over at 50 cores. Not enough memmory.
# system.time(
#   OISST_NAPA_correlation <- plyr::ddply(lon_row_multi, .variables = "x",
#                                         .fun = correlate_OISST_NAPA, .parallel = T)
# ) # 1235 seconds at 40 cores
# save(OISST_NAPA_correlation, file = "../data/OISST_NAPA_correlation.RData")
# Not run
# system.time(
#   OISST_NAPA_correlation_1 <- plyr::ddply(lon_row_multi[1:720, ], .variables = "x",
#                                         .fun = correlate_OISST_NAPA, .parallel = T)
# ) # xxx seconds at 50 cores
# system.time(
#   OISST_NAPA_correlation_2 <- plyr::ddply(lon_row_multi[721:1440, ], .variables = "x",
#                                           .fun = correlate_OISST_NAPA, .parallel = T)
# ) # xxx seconds at 50 cores
# OISST_NAPA_correlation <- rbind(OISST_NAPA_correlation_1, OISST_NAPA_correlation_2)
# save(OISST_NAPA_correlation, file = "../data/OISST_NAPA_correlation.RData")


# Visualise ---------------------------------------------------------------

# Load difference and correlation results
load("../data/OISST_NAPA_difference.RData")
load("../data/OISST_NAPA_correlation.RData")

# Remove NA and 0 rows for now...
diff_res_na_omit <- OISST_NAPA_difference %>% 
  na.omit() %>% 
  filter(min_NAPA != 0)

# The base map
map_base <- fortify(map(fill = TRUE, plot = FALSE)) %>% 
  dplyr::rename(lon = long) %>% 
  filter(lat >= 25.6, lon <= 180) #%>% 
  # mutate(lon = ifelse(lon > 180, lon-180, lon))

# Plotting function
polar_map <- function(the_chosen, diff_val = F){
  pp <- ggplot() + theme_void() +
    geom_polygon(data = map_base, aes(x = -lon, y = -lat, group = group)) +
    geom_point(data = filter(diff_res_na_omit, month == "overall"),
               aes_string(x = "-nav_lon", y = "-nav_lat", colour = the_chosen), size = 0.001) +
    # scale_colour_viridis_c() +
    scale_color_distiller(palette = "Spectral") +
    coord_polar() +
    # scale_y_continuous(limits = c(-90, -25)) +
    labs(x = "", y = "") +
    theme(legend.position = c(0.8, 0.5),
          legend.background = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(colour = "white"),
          axis.text = element_blank()#,
          # axis.ticks = element_blank(),
          # plot.background = element_blank(),
          # panel.background = element_blank(),
          # plot.title = element_text(hjust = 0.5, vjust = 1),
          # axis.line = element_blank()
    )
  if(diff_val){
    suppressMessages(
    pp <- pp + scale_colour_gradient2(low = "blue", 
                                      mid = "white",
                                      high = "red",
                                      midpoint = 0)
    )
  }
  return(pp)
}

# Make some pretty pictures
napa_map <- polar_map("quant_50_NAPA")
napa_title <- text_grob(label = "NAPA median", face = "bold",
                        size = 16, just = c("center","center"))
oisst_map <- polar_map("quant_50_OISST")
oisst_title <- text_grob(label = "OISST median",  face = "bold",
                         size = 16, just = c("center","center"))
diff_map <- polar_map("quant_50_diff", diff_val = T)
diff_title <- text_grob(label = "NAPA - OISST", face = "bold",
                        size = 16, just = c("center","center"))

tri_plot <- ggplot() +
  theme_void() +
  geom_blank() +
  scale_x_continuous(limits = c(0.1, 2.9)) +
  scale_y_continuous(limits = c(0.1,0.9)) +
  annotation_custom(grob = ggplotGrob(oisst_map),
                    xmin = -0.1, xmax = 1.1,
                    ymin = 0, ymax = 1) +
  annotation_custom(grob = oisst_title,
                    xmin = -0.1, xmax = 1.1,
                    ymin = 0.8, ymax = 0.9) +
  annotation_custom(grob = ggplotGrob(napa_map),
                    xmin = 0.9, xmax = 2.1,
                    ymin = 0, ymax = 1) +
  annotation_custom(grob = napa_title,
                    xmin = 0.9, xmax = 2.1,
                    ymin = 0.8, ymax = 0.9) +
  annotation_custom(grob = ggplotGrob(diff_map),
                    xmin = 1.9, xmax = 3.1,
                    ymin = 0, ymax = 1) +
  annotation_custom(grob = diff_title,
                    xmin = 1.9, xmax = 3.1,
                    ymin = 0.8, ymax = 0.9)

# ggarrange(oisst_map, napa_map, diff_map, nrow = 1, ncol = 3)
# grid.arrange(napa_title, napa_map, ncol = 1, heights = c(2,15)) 
ggsave(plot = tri_plot, filename = "graph/median_diff.png", height = 6, width = 16)
