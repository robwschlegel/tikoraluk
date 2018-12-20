# The purpose of this script is to have one central place for the creation 
# of the visualisations for the NAPA OISST comparisons


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggpubr)
doMC::registerDoMC(cores = 50)


# Meta-data ---------------------------------------------------------------

load("metadata/lon_lat_NAPA_OISST.RData")
load("metadata/lon_OISST.RData")
load("metadata/only_water.RData")


# Data --------------------------------------------------------------------

# load("~/Downloads/OISST_NAPA_ice_round.RData")

# The various comparison results
load("../data/OISST_NAPA_SST_summary.RData")
load("../data/OISST_NAPA_MHW_summary.RData")
load("../data/AVISO_NAPA_skewness_summary.RData")
load("../data/OISST_NAPA_ice_summary.RData")
load("../data/OISST_NAPA_ice_round.RData")

# A custom palette
pretty_palette <- c("#fefefe", "#f963fa", "#020135", "#00efe1", "#057400", "#fcfd00", "#ed0000", "#3d0000")

# The base map
map_base <- ggplot2::fortify(maps::map(fill = TRUE, plot = FALSE)) %>% 
  dplyr::rename(lon = long) %>% 
  filter(lat >= 25.6) %>%
  mutate(group = ifelse(lon > 180, group+9999, group),
         lon = ifelse(lon > 180, lon-360, lon))


# Prep --------------------------------------------------------------------

# Ice dataframes for easier contour plotting
## NB: Only needs to be run once
## Is loaded above
# OISST_NAPA_ice_round <- OISST_NAPA_ice_summary %>%
#   na.omit() %>%
#   filter(product != "difference") %>%
#   mutate(nav_lon = round(nav_lon, 0), 
#          nav_lat = round(nav_lat, 0)) %>% 
#   group_by(nav_lon, nav_lat, month, product) %>%
#   summarise(ice_round = mean(ice_mean, na.rm = T)) %>%
#   data.frame()
# save(OISST_NAPA_ice_round, file = "../data/OISST_NAPA_ice_round.RData")

# No-land mask
## NB: Only needs to be run once
## Is loaded above
# only_water <- OISST_NAPA_SST_summary %>%
#   select(nav_lon, nav_lat) %>%
#   dplyr::distinct() %>% 
#   left_join(lon_lat_NAPA_OISST, by = c("nav_lon", "nav_lat")) %>%
#   arrange(dist)
# save(only_water, file = "metadata/only_water.RData")

# Only overall MHW data for plotting
OISST_NAPA_MHW_summary_corrected <- OISST_NAPA_MHW_summary %>% 
  filter(month == "overall") %>% 
  mutate(month = as.character(month),
         month = "daily")


# Functions ---------------------------------------------------------------

# df <- OISST_NAPA_SST_summary
# df <- OISST_NAPA_MHW_summary_corrected
# sum_stat <- "quant_90"
# sum_stat <- "mean"
# sum_stat <- "intensity_max_max"
# plot_name = NA
# plot_title = NA
# chosen_palette = "red-blue"
# colour_range = NA
# legend_title = NA
# legend_position = c(0.9, 0.1)
# Function that creates and saves multiple plots from one input dataframe
polar_plot_output <- function(sum_stat, df, plot_name = NA, plot_title = NA,
                              chosen_palette = "red-blue", colour_range = NA, 
                              legend_title = NA, legend_position = c(0.9, 0.1)){
  if(is.na(legend_title)) legend_title <- "Temperature (°C)"
  if(sum_stat %in% c("dt", "ice_dt")) legend_title <- "Temp./dec (°C)"
  if(sum_stat == "count") legend_title <- "Events (n)"
  if(sum_stat %in% c("duration_min", "duration_mean", "duration_max")) legend_title <- "Duration (days)"
  if(sum_stat %in% c("cor_flat", "cor_norm")) legend_title <- "Correlation (r)"
  if(sum_stat %in% c("seas_t_test", "thresh_t_test", "seas_KS_test", "thresh_KS_test", 
                     "duration_t_test", "intensity_max_t_test")) legend_title <- "Probability (p)"
  if(sum_stat %in% c("ice_min", "ice_median", "ice_mean", 
                     "ice_max", "ice_sd")) legend_title <- "Prop. ice cover"
  if(is.na(plot_name)) plot_name <- sum_stat
  if(is.na(plot_title)) plot_title <- sum_stat
  
  if(!(is.nan(mean(filter(df, product != "difference")[,colnames(df) == sum_stat], na.rm = T)))){
    # Prep
    df_full <- filter(df, product != "difference")
    if(is.na(colour_range)[1]){
      if(legend_title == "Temperature (°C)"){
        colour_range_full <- c(-2, 30)
        } else {
          colour_range_full <- c(min(df_full[,colnames(df_full) == sum_stat], na.rm = T),
                                 max(df_full[,colnames(df_full) == sum_stat], na.rm = T))
          } 
      } else {
        colour_range_full <- colour_range
      }
    # NAPA
    if("NAPA" %in% unique(df_full$product)){
      if("AVISO" %in% unique(df_full$product) | sum_stat == "dt" | sum_stat == "ice_dt"){
        NAPA_palette <- "red-blue"
      } else {
        NAPA_palette <- "pretty"
      }
      polar_plots(filter(df_full, product == "NAPA"), sum_stat, plot_name = paste0(plot_name,"_NAPA"), 
                  plot_title = paste(plot_title, "NAPA"),  chosen_palette = NAPA_palette, 
                  colour_range = colour_range_full,  legend_title = legend_title, 
                  legend_position = legend_position)
    }
    # OISST
    if("OISST" %in% unique(df_full$product)){
      if(sum_stat == "dt" | sum_stat == "ice_dt"){
        OISST_palette <- "red-blue"
      } else {
        OISST_palette <- "pretty"
      }
      polar_plots(filter(df_full, product == "OISST"), sum_stat, plot_name = paste0(plot_name,"_OISST"), 
                  plot_title = paste(plot_title, "OISST"),  chosen_palette = OISST_palette, 
                  colour_range = colour_range_full,  legend_title = legend_title, 
                  legend_position = legend_position)
    }
    # AVISO
    if("AVISO" %in% unique(df_full$product)){
      polar_plots(filter(df_full, product == "AVISO"), sum_stat, plot_name = paste0(plot_name,"_AVISO"), 
                  plot_title = paste(plot_title, "AVISO"),  chosen_palette = chosen_palette, 
                  colour_range = colour_range_full,  legend_title = legend_title, 
                  legend_position = legend_position)
    }
  }
  # difference
  if("difference" %in% unique(df$product)){
    if(sum_stat %in% c("cor_norm", "cor_flat", "seas_t_test", "thresh_t_test", 
                       "seas_KS_test", "thresh_KS_test", "duration_t_test", "intensity_max_t_test")){
      legend_title_diff <- legend_title
      } else {
        legend_title_diff <- paste("Model - Obs\n", legend_title)
        }
    polar_plots(filter(df, product == "difference"), sum_stat, plot_name = paste0(plot_name,"_difference"), 
                plot_title = paste(plot_title, "difference"),  chosen_palette = chosen_palette, 
                colour_range = colour_range,  legend_title = legend_title_diff, 
                legend_position = legend_position, ice = TRUE)
  }
}


# Plotting function
polar_plots <- function(df, sum_stat, plot_name, plot_title, chosen_palette, 
                        colour_range, legend_title, legend_position, ice = F){
  
  # Prep
  df_complete <- df[complete.cases(colnames(df) == sum_stat),]
  # df_complete <- na.omit(df)
  # df_complete <- df
  if(is.na(colour_range[1])){
    colour_range <- c(min(df_complete[,colnames(df_complete) == sum_stat], na.rm = T),
                      max(df_complete[,colnames(df_complete) == sum_stat], na.rm = T))
  }
  ice_sub <- OISST_NAPA_ice_round %>% 
    filter(month %in% unique(df_complete$month))
  if(length(unique(df_complete$month)) == 1){
    ice_sub <- ice_sub %>% 
      mutate(month = as.character(month))
  }
  
  # Figure
  pp <- ggplot(data = df_complete) + theme_void() +
    geom_point(size = 0.001, aes_string(x = "nav_lon", y = "nav_lat", colour = sum_stat)) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
    coord_map("ortho", orientation = c(90, 0, 0)) +
    labs(x = "", y = "", colour = legend_title, title = paste0(plot_title,"\n")) +
    guides(colour = guide_colourbar(title.position = "top", direction = "horizontal"),
           fill = guide_legend(title.position = "top", direction = "horizontal",
                               override.aes = list(size = 5, alpha = 1))) +
    theme(legend.background = element_blank(),
          legend.text = element_text(colour = "black"),
          legend.position = legend_position,
          axis.text = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 25), 
          strip.text = element_text(size = 18), 
          panel.border = element_rect(colour = "black", fill = NA), 
          panel.spacing.x = unit(0,"cm"),
          panel.spacing.y = unit(0.5,"cm"),
          strip.background = element_rect(fill = "grey80")) +
    facet_wrap(~month, ncol = 4)
  if(chosen_palette == "red-blue"){
    pp_col <- pp + scale_colour_gradient2(low = "blue", mid = "white", high = "red", 
                                          midpoint = 0, limits = colour_range)
  } else if(chosen_palette == "viridis"){
    pp_col <- pp + scale_colour_viridis_c(limits = colour_range)
  } else if(chosen_palette == "spectral"){
    pp_col <- pp + scale_color_distiller(palette = "Spectral", limits = colour_range)
  }else if(chosen_palette == "pretty"){
    pp_col <- pp + scale_colour_gradientn(colours = pretty_palette, limits = colour_range)
  }
  if(ice & length(unique(df_complete$month)) == 1){
    pp_col <- pp_col + 
      geom_contour(data = ice_sub, breaks = 0.5, colour = "black", 
                   lineend = "round", size = 1.0, alpha = 0.5,
                   aes(x = nav_lon, y = nav_lat, z = ice_round, linetype = product)) +
      scale_linetype_manual(values = c("dotted", "solid"), "Product") +
      theme(legend.background = element_rect(fill = "white", colour = NA, size = 0.5))
  }
  if(ice & length(unique(df_complete$month)) > 1){
    pp_col <- pp_col + 
      geom_contour(data = ice_sub, breaks = 0.5, colour = "black", 
                   lineend = "round", size = 0.2, alpha = 0.5,
                   aes(x = nav_lon, y = nav_lat, z = ice_round, linetype = product)) +
      scale_linetype_manual(values = c("dotted", "solid"), "Product")
  }
  if(length(unique(df_complete$month)) == 1){
    pp_col <- pp_col +
      theme(legend.position = "bottom")
    ggsave(pp_col, filename = paste0("graph/diff_figs/",plot_name,".png"), width = 6, height = 7)
  } else if(length(unique(df_complete$month)) == 3){
    pp_col <- pp_col +
      theme(legend.position = "bottom")
    ggsave(pp_col, filename = paste0("graph/diff_figs/",plot_name,".png"), width = 12, height = 5)
  } else {
    ggsave(pp_col, filename = paste0("graph/diff_figs/",plot_name,".png"), width = 12, height = 14)
  }
}

# Plotting function
polar_plot_single <- function(sum_stat, df, chosen_palette = "pretty", 
                              colour_range = NA, ice = F, legend_title){
  
  # Prep
  df_complete <- df[complete.cases(colnames(df) == sum_stat),]
  # df_complete <- na.omit(df)
  # df_complete <- df
  if(is.na(colour_range[1])){
    colour_range <- c(min(df_complete[,colnames(df_complete) == sum_stat], na.rm = T),
                      max(df_complete[,colnames(df_complete) == sum_stat], na.rm = T))
  }
  ice_sub <- OISST_NAPA_ice_round %>% 
    filter(month %in% unique(df_complete$month))
  if(length(unique(df_complete$month)) == 1){
    ice_sub <- ice_sub %>% 
      mutate(month = as.character(month))
  }
  
  pp <- ggplot(data = df_complete) + theme_void() +
    geom_point(size = 0.001, aes_string(x = "nav_lon", y = "nav_lat", colour = sum_stat)) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
    coord_map("ortho", orientation = c(90, 0, 0)) +
    labs(x = "", y = "", colour = legend_title) +
    guides(fcolour = guide_colourbar(barheight = 5)) +
    theme(legend.position = c(0.8, 0.5),
          legend.background = element_blank(),
          legend.title = element_text(colour = "white", size = 8),
          legend.text = element_text(colour = "white", size = 7),
          axis.text = element_blank()
    )
  if(chosen_palette == "red-blue"){
    pp_col <- pp + scale_colour_gradient2(low = "blue", mid = "white", high = "red", 
                                          midpoint = 0, limits = colour_range)
  } else if(chosen_palette == "viridis"){
    pp_col <- pp + scale_colour_viridis_c(limits = colour_range)
  } else if(chosen_palette == "spectral"){
    pp_col <- pp + scale_color_distiller(palette = "Spectral", limits = colour_range)
  }else if(chosen_palette == "pretty"){
    pp_col <- pp + scale_colour_gradientn(colours = pretty_palette, limits = colour_range)
  }
  if(ice){
    pp_col <- pp_col + 
      geom_contour(data = ice_sub, breaks = 0.5, colour = "black", 
                   lineend = "round", size = 1.0, alpha = 0.5,
                   aes(x = nav_lon, y = nav_lat, z = ice_round, linetype = product)) +
      scale_linetype_manual(values = c("dotted", "solid"), "Product")
  }
  return(pp_col)
}

# Tri-panel visuals for a single summary stat
# df <- OISST_NAPA_SST_custom_DT
# sum_stat <- "dt"
# colour_range <- c(-2, 30)
# colour_range <- c(-0.2, 0.2)
# diff_range <- c(-0.2, 0.2)
# chosen_palette <-  "red-blue"
# ice <- F
tri_panel <- function(sum_stat, df, chosen_palette = "pretty",
                      colour_range = NA, diff_range = NA, legend_title = NA){
  if(is.na(legend_title)) legend_title <- "Temp. (°C)"
  if(sum_stat %in% c("dt", "ice_dt")) legend_title <- "Temp./dec (°C)"
  if(sum_stat == "count") legend_title <- "Events (n)"
  if(sum_stat %in% c("duration_min", "duration_mean", "duration_max")) legend_title <- "Duration (days)"
  if(sum_stat %in% c("cor_flat", "cor_norm")) legend_title <- "Correlation (r)"
  if(sum_stat %in% c("seas_t_test", "thresh_t_test", "seas_KS_test", "thresh_KS_test", 
                     "duration_t_test", "intensity_max_t_test")) legend_title <- "Probability (p)"
  # Create the three individual panels
  napa_map <- polar_plot_single(sum_stat = sum_stat, df = filter(df, product == "NAPA"),
                                colour_range = colour_range, legend_title = legend_title,
                                chosen_palette = chosen_palette)
  napa_title <- text_grob(label = paste0("NAPA ",sum_stat), face = "bold",
                          size = 16, just = c("center","center"))
  oisst_map <- polar_plot_single(sum_stat = sum_stat, df = filter(df, product == "OISST"),
                                 colour_range = colour_range, legend_title = legend_title, 
                                 chosen_palette = chosen_palette)
  oisst_title <- text_grob(label = paste0("OISST ",sum_stat), face = "bold",
                           size = 16, just = c("center","center"))
  diff_map <- polar_plot_single(sum_stat = sum_stat, df = filter(df, product == "difference"),
                                colour_range = diff_range, legend_title = legend_title,
                                chosen_palette = "red-blue", ice = T)
  diff_title <- text_grob(label = "NAPA - OISST", face = "bold",
                          size = 16, just = c("center","center"))
  
  # Create the tri-plot
  tri_plot <- ggplot() +
    theme_void() +
    geom_blank() +
    scale_x_continuous(limits = c(0.1, 2.9)) +
    scale_y_continuous(limits = c(0.1,0.9)) +
    annotation_custom(grob = ggplotGrob(oisst_map),
                      xmin = 0, xmax = 1,
                      ymin = 0, ymax = 1) +
    annotation_custom(grob = oisst_title,
                      xmin = 0, xmax = 1,
                      ymin = 0.9, ymax = 0.9) +
    annotation_custom(grob = ggplotGrob(napa_map),
                      xmin = 1, xmax = 2,
                      ymin = 0, ymax = 1) +
    annotation_custom(grob = napa_title,
                      xmin = 1, xmax = 2,
                      ymin = 0.9, ymax = 0.9) +
    annotation_custom(grob = ggplotGrob(diff_map),
                      xmin = 2, xmax = 3,
                      ymin = 0, ymax = 1) +
    annotation_custom(grob = diff_title,
                      xmin = 2, xmax = 3,
                      ymin = 0.9, ymax = 0.9)
  
  # ggarrange(oisst_map, napa_map, diff_map, nrow = 1, ncol = 3)
  # grid.arrange(napa_title, napa_map, ncol = 1, heights = c(2,15))
  ggsave(plot = tri_plot, filename = paste0("graph/diff_figs/",sum_stat,"_tri_panel.png"), height = 4, width = 12)
}

#
# Distance visual ---------------------------------------------------------

# Map of distance between pixels
# med_dist <- paste("Median dist. (km)\n", round(median(only_water$dist),1))
# dp <- ggplot() + theme_void() +
#   geom_point(data = only_water,
#              aes(x = nav_lon, y = nav_lat, colour = dist), size = 0.001) +
#   geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
#   # geom_polygon(data = map_base, aes(x = -lon, y = -lat, group = group)) +
#   # scale_colour_viridis_c() +
#   scale_color_distiller(palette = "BuPu") +
#   # scale_colour_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
#   # coord_polar() +
#   geom_text(aes(x = 88, y = 53, label = med_dist), colour = "grey90") +
#   coord_map("ortho", orientation = c(90, 0, 0)) +
#   labs(x = "", y = "", title = "Distance (km) between nearest pixels") +
#   theme(legend.position = "bottom",
#         legend.background = element_blank(),
#         legend.title = element_blank(),
#         legend.text = element_text(colour = "black"),
#         axis.text = element_blank(),
#         plot.title = element_text(hjust = 0.5))
# dp
# ggsave(dp, filename = "graph/diff_figs/distance.png", width = 6, height = 7)
# rm(dp)

# Histogram of distanes between pixels

# Line graphs showing relationship between summary stats and pixel distance


# SST visuals -------------------------------------------------------------

# plyr::ldply(colnames(OISST_NAPA_SST_summary[-c(1:4,10)]), .fun = polar_plot_output,
#             .parallel = T, OISST_NAPA_SST_summary)
# dt only due to different colour range caused by large outlying trends
# plyr::ldply(colnames(OISST_NAPA_SST_summary[14]), .fun = polar_plot_output,
#             .parallel = T, df = OISST_NAPA_SST_summary, colour_range = c(-0.2, 0.2))


# Skewness visuals --------------------------------------------------------

# polar_plot_output("skewness", AVISO_NAPA_skewness_summary)


# MHW visuals -------------------------------------------------------------

# plyr::ldply(colnames(OISST_NAPA_MHW_summary_corrected[-c(1:4)]), .fun = polar_plot_output,
#             .parallel = T, OISST_NAPA_MHW_summary_corrected)
# Re-print seas clim, KS test, duration t-test, intensity max t-test
# plyr::ldply(colnames(OISST_NAPA_MHW_summary_corrected[c(6,11,22,23)]), .fun = polar_plot_output,
#             .parallel = T, OISST_NAPA_MHW_summary_corrected)

# Ice visuals -------------------------------------------------------------

# plyr::ldply(colnames(OISST_NAPA_ice_summary[-c(1:4,10)]), .fun = polar_plot_output,
#             .parallel = T, df = OISST_NAPA_ice_summary, legend_title = "Ice prop.")
# ice_dt only due to different colour range caused by large outlying trends
# plyr::ldply(colnames(OISST_NAPA_ice_summary[10]), .fun = polar_plot_output,
#             .parallel = T, df = OISST_NAPA_ice_summary, legend_title = "Ice prop.",
#             colour_range = c(-0.1, 0.1))


# Gulf Stream visuals -----------------------------------------------------

# Due to the custom nature of these visuals, the code for them may be found
# in the script "gulf_stream.R"


# Labrador Sea visuals ----------------------------------------------------

# Due to the custom nature of these visuals, the code for them may be found
# in the script "labrador_sea.R"


# Greenland Sea visuals ---------------------------------------------------

# Due to the custom nature of these visuals, the code for them may be found
# in the script "greenland_sea.R"


# Slide-show visuals ------------------------------------------------------

# Start with the three panel SST mean figure
OISST_NAPA_SST_custom_mean <- OISST_NAPA_SST_summary %>% 
  filter(month == "daily") %>% 
  select(nav_lon:month, mean) #%>% 
  # mutate(dt = ifelse(dt > 0.2, 0.2, dt),
  #        dt = ifelse(dt < -0.2, -0.2, dt))
# tri_panel("mean", OISST_NAPA_SST_custom_mean, chosen_palette = "pretty",
#           colour_range = c(-2.0, 30), diff_range = NA)

# Change the ice_mean_difference colour bar to make the signals more clear
OISST_NAPA_ice_custom <- OISST_NAPA_ice_summary %>% 
  filter(product == "difference") %>% 
  mutate(ice_mean = ifelse(ice_mean > 0.5, 0.5, ice_mean),
         ice_mean = ifelse(ice_mean < -0.5, -0.5, ice_mean))
# polar_plot_output("ice_mean", OISST_NAPA_ice_custom, colour_range = c(-0.5, 0.5))

# Show just the daily decadal trend panels of obs, model, diff
OISST_NAPA_SST_custom_DT <- OISST_NAPA_SST_summary %>% 
  filter(month == "monthly") %>% 
  select(nav_lon:month, dt) %>% 
  mutate(dt = ifelse(dt > 0.2, 0.2, dt),
         dt = ifelse(dt < -0.2, -0.2, dt))
# tri_panel("dt", OISST_NAPA_SST_custom_DT, chosen_palette = "red-blue",
#                       colour_range = c(-0.2, 0.2), diff_range = c(-0.2, 0.2))

# Don’t show monthly panels for detrended correlation figure
# plyr::ldply(colnames(OISST_NAPA_SST_summary[15:16]), .fun = polar_plot_output, .parallel = T,
#             df = filter(OISST_NAPA_SST_summary, month %in% c("daily", "monthly", "yearly")))

# Show three panel figures for the three different MHW metrics
## More narrowly control for colour bar
OISST_NAPA_MHW_corrected_custom <- OISST_NAPA_MHW_summary_corrected %>%
  mutate(count = ifelse(count > 50, 50, count),
         duration_min = ifelse(duration_min > 10, 10, duration_min),
         duration_mean = ifelse(duration_mean > 50, 50, duration_mean),
         duration_max = ifelse(duration_max > 300, 300, duration_max))
tri_panel("count", OISST_NAPA_MHW_corrected_custom,
          colour_range = c(0, 50), diff_range = NA)
tri_panel("duration_min", OISST_NAPA_MHW_corrected_custom,
          colour_range = c(5, 10), diff_range = NA)
tri_panel("duration_mean", OISST_NAPA_MHW_corrected_custom,
          colour_range = c(5, 50), diff_range = NA)
tri_panel("duration_max", OISST_NAPA_MHW_corrected_custom,
          colour_range = c(5, 300), diff_range = NA)
tri_panel("intensity_max_mean", OISST_NAPA_MHW_corrected_custom,
          colour_range = c(0, 7), diff_range = NA)

