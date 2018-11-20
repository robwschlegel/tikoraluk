# The purpose of this script is to have one central place for the creation 
# of the visualisations for the NAPA OISST comparisons


# Libraries ---------------------------------------------------------------

library(tidyverse)


# Meta-data ---------------------------------------------------------------

load("metadata/lon_lat_NAPA_OISST.RData")
load("metadata/lon_OISST.RData")


# Data --------------------------------------------------------------------

# The various comparison results
load("../data/OISST_NAPA_SST_summary.RData")
load("../data/OISST_NAPA_MHW_summary.RData")
load("../data/AVISO_NAPA_skewness_diff.RData")
load("../data/OISST_NAPA_ice_summary.RData")

# A custom palette
pretty_palette <- c("#fefefe", "#f963fa", "#020135", "#00efe1", "#057400", "#fcfd00", "#ed0000", "#3d0000")

# The base map
map_base <- ggplot2::fortify(maps::map(fill = TRUE, plot = FALSE)) %>% 
  dplyr::rename(lon = long) %>% 
  # filter(lat >= 25.6, lon <= 180) #%>%
  filter(lat >= 25.6) %>%
  mutate(group = ifelse(lon > 180, group+9999, group),
         lon = ifelse(lon > 180, lon-360, lon))


# Prep --------------------------------------------------------------------

# Remove NA and 0 rows for now...
# OISST_NAPA_difference <- OISST_NAPA_difference %>% 
#   na.omit() %>% 
#   filter(min_NAPA != 0) %>% 
#   mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
#                                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
#                                           "overall")))

# Data frames with only difference results
# OISST_NAPA_SST_summary_diff <- filter(OISST_NAPA_SST_summary, product == "difference")
# OISST_NAPA_MHW_summary_diff <- filter(OISST_NAPA_MHW_summary, product == "difference")
# OISST_NAPA_ice_summary_diff <- filter(OISST_NAPA_ice_summary, product == "difference")

# # Correlation values
# OISST_NAPA_correlation <- OISST_NAPA_correlation %>% 
#   left_join(lon_lat_NAPA_OISST, by = c("lon", "lat"))

# No-land mask
# only_water <- OISST_NAPA_correlation %>% 
#   select(nav_lon, nav_lat) %>% 
#   unique()

# lon_lat_NAPA_OISST_no_land <- left_join(only_water, lon_lat_NAPA_OISST, 
#                                         by = c("nav_lon", "nav_lat")) %>% 
#   arrange(dist)



# Functions ---------------------------------------------------------------

# df <- AVISO_NAPA_skewness_diff

# Function that creates and saves multiple plots from one input dataframe
polar_plot_output <- function(df, sum_stat, plot_name = NA, plot_title = NA,
                              chosen_palette = "red-blue", colour_range = NA, 
                              legend_title = NA, legend_position = c(0.5, 0.1)){
  if(is.na(plot_name)) plot_name <- sum_stat
  if(is.na(plot_title)) plot_title <- sum_stat
  if(is.na(legend_title)) legend_title <- "Model - Obs (°C)"
  if(is.na(colour_range)){
    colour_range <- c(min(df[,colnames(df) == sum_stat], na.rm = T), 
                      max(df[,colnames(df) == sum_stat], na.rm = T))
  }
  # NAPA
  if("NAPA" %in% unique(df$product)){
    polar_plots(filter(df, product ==  "NAPA"), sum_stat, plot_name = plot_name, 
                plot_title = plot_title,  chosen_palette = chosen_palette, 
                colour_range = colour_range,  legend_title = legend_title, 
                legend_position = legend_position)
  }

  # OISST
  
  # AVISO
  
  # difference
  
}


# Plotting function
polar_plots <- function(df, sum_stat, plot_name, plot_title, chosen_palette, 
                        colour_range,  legend_title, legend_position){
  pp <- ggplot() + theme_void() +
    geom_point(data = df, size = 0.001,
               aes_string(x = "nav_lon", y = "nav_lat", colour = sum_stat)) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
    coord_map("ortho", orientation = c(90, 0, 0)) +
    labs(x = "", y = "", colour = legend_title, title = paste0(plot_title,"\n")) +
    guides(colour = guide_colourbar(title.position = "top", direction = "horizontal")) +
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
  ggsave(pp_col, filename = paste0("graph/diff_figs/",plot_name,".png"), width = 9, height = 10)
}#


# Distance visual ---------------------------------------------------------

# Map of distance between pixels
dp <- ggplot() + theme_void() +
  geom_point(data = lon_lat_NAPA_OISST_no_land,
             aes(x = -nav_lon, y = -nav_lat, colour = dist), size = 0.001) +
  geom_polygon(data = map_base, aes(x = -lon, y = -lat, group = group)) +
  # scale_colour_viridis_c() +
  scale_color_distiller(palette = "BuPu") +
  # scale_colour_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  coord_polar() +
  labs(x = "", y = "", title = "Distance (km) between nearest pixels") +
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(colour = "black"),
        axis.text = element_blank(),
        plot.title = element_text(hjust = 0.5))
dp
ggsave(dp, filename = "graph/diff_figs/distance.png", width = 4, height = 5)
rm(dp)

# Histogram of distanes between pixels

# Line graphs showing relationship between summary stats and pixel distance


# Difference visuals ------------------------------------------------------

polar_plot_output("min_diff", OISST_NAPA_difference_only)
polar_plot_output("quant_10_diff", OISST_NAPA_difference_only)
polar_plot_output("quant_25_diff", OISST_NAPA_difference_only)
polar_plot_output("quant_50_diff", OISST_NAPA_difference_only)
polar_plot_output("quant_75_diff", OISST_NAPA_difference_only)
polar_plot_output("quant_90_diff", OISST_NAPA_difference_only)
polar_plot_output("max_diff", OISST_NAPA_difference_only)
polar_plot_output("mean_diff", OISST_NAPA_difference_only)
polar_plot_output("sd_diff", OISST_NAPA_difference_only)
polar_plot_output("dt_diff", OISST_NAPA_difference_only)


# Correlation visuals -----------------------------------------------------



# Skewness visual ---------------------------------------------------------

polar_plot_output("skewness", AVISO_NAPA_skewness_diff)
polar_plot_output("skewness", filter(AVISO_NAPA_skewness_diff, product == "AVISO"), "skewness_AVISO")
polar_plot_output("skewness", filter(AVISO_NAPA_skewness_diff, product == "NAPA"), "skewness_NAPA")


# MHW difference visuals --------------------------------------------------

# Visuals for the clim stat differences
polar_plot_output("seas_min", OISST_NAPA_MHW_summary_diff)
polar_plot_output("seas_mean", OISST_NAPA_MHW_summary_diff)
polar_plot_output("seas_max", OISST_NAPA_MHW_summary_diff)
polar_plot_output("thresh_min", OISST_NAPA_MHW_summary_diff)
polar_plot_output("thresh_mean", OISST_NAPA_MHW_summary_diff)
polar_plot_output("thresh_max", OISST_NAPA_MHW_summary_diff)

# Visuals for the clim t-tests and KS-tests
polar_plot_output("seas_t_test", OISST_NAPA_MHW_summary_diff)
polar_plot_output("thresh_t_test", OISST_NAPA_MHW_summary_diff)
polar_plot_output("seas_KS_test", OISST_NAPA_MHW_summary_diff)
polar_plot_output("thresh_KS_test", OISST_NAPA_MHW_summary_diff)

# Visuals for the event count differences
polar_plot_output("count", OISST_NAPA_MHW_summary_diff)

# Visuals for the event stat differences
polar_plot_output("duration_min", OISST_NAPA_MHW_summary_diff)
polar_plot_output("duration_mean", OISST_NAPA_MHW_summary_diff)
polar_plot_output("duration_max", OISST_NAPA_MHW_summary_diff)
polar_plot_output("intensity_max_min", OISST_NAPA_MHW_summary_diff)
polar_plot_output("intensity_max_mean", OISST_NAPA_MHW_summary_diff)
polar_plot_output("intensity_max_max", OISST_NAPA_MHW_summary_diff)

# Visuals for the event t-tests
polar_plot_output("duration_t_test", OISST_NAPA_MHW_summary_diff)
polar_plot_output("intensity_max_t_test", OISST_NAPA_MHW_summary_diff)

# Visualise duration and maximum intensity values for just NAPA and OISST
polar_plot_output("duration_mean", filter(OISST_NAPA_MHW_summary, product == "NAPA"), 
               "duration_mean_NAPA", "pretty")
polar_plot_output("duration_mean", filter(OISST_NAPA_MHW_summary, product == "OISST"), 
               "duration_mean_OISST", "pretty")
polar_plot_output("intensity_max_mean", filter(OISST_NAPA_MHW_summary, product == "NAPA"), 
               "intensity_max_mean_NAPA", "pretty")
polar_plot_output("intensity_max_mean", filter(OISST_NAPA_MHW_summary, product == "OISST"), 
               "intensity_max_mean_OISST", "pretty")


# Ice visuals -------------------------------------------------------------


# Gulf Stream visuals -----------------------------------------------------
#

# Full visuals for one summary stat ---------------------------------------

# # Remove NA and 0 rows for now...
# diff_res_na_omit <- OISST_NAPA_difference %>% 
#   na.omit() %>% 
#   filter(min_NAPA != 0)
# 
# # Plotting function
# polar_map <- function(the_chosen, diff_val = F){
#   pp <- ggplot() + theme_void() +
#     geom_polygon(data = map_base, aes(x = -lon, y = -lat, group = group)) +
#     geom_point(data = filter(diff_res_na_omit, month == "overall"),
#                aes_string(x = "-nav_lon", y = "-nav_lat", colour = the_chosen), size = 0.001) +
#     # scale_colour_viridis_c() +
#     scale_colour_gradientn(colours = pretty_palette) +
#     coord_polar() +
#     # scale_y_continuous(limits = c(-90, -25)) +
#     labs(x = "", y = "") +
#     theme(legend.position = c(0.8, 0.5),
#           legend.background = element_blank(),
#           legend.title = element_blank(),
#           legend.text = element_text(colour = "white"),
#           axis.text = element_blank()#,
#           # axis.ticks = element_blank(),
#           # plot.background = element_blank(),
#           # panel.background = element_blank(),
#           # plot.title = element_text(hjust = 0.5, vjust = 1),
#           # axis.line = element_blank()
#     )
#   if(diff_val){
#     suppressMessages(
#       pp <- pp + scale_colour_gradient2(low = "blue", 
#                                         mid = "white",
#                                         high = "red",
#                                         midpoint = 0)
#     )
#   }
#   return(pp)
# }
# 
# # Make some pretty pictures
# napa_map <- polar_map("quant_50_NAPA")
# napa_title <- text_grob(label = "NAPA median", face = "bold",
#                         size = 16, just = c("center","center"))
# oisst_map <- polar_map("quant_50_OISST")
# oisst_title <- text_grob(label = "OISST median",  face = "bold",
#                          size = 16, just = c("center","center"))
# diff_map <- polar_map("quant_50_diff", diff_val = T)
# diff_title <- text_grob(label = "NAPA - OISST", face = "bold",
#                         size = 16, just = c("center","center"))
# 
# tri_plot <- ggplot() +
#   theme_void() +
#   geom_blank() +
#   scale_x_continuous(limits = c(0.1, 2.9)) +
#   scale_y_continuous(limits = c(0.1,0.9)) +
#   annotation_custom(grob = ggplotGrob(oisst_map),
#                     xmin = -0.1, xmax = 1.1,
#                     ymin = 0, ymax = 1) +
#   annotation_custom(grob = oisst_title,
#                     xmin = -0.1, xmax = 1.1,
#                     ymin = 0.8, ymax = 0.9) +
#   annotation_custom(grob = ggplotGrob(napa_map),
#                     xmin = 0.9, xmax = 2.1,
#                     ymin = 0, ymax = 1) +
#   annotation_custom(grob = napa_title,
#                     xmin = 0.9, xmax = 2.1,
#                     ymin = 0.8, ymax = 0.9) +
#   annotation_custom(grob = ggplotGrob(diff_map),
#                     xmin = 1.9, xmax = 3.1,
#                     ymin = 0, ymax = 1) +
#   annotation_custom(grob = diff_title,
#                     xmin = 1.9, xmax = 3.1,
#                     ymin = 0.8, ymax = 0.9)
# 
# # ggarrange(oisst_map, napa_map, diff_map, nrow = 1, ncol = 3)
# # grid.arrange(napa_title, napa_map, ncol = 1, heights = c(2,15)) 
# ggsave(plot = tri_plot, filename = "graph/median_diff.png", height = 6, width = 16)

