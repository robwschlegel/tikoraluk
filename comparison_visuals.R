# The purpose of this script is to have one central place for the creation 
# of the visualisations for the NAPA OISST comparisons


# libraries ---------------------------------------------------------------

library(tidyverse)



# Meta-data ---------------------------------------------------------------

load("metadata/lon_lat_NAPA_OISST.RData")
load("metadata/lon_OISST.RData")


# Data --------------------------------------------------------------------

load("../data/OISST_NAPA_difference.RData")
load("../data/OISST_NAPA_correlation.RData")

pretty_palette <- c("#fefefe", "#f963fa", "#020135", "#00efe1", "#057400", "#fcfd00", "#ed0000", "#3d0000")


# Full visuals for one summary stat ---------------------------------------

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
    scale_colour_gradientn(colours = pretty_palette) +
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


# Difference visuals ------------------------------------------------------

# Remove NA and 0 rows for now...
OISST_NAPA_difference <- OISST_NAPA_difference %>% 
  na.omit() %>% 
  filter(min_NAPA != 0) %>% 
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                                          "overall")))

# Data frame with only difference results
OISST_NAPA_difference_only <- OISST_NAPA_difference %>% 
  select(lon:month, min_diff:dt_diff)

# The base map
map_base <- ggplot2::fortify(maps::map(fill = TRUE, plot = FALSE)) %>% 
  dplyr::rename(lon = long) %>% 
  filter(lat >= 25.6, lon <= 180) #%>%
  # filter(lat >= 25.6)# %>%
  # mutate(lon = ifelse(lon > 180, lon-360, lon))

# Plotting function
polar_map_diff <- function(sum_stat){
  pp <- ggplot() + theme_void() +
    geom_point(data = OISST_NAPA_difference_only,
               aes_string(x = "-nav_lon", y = "-nav_lat", colour = sum_stat), size = 0.001) +
    geom_polygon(data = map_base, aes(x = -lon, y = -lat, group = group)) +
    # scale_colour_viridis_c() +
    # scale_color_distiller(palette = "Spectral") +
    # scale_colour_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    scale_colour_gradientn(colours = pretty_palette) +
    coord_polar() +
    labs(x = "", y = "", title = sum_stat) +
    theme(legend.position = "bottom",
          legend.background = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(colour = "black"),
          axis.text = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 30), 
          strip.text = element_text(size = 16)) +
    facet_wrap(~month, ncol = 3)
  ggsave(pp, filename = paste0("graph/diff_figs/",sum_stat,".png"), width = 12, height = 21)
}

polar_map_diff("min_diff")
polar_map_diff("quant_10_diff")
polar_map_diff("quant_25_diff")
polar_map_diff("quant_50_diff")
polar_map_diff("quant_75_diff")
polar_map_diff("quant_90_diff")
polar_map_diff("max_diff")
polar_map_diff("mean_diff")
polar_map_diff("sd_diff")
polar_map_diff("dt_diff")


# Correlation visuals -----------------------------------------------------

OISST_NAPA_correlation <- OISST_NAPA_correlation %>% 
  left_join(lon_lat_NAPA_OISST, by = c("lon", "lat"))

cp <- ggplot() + theme_void() +
  geom_polygon(data = map_base, aes(x = -lon, y = -lat, group = group)) +
  geom_point(data = OISST_NAPA_correlation,
             aes(x = -nav_lon, y = -nav_lat, colour = cor), size = 0.001) +
  scale_colour_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  coord_polar() +
  labs(x = "", y = "", title = "Per pixel correlation (r)") +
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(colour = "black"),
        axis.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 30), 
        strip.text = element_text(size = 16)) +
  facet_wrap(~time, ncol = 3)
ggsave(cp, filename = "graph/diff_figs/correlation.png", width = 12, height = 21)
rm(cp)


# Distance visual ---------------------------------------------------------

only_water <- OISST_NAPA_correlation %>% 
  select(nav_lon, nav_lat) %>% 
  unique()

lon_lat_NAPA_OISST_no_land <- left_join(only_water, lon_lat_NAPA_OISST, 
                                        by = c("nav_lon", "nav_lat")) %>% 
  arrange(dist)

# Map of distance between pixels
dp <- ggplot() + theme_void() +
  geom_polygon(data = map_base, aes(x = -lon, y = -lat, group = group)) +
  geom_point(data = lon_lat_NAPA_OISST_no_land,
             aes(x = -nav_lon, y = -nav_lat, colour = dist), size = 0.001) +
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
ggsave(dp, filename = "graph/diff_figs/distance.png", width = 4, height = 5)
rm(dp)

# Histogram of distanes between pixels

# Line graphs showing relationship between summary stats and pixel distance

