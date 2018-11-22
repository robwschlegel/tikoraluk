# The purpose of this script is to experiment with contouring and labelling options


# Libraries ---------------------------------------------------------------

library(tidyverse)
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
  # filter(lat >= 25.6, lon <= 180) #%>%
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
#   group_by(nav_lon, nav_lat, month, product) %>% 
#   summarise(ice_round = round(ice_mean, 1)) %>% 
#   as.data.frame()
# save(OISST_NAPA_ice_round, file = "../data/OISST_NAPA_ice_round.RData")

# No-land mask
## NB: Only needs to be run once
## Is loaded above
# only_water <- OISST_NAPA_SST_summary %>%
#   select(nav_lon, nav_lat) %>%
#   dplyr::distinct()
# save(only_water, file = "metadata/only_water.RData")

lon_lat_NAPA_OISST_no_land <- left_join(only_water, lon_lat_NAPA_OISST,
                                        by = c("nav_lon", "nav_lat")) %>%
  arrange(dist)

# Only the 0.5 ice coverage pixels
ice_sub <- filter(OISST_NAPA_ice_round) %>% 
  right_join(only_water, by = c("nav_lon", "nav_lat")) %>%
  na.omit()
# select(nav_lon, nav_lat, month, product, ice_round) %>% 
# na.omit()


# testing -----------------------------------------------------------------

# df <- filter(OISST_NAPA_SST_summary, product == "difference", month == "daily")
# ice_OISST <- filter(OISST_NAPA_ice_round, product == "OISST", month == "daily", ice_round == 0.5)
# ice_OISST_edge <- ice_OISST[chull(ice_OISST[,1:2]),]
# ice_NAPA <- filter(OISST_NAPA_ice_round, month == "daily", product == "NAPA", ice_round == 0.5)
# ice_sub_5 <- filter(ice_sub, month == "daily", product == "OISST", ice_round == 0.5) 
# ice_sub_all <- filter(ice_sub, month == "daily", product == "OISST") %>%
#   # mutate(nav_lon = round(nav_lon, 0),
#   #        nav_lat = round(nav_lat, 0)) %>%
#   group_by(nav_lon, nav_lat, month, product) %>%
#   summarise(ice_round = mean(ice_round, na.rm = T)) %>% 
#   filter(ice_round %in% c(0.4, 0.5, 0.6))
# sum_stat <- "min"
# plot_name <- "min_difference"
# plot_title <- "min difference"
# chosen_palette <- "red-blue"
# colour_range <- c(-5, 5)
# # colour_range <- c(min(df[,colnames(df) == sum_stat], na.rm = T),
# #                   max(df[,colnames(df) == sum_stat], na.rm = T))
# legend_title <- "Model - Obs\nTemperature (°C)"
# legend_position <- c(0.9, 0.1)
# 
# 
# 
# ggplot() + theme_void() +
#   # geom_point(size = 0.001, aes_string(x = "nav_lon", y = "nav_lat", colour = sum_stat)) +
#   # stat_density_2d(data = ice_sub_all, aes(x = nav_lon, y = nav_lat, colour = ice_round)) +
#   # geom_raster(data = ice_sub_all, aes(x = nav_lon, y = nav_lat, fill = ice_round)) +
#   # geom_contour(data = ice_sub_all, aes(x = nav_lon, y = nav_lat, z = ice_round), bins = 1) +
#   # stat_contour(data = ice_sub_all, aes(x = nav_lon, y = nav_lat, z = ice_round)) +
#   geom_
#   # geom_hex() +
#   # geom_point(data = ice_OISST, aes(x = nav_lon, y = nav_lat), colour = "green", size = 0.1, alpha = 0.1) +
#   # geom_point(data = ice_NAPA, aes(x = nav_lon, y = nav_lat), colour = "yellow", size = 0.1, alpha = 0.1) +
#   # geom_point(data = ice_sub, aes(x = nav_lon, y = nav_lat, fill = product), shape = 21, stroke = 0, size = 0.01, alpha = 0.3) +
#   # geom_line(data = ice_sub, aes(x = nav_lon, y = nav_lat, group = product)) +
#   # geom_polygon(data = ice_OISST_edge, aes(x = nav_lon, y = nav_lat), colour = "green", size = 0.1, alpha = 0.3) +
#   # geom_contour(data = ice_OISST, aes(z = ice_round, x = nav_lon, y = nav_lat), breaks = 0.5) +
#   geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
#   coord_map("ortho", orientation = c(90, 0, 0)) +
#   # scale_fill_manual(values = c("green", "yellow")) +
#   labs(x = "", y = "", colour = legend_title, title = paste0(plot_title,"\n")) +
#   # guides(colour = guide_colourbar(title.position = "top", direction = "horizontal"),
#   #        fill = guide_legend(title.position = "top", direction = "horizontal",
#   #                            override.aes = list(size = 5, alpha = 1))) +
#   theme(legend.background = element_blank(),
#         legend.text = element_text(colour = "black"),
#         legend.position = legend_position,
#         axis.text = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 25),
#         strip.text = element_text(size = 18),
#         panel.border = element_rect(colour = "black", fill = NA),
#         panel.spacing.x = unit(0,"cm"),
#         panel.spacing.y = unit(0.5,"cm"),
#         strip.background = element_rect(fill = "grey80")) #+
#   # scale_colour_gradient2(low = "blue", mid = "white", high = "red",
#   #                        midpoint = 0, limits = colour_range)




