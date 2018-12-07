# The purpose of this script is to define the area of interest around
# the Labrador Sea, and then to create useful visuals


# Libraries ---------------------------------------------------------------

library(tidyverse)


# Meta-data ---------------------------------------------------------------

load("metadata/lon_lat_NAPA_OISST.RData")
load("metadata/lon_OISST.RData")
LS_bound <- c(50, 80, -70, -40)


# Data --------------------------------------------------------------------

# SST summary data
load("../data/OISST_NAPA_SST_summary.RData")
LS_summary <- OISST_NAPA_SST_summary %>% 
  # na.omit() %>% 
  left_join(lon_lat_NAPA_OISST, by = c("nav_lon", "nav_lat")) %>%
  mutate(lon_O_corrected = ifelse(lon_O > 180, lon_O-360, lon_O)) %>% 
  filter(lon_O_corrected <= LS_bound[4], lon_O_corrected >= LS_bound[3],
         lat_O <= LS_bound[2], lat_O >= LS_bound[1],
         min != 0,
         month == "daily") %>% 
  select(lon_O_corrected, lat_O, month, product, min:dt) %>% 
  group_by(lon_O_corrected, lat_O, month, product) %>%
  summarise_all(.funs = "mean")

# Ice data
load("../data/OISST_NAPA_ice_round.RData")
ice_round <- OISST_NAPA_ice_round %>% 
  filter(month == "daily") %>% 
  left_join(lon_lat_NAPA_OISST, by = c("nav_lon", "nav_lat")) %>%
  mutate(lon_O_corrected = ifelse(lon_O > 0, lon_O-360, lon_O))

# The base map
map_base <- ggplot2::fortify(maps::map(fill = TRUE, plot = FALSE)) %>% 
  dplyr::rename(lon = long) #%>% 
# mutate(lon = ifelse(lon < 0, lon+360, lon)) #%>% 
# filter(lon >= 270, lon <= 330,
#        lat <= 50, lat >= 25)

# A custom palette
pretty_palette <- c("#fefefe", "#f963fa", "#020135", "#00efe1", "#057400", "#fcfd00", "#ed0000", "#3d0000")


# Visuals -----------------------------------------------------------------

# Visualise ice contours on top of SST
plot_1 <- ggplot(filter(ice_round, product == "OISST"), 
                 aes(x = nav_lon, y = nav_lat)) +
  geom_raster(data = filter(LS_summary, product == "OISST"), 
              aes(x = lon_O_corrected, y = lat_O, fill = mean)) +
  geom_contour(aes(z = ice_round), breaks = 0.25, colour = "white") +
  geom_contour(aes(z = ice_round), breaks = 0.5, colour = "grey 50") +
  geom_contour(aes(z = ice_round), breaks = 0.75, colour = "black") +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  scale_fill_gradientn(colours = pretty_palette, limits = c(0, 30)) +
  coord_cartesian(xlim = LS_bound[3:4], ylim = LS_bound[1:2], expand = F) +
  labs(x = "", y = "", title = "OISST", fill = "SST (°C)")
plot_1

plot_2 <- ggplot(filter(ice_round, product == "NAPA"), 
                 aes(x = nav_lon, y = nav_lat)) +
  geom_raster(data = filter(LS_summary, product == "NAPA"), 
              aes(x = lon_O_corrected, y = lat_O, fill = mean)) +
  geom_contour(aes(z = ice_round), breaks = 0.25, colour = "white") +
  geom_contour(aes(z = ice_round), breaks = 0.5, colour = "grey 50") +
  geom_contour(aes(z = ice_round), breaks = 0.75, colour = "black") +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  scale_fill_gradientn(colours = pretty_palette, limits = c(0, 30)) +
  coord_cartesian(xlim = LS_bound[3:4], ylim = LS_bound[1:2], expand = F) +
  labs(x = "", y = "", title = "NAPA", fill = "SST (°C)")
plot_2

# Visualise the different 0ice contours on top of the SST differences
plot_3 <- ggplot(filter(ice_round, product != "difference"), 
                 aes(x = nav_lon, y = nav_lat)) +
  geom_raster(data = filter(LS_summary, product == "difference"),
              aes(x = lon_O_corrected, y = lat_O, fill = mean)) +
  geom_contour(aes(z = ice_round, linetype = product), breaks = 0.25, colour = "white") +
  geom_contour(aes(z = ice_round, linetype = product), breaks = 0.5, colour = "grey50") +
  geom_contour(aes(z = ice_round, linetype = product), breaks = 0.75, colour = "black") +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  scale_linetype_manual(values = c("dotted", "solid"), "Product") +
  coord_cartesian(xlim = LS_bound[3:4], ylim = LS_bound[1:2], expand = F) +
  labs(x = "", y = "", title = "NAPA - OISST", fill = "Mean\ndifference (°C)") +
  theme(legend.position = "bottom",
        legend.box = "vertical")
plot_3

plot_combi_1 <- ggpubr::ggarrange(plot_1, plot_2, common.legend = T, legend = "bottom")
plot_combi_2 <- ggpubr::ggarrange(plot_combi_1, plot_3, widths = c(2, 1))
plot_combi_2
ggsave(plot_combi_2, filename = "graph/LS/SST_Ice.png", height = 6, width = 12)
