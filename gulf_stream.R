# The purpose of this script is to define the area of interest around
# thh Gulf Stream, and then to create useful visuals


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(directlabels)


# Data --------------------------------------------------------------------

# Skewness data
load("../data/AVISO_NAPA_skewness_summary.RData")
GS_skew <- AVISO_NAPA_skewness_summary %>% 
  # na.omit() %>% 
  left_join(lon_lat_NAPA_OISST, by = c("nav_lon", "nav_lat")) %>% 
  mutate(lon_O_corrected = ifelse(lon_O > 0, lon_O-360, lon_O),
         product = factor(product, levels = c("AVISO", "NAPA", "difference"))) %>% 
  filter(lon_O_corrected <= -40, lon_O_corrected >= -85,
         lat_O <= 50, lat_O >= 25,
         month == "daily") %>% 
  group_by(lon_O_corrected, lat_O, product, month) %>% 
  summarise(skewness = mean(skewness, na.rm = T)) %>% 
  mutate(skew_label = as.factor(round(skewness, 0))) %>% 
  arrange(skew_label)

# SST summary data
load("../data/OISST_NAPA_SST_summary.RData")
GS_summary <- OISST_NAPA_SST_summary %>% 
  # na.omit() %>% 
  left_join(lon_lat_NAPA_OISST, by = c("nav_lon", "nav_lat")) %>%
  mutate(lon_O_corrected = ifelse(lon_O > 0, lon_O-360, lon_O)) %>% 
  filter(lon_O_corrected <= -40, lon_O_corrected >= -85,
         lat_O <= 50, lat_O >= 25,
         min != 0,
         month == "daily") %>% 
  select(lon_O_corrected, lat_O, month, product, min:dt) %>% 
  group_by(lon_O_corrected, lat_O, month, product) %>%
  summarise_all(.funs = "mean")

# Metadata
load("metadata/lon_lat_NAPA_OISST.RData")
load("metadata/lon_OISST.RData")
GS_bound <- c(25, 50, -80, -40)

# The base map
map_base <- ggplot2::fortify(maps::map(fill = TRUE, plot = FALSE)) %>% 
  dplyr::rename(lon = long) #%>% 
  # mutate(lon = ifelse(lon < 0, lon+360, lon)) #%>% 
  # filter(lon >= 270, lon <= 330,
  #        lat <= 50, lat >= 25)

# A custom palette
pretty_palette <- c("#fefefe", "#f963fa", "#020135", "#00efe1", "#057400", "#fcfd00", "#ed0000", "#3d0000")


# Functions ---------------------------------------------------------------



# Analyses ----------------------------------------------------------------



# Visuals -----------------------------------------------------------------

# Visualise one skewness map
plot_1 <- ggplot(GS_skew, aes(x = lon_O_corrected, y = lat_O)) +
  geom_raster(aes(fill = skewness)) +
  # geom_contour(aes(z = skewness), breaks = c(-2, 2), colour = "grey 80") +
  geom_contour(aes(z = skewness), breaks = c(-1, 1), colour = "grey 50") +
  geom_contour(aes(z = skewness), breaks = 0, colour = "black") +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  coord_cartesian(xlim = c(-85, -40), ylim = c(25.25, 50), expand = F) +
  labs(x = "", y = "") +
  facet_wrap(~product) +
  theme(legend.position = "bottom")
plot_1
ggsave(plot_1, filename = "graph/GS/skewness.png", width = 10, height = 3)

# Visualise AVISO and NAPA contours together
plot_2 <- ggplot(filter(GS_skew, product != "difference"), 
                 aes(x = lon_O_corrected, y = lat_O)) +
  # geom_raster(aes(fill = skewness)) +
  # geom_contour(aes(z = skewness), breaks = c(-2, 2), colour = "grey 80") +
  # geom_contour(data = aes(z = skewness), breaks = c(-1, 1), colour = "grey 50") +
  geom_contour(aes(z = skewness, colour = product), breaks = 0) +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  # scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  coord_cartesian(xlim = c(-85, -40), ylim = c(25.25, 50), expand = F) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom")
plot_2
ggsave(plot_2, filename = "graph/GS/contour_only.png", width = 4, height = 3)

# Visualise skewness contours on top of SST
plot_3 <- ggplot(filter(GS_skew, product == "AVISO"), 
                 aes(x = lon_O_corrected, y = lat_O)) +
  geom_raster(data = filter(GS_summary, product == "OISST"), aes(fill = mean)) +
  # geom_contour(aes(z = skewness), breaks = c(-2, 2), colour = "grey 80") +
  geom_contour(aes(z = skewness), breaks = c(-1, 1), colour = "grey 50") +
  geom_contour(aes(z = skewness), breaks = 0, colour = "black") +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  # scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  scale_fill_gradientn(colours = pretty_palette, limits = c(0, 30)) +
  coord_cartesian(xlim = c(-85, -40), ylim = c(25.5, 50), expand = F) +
  labs(x = "", y = "", title = "AVISO + OISST", fill = "SST (°C)")
plot_3

plot_4 <- ggplot(filter(GS_skew, product == "NAPA"), 
                 aes(x = lon_O_corrected, y = lat_O)) +
  geom_raster(data = filter(GS_summary, product == "NAPA"), aes(fill = mean)) +
  # geom_contour(aes(z = skewness), breaks = c(-2, 2), colour = "grey 80") +
  geom_contour(aes(z = skewness), breaks = c(-1, 1), colour = "grey 50") +
  geom_contour(aes(z = skewness), breaks = 0, colour = "black") +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  # geom_dl(method = "calc.boxes", aes(label = skewness)) +
  # scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  scale_fill_gradientn(colours = pretty_palette, limits = c(0, 30)) +
  coord_cartesian(xlim = c(-85, -40), ylim = c(25.5, 50), expand = F) +
  labs(x = "", y = "", title = "NAPA", fill = "SST (°C)")
plot_4

# Visualise the different 0 skewness contours on top of the SST differences
plot_5 <- ggplot(filter(GS_skew, product != "difference"), 
                 aes(x = lon_O_corrected, y = lat_O)) +
  geom_raster(data = filter(GS_summary, product == "difference"), aes(fill = mean)) +
  # geom_contour(aes(z = skewness), breaks = c(-2, 2), colour = "grey 80") +
  # geom_contour(data = aes(z = skewness), breaks = c(-1, 1), colour = "grey 50") +
  geom_contour(aes(z = skewness, colour = product), breaks = 0) +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  scale_colour_manual(values = c("black", "purple")) +
  coord_cartesian(xlim = c(-85, -40), ylim = c(25.5, 50), expand = F) +
  labs(x = "", y = "", title = "NAPA - OISST", fill = "Mean\ndifference (°C)") +
  theme(legend.position = "bottom")
plot_5

plot_combi_1 <- ggpubr::ggarrange(plot_3, plot_4, common.legend = T, legend = "bottom")
plot_combi_2 <- ggpubr::ggarrange(plot_combi_1, plot_5, widths = c(2, 1))
plot_combi_2
ggsave(plot_combi_2, filename = "graph/GS/SST_SSH.png", height = 4, width = 15)

