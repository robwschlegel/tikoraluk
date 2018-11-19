# The purpose of this script is to define the area of interest around
# thh Gulf Stream, and then to create useful visuals


# Libraries ---------------------------------------------------------------

library(tidyverse)


# Data --------------------------------------------------------------------

# Skewness data
load("../data/AVISO_NAPA_skewness_diff.RData")
GS_skew <- AVISO_NAPA_skewness_diff %>% 
  na.omit() %>% 
  left_join(lon_lat_NAPA_OISST, by = c("nav_lon", "nav_lat")) %>% 
  mutate(lon_O_corrected = ifelse(lon_O > 0, lon_O-360, lon_O),
         product = factor(product, levels = c("AVISO", "NAPA", "difference"))) %>% 
  filter(lon_O_corrected <= -40, lon_O_corrected >= -85,
         lat_O <= 50, lat_O >= 25) %>% 
  group_by(lon_O_corrected, lat_O, product, month) %>% 
  summarise(skewness = mean(skewness, na.rm = T))

# SST summary data
load("../data/OISST_NAPA_difference.RData")
GS_summary <- OISST_NAPA_difference %>% 
  na.omit() %>% 
  # left_join(lon_lat_NAPA_OISST, by = c("nav_lon", "nav_lat")) %>% 
  mutate(lon_O_corrected = ifelse(lon_O > 0, lon_O-360, lon_O)) %>% 
  filter(lon_O_corrected <= -40, lon_O_corrected >= -85,
         lat_O <= 50, lat_O >= 25,
         min_NAPA != 0) %>% 
  group_by(lon_O_corrected, lat_O, month) %>% 
  select(lon_O_corrected, lat_O, month, min_NAPA:dt_diff) %>% 
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
ggplot(filter(GS_skew, month == "overall"), 
       aes(x = lon_O_corrected, y = lat_O)) +
  geom_raster(aes(fill = skewness)) +
  # geom_contour(aes(z = skewness), breaks = c(-2, 2), colour = "grey 80") +
  geom_contour(aes(z = skewness), breaks = c(-1, 1), colour = "grey 50") +
  geom_contour(aes(z = skewness), breaks = 0, colour = "black") +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  coord_cartesian(xlim = c(-85, -40), ylim = c(25.25, 50), expand = F) +
  labs(x = "", y = "") +
  facet_wrap(~product)

# Visualise AVISO and NAPA contours together
ggplot(filter(GS_skew, month == "overall", product != "difference"), 
       aes(x = lon_O_corrected, y = lat_O)) +
  # geom_raster(aes(fill = skewness)) +
  # geom_contour(aes(z = skewness), breaks = c(-2, 2), colour = "grey 80") +
  # geom_contour(data = aes(z = skewness), breaks = c(-1, 1), colour = "grey 50") +
  geom_contour(aes(z = skewness, colour = product), breaks = 0) +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  # scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  coord_cartesian(xlim = c(-85, -40), ylim = c(25.25, 50), expand = F) +
  labs(x = "", y = "")# +
  # facet_wrap(~product)

# Visualise skewness contours on top of SST
plot_1 <- ggplot(filter(GS_skew, month == "overall", product == "AVISO"), 
                 aes(x = lon_O_corrected, y = lat_O)) +
  geom_raster(data = filter(GS_summary, month == "overall"),
              aes(fill = mean_OISST)) +
  # geom_contour(aes(z = skewness), breaks = c(-2, 2), colour = "grey 80") +
  # geom_contour(aes(z = skewness), breaks = c(-1, 1), colour = "grey 50") +
  geom_contour(aes(z = skewness), breaks = 0, colour = "black") +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  # scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  scale_fill_gradientn(colours = pretty_palette, limits = c(0, 30)) +
  coord_cartesian(xlim = c(-85, -40), ylim = c(25.25, 50), expand = F) +
  labs(x = "", y = "", title = "AVISO + OISST", fill = "SST (C)")
plot_1

plot_2 <- ggplot(filter(GS_skew, month == "overall", product == "NAPA"), 
                 aes(x = lon_O_corrected, y = lat_O)) +
  geom_raster(data = filter(GS_summary, month == "overall"),
              aes(fill = mean_NAPA)) +
  # geom_contour(aes(z = skewness), breaks = c(-2, 2), colour = "grey 80") +
  # geom_contour(aes(z = skewness), breaks = c(-1, 1), colour = "grey 50") +
  geom_contour(aes(z = skewness), breaks = 0, colour = "black") +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  # scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  scale_fill_gradientn(colours = pretty_palette, limits = c(0, 30)) +
  coord_cartesian(xlim = c(-85, -40), ylim = c(25.25, 50), expand = F) +
  labs(x = "", y = "", title = "NAPA", fill = "SST (C)")
plot_2

plot_combi <- ggpubr::ggarrange(plot_1, plot_2, common.legend = T)
plot_combi
ggsave(plot_combi, filename = "graph/SST_SSH.pdf", height = 6,width = 13)
