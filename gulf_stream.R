# The purpose of this script is to define the area of interest around
# the Gulf Stream, and then to create useful visuals


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(akima)
# library(directlabels)


# Meta-data ---------------------------------------------------------------

load("metadata/lon_lat_NAPA_OISST.RData")
load("metadata/lon_OISST.RData")
GS_bound <- c(25, 50, -85, -40)
# load("metadata/only_water.RData")
# only_water <- only_water %>% 
#   mutate(lon_O_corrected = ifelse(lon_O > 180, lon_O-360, lon_O))


# Data --------------------------------------------------------------------

# Skewness data
load("../data/AVISO_NAPA_skewness_summary.RData")
GS_skew <- AVISO_NAPA_skewness_summary %>% 
  # na.omit() %>% 
  left_join(lon_lat_NAPA_OISST, by = c("nav_lon", "nav_lat")) %>% 
  mutate(lon_O_corrected = ifelse(lon_O > 180, lon_O-360, lon_O),
         product = factor(product, levels = c("AVISO", "NAPA", "difference"))) %>% 
  filter(lon_O_corrected <= GS_bound[4], lon_O_corrected >= GS_bound[3],
         lat_O <= GS_bound[2], lat_O >= GS_bound[1],
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
  mutate(lon_O_corrected = ifelse(lon_O > 180, lon_O-360, lon_O)) %>% 
  filter(lon_O_corrected <= GS_bound[4], lon_O_corrected >= GS_bound[3],
         lat_O <= GS_bound[2], lat_O >= GS_bound[1],
         min != 0,
         month == "daily") %>% 
  select(lon_O_corrected, lat_O, month, product, min:dt) %>% 
  group_by(lon_O_corrected, lat_O, month, product) %>%
  summarise_all(.funs = "mean")


# The base map
map_base <- ggplot2::fortify(maps::map(fill = TRUE, plot = FALSE)) %>% 
  dplyr::rename(lon = long) #%>% 
  # mutate(lon = ifelse(lon < 0, lon+360, lon)) #%>% 
  # filter(lon >= 270, lon <= 330,
  #        lat <= 50, lat >= 25)

# A custom palette
pretty_palette <- c("#fefefe", "#f963fa", "#020135", "#00efe1", "#057400", "#fcfd00", "#ed0000", "#3d0000")

# The grid for interpolation
GS_grid_base <- GS_summary %>% 
  ungroup() %>% 
  select(lon_O_corrected, lat_O) %>% 
  distinct() %>% 
  filter(lat_O > GS_bound[1], lat_O < GS_bound[2],
         lon_O_corrected > GS_bound[3], lon_O_corrected < GS_bound[4]) %>% 
  summarise_all(.funs = c("max", "min"))

GS_grid <- expand.grid(seq(GS_grid_base$lon_O_corrected_min, GS_grid_base$lon_O_corrected_max, by = 0.25),
                       seq(GS_grid_base$lat_O_min, GS_grid_base$lat_O_max, by = 0.25)) %>% 
  dplyr::rename(lon_O_corrected = Var1, lat_O = Var2)

# Function for interpolating while nested
interpp_data <- function(df_base, df_grid, interpp_stat){
  df_base <- data.frame(df_base)
  suppressWarnings(
  res <- as.data.frame(interpp(x = as.vector(df_base$lon_O_corrected), y = as.vector(df_base$lat_O), 
                 as.vector(df_base[,colnames(df_base) == interpp_stat]),
                    xo = as.vector(df_grid$lon_O_corrected), yo = as.vector(df_grid$lat_O), linear = T,
                    extrap = FALSE, duplicate = "mean"))
  )
  colnames(res) <- c("lon_O_corrected", "lat_O", "interp")
  suppressMessages(
  df_res <- right_join(df_base, res)
  )
  return(df_res)
}

GS_summary_interp <- GS_summary %>% 
  group_by(product, month) %>% 
  nest() %>% 
  mutate(interp = map(data, interpp_data, df_grid = GS_grid, interpp_stat = "mean")) %>% 
  select(-data) %>%
  unnest()

GS_skew_interp <- GS_skew %>% 
  group_by(product, month) %>% 
  nest() %>% 
  mutate(interp = map(data, interpp_data, df_grid = GS_grid, interpp_stat = "skewness")) %>% 
  select(-data) %>%
  unnest()


# Functions ---------------------------------------------------------------



# Analyses ----------------------------------------------------------------



# Visuals -----------------------------------------------------------------

# Visualise one skewness map
plot_1 <- ggplot(GS_skew_interp, aes(x = lon_O_corrected, y = lat_O)) +
  geom_raster(aes(fill = interp)) +
  geom_contour(aes(z = interp), breaks = c(-1, 1), colour = "grey 50") +
  geom_contour(aes(z = interp), breaks = 0, colour = "black") +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  coord_cartesian(xlim = GS_bound[3:4], ylim = GS_bound[1:2], expand = F) +
  labs(x = "", y = "") +
  facet_wrap(~product) +
  theme(legend.position = "bottom")
plot_1
ggsave(plot_1, filename = "graph/GS/skewness.png", width = 10, height = 3)

# Visualise AVISO and NAPA contours together
plot_2 <- ggplot(filter(GS_skew_interp, product != "difference"), 
                 aes(x = lon_O_corrected, y = lat_O)) +
  geom_contour(aes(z = interp, colour = product), breaks = 0) +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  coord_cartesian(xlim = GS_bound[3:4], ylim = GS_bound[1:2], expand = F) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom")
plot_2
ggsave(plot_2, filename = "graph/GS/contour_only.png", width = 4, height = 3)

# Visualise skewness contours on top of SST
plot_3 <- ggplot(filter(GS_skew_interp, product == "AVISO"), 
                 aes(x = lon_O_corrected, y = lat_O)) +
  geom_raster(data = filter(GS_summary_interp, product == "OISST"), aes(fill = interp)) +
  geom_contour(aes(z = interp), breaks = c(-1, 1), colour = "grey 50") +
  geom_contour(aes(z = interp), breaks = 0, colour = "black") +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  scale_fill_gradientn(colours = pretty_palette, limits = c(-2, 30)) +
  coord_cartesian(xlim = GS_bound[3:4], ylim = GS_bound[1:2], expand = F) +
  labs(x = "", y = "", title = "AVISO + OISST", fill = "SST (°C)")
plot_3

plot_4 <- ggplot(filter(GS_skew_interp, product == "NAPA"), 
                 aes(x = lon_O_corrected, y = lat_O)) +
  geom_raster(data = filter(GS_summary_interp, product == "NAPA"), aes(fill = interp)) +
  geom_contour(aes(z = interp), breaks = c(-1, 1), colour = "grey 50") +
  geom_contour(aes(z = interp), breaks = 0, colour = "black") +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  scale_fill_gradientn(colours = pretty_palette, limits = c(-2, 30)) +
  coord_cartesian(xlim = GS_bound[3:4], ylim = GS_bound[1:2], expand = F) +
  labs(x = "", y = "", title = "NAPA", fill = "SST (°C)")
plot_4

# Visualise the different 0 skewness contours on top of the SST differences
plot_5 <- ggplot(filter(GS_skew_interp, product != "difference"), 
                 aes(x = lon_O_corrected, y = lat_O)) +
  geom_raster(data = filter(GS_summary_interp, product == "difference"), aes(fill = interp)) +
  geom_contour(aes(z = interp, colour = product), breaks = 0) +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  scale_colour_manual(values = c("black", "purple")) +
  coord_cartesian(xlim = GS_bound[3:4], ylim = GS_bound[1:2], expand = F) +
  labs(x = "", y = "", title = "NAPA - OISST", fill = "Mean\ndifference (°C)") +
  theme(legend.position = "bottom")
plot_5

plot_combi_1 <- ggpubr::ggarrange(plot_3, plot_4, common.legend = T, legend = "bottom")
plot_combi_2 <- ggpubr::ggarrange(plot_combi_1, plot_5, widths = c(2, 1))
plot_combi_2
ggsave(plot_combi_2, filename = "graph/GS/SST_SSH.png", height = 4, width = 15)

