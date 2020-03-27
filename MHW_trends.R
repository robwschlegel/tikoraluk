# MHW_trends.R
# The purpose of this script is to load Eric Oliver's MHW trend results from his Nature paper
# Nothing further is currently planned, but these data are very useful to have access to


# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(reticulate)
np <- import("numpy")

lon_OISST <- c(seq(0.125, 179.875, by = 0.25), seq(-179.875, -0.125, by = 0.25))
lat_OISST <- seq(-89.875, 89.875, by = 0.25)


# Load data ---------------------------------------------------------------

npz1 <- np$load("../../oliver/data/MHW/Trends/mhw_census.2019.npz")


# Prep a layer ------------------------------------------------------------

layer_prep <- function(data_layer){
  res <- as.data.frame(npz1$f[[data_layer]]) %>% 
    `colnames<-`(lon_OISST) %>% 
    mutate(lat = lat_OISST) %>% 
    reshape2::melt(id = "lat", variable.name = "lon", value.name = "value") %>% 
    mutate(lon = as.numeric(as.character(lon)),
           value = replace_na(value, NA)) %>% 
    `colnames<-`(c("lon", "lat", {{data_layer}})) 
}

layer_plot <- function(df){
  ggplot(df, aes(x = lon, y = lat)) +
    geom_tile(aes(fill = df[,3])) +
    scale_fill_gradient2(low = "blue", high = "red") +
    coord_cartesian(expand = F) +
    labs(x = NULL, y = NULL, fill = colnames(df)[3])
}

# Extract layers ----------------------------------------------------------

# View data layers
npz1$files

max_trend <- layer_prep("MHW_max_tr")

dur_trend <- layer_prep("MHW_dur_tr")

mean_trend <- layer_prep("MHW_mean_tr")


# Visualise ---------------------------------------------------------------

layer_plot(max_trend)

layer_plot(dur_trend)

layer_plot(mean_trend)

