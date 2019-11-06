# MHW_trends.R
# The purpose of this script is to load Eric Oliver's MHW trend results from his Nature paper
# Nothing further is currently planned, but these data are very useful to have access to


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(reticulate)
np <- import("numpy")

lon_OISST <- c(seq(0.125, 179.875, by = 0.25), seq(-179.875, -0.125, by = 0.25))
lat_OISST <- seq(-89.875, 89.875, by = 0.25)


# Load data ---------------------------------------------------------------

npz1 <- np$load("../../oliver/data/MHW/Trends/mhw_census.2017.npz")

# View data layers
npz1$files


# Prep a layer ------------------------------------------------------------

layer_prep <- function(data_layer){
  res <- as.data.frame(npz1$f[[data_layer]]) %>% 
    `colnames<-`(lon_OISST) %>% 
    mutate(lat = lat_OISST) %>% 
    reshape2::melt(id = "lat", variable.name = "lon", value.name = data_layer) %>% 
    mutate(lon = as.numeric(as.character(lon)))
}


# Extract layers ----------------------------------------------------------

max_trend <- layer_prep("MHW_max_tr")


# Visualise ---------------------------------------------------------------

ggplot(max_trend, aes(x = lon, y = lat)) +
  geom_tile(aes(fill = MHW_max_tr)) +
  scale_fill_gradient2(low = "blue", high = "red") +
  coord_cartesian(expand = F) +
  labs(x = NULL, y = NULL)

