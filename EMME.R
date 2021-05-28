# EMME
# This script houses the code used for the EMME-CCI analyses


# Setup -------------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(sf)
library(doParallel); registerDoParallel(cores = 50)

# The MHW cat files
MHW_cat_files <- dir("../data/cat_clim", full.names = T, recursive = T)

# The MHW lon files
MHW_lon_files <- dir("../data/cat_lon", full.names = T, recursive = T)

# OISST ocean only pixels 
load("metadata/OISST_ocean_coords.Rdata")

# Base map for plotting
load("metadata/map_base.Rdata")

# The MHW colour palette
# The MHW colour palette
MHW_colours <- c(
  "I Moderate" = "#ffc866",
  "II Strong" = "#ff6900",
  "III Severe" = "#9e0000",
  "IV Extreme" = "#2d0000"
)

# Function that loads a MHW cat file, includes the date, and subsets by bbox
readRDS_date_sub <- function(file_name, bbox){
  file_date <- sapply(str_split(file_name, "/"), "[[", 5)
  file_date <- as.Date(sapply(str_split(file_date, "[.]"), "[[", 3))
  res <- readRDS(file_name)
  res_sub <- res %>% 
    mutate(t = file_date) %>% 
    filter(lon >= bbox[1], lon <= bbox[2],
           lat >= bbox[3], lat <= bbox[4])
  rm(res); gc()
  return(res_sub)
}

# Function for finding the first date of the highest category MHW per pixel
max_event_date <- function(df){
  df %>% 
    group_by(lat) %>% 
    filter(as.integer(category) == max(as.integer(category))) %>% 
    filter(t == min(t)) %>% 
    ungroup()
}

# Function for finding and cleaning up points within a given region polygon
points_in_region <- function(region_in){
  region_sub <- MEOW %>% 
    filter(ECOREGION == region_in) %>% 
    dplyr::select(geometry)
  region_sub <- as.data.frame(region_sub$geometry[[1]][[1]]) %>%
    `colnames<-`(c("lon", "lat"))
  # unnest()
  coords_in <- OISST_ocean_coords %>% 
    mutate(in_grid = sp::point.in.polygon(point.x = OISST_ocean_coords[["lon"]], point.y = OISST_ocean_coords[["lat"]], 
                                          pol.x = region_sub[["lon"]], pol.y = region_sub[["lat"]])) %>% 
    filter(in_grid >= 1) %>% 
    mutate(Ecoregion = region_in) %>% 
    dplyr::select(lon, lat, Ecoregion)
  return(coords_in)
}


# Establish regions -------------------------------------------------------

# Load MEOW
MEOW <- read_sf("metadata/MEOW/meow_ecos.shp") %>% 
  # filter(PROVINCE %in% c("Mediterranean Sea", "Red Sea and Gulf of Aden", "Somali/Arabian"),
         # !ECOREGION %in% c("Alboran Sea", "Western Mediterranean"))
  filter(ECOREGION %in% c("Arabian (Persian) Gulf", "Northern and Central Red Sea", "Southern Red Sea",  "Adriatic Sea",
                          "Levantine Sea", "Tunisian Plateau/Gulf of Sidra", "Ionian Sea", "Aegean Sea"))

# Find SST pixels within Med MEOW
registerDoParallel(cores = 15)
ecoregion_pixels <- plyr::ldply(unique(MEOW$ECOREGION), points_in_region, .parallel = T) %>% 

# Plot pixels
ecoregion_pixels %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = Ecoregion)) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_quickmap(xlim = c(9, 60), ylim = c(10, 50), expand = F) +
  labs(x = "Longitude (°E)", y = "Latitude (°N)") +
  # guides(fill = guide_legend(nrow = 3)) +
  theme(legend.position = "bottom")
ggsave("EMME_pixels.png", height = 8, width = 8)


# Load data by region -----------------------------------------------------


# Load rough bounding box
rough_bbox <- c(range(ecoregion_pixels$lon), range(ecoregion_pixels$lat))

# Load cat files
registerDoParallel(cores = 50)
system.time(
MHW_cat <- plyr::ldply(MHW_cat_files, readRDS_date_sub, .parallel = T, bbox = rough_bbox)
) # 149 seconds on 10 cores, 214 seconds on 50 cores???

# Crop Cat files
MHW_cat_crop <- MHW_cat %>% 
  right_join(ecoregion_pixels, by = c("lon", "lat")) %>% 
  mutate(year = lubridate::year(t))

# Load lon files


# Crop lon files


# Test visual map
MHW_cat_crop %>% 
  dplyr::select(lon, lat) %>% 
  distinct() %>%  
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(colour = "blue") +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_quickmap(xlim = c(9, 60), ylim = c(10, 50))
ggsave("EMME_pixels_rough.png")

# Clean up
rm(MHW_cat, MHW_lon); gc()


# Calculate annual stats --------------------------------------------------

# Sum of intensities per pixel per year
MHW_intensity <- MHW_cat_crop %>% 
  group_by(lon, lat, year) %>% 
  summarise(intensity_sum = sum(intensity), .groups = "drop")

# Date of highest category per year
MHW_cat_pixel <- MHW_cat_crop %>% 
  dplyr::select(-event_no) %>% 
  plyr::ddply(., c("lon", "year"), max_event_date, 
              .parallel = T, .paropts = c(.inorder = FALSE)) %>% 
  unique() %>%
  left_join(MHW_intensity, by = c("lon", "lat", "year"))
rm(MHW_intensity);gc()

# Daily count and cumulative count per pixel
# Complete dates by categories data.frame
full_grid <- expand_grid(t = seq(as.Date("1982-01-01"), max(MHW_cat_crop$t), by = "day"), 
                         Ecoregion = unique(ecoregion_pixels$Ecoregion),  
                         category = as.factor(levels(MHW_cat_crop$category))) %>% 
  mutate(category = factor(category, levels = levels(MHW_cat_crop$category)))
MHW_cat_single <- MHW_cat_pixel %>%
  group_by(Ecoregion, t) %>%
  count(category) %>%
  dplyr::rename(first_n = n) %>% 
  ungroup() %>% 
  right_join(full_grid, by = c("Ecoregion", "t", "category")) %>% 
  mutate(first_n = ifelse(is.na(first_n), 0, first_n)) %>% 
  arrange(Ecoregion, t) %>% 
  group_by(category) %>%
  mutate(first_n_cum = cumsum(first_n)) %>% 
  ungroup()
MHW_cat_daily <- MHW_cat_crop %>% 
  group_by(Ecoregion, t) %>% 
  count(category) %>% 
  ungroup() %>% 
  right_join(full_grid, by = c("Ecoregion", "t", "category")) %>% 
  dplyr::rename(cat_n = n) %>% 
  mutate(cat_n = ifelse(is.na(cat_n), 0, cat_n)) %>% 
  group_by(category) %>% 
  mutate(cat_n_cum = cumsum(cat_n),
         cat_n_prop = round(cat_n_cum/nrow(bbox_pixels), 4)) %>% # Need to fix the pixel count divider for proportion values. This is a recurring issue below.
  ungroup() %>% 
  right_join(MHW_cat_single, by = c("Ecoregion", "t", "category")) %>% 
  mutate(year = lubridate::year(t),
         first_n_cum_prop = round(first_n_cum/nrow(bbox_pixels), 4),
         cat_prop = round(cat_n/nrow(bbox_pixels), 4))

# Extract small data.frame for easier labeling
MHW_cat_daily_labels <- MHW_cat_daily %>% 
  group_by(category) %>% 
  filter(t == max(t)) %>% 
  ungroup() %>% 
  mutate(label_first_n_cum = cumsum(first_n_cum_prop))

# Tidy up
rm(MHW_cat_single); gc()


# Total stats figure ------------------------------------------------------

# Now with a year column!
full_daily_grid <- expand_grid(t = seq(as.Date(paste0("1982-01-01")), as.Date("2020-12-31"), by = "day"), 
                               category = as.factor(c("I Moderate", "II Strong", "III Severe", "IV Extreme"))) %>% 
  mutate(year = lubridate::year(t))

# The daily count of the first time the largest category pixel occurs over the whole Med and the cumulative values
cat_first_annual <- MHW_cat_pixel %>%
  group_by(t, year, category) %>%
  summarise(first_n = n(), .groups = "drop") %>%
  right_join(full_daily_grid, by = c("t", "year", "category")) %>%
  arrange(year, t, category) %>%
  mutate(first_n = ifelse(is.na(first_n), 0, first_n),
         first_n_prop = round(first_n/nrow(bbox_pixels), 4)) %>%
  group_by(year, category) %>%
  mutate(first_n_cum = cumsum(first_n),
         first_n_cum_prop = round(first_n_cum/nrow(bbox_pixels), 4)) %>%
  ungroup()

# The count of categories of MHWs happening on a given day, and cumulatively throughout the year
cat_summary_annual <- MHW_cat_daily %>%
  arrange(year, t, category) %>%
  group_by(t, year, category) %>%
  summarise(cat_n = sum(cat_n), .groups = "keep") %>%
  mutate(cat_n_prop = round(cat_n/nrow(bbox_pixels), 4)) %>%
  group_by(year, category) %>%
  mutate(cat_n_cum = cumsum(cat_n),
         cat_n_cum_prop = round(cat_n_cum/nrow(bbox_pixels), 4)) %>%
  right_join(cat_first_annual, by = c("t", "year", "category"))

# Create mean values of daily count
cat_daily_mean <- cat_summary_annual %>%
  group_by(year, category) %>%
  summarise(cat_n_prop_mean = mean(cat_n_prop, na.rm = T),
            cat_n_cum_prop = max(cat_n_cum_prop, na.rm = T), .groups = "drop")

# Extract only values from December 31st
cat_daily <- cat_summary_annual %>%
  group_by(year, category) %>%
  filter(lubridate::month(t) == 12, lubridate::day(t) == 31)

# Load OISST annual global MHW summaries
OISST_global <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1982-2011_total.Rds") %>% 
  group_by(t) %>% 
  mutate(cat_n_prop_stack = cumsum(cat_n_prop),
         first_n_cum_prop_stack = cumsum(first_n_cum_prop)) %>% 
  filter(t <= 2020)

# Stacked barplot of global daily count of MHWs by category
fig_count_historic <- ggplot(cat_daily_mean, aes(x = year, y = cat_n_cum_prop)) +
  geom_bar(aes(fill = category), stat = "identity", show.legend = T,
           position = position_stack(reverse = TRUE), width = 1) +
  geom_point(data = OISST_global, aes(x = t, y = cat_n_prop_stack, fill = category), 
             shape = 21, show.legend = F) +
  scale_fill_manual("Category", values = MHW_colours) +
  scale_colour_manual("Category", values = MHW_colours) +
  scale_y_continuous(limits = c(0, 150),
                     breaks = seq(30, 120, length.out = 4),
                     sec.axis = sec_axis(name = "Average daily MHW coverage", 
                                         trans = ~ . + 0,
                                         breaks = c(36.5, 73, 109.5),
                                         labels = c("10%", "20%", "30%"))) +
  scale_x_continuous(breaks = seq(1984, 2019, 7)) +
  guides(pattern_colour = FALSE, colour = FALSE) +
  labs(y = "Average MHW days", x = NULL) +
  coord_cartesian(expand = F) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
# fig_count_historic

# Stacked barplot of cumulative percent of ocean affected by MHWs
fig_cum_historic <- ggplot(cat_daily, aes(x = year, y = first_n_cum_prop)) +
  geom_bar(aes(fill = category), stat = "identity", show.legend = T,
           position = position_stack(reverse = TRUE), width = 1) +
  geom_point(data = OISST_global, aes(x = t, y = first_n_cum_prop_stack, fill = category), 
             shape = 21, show.legend = F) +
  scale_fill_manual("Category", values = MHW_colours) +
  scale_colour_manual("Category", values = MHW_colours) +
  scale_y_continuous(position = "right", 
                     limits = c(0, 1),
                     breaks = seq(0.2, 0.8, length.out = 4),
                     labels = paste0(seq(20, 80, by = 20), "%")) +
  scale_x_continuous(breaks = seq(1984, 2019, 7)) +
  labs(y = "Total MHW coverage", x = NULL) +
  coord_cartesian(expand = F) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
# fig_cum_historic

# Create the figure title
fig_title <- paste0("EMME-CCI MHW categories summary: 1982-2020",
                    "\nNOAA OISST; Climatogy period: 1982-2011")

# Stick them together and save
fig_ALL_historic <- ggpubr::ggarrange(fig_count_historic, fig_cum_historic,
                                      ncol = 2, align = "hv", labels = c("A)", "B)"), hjust = -0.1,
                                      font.label = list(size = 14), common.legend = T, legend = "bottom")
fig_ALL_cap <- grid::textGrob(fig_title, x = 0.01, just = "left", gp = grid::gpar(fontsize = 18))
fig_ALL_full <- ggpubr::ggarrange(fig_ALL_cap, fig_ALL_historic, heights = c(0.25, 1), nrow = 2)
ggsave(fig_ALL_full, filename = "EMME_MHW_cat_historic.png", height = 4.25, width = 12)


# Shifts in phenology -----------------------------------------------------



# Map of trends -----------------------------------------------------------

# Stack the values and get trends
cat_daily_mean_stack <- cat_daily_mean %>% 
  group_by(year) %>% 
  summarise(cat_n_prop_stack = sum(cat_n_cum_prop), .groups = "drop")

