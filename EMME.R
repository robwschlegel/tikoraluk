# EMME
# This script houses the code used for the EMME-CCI analyseis


# Setup -------------------------------------------------------------------

# Bounding box
# - All of the Red sea and Persian Gulf
# - Levant east of 22°E lon
# Map
# - Spatial average over full time series
# Bar plot
# - Annual averages of MHW cats
# Results
# - Also look at shifts in phenology, onset of Spring specifically

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(doParallel); registerDoParallel(cores = 50)

# Base map for plotting
load("metadata/map_base.Rdata")

# The MHW colour palette
# The MHW colour palette
MHW_colours <- c(
  "Moderate" = "#ffc866",
  "Strong" = "#ff6900",
  "Severe" = "#9e0000",
  "Extreme" = "#2d0000"
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

# The MHW cat files
MHW_cat_files <- dir(paste0("../data/cat_clim"), full.names = T, recursive = T)


# Load region of interest -------------------------------------------------

# Load rough bounding box
rough_bbox <- c(28, 56, 11, 38)

# Load
registerDoParallel(cores = 10)
system.time(
MHW_cat <- plyr::ldply(MHW_cat_files, readRDS_date_sub, .parallel = T, bbox = rough_bbox)
) # 422 seconds on 10 cores

# Tighter crop for only Levantine Sea, Red Sea, Persian Gulf
MHW_cat_crop <- MHW_cat %>% 
  mutate(crop_idx = case_when(lon >= 37 & lat >= 36 ~ "crop",
                              lon >= 44 & lat <= 23 ~ "crop",
                              TRUE ~ "keep"),
         year = lubridate::year(t))  %>% 
  filter(crop_idx == "keep",
         lat >= 12.5,
         year <= 2020) %>% 
  dplyr::select(-crop_idx)

# Unique ocean pixels
bbox_pixels <- MHW_cat_crop %>% 
  dplyr::select(lon, lat) %>% 
  distinct()

# Test visual map
MHW_cat_crop %>% 
  dplyr::select(lon, lat) %>% 
  distinct() %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(colour = "blue") +
  geom_polygon(data = map_base, aes(group = group))
ggsave("test.png") # RStudio outdated, plot panel not working

# Clean up
rm(MHW_cat); gc()


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
                         category = as.factor(levels(MHW_cat_crop$category))) %>% 
  mutate(category = factor(category, levels = levels(MHW_cat_crop$category)))
MHW_cat_single <- MHW_cat_pixel %>%
  group_by(t) %>%
  count(category) %>%
  dplyr::rename(first_n = n) %>% 
  ungroup() %>% 
  right_join(full_grid, by = c("t", "category")) %>% 
  mutate(first_n = ifelse(is.na(first_n), 0, first_n)) %>% 
  arrange(t) %>% 
  group_by(category) %>%
  mutate(first_n_cum = cumsum(first_n)) %>% 
  ungroup()
MHW_cat_daily <- MHW_cat_crop %>% 
  group_by(t) %>% 
  count(category) %>% 
  ungroup() %>% 
  right_join(full_grid, by = c("t", "category")) %>% 
  dplyr::rename(cat_n = n) %>% 
  mutate(cat_n = ifelse(is.na(cat_n), 0, cat_n)) %>% 
  group_by(category) %>% 
  mutate(cat_n_cum = cumsum(cat_n),
         cat_n_prop = round(cat_n_cum/nrow(bbox_pixels), 4)) %>% 
  ungroup() %>% 
  right_join(MHW_cat_single, by = c("t", "category")) %>% 
  mutate(year = lubridate::year(t))

# Add prop columns for more accurate plotting
MHW_cat_daily <- MHW_cat_daily %>% 
  mutate(first_n_cum_prop = round(first_n_cum/nrow(bbox_pixels), 4),
         cat_prop = round(cat_n/nrow(bbox_pixels), 4))

# Extract small data.frame for easier labeling
MHW_cat_daily_labels <- MHW_cat_daily %>% 
  group_by(category) %>% 
  filter(t == max(t)) %>% 
  ungroup() %>% 
  mutate(label_first_n_cum = cumsum(first_n_cum_prop))


# Total stats figure ------------------------------------------------------

cat_daily_mean <- MHW_cat_daily %>%
  group_by(year, category) %>%
  summarise(cat_n = mean(cat_n, na.rm = T), .groups = "drop") %>%
  ungroup() %>%
  mutate(cat_prop_daily_mean = round(cat_n/nrow(bbox_pixels), 4))

# Extract only values from December 31st
cat_daily <- MHW_cat_daily %>%
  group_by(year) %>% 
  filter(t == max(t)) %>% 
  mutate(first_n_cum_prop = round(first_n_cum/nrow(bbox_pixels), 4)) %>% 
  dplyr::select(-cat_n) %>% 
  left_join(cat_daily_mean, by = c("year", "category"))

# Second y-axis labels
y2_labs <- c("5%", "10%", "15%", "20%", "25%")

# Total summary
# Create mean values of daily count
cat_daily_mean <- df %>%
  group_by(year, category) %>%
  summarise(cat_n_prop_mean = mean(cat_n_prop, na.rm = T),
            cat_n_cum_prop = max(cat_n_cum_prop, na.rm = T), .groups = "drop")

# Extract only values from December 31st
cat_daily <- df %>%
  group_by(year, category) %>%
  filter(lubridate::month(t) == month_sub, lubridate::day(t) == day_sub)


# Load OISST annual global MHW summaries
OISST_global <- readRDS("../MHWapp/data/annual_summary/OISST_cat_daily_1982-2011_total.Rds") %>% 
  group_by(t) %>% 
  mutate(cat_n_prop_stack = cumsum(cat_n_prop),
         first_n_cum_prop_stack = cumsum(first_n_cum_prop))

# Stacked barplot of global daily count of MHWs by category
fig_count_historic <- ggplot(cat_daily_mean, aes(x = year, y = cat_n_cum_prop)) +
  geom_bar(aes(fill = category), stat = "identity", show.legend = T,
           position = position_stack(reverse = TRUE), width = 1) +
  # geom_line(data = OISST_global, aes(x = t, y = cat_n_prop_stack/dd, colour = category), 
  # linetype = "dotted", show.legend = F) +
  geom_point(data = OISST_global, aes(x = t, y = cat_n_prop_stack/dd, fill = category), 
             shape = 21, show.legend = F) +
  geom_segment(aes(x = 2015, xend = 2019, y = 20, yend = 20), 
               size = 2, colour = "red", lineend = "round") +
  scale_fill_manual("Category", values = MHW_colours) +
  scale_colour_manual("Category", values = MHW_colours) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(20, 80, length.out = 4),
                     sec.axis = sec_axis(name = "Average daily MHW coverage", 
                                         trans = ~ . + 0,
                                         breaks = c(18.25, 36.5, 54.75, 73, 91.25),
                                         labels = y2_labs)) +
  scale_x_continuous(breaks = seq(1984, 2019, 7)) +
  guides(pattern_colour = FALSE, colour = FALSE) +
  labs(y = "Average MHW days", x = NULL) +
  coord_cartesian(expand = F) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
fig_count_historic

# Stacked barplot of cumulative percent of ocean affected by MHWs
fig_cum_historic <- ggplot(cat_daily, aes(x = year, y = first_n_cum_prop)) +
  geom_bar(aes(fill = category), stat = "identity", show.legend = T,
           position = position_stack(reverse = TRUE), width = 1) +
  # geom_line(data = OISST_global, aes(x = t, y = first_n_cum_prop_stack, colour = category), 
  # linetype = "dotted", show.legend = F) +
  geom_point(data = OISST_global, aes(x = t, y = first_n_cum_prop_stack, fill = category), 
             shape = 21, show.legend = F) +
  geom_segment(aes(x = 2015, xend = 2019, y = 0.2, yend = 0.2), 
               size = 2, colour = "red", lineend = "round") +
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
min_year <- min(cat_daily_mean$year)
max_year <- max(cat_daily_mean$year)
fig_title <- paste0("Mediterranean MHW categories summary: ",min_year," - ", max_year, JJASON_bit,
                    "\nCMEMS Med SST ~4km; Climatogy period: 1982-2011")

# Stick them together and save
fig_ALL_historic <- ggpubr::ggarrange(fig_count_historic, fig_cum_historic,
                                      ncol = 2, align = "hv", labels = c("A)", "B)"), hjust = -0.1,
                                      font.label = list(size = 14), common.legend = T, legend = "bottom")
fig_ALL_cap <- grid::textGrob(fig_title, x = 0.01, just = "left", gp = grid::gpar(fontsize = 18))
fig_ALL_full <- ggpubr::ggarrange(fig_ALL_cap, fig_ALL_historic, heights = c(0.25, 1), nrow = 2)
# ggsave(fig_ALL_full, filename = "figures/MHW_cat_historic.png", height = 4.25, width = 12)


# Shifts in phenology -----------------------------------------------------



# Map of trends -----------------------------------------------------------



