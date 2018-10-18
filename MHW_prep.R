# The purpose of this script is to provide easy wrapper 
# functions for loading/unpacking the output from MHW_calc.R


# Source scripts ----------------------------------------------------------

source("MHW_func.R")
library(padr)


# Prep functions ----------------------------------------------------------

# Tester...
# load("../data/MHW.calc.0001.RData")

# Pull out climatologies
MHW_clim <- function(df){
  clim <- df %>% 
    unnest(event) %>% 
    filter(row_number() %% 2 == 1) %>% 
    unnest(event)# %>% 
    # select(-(threshCriterion:event))
}
# test <- MHW_clim(MHW_res)

# Pull out events
MHW_event <- function(df){
  event <- df %>% 
    unnest(event) %>% 
    filter(row_number() %% 2 == 0) %>% 
    unnest(event)
}
# test <- MHW_event(MHW_res)

# Pull out category climatologies
MHW_cat_clim <- function(df, long = FALSE){
  cat_clim <- df %>% 
    unnest(cat) %>% 
    filter(row_number() %% 2 == 1) %>% 
    unnest(cat)
  if(long){
    cat_clim_long <- cat_clim %>% 
      group_by(lon, lat) %>%
      nest() %>%
      mutate(long = map(data, pad, interval = "day", 
                        start_val = as.Date("1982-01-01"))) %>% 
      dplyr::select(-data) %>%
      unnest()
  } else {
    return(cat_clim)
  }
}
# test <- MHW_cat_clim(MHW_res)
# test <- MHW_cat_clim(MHW_res, long = T)

# Pull out event category summaries
MHW_cat_event <- function(df){
  suppressWarnings(
    cat_event <- df %>% 
    unnest(cat) %>% 
    filter(row_number() %% 2 == 0) %>% 
    unnest(cat)
  )
}
# test <- MHW_cat_event(MHW_res)
