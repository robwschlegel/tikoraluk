# The purpose of this script is to keep track of which
# OISST files had their MHW's calculated when

# My current thinking is that this is easier with a simple for loop...

# I haven't presently put in any failsafes for oversaving...


# Source scripts ----------------------------------------------------------

source("MHW_func.R")
library(doMC); doMC::registerDoMC(cores = 50)


# Calculate MHWs ----------------------------------------------------------

# Run on Monday, October 1st, 2018
system.time(
for(i in 1){
  MHW_calc(file_list[i,])
}
) # ~ 126 seconds for 1 file
system.time(
  for(i in 2:30){
    MHW_calc(file_list[i,])
  }
) # 3559 seconds
system.time(
  for(i in 31:60){
    MHW_calc(file_list[i,])
  }
) # 3044 seconds
system.time(
  for(i in 61:100){
    MHW_calc(file_list[i,])
  }
) # 2945 seconds
system.time(
  for(i in 101:200){
    MHW_calc(file_list[i,])
  }
) # 9023 seconds
system.time(
  for(i in 201:300){
    MHW_calc(file_list[i,])
  }
) # 12629 seconds
system.time(
  for(i in 301:400){
    MHW_calc(file_list[i,])
  }
) # 10527 seconds

## Switching over to plyr for multi-cores
# Run on Tuesday, October 2nd, 2018
file_list_multi <- file_list %>% 
  mutate(x = file_num) %>% 
  slice(401:405)
system.time(
plyr::ddply(file_list_multi, .variables = "x", 
            .fun = MHW_calc, .parallel = TRUE)
) # 86 seconds at 16 cores
file_list_multi <- file_list %>% 
  mutate(x = file_num) %>% 
  slice(406:415)
system.time(
  plyr::ddply(file_list_multi, .variables = "x", 
              .fun = MHW_calc, .parallel = TRUE)
) # 86 seconds at 16 cores
file_list_multi <- file_list %>% 
  mutate(x = file_num) %>% 
  slice(416:445)
system.time(
  plyr::ddply(file_list_multi, .variables = "x", 
              .fun = MHW_calc, .parallel = TRUE)
) # 199 seconds at 32 cores
file_list_multi <- file_list %>% 
  mutate(x = file_num) %>% 
  slice(446:500)
system.time(
  plyr::ddply(file_list_multi, .variables = "x", 
              .fun = MHW_calc, .parallel = TRUE)
) # 398 seconds at 32 cores
file_list_multi <- file_list %>% 
  mutate(x = file_num) %>% 
  slice(501:600)
system.time(
  plyr::ddply(file_list_multi, .variables = "x", 
              .fun = MHW_calc, .parallel = TRUE)
) # 534 seconds at 50 cores
file_list_multi <- file_list %>% 
  mutate(x = file_num) %>% 
  slice(601:700)
system.time(
  plyr::ddply(file_list_multi, .variables = "x", 
              .fun = MHW_calc, .parallel = TRUE)
) # 730 seconds at 50 cores
file_list_multi <- file_list %>% 
  mutate(x = file_num) %>% 
  slice(701:800)
system.time(
  plyr::ddply(file_list_multi, .variables = "x", 
              .fun = MHW_calc, .parallel = TRUE)
) # 671 seconds at 50 cores

# Run on Wednesday, October 3rd, 2018
file_list_multi <- file_list %>% 
  mutate(x = file_num) %>% 
  slice(801:850)
system.time(
  plyr::ddply(file_list_multi, .variables = "x", 
              .fun = MHW_calc, .parallel = TRUE)
) # 336 seconds at 50 cores
file_list_multi <- file_list %>% 
  mutate(x = file_num) %>% 
  slice(851:900)
system.time(
  plyr::ddply(file_list_multi, .variables = "x", 
              .fun = MHW_calc, .parallel = TRUE)
) # 348 seconds at 50 cores
file_list_multi <- file_list %>% 
  mutate(x = file_num) %>% 
  slice(901:1200)
system.time(
  plyr::ddply(file_list_multi, .variables = "x", 
              .fun = MHW_calc, .parallel = TRUE)
) # 1383 seconds at 50 cores
## NB: File 1137 throws an error
file_list_multi <- file_list %>% 
  mutate(x = file_num) %>% 
  slice(1201:1440)
system.time(
  plyr::ddply(file_list_multi, .variables = "x", 
              .fun = MHW_calc, .parallel = TRUE)
) # 1270 seconds on 50 cores
file_list_multi <- file_list %>% 
  mutate(x = file_num) %>% 
  slice(1137)
system.time(
  plyr::ddply(file_list_multi, .variables = "x", 
              .fun = MHW_calc, .parallel = TRUE)
) # 1270 seconds on 50 cores



# Need to look more closely at file 1137
file_list_multi <- file_list %>% 
  mutate(x = file_num) %>% 
  slice(1137)
OISST <- load_OISST_mat(file_list_multi)
MHW_res <- OISST %>%
  group_by(lon, lat) %>%
  nest() %>% 
  slice(3) %>% 
  mutate(clim = purrr::map(data, ts2clm, robust = FALSE,
                           climatologyPeriod = c("1982-01-01", "2011-12-31")),
         event = purrr::map(clim, detect_event),
         cat = purrr::map(event, category, climatology = TRUE)) %>% 
  select(-data, -clim)
save(MHW_res, file = paste0("../data/MHW.calc.", df$file_num,".RData"))

error_file <- OISST %>%
  group_by(lon, lat) %>%
  nest() %>% 
  slice(3) %>% 
  unnest()

ts2clm(error_file, robust = FALSE,
       climatologyPeriod = c("1982-01-01", "2011-12-31"))
detect_event(ts2clm(error_file, robust = FALSE,
                    climatologyPeriod = c("1982-01-01", "2011-12-31")))
