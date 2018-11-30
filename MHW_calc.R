# The purpose of this script is to keep track of which
# OISST files had their MHW's calculated when

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
) # 103 seconds on 50 cores


# NAPA MHWs ---------------------------------------------------------------

NAPA_RData_multi <- NAPA_RData %>% 
  mutate(x = file_num)

# Run on Monday, October 29th, 2018
system.time(
  plyr::ddply(NAPA_RData_multi[1:100,], .variables = "x", 
              .fun = MHW_NAPA_calc, .parallel = TRUE)
) # 138 seconds on 50 cores
system.time(
  plyr::ddply(NAPA_RData_multi[101:600,], .variables = "x", 
              .fun = MHW_NAPA_calc, .parallel = TRUE)
) # 240 seconds on 50 cores
system.time(
  plyr::ddply(NAPA_RData_multi[601:1100,], .variables = "x", 
              .fun = MHW_NAPA_calc, .parallel = TRUE)
) # 437 seconds on 50 cores
system.time(
  plyr::ddply(NAPA_RData_multi[1101:1440,], .variables = "x", 
              .fun = MHW_NAPA_calc, .parallel = TRUE)
) # 462 seconds on 50 cores


# OISST MHW match ---------------------------------------------------------

OISST_RData_multi <- OISST_RData %>% 
  mutate(x = file_num)

# Run on Monday, October 29th, 2018
system.time(
  plyr::ddply(OISST_RData_multi[1:100,], .variables = "x", 
              .fun = MHW_match_calc, .parallel = TRUE)
) # 195 seconds on 50 cores
system.time(
  plyr::ddply(OISST_RData_multi[101:770,], .variables = "x", 
              .fun = MHW_match_calc, .parallel = TRUE)
) # 743 seconds on 50 cores
system.time(
  plyr::ddply(OISST_RData_multi[771:1440,], .variables = "x", 
              .fun = MHW_match_calc, .parallel = TRUE)
) # 895 seconds on 50 cores
