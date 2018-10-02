# The purpose of this script is to keep track of which
# OISST files had their MHW's calculated when

# My current thinking is that this is easier with a simple for loop...

# I haven't presently put in any failsafes for oversaving...


# Source scripts ----------------------------------------------------------

source("MHW_func.R")


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
