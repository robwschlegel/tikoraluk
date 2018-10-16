# tikoraluk
Code for use on tikoraluk

* October 16, 2018
  * Correlation between temperatures calculated for each pixel

* October 12, 2018
  * Working on visuals
  * Produced a first draft comparison image

* October 11, 2018
  * Sorted error in NAPA data
  * All initial comparisons completed

* October 10, 2018
  * Calculated all of the summary stats for OISST and NAPA (excluding ice coverage)
  * Basic visualisations
  * Found error in the NAPA lat values when converting from NetCDF to RData

* October 09, 2018
  * Playing about with the OISST-NAPA analysis functions
  
* October 03, 2018
  * Interpolated OISST to match NAPA grid
  * Also tested nearest neighbour
  * Getting ready for per-pixel time series creation from NAPA data
  * Per-pixel NAPA time series creation set
  * Need to change directions on how the NAPA data are prepped
  * Went for a nested multi-core approach where the NAPA data are subsetted
    based on their match to the pixels present in each OISST MATLAB file

* October 02, 2018
  * Loading NAPA model data
  * Initial visualisations
  * Basic masks prepared
  
* October 01, 2018
  * Added script for MHW calculating functions
  * Added script for tracking the running of all of the MHW calculations

* September 28, 2018
  * Added a function for loading OISST data saved in MATLAB files
  * Added a function for calculating MHWs from nested data
  
