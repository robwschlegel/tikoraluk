# tikoraluk
Code for use on tikoraluk

* September 28, 2018
  * Added a function for loading OISST data saved in MATLAB files
  * Added a function for calculating MHWs from nested data
  
* October 01, 2018
  * Added script for MHW calculating functions
  * Added script for tracking the running of all of the MHW calculations
  
* October 02, 2018
  * Loading NAPA model data
  * Initial visualisations
  * Basic masks prepared
  
* October 03, 2018
  * Interpolated OISST to match NAPA grid
  * Also tested nearest neighbour
  * Getting ready for per-pixel time series creation from NAPA data

* October 03, 2018
  * Per-pixel NAPA time series creation set
  * Need to change directions on how the NAPA data are prepped
  * Went for a nested multi-core approach where the NAPA data are subsetted
    based on their match to the pixels present in each OISST MATLAB file