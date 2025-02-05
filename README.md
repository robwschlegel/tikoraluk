# tikoraluk
The code in this repository is intended for use on tikoraluk.  

Many of the file pathways will not work when not run on Robert Schlegel's username when logged into the server.

## Updates  

* March 18, 2020
  * Recalculated all of the MCSs

* January 16, 2020
  * Wrote a script for downloading OSTIA data
  * Wrote a script for downloading GLORYS data
  
* January 13, 2020
  * Wrote script for downloading CCI data

* March 25, 2019
  * Wrote a script for easily subsetting OISST data

* February 02, 2019
  * Working on fix to MHE event data files

* January 30, 2019
  * Fixed NetCDF time indexing error

* January 29, 2019
  * Added 2018 data to NetCDF files

* January 28, 2019
  * Downloaded NOAA OISST 2018 data

* January 26, 2019
  * Prepping script for downloading 2018 NOAA data

* December 21, 2018
  * Some minor edits

* December 20, 2018
  * Playing around with MCS visuals for the Halifax areas

* December 11, 2018
  * Made most changes for upcoming NAPA - OISST presentation

* December 7, 2018
  * More work on the slide-show

* December 6, 2018
  * Created rough draft for NAPA OISST comparison presentation slides
  * Fully edited results found in the larger comparison document

* December 5, 2018
  * Working on script for converting MHW results to NetCDF files

* November 29, 2018
  * All MCSs calculated from OISST
  * A bit of work on MCS visualisations
  
* November 28, 2018
  * All figures re-created with lekker ice contours

* November 27, 2018
  * Ice contours sorted
  * Working on balance between ice contours and figures

* November 22, 2018
  * Re-calculated ice to remove land ice in the OISST data
  * First-draft of all visuals sorted

* November 21, 2018
  * Recalculated the ice and skewness values as well
  * More work on visuals

* November 20, 2018
  * Re-calculated the NAPA OISST summaries to account for some minor oversights
    * This includes the difference and correlation values
  * Working on streamlining the visualisations

* November 19, 2018
  * Work on the Gulf Stream visuals

* November 14, 2018
  * Calculated ice cover summary stats

* November 14, 2018
  * Work on comparison figures
  * Created ice cover time series for NAPA data

* November 09, 2018
  * More work on comparing the NAPA and OISST/AVISO products

* November 08, 2018
  * Visualised difference between OSTIA and OISST data on 2007-12-31
  * Putting NAPA -- OISST comparison document together

* November 07, 2018
  * Re-calculated MHW comparison stats
  * Re-created all comparison visuals

* November 06, 2018
  * Calculated and visualised skewness differences

* November 05, 2018
  * Calculated differences in MHWs between NAPA and OISST
  * Visualised the differences

* November 01, 2018
  * AVISO anomaly extractions underway
  * Some of the code for the MHW comparisons complete

* October 30, 2018
  * Completed the NAPA SLA extractions
  * Began AVISO anomaly extractions

* October 18 - 29, 2018
  * Lots of things...

* October 17, 2018
  * Minor tweaks

* October 17, 2018
  * Correlations run by months as well
  * All figures created

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
  
