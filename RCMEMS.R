# RCMEMS
# The purpose of this script is to download data from the CMEMS portal
# This requires python and the Motu client to be present and available
# The RCMEMS page: https://github.com/markpayneatwork/RCMEMS


# Setup -------------------------------------------------------------------

# Install RCMEMS
# devtools::install_github("markpayneatwork/RCMEMS")
library(RCMEMS)

# Find where python is located
# whereis python
# However, if one activates Python (Ubuntu) and can see there is a python virtual environment 
# running in the terminal, that seems to do the trick

# Download and install the MOTU client
# https://github.com/clstoulouse/motu-client-python
# https://github.com/clstoulouse/motu-client-python/releases
# I found it best to download the .tar.gz file and unpack it in a directory close to where one is working
# e.g. "~/motuclient-python/motuclient.py"


# Prepare CMEMS object ----------------------------------------------------

# This is a copy of the script that CMEMS provides for downloading data
OSTIA_script <- 'python ~/motuclient-python/motuclient.py --motu http://nrt.cmems-du.eu/motu-web/Motu --service-id SST_GLO_SST_L4_NRT_OBSERVATIONS_010_001-TDS --product-id METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2 --longitude-min -179.97500610351562 --longitude-max 179.97500610351562 --latitude-min -89.9749984741211 --latitude-max 89.9749984741211 --date-min "2020-01-15 12:00:00" --date-max "2020-01-15 12:00:00" --depth-min 0 --depth-max 1 --variable analysed_sst --variable sea_ice_fraction --variable analysis_error --variable mask --out-dir Downloads --out-name test.nc --user rschlegel1 --pwd RobertCMEMS2018'

# We then feed that string into the following function where it parses it into it's important bits
cfg <- parse.CMEMS.script(OSTIA_script, parse.user = T)

# Here we can look at the results
# If there is anything wonky here it needs to be addressed before downloading may commence
cfg


# Download data -----------------------------------------------------------

# If everything looks correct this function will download the desired data
# Note that ne may not download over 1024MB in one shot
# A single day of data is ~128MB
CMEMS.download(cfg)

