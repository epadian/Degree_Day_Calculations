# Code to look at the following locations:
# Emmett: 43.8735, -116.4993
# Caldwell: 43.6629, -116.6874
# Parma: 43.7852, -116.9432
# Boise: 43.6150, -116.2023
# Sunnyslope: 43.5885, -116.7932
# Twin Falls: 42.5558, -114.4701
# Weiser: 44.2510, -116.9693

library(ncdf4)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RNetCDF)
library(weathermetrics)

lat_lon_info <- nc_open("wrf_geoinfo.nc")
lat <- ncvar_get(lat_lon_info, "XLAT")
lon <- ncvar_get(lat_lon_info, "XLONG")

# Code to find the index in lat/lon for each location.  Using the first index
# that is over the value indicated. 

Emmett_lat <- as.numeric(which(lat[1,,1]>=43.8735)[1])
Emmett_lon <- as.numeric(which(lon[,1,1]>=-116.4993)[1])

Caldwell_lat <- as.numeric(which(lat[1,,1]>=43.6629)[1])
Caldwell_lon <- as.numeric(which(lon[,1,1]>=-116.6974)[1])

Parma_lat <- as.numeric(which(lat[1,,1]>=43.7852)[1])
Parma_lon <- as.numeric(which(lon[,1,1]>=-116.9432)[1])

Boise_lat <- as.numeric(which(lat[1,,1]>=43.6150)[1])
Boise_lon <- as.numeric(which(lon[,1,1]>=-116.2023)[1])

Sunnyslope_lat <- as.numeric(which(lat[1,,1]>=43.5885)[1])
Sunnyslope_lon <- as.numeric(which(lon[,1,1]>=-116.7932)[1])

TwinFalls_lat <- as.numeric(which(lat[1,,1]>=42.5558)[1])
TwinFalls_lon <- as.numeric(which(lon[,1,1]>=-114.4701)[1])

Weiser_lat <- as.numeric(which(lat[1,,1]>=44.2510)[1])
Weiser_lon <- as.numeric(which(lon[,1,1]>=-116.9693)[1])