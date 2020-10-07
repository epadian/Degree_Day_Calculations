# Use netCDF files

# suppressPackageStartupMessages()
library(ncdf4)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RNetCDF)
library(weathermetrics)

# Open files
WRF_WY2016 <- nc_open("WRF-NARR-1km-WY2016.nc")
WRF_WY2017 <- nc_open("WRF-NARR-1km-WY2017.nc")

# Extract the max values, and extract for 1 location
tmax_16 <- ncvar_get(WRF_WY2016, "TMAX")
tmax_location1_16 <- tmax_16[1,1,]

# Extract the min values, and extract for 1 location
tmin_16 <- ncvar_get(WRF_WY2016, "TMIN")
tmin_location1_16 <- tmin_16[1,1,]

# Convert from Kelvin to Fahrenheit
tmax_F_location1_16 <- as.data.frame(kelvin.to.fahrenheit(tmax_location1_16))
tmin_F_location1_16 <- as.data.frame(kelvin.to.fahrenheit(tmin_location1_16))

# Extract the max values, and extract for 1 location
tmax_17 <- ncvar_get(WRF_WY2017, "TMAX")
tmax_location1_17 <- tmax_17[1,1,]

# Extract the min values, and extract for 1 location
tmin_17 <- ncvar_get(WRF_WY2017, "TMIN")
tmin_location1_17 <- tmin_17[1,1,]

# Convert from Kelvin to Fahrenheit
tmax_F_location1_17 <- as.data.frame(kelvin.to.fahrenheit(tmax_location1_17))
tmin_F_location1_17 <- as.data.frame(kelvin.to.fahrenheit(tmin_location1_17))

# Extract October 1, 2016 through Sept 30, 2017
# Oct 1 - Dec 31 = 92 days
# Jan 1 - Sept 30 = 273 days

# Create Calendar year 2016 for location 1

# mins
jan_sep_min_16 <- as.data.frame(tmin_F_location1_16[93:366,1])
oct_dec_min_16 <- as.data.frame(tmin_F_location1_17[1:92,1])
min_2016 <- paste(jan_sep_min_16, oct_dec_min_16, ncol=1)

# max
jan_sep_max_16 <- as.data.frame(tmax_F_location1_16[93:366,1])
oct_dec_max_16 <- as.data.frame(tmax_F_location1_17[1:92,1])

# Combine to get a calendar year
min_2016 <- append(jan_sep_min_16, oct_dec_min_16)
max_2016 <- append(jan_sep_max_16, oct_dec_max_16)


# Create Degree Day Calculations, sum, and plot - 2016
DD_location1 <- DD_Calculation_2files(min_2016, max_2016)
DD_location1_sum <- data.frame(cumsum(DD_location1))
x <- data.frame(1:365)
table <- cbind(x,DD_location1_sum)
colnames(table) <- c("Day of Year", "DD")
plot(table)










################################################
#WRF_WY2016 <- nc_open("WRF-NARR-1km-WY2016.nc")

#getvar
#print(WRF_WY2016)

#TMAX <- paste(WRF_WY2016$var$TMAX$WR)
#highs <- paste(WRF_WY2016$var$TMAX$varsize)
#high_T <- ncatt_get(WRF_WY2016, TMAX)


#lon <- var.get.nc(WRF_WY2016, "xc")
#high <- ncvar_get(WRF_WY2016, varid = TMAX)

#ncdim_def(WRF_WY2016)

#dim(WRF_WY2016)



#lon <- dim.get.nc(WRF_WY2016, 'west_east')

#lon <- ncvar_get(WRF_WY2016, "west_east")

#lookatthis <- paste(WRF_WY2016$groups)

#lat <- var.get.nc(WRF_WY2016, 'south_north')

#minT <- high[long, lat, :] # high is the variable I made before.[lat, long, day/time]

#var.get
#extract()
