# This is the script to calc DD for Boise Airport at 
# lat 105 lon 125
# KBOI: 43.5658N, -116.223W

library(ncdf4)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RNetCDF)
library(weathermetrics)

#################################################################
####################### Functions ###############################

# This function is used to create the Degree Day column.  It's input is a data frame
# that contains daily high and low temperatures for a calendar year at one location.
# This will be set up to take in 2 files names for high and low

DD_Calculation_2files <- function(file_name_min, file_name_max) {
  DD <- matrix(-9999, nrow = 365, ncol=1)
  DD <- data.frame(DD)
  for(i in 1:365) {
    high <- as.numeric(file_name_max[i])
    low <- as.numeric(file_name_min[i])
    DD_Calc <- DD_S(low, high)
    DD[i,1] <- (DD_Calc)
  }
  return(DD)
}

#  This function takes the input of a sum, difference and a variable
#  fk1, and calculates the number of "heat days" for that day.
#  This is used in the DD_S (Degree Day Sine) function (below) only to help
#  calculate "degree days"

#  This is the Single and Double Sine Curve method
#  (Baskerville & Emin 1969, Ecology 50:514-517)

sinec <- function(sum, diff, fk1) {
  two_pi <- 2*pi
  half_pi <- pi/2
  d2 <- fk1-sum
  theta <- atan2(d2, sqrt(diff * diff - d2 * d2))
  
  if(d2 < 0 & theta > 0) {
    theta <- theta - pi
  }
  heat <- (diff * cos(theta) - d2 * (half_pi - theta)) / two_pi
}

#  DD_S is the Degree Day Calculation using the Sine Method:
#  This function takes the input of a high temperature and a low
#  temperature, and calculates the number of "degree days" for that day.
#  The high and low will be from one day at one station in units of
#  Fahrenheit.

#  This is the Single and Double Sine Curve method
#  (Baskerville & Emin 1969, Ecology 50:514-517)

DD_S <- function(Low_Temp , High_Temp) {
  t_hi <- 88
  t_lo <- 50
  min <- Low_Temp
  max <- High_Temp
  
  if (min > t_hi) {
    heat <- t_hi - t_lo
  }
  else {
    if (max <= t_lo){
      heat <- 0
    }
    else {
      fk1 <- 2 * t_lo
      diff <- max - min
      sum <- max + min
      if (min >= t_lo) {
        heat <- (sum - fk1)/2
      }
      else {
        heat <- sinec(sum, diff, fk1)
      }
      if(max > t_hi) {
        fk1 <- 2 * t_hi
        zheat <- heat
        heat <- sinec(sum, diff, fk1)
        heat <- zheat - heat
      }
    }
  }
  return(heat)    
}

#################################################################
################### Data Extraction #############################

# Open files
WRF_WY2016 <- nc_open("WRF-NARR-1km-WY2016.nc")
WRF_WY2017 <- nc_open("WRF-NARR-1km-WY2017.nc")

# Extract the max values, and extract for kboi
tmax_16 <- ncvar_get(WRF_WY2016, "TMAX")
tmax_kboi_16 <- tmax_16[125,105,]

# Extract the min values, and extract for kboi
tmin_16 <- ncvar_get(WRF_WY2016, "TMIN")
tmin_kboi_16 <- tmin_16[125,105,]

# Convert from Kelvin to Fahrenheit
tmax_F_kboi_16 <- as.data.frame(kelvin.to.fahrenheit(tmax_kboi_16))
tmin_F_kboi_16 <- as.data.frame(kelvin.to.fahrenheit(tmin_kboi_16))

# Extract the max values, and extract for kboi
tmax_17 <- ncvar_get(WRF_WY2017, "TMAX")
tmax_kboi_17 <- tmax_17[125,105,]

# Extract the min values, and extract for kboi
tmin_17 <- ncvar_get(WRF_WY2017, "TMIN")
tmin_kboi_17 <- tmin_17[125,105,]

# Convert from Kelvin to Fahrenheit
tmax_F_kboi_17 <- as.data.frame(kelvin.to.fahrenheit(tmax_kboi_17))
tmin_F_kboi_17 <- as.data.frame(kelvin.to.fahrenheit(tmin_kboi_17))

# Extract October 1, 2016 through Sept 30, 2017
# Oct 1 - Dec 31 = 92 days
# Jan 1 - Sept 30 = 273 days

# Create Calendar year 2016 for kboi

# mins
jan_sep_min_16 <- tmin_F_kboi_16[93:366,1]
oct_dec_min_16 <- tmin_F_kboi_17[1:92,1]
min_2016 <- c(jan_sep_min_16, oct_dec_min_16)

# max
jan_sep_max_16 <- tmax_F_kboi_16[93:366,1]
oct_dec_max_16 <- tmax_F_kboi_17[1:92,1]
max_2016 <- c(jan_sep_max_16, oct_dec_max_16)

# Combine to get a calendar year
min_2016 <- append(jan_sep_min_16, oct_dec_min_16)
max_2016 <- as.numeric(append(jan_sep_max_16, oct_dec_max_16))

# Partial 2017
min_2017 <- tmin_F_kboi_17[93:365,1]
max_2017 <- tmax_F_kboi_17[93:365,1]


# Create Degree Day Calculations, sum, and plot - 2016
DD_kboi <- DD_Calculation_2files(min_2016, max_2016)
DD_kboi_sum <- data.frame(cumsum(DD_kboi))
x <- data.frame(1:365)
table <- cbind(x,DD_kboi_sum)
colnames(table) <- c("Day of Year", "DD")
plot(table, main = "KBOI 2016")

rm(DD_kboi, DD_kboi_sum, x, table)

# Create Degree Day Calculations, sum, and plot - 2017
DD_kboi <- DD_Calculation_2files(min_2017, max_2017)
DD_kboi_sum <- data.frame(cumsum(DD_kboi))
x <- data.frame(1:365)
table <- cbind(x,DD_kboi_sum)
colnames(table) <- c("Day of Year", "DD")
plot(table, main = "KBOI 2017")
