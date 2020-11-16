# Function: Input lat/lon, get a degree day chart.



Degree_Day_Plot <- function(latitude, longitude) {
  if(latitude < 42 | latitude > 45.53419)
    print("Latitude out of domain")
  else if(longitude > -113.4778 | longitude < -118)
    print("Longitude out of domain")
  else
    print("Location within domain")
  
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
  location_lat <- as.numeric(which(lat[1,,1]>=latitude)[1])
  location_lon <- as.numeric(which(lon[,1,1]>=longitude)[1])
  
  WRF_WY2016 <- nc_open("WRF-NARR-1km-WY2016.nc")
  WRF_WY2017 <- nc_open("WRF-NARR-1km-WY2017.nc")
  
  tmax_16 <- ncvar_get(WRF_WY2016, "TMAX")
  tmax_16 <- tmax_16[location_lon,location_lat, ]
  
  # Extract the min values, and extract for kboi
  tmin_16 <- ncvar_get(WRF_WY2016, "TMIN")
  tmin_16 <- tmin_16[location_lon,location_lat, ]
  
  # Convert from Kelvin to Fahrenheit
  tmax_F_16 <- as.data.frame(kelvin.to.fahrenheit(tmax_16))
  tmin_F_16 <- as.data.frame(kelvin.to.fahrenheit(tmin_16))
  
  # Extract the max values, and extract for kboi
  tmax_17 <- ncvar_get(WRF_WY2017, "TMAX")
  tmax_17 <- tmax_17[location_lon,location_lat, ]
  
  # Extract the min values, and extract for kboi
  tmin_17 <- ncvar_get(WRF_WY2017, "TMIN")
  tmin_17 <- tmin_17[location_lon,location_lat, ]
  
  # Convert from Kelvin to Fahrenheit
  tmax_F_17 <- as.data.frame(kelvin.to.fahrenheit(tmax_17))
  tmin_F_17 <- as.data.frame(kelvin.to.fahrenheit(tmin_17))
  
  # Extract October 1, 2016 through Sept 30, 2017
  # Oct 1 - Dec 31 = 92 days
  # Jan 1 - Sept 30 = 273 days
  
  # Create Calendar year 2016
  
  # mins
  jan_sep_min_16 <- tmin_F_16[93:366, 1]
  oct_dec_min_16 <- tmin_F_17[1:92, 1]
  min_2016 <- c(jan_sep_min_16, oct_dec_min_16)
  
  # max
  jan_sep_max_16 <- tmax_F_16[93:366, 1]
  oct_dec_max_16 <- tmax_F_17[1:92, 1]
  max_2016 <- c(jan_sep_max_16, oct_dec_max_16)
  
  # Combine to get a calendar year
  min_2016 <- append(jan_sep_min_16, oct_dec_min_16)
  max_2016 <- as.numeric(append(jan_sep_max_16, oct_dec_max_16))
  
  # Create Degree Day Calculations, sum, and plot - 2016
  DD <- DD_Calculation_2files(min_2016, max_2016)
  DD_sum <- data.frame(cumsum(DD))
  x <- data.frame(1:365)
  table <- cbind(x,DD_sum)
  colnames(table) <- c("DOY", "DD")
  
  #261 - 1st egg laying by ow females
  first_egg_laying <- as.numeric(as.data.frame(which(DD_sum>=261))[1,1])
  #755 - peak adult emerge
  peak_adult_emerge <- as.numeric(as.data.frame(which(DD_sum>=755))[1,1])
  #2971 - 5th gen egg laying
  fifth_egg_laying <- as.numeric(as.data.frame(which(DD_sum>=2971))[1,1])
  
  ggplot(table, aes(x=DOY, y=DD)) +
    geom_line()+
    geom_point(aes(x=first_egg_laying, y=261, color='red'))+
    geom_point(aes(x=peak_adult_emerge, y=755, color='green'))+
    geom_point(aes(x=fifth_egg_laying, y=2971, color='blue'))
}