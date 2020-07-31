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