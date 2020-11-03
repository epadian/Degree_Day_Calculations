# This function is used to create the Degree Day column.  It's imput is a data frame
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