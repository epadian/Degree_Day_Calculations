# This function is used to create the Degree Day column.  It's imput is a data frame
# that contains daily high and low temperatures for a calendar year at one location.
# As it is currently written, the data frame of temperatures must have the high
# temperatures in the 3rd column and the low temperatures in the 4th column.

DD_Calculation <- function(file_name) {
  DD <- matrix(-9999, nrow = nrow(file_name), ncol=1)
  DD <- data.frame(DD)
  for(i in 1:nrow(file_name)) {
    high <- as.numeric(file_name[i, 3])
    low <- as.numeric(file_name[i, 4])
    DD_Calc <- DD_S(low, high)
    DD[i,1] <- (DD_Calc)
  }
  return(DD)
}