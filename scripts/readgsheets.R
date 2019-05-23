# Litter kriging
# This script reads a google sheet and returns it as a dataframe

library(gsheet)


# find which sample points are already assessed

getCompletes <- function(dtf){
  return((dtf[which(as.numeric(dtf$Point.ID) > 0),2]))
}

getLength <- function(dtf){
  return(nrow(dtf))
}

