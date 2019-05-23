# This file will provide the points to turn into markers
# Starting with dummy points
library(sf)


getPoints <- function(path){
  samplepoints <- st_read(path)
  return (samplepoints)
}

