# This file will provide the points to turn into markers
# Starting with dummy points
library(sf)


getPoints <- function(){
  samplepoints <- st_read("data/TestPoints.shp")
  return (samplepoints)
}

