# Script for selection of layer in indicator kriging raster stack or brick
library(raster)

# dummy data
findMaxLayer <- function(rasterbrick){
  r <- which.max(rasterbrick)
  return(r)
}




# simple code for testing function

# b <- brick(system.file("external/rlogo.grd", package="raster")) 
# maxed <- findMaxLayer(b)



