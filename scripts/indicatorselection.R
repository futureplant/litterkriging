# Script for selection of layer in indicator kriging raster stack or brick
library(raster)

# dummy data
findMaxLayer <- function(rasterbrick){
  r <- which.min(rasterbrick)
  classmatrix <-  matrix(c(1:3), nrow = 1, ncol = 3, 
                    dimnames = list(c("cat1"),
                                    c("from", "to", "becomes")))
  r<- reclassify(r,classmatrix,include.lowest=T)
  return(r)
}




# simple code for testing function

# b <- brick(system.file("external/rlogo.grd", package="raster")) 
# maxed <- findMaxLayer(b)



