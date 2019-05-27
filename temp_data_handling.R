# Code to merge sample points and validation dataset

source("scripts/getpoints.R")

validation <- getPoints("data/validation_points_wgs84.shp")
samples    <- getPoints("data/sample_points_wgs84.shp") 
samples <- samples[,c(1,4)]
samples <- samples[order(samples$ID),]
samples[,1] <- c(1:104)

allpoints <- rbind(validation,samples)
st_write(allpoints, "data/allpoints.shp")


