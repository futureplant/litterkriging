source("scripts/getpoints.R")
validation <- getPoints("data/validation_points_wgs84.shp")
samples    <- getPoints("data/sample_points_wgs84.shp") 
samples <- samples[,c(1,4)]
allpoints <- rbind(validation,samples)
st_write(allpoints, "data/allpoints.shp")


