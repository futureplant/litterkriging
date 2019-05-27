library(sp)
source('scripts/getdataframe.R')
source('scripts/getpoints.R')

# Main script for litterkriging
# Use as bookkeeping script, refer to /scripts for logic


# Get data from google sheets
sampledata <- getDataFrame('https://docs.google.com/spreadsheets/d/1Dn96ArmKeIu-lnDSUHzAKnGcJv7Kjmqii_H-Y-zVd74/edit?usp=sharing')
sampledata$lon <- as.numeric(sub(".*,", "", sampledata$Coordinates))
sampledata$lat <- as.numeric(sub(",.*", "", sampledata$Coordinates))

# turn data frame into sp object
coordinates(sampledata) <- ~lon+lat
proj4string(sampledata) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# visual check if all points are on road network
roads <- getPoints("data/osm_roads_aoi_wgs84.shp")
plot(sampledata)
plot(roads, add=T)

# create dummy data
allpoints <- getPoints("data/allpoints.shp")
allpoints$litter1 <- floor(runif(134, min=0, max =10))
allpoints$litter2 <- floor(runif(134, min=0, max =5))
dummydata <- as(allpoints,'Spatial')

plot(dummydata)
plot(roads, add=T)

# Turn data into spatial object (sp)




# Make semivariogram




# Export semivariogram to png




# Perform Kriging (euclidian or linear network)




# (optional) Clip Raster on buffered roads





# Visualize 




# Export to geotiff