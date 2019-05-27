library(sp)
source('scripts/getdataframe.R')
source('scripts/getpoints.R')

# Main script for litterkriging
# Use as bookkeeping script, refer to /scripts for logic


# Get data from google sheets
  # temporarily simulated data
sampledata <- getDataFrame('https://docs.google.com/spreadsheets/d/1Dn96ArmKeIu-lnDSUHzAKnGcJv7Kjmqii_H-Y-zVd74/edit?usp=sharing')
#points = SpatialPoints(sampledata$Coordinates, proj4string="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", bbox = NULL)

sampledata$Coordinates
sampledata$lon <- as.numeric(sub(".*,", "", sampledata$Coordinates))
sampledata$lat <- as.numeric(sub(",.*", "", sampledata$Coordinates))
coordinates(sampledata) <- ~lon+lat
proj4string(sampledata) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
roads <- getPoints("data/osm_roads_aoi_wgs84.shp")


# Turn data into spatial object (sp)




# Make semivariogram




# Export semivariogram to png




# Perform Kriging (euclidian or linear network)




# (optional) Clip Raster on buffered roads





# Visualize 




# Export to geotiff