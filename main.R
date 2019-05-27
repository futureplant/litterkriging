library(sp)
library(gstat)
library(raster)
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



# Exploratory analysis
hist(dummydata$litter1)
summary(dummydata)


# Make semivariogram
glitter <- gstat(formula = litter1 ~ 1, data = dummydata)

vglitter <- variogram(glitter)
plot(vglitter)

vgmlitter <- vgm(nugget = 7, psill = 8, range = 0.1, model = 'Sph')
vgmlitter <- fit.variogram(vglitter, vgmlitter)

plot(vglitter, vgmlitter)
attr(vgmlitter, 'SSErr')


# Export semivariogram to png



# Perform Kriging (euclidian or linear network)
  # cross-validation
litter_cv <- krige.cv(formula = litter1 ~ 1, locations = dummydata, vgmlitter)
plot(litter_cv$residual)
bubble(litter_cv, zcol = 'residual')
mean(litter_cv$zscore)
sd(litter_cv$zscore)

  # get roadnetwork raster
roadnetwork <- raster('data/a04_osm_roads_buffer_raster_wgs84.tif')
roadnetwork[roadnetwork == -9999] <- NA
roadnetwork <- as(roadnetwork, 'SpatialGridDataFrame')

  # ordinary kriging
litter_krig = krige(litter1 ~ 1, locations = dummydata, newdata = roadnetwork, model = vgmlitter, nmax = 15)

spplot(litter_krig['var1.pred'])
spplot(litter_krig['var1.var'], sp.layout = list('sp.points', dummydata, col = 'black'))

# (optional) Clip Raster on buffered roads





# Visualize 




# Export to geotiff