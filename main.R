library(sp)
library(gstat)
library(raster)
library(gsheet)
library(stars)
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
# allpoints <- getPoints("data/allpoints.shp")
# allpoints$litter1 <- floor(runif(134, min=0, max =10))
# allpoints$litter2 <- floor(runif(134, min=0, max =5))
# dummydata <- as(allpoints,'Spatial')
# 
# plot(dummydata)
# plot(roads, add=T)

# Turn data into spatial object (sf)
sampledata <- st_as_sf(sampledata)
sampledata <- st_transform(sampledata, crs =28992)


# Exploratory analysis
hist(sampledata$total, breaks = c(0,0.5,1,2,3,4,5,6,7,8,9,10,20,30,40,50))
summary(sampledata)

# Without 119
sampledata_low <- sampledata[-119,]

# Make semivariogram
glitter <- gstat(formula = total ~ 1, data = sampledata_low)

vglitter <- variogram(glitter, boundaries = c(25, 60, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1100))
plot(vglitter, plot.numbers = T)

vgmlitter <- vgm(nugget = 1.5, psill = 7.5, range = 400, model = 'Exp')
vgmlitter <- fit.variogram(vglitter, vgmlitter)

plot(vglitter, vgmlitter)
attr(vgmlitter, 'SSErr')


# Export semivariogram to png



# Perform Kriging (euclidian or linear network)
  # cross-validation
litter_cv <- krige.cv(formula = total ~ 1, locations = sampledata_low, vgmlitter, nmax = 15)
plot(litter_cv$residual)
bubble(litter_cv, zcol = 'residual')
mean(litter_cv$zscore)
sd(litter_cv$zscore)

  # get roadnetwork raster
roadnetwork <- raster('data/a04_osm_roads_buffer_raster_wgs84.tif')
roadnetwork[roadnetwork == -9999] <- NA
roadnetwork <- as(roadnetwork, 'SpatialGridDataFrame')

roadnetwork <- projectRaster(roadnetwork, crs = 28992)


  # ordinary kriging
litter_krig = krige(total ~ 1, locations = sampledata_low, newdata = roadnetwork, model = vgmlitter, nmax = 15)

spplot(litter_krig['var1.pred'])
spplot(litter_krig['var1.var'], sp.layout = list('sp.points', dummydata, col = 'black'))

# (optional) Clip Raster on buffered roads





# Visualize 




# Export to geotiff