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


# Work only with kriging points (not validation or bias data)
sampledata_krig <- sampledata[sampledata$Point.ID < 200,]
sampledata_low <- sampledata_krig[sampledata_krig$total < 40,]

# Exploratory analysis
hist(sampledata_low$total, breaks = c(0,0.5,1,2,3,4,5,6,7,8,9,10,20,30,40,50))
summary(sampledata_low)


# Make semivariogram
glitter <- gstat(formula = total ~ 1, data = sampledata_low)

vglitter <- variogram(glitter, boundaries = c(70, 125, 175, 300, 400, 500, 600, 800, 1000, 1500))
plot(vglitter, plot.numbers = T)

vgmlitter <- vgm(nugget = 1.5, psill = 12, range = 350, model = 'Exp')
vgmlitter <- fit.variogram(vglitter, vgmlitter)

plot(vglitter, vgmlitter)
attr(vgmlitter, 'SSErr')


# Export semivariogram to png



# Perform Kriging (euclidian or linear network)
  # cross-validation
litter_cv <- krige.cv(formula = total ~ 1, locations = sampledata_low, vgmlitter, nmax = 15)
plot(litter_cv$residual)


litter_cv_sp <- as(litter_cv, 'Spatial')
bubble(litter_cv_sp_na, zcol = 'residual')

mean(litter_cv$zscore, na.rm = T)
sd(litter_cv$zscore, na.rm = T)


  # get roadnetwork raster
roadnetwork_ori <- raster('data/a04_osm_roads_buffer_raster_wgs84.tif')
roadnetwork_ori[roadnetwork_ori == -9999] <- NA
roadnetwork_ori <- as(roadnetwork_ori, 'SpatialGridDataFrame')

roadnetwork <- spTransform(roadnetwork, CRS(proj4string(sampledata_low)))

# "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs"


sampledata_low <- as(sampledata_low, 'Spatial')

  # ordinary kriging
litter_krig = krige(total ~ 1, locations = sampledata_low, newdata = roadnetwork, model = vgmlitter, nmax = 15)

spplot(litter_krig['var1.pred'])
spplot(litter_krig['var1.var'], sp.layout = list('sp.points', dummydata, col = 'black'))

# (optional) Clip Raster on buffered roads





# Visualize 




# Export to geotiff