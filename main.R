# Script for performing kriging methods on littering observations in Groenlo
# Main script for litterkriging
# Use as bookkeeping script, refer to /scripts for logic


# Libraries & Imports ################
library(sp)
library(gstat)
library(raster)
library(gsheet)
library(stars)
library(rgdal)
library(automap)
source('scripts/getdataframe.R')
source('scripts/getpoints.R')
source('scripts/variogramkriging.R')


# Retrieve and preprocess data ######################

# Study area for kriging (new_data)
study_area <- readOGR(dsn = "data", layer = "mapping_area_groenlo")
crs(study_area) <- crs(sampledata)
area_raster <- raster(extent(study_area), resolution = c(10,10))
crs(area_raster) <- crs(sampledata)
area_raster <- as(area_raster, 'SpatialGrid')


# Get data from google sheets
sampledata <- getDataFrame('https://docs.google.com/spreadsheets/d/1MyHRcpDJX2iro6a_2nk0mOJBRSm_x0lpkLH04IoKJII/edit?usp=sharing')
sampledata$lon <- as.numeric(sub(".*,", "", sampledata$Coordinates))
sampledata$lat <- as.numeric(sub(",.*", "", sampledata$Coordinates))

# Turn data frame into sp object
coordinates(sampledata) <- ~lon+lat
proj4string(sampledata) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Visual check if all points are on road network
# roads <- getPoints("data/osm_roads_aoi_wgs84.shp")
# plot(sampledata)
# plot(roads, add=T)

# Turn data into spatial object (sf)
sampledata <- st_as_sf(sampledata)
sampledata <- st_transform(sampledata, crs =28992)

# Work only with kriging points (not validation or bias data)
sampledata <- sampledata[sampledata$point_id < 200,]
sampledata <- sampledata[sampledata$total < 40,]

# Exploratory analysis
hist(sampledata$total, breaks = c(0,1,2,3,4,5,6,7,8,9,10,20,30,40,50))
summary(sampledata)

# Prepare indicator kriging variables
sampledata$category_zero <- ifelse(sampledata$total > 0, 1, 0)
sampledata$category_one <- ifelse(sampledata$total >= 1 & sampledata$total <= 2, 1, 0)
sampledata$category_two <- ifelse(sampledata$total >= 3 & sampledata$total <= 4, 1, 0)
sampledata$category_three <- ifelse(sampledata$total >= 5 & sampledata$total <= 6, 1, 0)
sampledata$category_four <- ifelse(sampledata$total >= 7, 1, 0)

sampledata <- as(sampledata, 'Spatial')



# Indicator Kriging #####################

kriglist_zero <- variogram_kriging(category_zero~1, sampledata, area_raster)
vgm_zero <- kriglist_zero[[1]]
cv_zero <- kriglist_zero[[2]]
krig_zero <- kriglist_zero[[3]]

kriglist_one <- variogram_kriging(category_one~1, sampledata, area_raster)
vgm_one <- kriglist_one[[1]]
cv_one <- kriglist_one[[2]]
krig_one <- kriglist_one[[3]]

kriglist_two <- variogram_kriging(category_two~1, sampledata, area_raster)
vgm_two <- kriglist_two[[1]]
cv_two <- kriglist_two[[2]]
krig_two <- kriglist_two[[3]]

kriglist_three <- variogram_kriging(category_three~1, sampledata, area_raster)
vgm_three <- kriglist_three[[1]]
cv_three <- kriglist_three[[2]]
krig_three <- kriglist_three[[3]]

kriglist_four <- variogram_kriging(category_four~1, sampledata, area_raster)
vgm_four <- kriglist_four[[1]]
cv_four <- kriglist_four[[2]]
krig_four <- kriglist_four[[3]]




# Ordinary Kriging #################

# Make semivariogram
glitter <- gstat(formula = total ~ 1, data = sampledata)

vglitter <- variogram(glitter, boundaries = c(70, 125, 175, 300, 400, 500, 600, 800, 1000, 1500))
vglitter
plot(vglitter, plot.numbers = T)

vgmlitter <- vgm(nugget = 2, psill = 12, range = 350, model = 'Exp')
vgmlitter <- fit.variogram(vglitter, vgmlitter)

plot(vglitter, vgmlitter)
attr(vgmlitter, 'SSErr')

# Export semivariogram to png


# Perform Kriging (euclidian distance)
  # cross-validation
litter_cv <- krige.cv(formula = total ~ 1, locations = sampledata, vgmlitter)
litter_cv$residual

# IDW: litter_cv <- krige.cv(formula = total ~ 1, locations = sampledata)

plot(litter_cv$residual)

# litter_cv_sp <- as(litter_cv, 'Spatial')
# bubble(litter_cv_sp_na, zcol = 'residual')

mean(litter_cv$zscore)
sd(litter_cv$zscore)


  # get roadnetwork with buffer 
# roadnetwork <- raster('data/c02_osm_road_raster.tif')
# roadnetwork[roadnetwork == -9999] <- NA
# roadnetwork <- as(roadnetwork, 'SpatialGridDataFrame')
roadnetwork <- readOGR(dsn = 'data', layer = 'c03_osm_roads_buffer_Dissolve')

# roadnetwork <- spTransform(roadnetwork, CRS(proj4string(sampledata)))

# "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs"



  # ordinary kriging
litter_krig = krige(total ~ 1, locations = sampledata, newdata = area_raster, model = vgmlitter, nmax = 15)

spplot(litter_krig['var1.pred'])
spplot(litter_krig['var1.var'], sp.layout = list('sp.points', sampledata, col = 'black'))

# Clip Raster on buffered roads

litter_krig_rast <- raster(litter_krig)
final <- mask(litter_krig_rast, roadnetwork)

# Visualize 
spplot(final, zcol = 'var1.pred')

# Export to geotiff
writeRaster(final, 'output/litter_ordinarykriging', format = 'GTiff')
