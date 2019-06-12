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
source('scripts/indicatorselection.R')


# Retrieve and preprocess data ######################

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
sampledata$category_zero <- ifelse(sampledata$total > 0, 0, 1)
sampledata$category_one <- ifelse(sampledata$total >= 1 & sampledata$total <= 2, 1, 0)
sampledata$category_two <- ifelse(sampledata$total >= 3 & sampledata$total <= 4, 1, 0)
sampledata$category_three <- ifelse(sampledata$total >= 5 & sampledata$total <= 6, 1, 0)
sampledata$category_four <- ifelse(sampledata$total >= 7, 1, 0)

sampledata <- as(sampledata, 'Spatial')


# Study area for kriging (new_data)
study_area <- readOGR(dsn = "data", layer = "mapping_area_groenlo")
crs(study_area) <- crs(sampledata)
area_raster <- raster(extent(study_area), resolution = c(3,3))
crs(area_raster) <- crs(sampledata)
area_raster <- as(area_raster, 'SpatialGrid')
roadnetwork <- readOGR(dsn = 'data', layer = 'c03_osm_roads_buffer_Dissolve')



# Indicator Kriging #####################

kriglist_zero <- variogram_kriging(category_zero~1, sampledata, area_raster)
vgm_zero <- kriglist_zero[[1]]
cv_zero <- kriglist_zero[[2]]
krig_zero <- raster(kriglist_zero[[3]])

kriglist_one <- variogram_kriging(category_one~1, sampledata, area_raster)
vgm_one <- kriglist_one[[1]]
cv_one <- kriglist_one[[2]]
krig_one <- raster(kriglist_one[[3]])

kriglist_two <- variogram_kriging(category_two~1, sampledata, area_raster)
vgm_two <- kriglist_two[[1]]
cv_two <- kriglist_two[[2]]
krig_two <- raster(kriglist_two[[3]])

kriglist_three <- variogram_kriging(category_three~1, sampledata, area_raster)
vgm_three <- kriglist_three[[1]]
cv_three <- kriglist_three[[2]]
krig_three <- raster(kriglist_three[[3]])

kriglist_four <- variogram_kriging(category_four~1, sampledata, area_raster)
vgm_four <- kriglist_four[[1]]
cv_four <- kriglist_four[[2]]
krig_four <- raster(kriglist_four[[3]])

krigebrick <- brick(c(krig_zero,krig_one,krig_two,krig_three,krig_four))
max_layer <- findMaxLayer(krigebrick)

# Clip Raster on buffered roads

final <- mask(max_layer, roadnetwork)


# Visualize 
plot(final)



