### Sampling Strategy for Measuring Litter Intensity in Groenlo, the Netherlands ###
# This script contains code to create sampling schemes for three different goals:

# The first part creates a regular grid of sampling points along a road network
# as well as coordinates for random short-distance points which will be used in ArcMap.
# This sampling scheme enables us to make a variogram and perform a kriging analysis.

# The second sampling scheme is to create random validation points in the same study area.

# The final sampling scheme is used to determine observer bias and is again a random sampling method.


# Date: May 2019


# Libraries ------------------------------------
library(sp)
library(rgdal)
library(rgeos)
library(raster)
source('scripts/create_samplingpoints_dataframe.R')

# Import road networks and study area ------------------------------------
roadnetwork <- readOGR(dsn = "data", layer = "osm_roads_groenlo")

study_area <- readOGR(dsn = "data", layer = "mapping_area_groenlo")

study_roadnetwork <- crop(roadnetwork, study_area)


# Test spsample -----------------------------------------
test_road <- readOGR(dsn = "data", layer = "Test_road")
plot(test_road)
test_samples <- spsample(test_road, n = 10, type = 'regular')
plot(test_samples, add = T, col = 'red')


## KRIGING SAMPLING ## -----------------------------

# Create a regular grid along road ------------------------------------
sample_reg <- create_samples_to_spatialDF(study_roadnetwork, numberpoints = 90, scheme = 'regular')

length(sample_reg)
plot(study_roadnetwork) 
plot(sample_reg, add = T, col = 'red')

# Export regular grid sampling points
writeOGR(sample_reg, 'output', layer = 'regular_sample_points', driver = 'ESRI Shapefile')


# Create short-distance random distribution ------------------------------
set.seed(500)
sample_short <- sample(1:nrow(sample_reg@coords), 30)

short_dist <- c(10, 20, 30, 50, 70)
set.seed(500)
short_dist_rndm <- sample(short_dist, 30, replace = T)

short_dist_df <- data.frame(sample_short, short_dist_rndm)

### Note: ArcMap used for adding short-distance points to the regular points ### 




# Transform sample points to WGS84

sample_points <- readOGR('output', 'regular_sample_points')

crs(sample_points) <- crs("+proj=longlat +datum=WGS84 +no_defs")    # '+init=epsg:4326')
sample_points_wgs84 <- spTransform(sample_points, "+proj=longlat +datum=WGS84 +no_defs")

# Write final output to file

writeOGR(sample_points_wgs84, 'output', layer = 'sample_points', driver = 'ESRI Shapefile')





## VALIDATION SAMPLING ## -----------------------------
validation_points <- create_samples_to_spatialDF(study_roadnetwork, numberpoints = 50, scheme = 'random')

length(validation_points)
plot(study_roadnetwork) 
plot(sample_reg, add = T, col = 'red')
plot(validation_points, add = T, col = 'blue')

# Export validation random sampling points
writeOGR(validation_points, 'output', layer = 'validation_points', driver = 'ESRI Shapefile')


## PERSONAL BIAS SAMPLING ## --------------------------------
# Import road networks outside of original study area -----------------------------------
roadnetwork_bias <- readOGR(dsn = "data", layer = "extra_roads_wgs84")
plot(roadnetwork_bias)

# Create random sampling points ----------------------------------------
sample_bias <- create_samples_to_spatialDF(roadnetwork_bias, numberpoints = 11, scheme = 'random')

length(sample_bias)
plot(roadnetwork_bias)
plot(sample_bias, add = T, col = 'red')


# Export points for personal bias investigation
writeOGR(sample_bias, 'output', layer = 'bias_samples', driver = 'ESRI Shapefile')