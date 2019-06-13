### Sampling Strategy for Measuring Litter Intensity in Groenlo ###

# Date: 20 May 2019

library(rgdal)
library(sp)
library(rgeos)
library(raster)

# Import road networks

roadnetwork <- readOGR(dsn = "data", layer = "osm_roads_groenlo")

# roadnetwork <- gLineMerge(roadnetwork)

study_area <- readOGR(dsn = "data", layer = "mapping_area_groenlo")

study_roadnetwork <- crop(roadnetwork, study_area)


# Regular grid along road

# numOfPoints <- gLength(study_roadnetwork) / 1000
sample_reg <- spsample(study_roadnetwork, n = 90, type = "regular")
length(sample_reg)

id_df = as.data.frame((1:length(sample_reg)))
id_df[1:74,] = as.numeric(id_df[1:74,])
names(id_df) = 'ID'
sample_reg <- SpatialPointsDataFrame(sample_reg, id_df)


plot(study_roadnetwork) #, col = study_roadnetwork@data$code)
plot(sample_reg, add = T, col = 'red')

# Short-distance random grid
set.seed(500)
sample_short <- sample(1:nrow(sample_reg@coords), 30)

short_dist <- c(10, 20, 30, 50, 70)
set.seed(500)
short_dist_rndm <- sample(short_dist, 30, replace = T)

short_dist_df <- data.frame(sample_short, short_dist_rndm)

### ArcMap needed for adding short-distance points ### 


# Export sampling points

writeOGR(sample_reg, 'output', layer = 'regular_sample_points', driver = 'ESRI Shapefile')


# Transform sample points to WGS84

sample_points <- readOGR('output', 'regular_sample_points')

crs(sample_points) <- crs("+proj=longlat +datum=WGS84 +no_defs")    # '+init=epsg:4326')
sample_points_wgs84 <- spTransform(sample_points, "+proj=longlat +datum=WGS84 +no_defs")

# Write final output to file

writeOGR(sample_points_wgs84, 'output', layer = 'sample_points', driver = 'ESRI Shapefile')





# Validation points
set.seed(800)
validation_points <- spsample(study_roadnetwork, n = 50, type = "random")
length(validation_points)

val_df = as.data.frame((1:length(validation_points)))
val_df[1:31,] = as.numeric(val_df[1:31,])
names(val_df) = 'ID'
validation_points <- SpatialPointsDataFrame(validation_points, val_df)

writeOGR(validation_points, 'output', layer = 'validation_points', driver = 'ESRI Shapefile')





# Test spsample

test_road <- readOGR(dsn = "data", layer = "Test_road")

plot(test_road)

test_samples <- spsample(test_road, 10, type = 'regular')

plot(test_samples, add = T, col = 'red')
