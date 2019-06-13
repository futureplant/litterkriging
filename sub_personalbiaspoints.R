### Sample points to detect interpersonal bias ###

# Date: 06 June 2019

library(rgdal)
library(sp)
library(rgeos)
library(raster)

# Import road networks
roadnetwork <- readOGR(dsn = "data", layer = "extra_roads_wgs84")

plot(roadnetwork)

# Random sampling points
set.seed(850)
sample_rndm <- spsample(roadnetwork, n = 11, type = "random")
length(sample_rndm)

plot(sample_rndm, add = T, col = 'red')

id_df = as.data.frame((1:length(sample_rndm)))
id_df[1:nrow(id_df),] = as.numeric(id_df[1:nrow(id_df),])
names(id_df) = 'ID'
sample_rndm <- SpatialPointsDataFrame(sample_rndm, id_df)



writeOGR(sample_rndm, 'output', layer = 'bias_samples', driver = 'ESRI Shapefile')

