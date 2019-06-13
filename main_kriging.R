### Spatial Modelling of Littering patterns in Groenlo, the Netherlands ###
# Script for performing kriging methods on littering observations in Groenlo

# Date: June 2019


# Libraries & Imports -------------------------------
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


# Retrieve and preprocess data -------------------------------

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

sampledata <- as(sampledata, 'Spatial')


# Study area for kriging (new_data)
study_area <- readOGR(dsn = "data", layer = "mapping_area_groenlo")
crs(study_area) <- crs(sampledata)
area_raster <- raster(extent(study_area), resolution = c(1,1))
crs(area_raster) <- crs(sampledata)
area_raster <- as(area_raster, 'SpatialGrid')

roadnetwork <- readOGR(dsn = 'data', layer = 'c03_osm_roads_buffer_Dissolve')



# Exploratory analysis -------------------------------
summary(sampledata)

hist(sampledata$total, breaks = c(0,1,2,3,4,5,6,7,8,9,10,20,30,40,50))
spplot(sampledata, zcol = 'total')

hist(sampledata$plastics, breaks = c(0,1,2,3,4,5,6,7,8,9,10,20,30,40,50))
hist(sampledata$paper, breaks = c(0,1,2,3,4,5,6,7,8,9,10,20))
hist(sampledata$organic_waste, breaks = c(0,1,2,3,4,5,6,7,8,9,10,20))
hist(sampledata$cigarette_butts, breaks = c(0,1,2,3,4,5,6,7,8,9,10,20))
hist(sampledata$other, breaks = c(0,1,2,3,4,5,6,7,8,9,10,20))

# Prepare indicator kriging variables for each category -------------------------------
# total, plastic, paper, organic, cigarettes, other

   # total
sampledata$total_zero <- ifelse(sampledata$total > 0, 0, 1)
sampledata$total_one <- ifelse(sampledata$total >= 1 & sampledata$total <= 2, 1, 0)
sampledata$total_two <- ifelse(sampledata$total >= 3 & sampledata$total <= 4, 1, 0)
sampledata$total_three <- ifelse(sampledata$total >= 5 & sampledata$total <= 6, 1, 0)
sampledata$total_four <- ifelse(sampledata$total >= 7, 1, 0)

   # plastics
sampledata$plastics_zero <- ifelse(sampledata$plastics == 0, 1, 0)
sampledata$plastics_one <- ifelse(sampledata$plastics == 1, 1, 0)
sampledata$plastics_two <- ifelse(sampledata$plastics == 2, 1, 0)
sampledata$plastics_three <- ifelse(sampledata$plastics == 3, 1, 0)
sampledata$plastics_four <- ifelse(sampledata$plastics >= 4, 1, 0)

   # organic
sampledata$organic_zero <- ifelse(sampledata$organic_waste == 0, 1, 0)
sampledata$organic_one <- ifelse(sampledata$organic_waste == 1, 1, 0)
sampledata$organic_two <- ifelse(sampledata$organic_waste == 2, 1, 0)
sampledata$organic_three <- ifelse(sampledata$organic_waste == 3, 1, 0)
sampledata$organic_four <- ifelse(sampledata$organic_waste >= 4, 1, 0)

   # paper
sampledata$paper_zero <- ifelse(sampledata$paper == 0, 1, 0)
sampledata$paper_one <- ifelse(sampledata$paper == 1, 1, 0)
sampledata$paper_two <- ifelse(sampledata$paper == 2, 1, 0)
sampledata$paper_three <- ifelse(sampledata$paper == 3, 1, 0)
sampledata$paper_four <- ifelse(sampledata$paper >= 4, 1, 0)

   # cigarettes
sampledata$cigarette_zero <- ifelse(sampledata$cigarette_butts == 0, 1, 0)
sampledata$cigarette_one <- ifelse(sampledata$cigarette_butts == 1, 1, 0)
sampledata$cigarette_two <- ifelse(sampledata$cigarette_butts == 2, 1, 0)
sampledata$cigarette_three <- ifelse(sampledata$cigarette_butts == 3, 1, 0)
sampledata$cigarette_four <- ifelse(sampledata$cigarette_butts >= 4, 1, 0)

   # other
sampledata$other_zero <- ifelse(sampledata$other == 0, 1, 0)
sampledata$other_one <- ifelse(sampledata$other == 1, 1, 0)
sampledata$other_two <- ifelse(sampledata$other == 2, 1, 0)
sampledata$other_three <- ifelse(sampledata$other == 3, 1, 0)
sampledata$other_four <- ifelse(sampledata$other >= 4, 1, 0)



# Indicator Kriging -------------------------------

   # total -----------------------
kriglist_zero_t <- variogram_kriging(total_zero~1, sampledata, area_raster)
vgm_zero_t <- kriglist_zero_t[[1]]
cv_zero_t <- kriglist_zero_t[[2]]
krig_zero_t <- raster(kriglist_zero_t[[3]])

kriglist_one_t <- variogram_kriging(total_one~1, sampledata, area_raster)
vgm_one_t <- kriglist_one_t[[1]]
cv_one_t <- kriglist_one_t[[2]]
krig_one_t <- raster(kriglist_one_t[[3]])

kriglist_two_t <- variogram_kriging(total_two~1, sampledata, area_raster)
vgm_two_t <- kriglist_two_t[[1]]
cv_two_t <- kriglist_two_t[[2]]
krig_two_t <- raster(kriglist_two_t[[3]])

kriglist_three_t <- variogram_kriging(total_three~1, sampledata, area_raster)
vgm_three_t <- kriglist_three_t[[1]]
cv_three_t <- kriglist_three_t[[2]]
krig_three_t <- raster(kriglist_three_t[[3]])

kriglist_four_t <- variogram_kriging(total_four~1, sampledata, area_raster)
vgm_four_t <- kriglist_four_t[[1]]
cv_four_t <- kriglist_four_t[[2]]
krig_four_t <- raster(kriglist_four_t[[3]])

krigebrick_t <- brick(c(krig_zero_t,krig_one_t,krig_two_t,krig_three_t,krig_four_t))
max_layer_total <- findMaxLayer(krigebrick_t)


   # plastics --------------------------
kriglist_zero_pl <- variogram_kriging(plastics_zero~1, sampledata, area_raster)
vgm_zero_pl <- kriglist_zero_pl[[1]]
cv_zero_pl <- kriglist_zero_pl[[2]]
krig_zero_pl <- raster(kriglist_zero_pl[[3]])

kriglist_one_pl <- variogram_kriging(plastics_one~1, sampledata, area_raster)
vgm_one_pl <- kriglist_one_pl[[1]]
cv_one_pl <- kriglist_one_pl[[2]]
krig_one_pl <- raster(kriglist_one_pl[[3]])

kriglist_two_pl <- variogram_kriging(plastics_two~1, sampledata, area_raster)
vgm_two_pl <- kriglist_two_pl[[1]]
cv_two_pl <- kriglist_two_pl[[2]]
krig_two_pl <- raster(kriglist_two_pl[[3]])

kriglist_three_pl <- variogram_kriging(plastics_three~1, sampledata, area_raster)
vgm_three_pl <- kriglist_three_pl[[1]]
cv_three_pl <- kriglist_three_pl[[2]]
krig_three_pl <- raster(kriglist_three_pl[[3]])

kriglist_four_pl <- variogram_kriging(plastics_four~1, sampledata, area_raster)
vgm_four_pl <- kriglist_four_pl[[1]]
cv_four_pl <- kriglist_four_pl[[2]]
krig_four_pl <- raster(kriglist_four_pl[[3]])

krigebrick_pl <- brick(c(krig_zero_pl,krig_one_pl,krig_two_pl,krig_three_pl,krig_four_pl))
max_layer_plastics <- findMaxLayer(krigebrick_pl)

   # organic ---------------------------------
kriglist_zero_or <- variogram_kriging(organic_zero~1, sampledata, area_raster)
vgm_zero_or <- kriglist_zero_or[[1]]
cv_zero_or <- kriglist_zero_or[[2]]
krig_zero_or <- raster(kriglist_zero_or[[3]])

kriglist_one_or <- variogram_kriging(organic_one~1, sampledata, area_raster)
vgm_one_or <- kriglist_one_or[[1]]
cv_one_or <- kriglist_one_or[[2]]
krig_one_or <- raster(kriglist_one_or[[3]])

kriglist_two_or <- variogram_kriging(organic_two~1, sampledata, area_raster)
vgm_two_or <- kriglist_two_or[[1]]
cv_two_or <- kriglist_two_or[[2]]
krig_two_or <- raster(kriglist_two_or[[3]])

kriglist_three_or <- variogram_kriging(organic_three~1, sampledata, area_raster)
vgm_three_or <- kriglist_three_or[[1]]
cv_three_or <- kriglist_three_or[[2]]
krig_three_or <- raster(kriglist_three_or[[3]])

kriglist_four_or <- variogram_kriging(organic_four~1, sampledata, area_raster)
vgm_four_or <- kriglist_four_or[[1]]
cv_four_or <- kriglist_four_or[[2]]
krig_four_or <- raster(kriglist_four_or[[3]])

krigebrick_or <- brick(c(krig_zero_or,krig_one_or,krig_two_or,krig_three_or,krig_four_or))
max_layer_organic <- findMaxLayer(krigebrick_or)

   # paper --------------------------------
kriglist_zero_pa <- variogram_kriging(paper_zero~1, sampledata, area_raster)
vgm_zero_pa <- kriglist_zero_pa[[1]]
cv_zero_pa <- kriglist_zero_pa[[2]]
krig_zero_pa <- raster(kriglist_zero_pa[[3]])

kriglist_one_pa <- variogram_kriging(paper_one~1, sampledata, area_raster)
vgm_one_pa <- kriglist_one_pa[[1]]
cv_one_pa <- kriglist_one_pa[[2]]
krig_one_pa <- raster(kriglist_one_pa[[3]])

kriglist_two_pa <- variogram_kriging(paper_two~1, sampledata, area_raster)
vgm_two_pa <- kriglist_two_pa[[1]]
cv_two_pa <- kriglist_two_pa[[2]]
krig_two_pa <- raster(kriglist_two_pa[[3]])

kriglist_three_pa <- variogram_kriging(paper_three~1, sampledata, area_raster)
vgm_three_pa <- kriglist_three_pa[[1]]
cv_three_pa <- kriglist_three_pa[[2]]
krig_three_pa <- raster(kriglist_three_pa[[3]])

kriglist_four_pa <- variogram_kriging(paper_four~1, sampledata, area_raster)
vgm_four_pa <- kriglist_four_pa[[1]]
cv_four_pa <- kriglist_four_pa[[2]]
krig_four_pa <- raster(kriglist_four_pa[[3]])

krigebrick_pa <- brick(c(krig_zero_pa,krig_one_pa,krig_two_pa,krig_three_pa,krig_four_pa))
max_layer_paper <- findMaxLayer(krigebrick_pa)

   # cigarettes --------------------------------
kriglist_zero_c <- variogram_kriging(cigarette_zero~1, sampledata, area_raster)
vgm_zero_c <- kriglist_zero_c[[1]]
cv_zero_c <- kriglist_zero_c[[2]]
krig_zero_c <- raster(kriglist_zero_c[[3]])

kriglist_one_c <- variogram_kriging(cigarette_one~1, sampledata, area_raster)
vgm_one_c <- kriglist_one_c[[1]]
cv_one_c <- kriglist_one_c[[2]]
krig_one_c <- raster(kriglist_one_c[[3]])

kriglist_two_c <- variogram_kriging(cigarette_two~1, sampledata, area_raster)
vgm_two_c <- kriglist_two_c[[1]]
cv_two_c <- kriglist_two_c[[2]]
krig_two_c <- raster(kriglist_two_c[[3]])

kriglist_three_c <- variogram_kriging(cigarette_three~1, sampledata, area_raster)
vgm_three_c <- kriglist_three_c[[1]]
cv_three_c <- kriglist_three_c[[2]]
krig_three_c <- raster(kriglist_three_c[[3]])

kriglist_four_c <- variogram_kriging(cigarette_four~1, sampledata, area_raster)
vgm_four_c <- kriglist_four_c[[1]]
cv_four_c <- kriglist_four_c[[2]]
krig_four_c <- raster(kriglist_four_c[[3]])

krigebrick_c <- brick(c(krig_zero_c,krig_one_c,krig_two_c,krig_three_c,krig_four_c))
max_layer_cigarette <- findMaxLayer(krigebrick_c)

   # other -----------------------------------
kriglist_zero_ot <- variogram_kriging(other_zero~1, sampledata, area_raster)
vgm_zero_ot <- kriglist_zero_ot[[1]]
cv_zero_ot <- kriglist_zero_ot[[2]]
krig_zero_ot <- raster(kriglist_zero_ot[[3]])

kriglist_one_ot <- variogram_kriging(other_one~1, sampledata, area_raster)
vgm_one_ot <- kriglist_one_ot[[1]]
cv_one_ot <- kriglist_one_ot[[2]]
krig_one_ot <- raster(kriglist_one_ot[[3]])

kriglist_two_ot <- variogram_kriging(other_two~1, sampledata, area_raster)
vgm_two_ot <- kriglist_two_ot[[1]]
cv_two_ot <- kriglist_two_ot[[2]]
krig_two_ot <- raster(kriglist_two_ot[[3]])

kriglist_three_ot <- variogram_kriging(other_three~1, sampledata, area_raster)
vgm_three_ot <- kriglist_three_ot[[1]]
cv_three_ot <- kriglist_three_ot[[2]]
krig_three_ot <- raster(kriglist_three_ot[[3]])

kriglist_four_ot <- variogram_kriging(other_four~1, sampledata, area_raster)
vgm_four_ot <- kriglist_four_ot[[1]]
cv_four_ot <- kriglist_four_ot[[2]]
krig_four_ot <- raster(kriglist_four_ot[[3]])

krigebrick_ot <- brick(c(krig_zero_ot,krig_one_ot,krig_two_ot,krig_three_ot,krig_four_ot))
max_layer_other <- findMaxLayer(krigebrick_ot)



# Clip rasterlayers with kriging results to roadnetwork -------------------------------
final_total <- mask(max_layer_total, roadnetwork)
final_plastics <- mask(max_layer_plastics, roadnetwork)
final_paper <- mask(max_layer_paper, roadnetwork)
final_organic <- mask(max_layer_organic, roadnetwork)
final_cigarette <- mask(max_layer_cigarette, roadnetwork)
final_other <- mask(max_layer_other, roadnetwork)



# Visualize 
plot(final_total)
plot(final_plastics)
plot(final_paper)
plot(final_organic)
plot(final_cigarette)
plot(final_other)


# Export litter maps as geotiff -------------------------------
writeRaster(final_total, 'output/total_indicatorkriging', format = 'GTiff', overwrite=T)
writeRaster(final_plastics, 'output/plastics_indicatorkriging', format = 'GTiff', overwrite=T)
writeRaster(final_paper, 'output/paper_indicatorkriging', format = 'GTiff', overwrite=T)
writeRaster(final_organic, 'output/organic_indicatorkriging', format = 'GTiff', overwrite=T)
writeRaster(final_cigarette, 'output/cigarette_indicatorkriging', format = 'GTiff',overwrite=T)
writeRaster(final_other, 'output/other_indicatorkriging', format = 'GTiff',overwrite=T)


