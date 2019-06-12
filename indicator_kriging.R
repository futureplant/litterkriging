### INDICATOR KRIGING ###


library(sp)
library(gstat)
library(raster)
library(gsheet)
library(stars)
library(rgdal)
library(automap)
source('scripts/variogramkriging.R')

# Get data from google sheets
sampledata <- read.csv(text=gsheet2text('https://docs.google.com/spreadsheets/d/1MyHRcpDJX2iro6a_2nk0mOJBRSm_x0lpkLH04IoKJII/edit?usp=sharing', 
                                        format='csv'), stringsAsFactors=FALSE,fileEncoding = "UTF-8",encoding = "UTF-8")
sampledata$lon <- as.numeric(sub(".*,", "", sampledata$Coordinates))
sampledata$lat <- as.numeric(sub(",.*", "", sampledata$Coordinates))

# Turn data frame into sp object
coordinates(sampledata) <- ~lon+lat
proj4string(sampledata) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Turn data into spatial object (sf) and transform to RDnew
sampledata <- st_as_sf(sampledata)
sampledata <- st_transform(sampledata, crs =28992)

# Work only with kriging points (not validation or bias data)
sampledata <- sampledata[sampledata$point_id < 200,]
sampledata <- sampledata[sampledata$total < 40,]


# Threshold: larger than zero (0 when total==0; 1 when total>0)
sampledata$category_zero <- ifelse(sampledata$total > 0, 1, 0)

hist(sampledata$category_zero)


# study area
study_area <- readOGR(dsn = "data", layer = "mapping_area_groenlo")
crs(study_area) <- crs(sampledata)

area_raster <- raster(extent(study_area), resolution = c(5,5))
crs(area_raster) <- crs(sampledata)
area_raster <- as(area_raster, 'SpatialGrid')


sampledata <- as(sampledata, 'Spatial')



kriglist <- variogram_kriging(category_zero~1, sampledata, area_raster)
vgm1 <- kriglist[[1]]
cv <- kriglist[[2]]
krig <- kriglist[[3]]

##################################################

# auto semivariogram
vgmzero <- autofitVariogram(category_zero ~ 1, sampledata)
plot(variogram)

# Make semivariogram
gzero <- gstat(formula = category_zero ~ 1, data = sampledata)

vgzero <- variogram(gzero)# , boundaries = c(70, 125, 175, 300, 400, 500, 600, 800, 1000, 1500))
vgzero
plot(vgzero, plot.numbers = T)

vgmzero <- vgm(nugget = 0.075, psill = 0.225, range = 1000, model = 'Sph')
vgmzero <- fit.variogram(vgzero, vgmzero)

plot(vgzero, vgmzero)
attr(vgmzero, 'SSErr')

# kriging cross-validation
zero_cv <- krige.cv(formula = category_zero ~ 1, locations = sampledata, vgmzero$var_model)

plot(zero_cv$residual)

zero_cv_sp <- as(zero_cv, 'Spatial')
bubble(zero_cv_sp, zcol = 'residual')

mean(zero_cv$zscore)
sd(zero_cv$zscore)

# auto kriging
# zero_krig_auto <- autoKrige(category_zero ~1, sampledata, new_data = area_raster)
# spplot(zero_krig_auto, zcol = 'krige_output')

# indicator kriging
zero_krig = krige(category_zero ~ 1, locations = sampledata, newdata = area_raster, model = vgmzero$var_model, nmax = 15)

spplot(zero_krig['var1.pred'])
spplot(zero_krig['var1.var'], sp.layout = list('sp.points', sampledata, col = 'black'))


####################################



# Clip Raster on buffered roads

roadnetwork <- readOGR(dsn = 'data', layer = 'c03_osm_roads_buffer_Dissolve')
zero_krig_rast <- raster(zero_krig)
final <- mask(zero_krig_rast, roadnetwork)


# Visualize 
spplot(final, zcol = 'var1.pred')



# Export to geotiff
writeRaster(final, 'output/zero_ordinarykriging', format = 'GTiff')