### INDICATOR KRIGING ###


library(sp)
library(gstat)
library(raster)
library(gsheet)
library(stars)
library(rgdal)


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


# Make semivariogram
gzero <- gstat(formula = category_zero ~ 1, data = sampledata)

vgzero <- variogram(gzero)# , boundaries = c(70, 125, 175, 300, 400, 500, 600, 800, 1000, 1500))
vgzero
plot(vgzero, plot.numbers = T)

vgmzero <- vgm(nugget = 0.075, psill = 0.225, range = 1000, model = 'Exp')
vgmzero <- fit.variogram(vgzero, vgmzero)

plot(vgzero, vgmzero)
attr(vgmzero, 'SSErr')

