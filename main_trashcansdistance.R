library(nngeo)
library(gsheet)
source('scripts/getpoints.R')


# load data
trashcans <- getPoints('data/prb02_trashcan_6jun2019.shp')
trashcans <- st_transform(trashcans, crs =28992) #  transform to rd new

# Get data from google sheets
litterdata <- getDataFrame('https://docs.google.com/spreadsheets/d/1MyHRcpDJX2iro6a_2nk0mOJBRSm_x0lpkLH04IoKJII/edit?usp=sharing')
litterdata$lon <- as.numeric(sub(".*,", "", litterdata$Coordinates))
litterdata$lat <- as.numeric(sub(",.*", "", litterdata$Coordinates))

# Turn data frame into sp object
coordinates(litterdata) <- ~lon+lat
proj4string(litterdata) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Turn data into spatial object (sf)
litterdata <- st_as_sf(litterdata)
litterdata <- st_transform(litterdata, crs =28992) #  transform to rd new

# Work only with kriging points (not validation or bias data)
litterdata <- litterdata[litterdata$point_id < 200,]
litterdata <- litterdata[litterdata$total < 40,]

# find distance to neireighst trashcan
distancelist <- st_nn(litterdata, trashcans, returnDist = TRUE)
litterdata$distance <- distancelist$dist[,1]

plot(st_geometry(litterdata))
plot(st_geometry(trashcans), add=T, col="red")

regression <- lm(total ~ distance, data=litterdata)
