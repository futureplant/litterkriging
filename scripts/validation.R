library(gsheet)
library(sp)
library(raster)
library(optpart)
source('scripts/getdataframe.R')

# Get data from google sheets
sampledata <- getDataFrame('https://docs.google.com/spreadsheets/d/1MyHRcpDJX2iro6a_2nk0mOJBRSm_x0lpkLH04IoKJII/edit?usp=sharing')
sampledata$lon <- as.numeric(sub(".*,", "", sampledata$Coordinates))
sampledata$lat <- as.numeric(sub(",.*", "", sampledata$Coordinates))

# Turn data frame into sp object
coordinates(sampledata) <- ~lon+lat
proj4string(sampledata) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
sampledata <- sampledata[sampledata$point_id >= 200,]
sampledata <- sampledata[sampledata$point_id < 400,]

# make a reclass matrix to check reclassification scheme
recMatrix <- matrix(c(0,0,1,1,2,2,3,4,3,5,6,4,7,100,5),
                    ncol=3, byrow = T, dimnames = list(NULL,c("from","to","becomes")))

# Reclass data to match Indicator Kriging datasets
sampledata_reclassed <- c()
for (value in sampledata$total){
  if (value == 0){
    sampledata_reclassed <- c(sampledata_reclassed,1)
  } else if (value == 1 | value == 2){
    sampledata_reclassed <- c(sampledata_reclassed,2)
  } else if (value == 3 | value == 4){
    sampledata_reclassed <- c(sampledata_reclassed,3)
  } else if (value == 5 | value == 6){
    sampledata_reclassed <- c(sampledata_reclassed,4)
  } else if (value >= 7){
    sampledata_reclassed <- c(sampledata_reclassed,5)
  }

}

# read in kriged predictions
predicted <- raster('output/total_indicatorkriging.tif')

# extract predicted values at validation points
predictions <- extract(predicted,sampledata)

# make a dataframe with predicted and measured values
validation_df <- data.frame(matrix(c(sampledata$point_id,predictions,sampledata_reclassed),ncol=3,dimnames = list(NULL,c("point","predicted","measured"))))
validation_df$difference <- validation_df$predicted - validation_df$measured
mean(sqrt(validation_df$difference^2))

confusion_matrix <- table(pred = validation_df$predicted, true = validation_df$measured)





