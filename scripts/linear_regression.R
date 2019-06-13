rm(list = ls())
library(gsheet)
source('scripts/getdataframe.R')

# Get data from google sheets
sampledata <- getDataFrame('https://docs.google.com/spreadsheets/d/1MyHRcpDJX2iro6a_2nk0mOJBRSm_x0lpkLH04IoKJII/edit?usp=sharing')
sampledata$lon <- as.numeric(sub(".*,", "", sampledata$Coordinates))
sampledata$lat <- as.numeric(sub(",.*", "", sampledata$Coordinates))
sampledata$total <- sampledata$total + 0.0001
#sampledata$road_type <- factor(sampledata$road_type)

regression <- lm(total ~ road_type, data=sampledata)
regression$residuals
summary(regression)
