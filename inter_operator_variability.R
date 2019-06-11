# inter operator variability is calculated here
library(gsheet)
library(dplyr)

sampledata <- getDataFrame('https://docs.google.com/spreadsheets/d/1Dn96ArmKeIu-lnDSUHzAKnGcJv7Kjmqii_H-Y-zVd74/edit?usp=sharing')
bias <- sampledata[sampledata$Point.ID >= 400,]

bias <- select(bias,c(Point.ID, Your.name, total))
bias$total <- as.numeric(bias$total)
bias$Point.ID <- as.numeric(bias$Point.ID)
plot(bias$Point.ID,bias$total)

bias <- split(bias,bias$Point.ID)
bias$`401`


