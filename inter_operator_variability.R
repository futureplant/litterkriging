rm(list = ls())

# load libraries and scripts
library(gsheet)
library(dplyr)
source('scripts/getdataframe.R')
library(ggplot2)

# retrieve data
sampledata <- getDataFrame('https://docs.google.com/spreadsheets/d/1MyHRcpDJX2iro6a_2nk0mOJBRSm_x0lpkLH04IoKJII/edit?usp=sharing')

# filter out bias point
bias <- sampledata[sampledata$point_id >= 400,]

# filter out relevant columns
bias <- select(bias,c(point_id, your_name, total))
bias$total <- as.numeric(bias$total) # convert to numeric

# create new dataframe
df <- data.frame()

# loop over sample points and calculate mean and sd
starter = 1
for (factor in c(401:410)){
  df[starter,1] <- factor
  df[starter,2] <- mean(bias$total[bias$point_id == factor])
  df[starter,3] <- sd(bias$total[bias$point_id == factor])
  starter <- starter + 1
}
colnames(df) <- c("point", "mean", "sd")


# plot preliminary results
plot(bias$point_id,bias$total)
text(bias$point_id,bias$total, labels=bias$your_name, cex= 0.7)

# get sd of entire dataset
sd_final <- mean(df$sd)

# create table that shows measurements at all bias points per person
namelist <- c("intan", "joep","melanie","willy\n","joey")
for (name in namelist){
  df[,(ncol(df)+1)] <- bias[bias$your_name == name,3]
}
names(df) <- c("point", "mean", "sd", namelist) 

# prepare a table that allows for boxplot plotting with ggplot 2
finaltable <- data.frame()
personals <- data.frame(matrix(NA, nrow = 10,ncol = 0))
for (name in namelist){
  personals[,"name"] <- name
  personals[,"error"] <- (df[,name]-df[,"mean"])
  finaltable <- rbind(finaltable,personals)
}

# save Boxplot to png 
png("output/inter_operator_variability.png")
p10 <- ggplot(finaltable, aes(x = name, y = error)) +
  geom_boxplot()
p10 <- p10 + stat_summary(fun.y=mean, geom="point", shape=21, size=4, fill = "blue", show.legend = T)
p10 <- p10 + geom_hline(yintercept=0, linetype="dashed", color = "red") 
p10 <- p10 + labs(title="Inter operator variability")
text <- paste("Overall standard deviation:", format(round(sd_final, 2), nsmall = 2))
p10 <- p10 + labs(caption = text,  xmin = 4, xmax = 4, ymin = -3, ymax = -3)

p10
dev.off()

p10

