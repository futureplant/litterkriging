# # testgrounds
# 
# urlobs <- 'https://docs.google.com/spreadsheets/d/12UrAPu96rbjzFuqlTIaklLiFUNPFxCYOxtTmAbL786A/edit?usp=sharing'
# obsdtf <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE,fileEncoding = "UTF-8",encoding = "UTF-8")
# pointids <- dtf[,1]
# class(pointids)
# sum(pointids==999)
# 
# 
# sum(pointids==999)
# 
# 
# urlobs <- 'https://docs.google.com/spreadsheets/d/12UrAPu96rbjzFuqlTIaklLiFUNPFxCYOxtTmAbL786A/edit?usp=sharing'
# obsdtf <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE,fileEncoding = "UTF-8",encoding = "UTF-8")[,1]
# obsdtf
# 
# 
# 
# waterpoints <- st_read("data/waterpoints.shp")
# head(waterpoints)
# waterpoints$Observations <- lapply(waterpoints$OBJECTID,FUN=function(x) sum(obsdtf == x))
# waterpoints[568,]
