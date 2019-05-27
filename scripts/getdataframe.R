
getDataFrame <- function(url){
  dtf <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE,fileEncoding = "UTF-8",encoding = "UTF-8")
  return(dtf)
  
}

