# adds hyperlinks to df

#&entry.844325190=99999&entry.166518206=888888


createFormLinks <- function(dataframe){
  basicFormURL <- "https://docs.google.com/forms/d/e/1FAIpQLSfrTYBhp2sue2L7w9723-RWJ7IlhHA6_lEsxks-wpdxiBCJnA/viewform?usp=pp_url"
  basicMapURL <- "https://www.google.com/maps/search/?api=1&query="

  dataframe$formURL <- paste0(
                        
                        '<strong>Point ',
                        dataframe$OBJECTID,
                        '</strong><br />',
                        '<br /><a href ="',
                        basicMapURL,
                        st_coordinates(dataframe)[,'Y'],
                        ",",
                        st_coordinates(dataframe)[,'X'],
                        '"',
                        "> Directions </a><br /><br />",
                       '<a href ="',
                       basicFormURL,
                       "&entry.1104805026=",
                       dataframe$OBJECTID,
                       "&entry.541960516=",
                       st_coordinates(dataframe)[,'Y'],
                       ",",
                       st_coordinates(dataframe)[,'X'],
                       '"',
                       "> Assessment </a> <br /><br />    "
                       
                       
        
                       )
  

  
  dataframe <- st_as_sf(dataframe, coords =c("longitude","latitude"), crs = 4326)
  return (dataframe)
}



