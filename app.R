# Main file of the litterkriging

library(shiny)
library(shinydashboard)
library(leaflet)

source("ui.R",encoding = "Latin1")
source("scripts/getpoints.R")
source("scripts/hyperlinks.R")
source("scripts/readgsheets.R",encoding = "Latin1")



# server serves information to the user interface that is constructed in scripts/ui.R file
server <- function(input, output, session) {
  

  

  
  # url of gsheet which contains form answers
  url <- 'https://docs.google.com/spreadsheets/d/1Dn96ArmKeIu-lnDSUHzAKnGcJv7Kjmqii_H-Y-zVd74/edit?usp=sharing'
  # creates dataframe from google sheet answers
  dtf <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE,fileEncoding = "UTF-8",encoding = "UTF-8")
  
  


  samplepoints <- getPoints()
  completes <- getCompletes(dtf)


  

  
  getColor <- function(samples,samplepoints) {
    sapply(samplepoints$OBJECTID, function(OBJECTID) {
      if(OBJECTID %in% completes){
        "green"
      } else {
        "blue"
      } 
      })
  }
  
  icons <- awesomeIcons(
    icon = 'square',
    iconColor = 'white',
    library = 'ion',
    markerColor = getColor(samples,samplepoints)
  )
  
  # Render background map for shiny app
  output$map <- renderLeaflet({
    
    waterlinks <- createFormLinks(samplepoints)
    # Add default OpenStreetMap map tiles 
    leaflet(data = waterlinks) %>% setView(lng = 6.6034176,13, lat = 52.0321426, zoom = 13) %>%  
      addTiles() %>% 
      addAwesomeMarkers(
                  icon = icons,
                  popup = ~formURL
                 ) %>%
      addLabelOnlyMarkers(label =  ~as.character(OBJECTID), 
                          labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))
    
    })
  
  # Make table which provides a summary of points to be assessed
  output$overviewTable <- renderUI(
    tags$table(class = "table",
               
               tags$thead(tags$tr(
                 tags$th("Color"),
                 tags$th("Description"),
                 tags$th("Number of points"))),
                
               tags$tbody(
                 tags$tr(tags$td(span(style = sprintf(
                   "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                   "lightgreen"
                 ))),
                 tags$td("Completed"),
                 tags$td(getLength(dtf))
                 ),
                 tags$tr(tags$td(span(style = sprintf(
                   "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                   "lightblue"
                 ))),
                 tags$td("To be assessed"),
                 tags$td(getLength(samplepoints)-getLength(dtf))
                 )
                 
               )
  ))  
}

shinyApp(ui, server)



