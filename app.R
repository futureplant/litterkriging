# main Fieldwork app server file

# date: May 2019

# load libraries ----
library(shiny)
library(shinydashboard)
library(leaflet)

# source scripts ----
source("ui.R",encoding = "Latin1")
source("scripts/getpoints.R")
source("scripts/hyperlinks.R")
source("scripts/readgsheets.R",encoding = "Latin1")
source("scripts/getdataframe.R")


# construct server ----
# server serves information to the user interface that is constructed in scripts/ui.R file ----
server <- function(input, output, session) {
  # url of gsheet which contains form answers
  dtf <- getDataFrame('https://docs.google.com/spreadsheets/d/1Dn96ArmKeIu-lnDSUHzAKnGcJv7Kjmqii_H-Y-zVd74/edit?usp=sharing')
  
  # read in data to be plotted in app
  samplepoints <- getPoints("data/bias_samples.shp")
  roads <- getPoints("data/osm_roads_aoi_wgs84.shp")
  completes <- getCompletes(dtf)

  # assign colors to markers
  getColor <- function(samples,samplepoints) {
    sapply(samplepoints$ID, function(ID) {
      if(ID %in% completes){
        "green"
      } else if(ID >= 200){
        "orange"
      } else {
        "blue"
      } 
      })
  }
  
  # construct markers
  icons <- awesomeIcons(
    icon = 'square',
    iconColor = 'white',
    library = 'ion',
    markerColor = getColor(samples,samplepoints)
  )
  
  # construct values for table
  totalpoints <- nrow(samplepoints)
  donepoints <- nrow(dtf)
  
  # take inputs for sliders for sample time estimation
  sliderValues <- reactive({
    time <- input$sampletime
    groups <- input$teams
    mins <- (totalpoints - donepoints)*time/groups
    paste(floor(mins/60), " hours ", round(mins%%60), " minutes needed")
  })
  
  # output estimated sampletime
  output$selected_var <- renderText(sliderValues())
  
  # Render background map for shiny app
  output$map <- renderLeaflet({
    waterlinks <- createFormLinks(samplepoints)
    # Add default OpenStreetMap map tiles 52.0368481718756,6.64762982247062
    leaflet(data = waterlinks) %>% setView(lng = 6.647629, lat = 52.0368481, zoom = 13) %>%  
      addTiles() %>% 
      addAwesomeMarkers(
                  icon = icons,
                  popup = ~formURL
                 ) %>%
      addLabelOnlyMarkers(label =  ~as.character(ID), 
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

# start app ----
shinyApp(ui, server)



