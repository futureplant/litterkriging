library(shinydashboard)
library(shiny)



# Construct the UI argument for the call to the Shiny App in the main.R script
ui <- dashboardPage(
  dashboardHeader(title = "Littering"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(column(width = 9,
                    box(width = NULL, solidHeader = TRUE,      #  This constructs a space for the map 
                        leafletOutput("map", height = 500)),
                    box(width = NULL,                          #  This constructs a space for the table
                        uiOutput("overviewTable")
                    )
    ),
    
    column(width = 3,
           
           # Construct box with image and introductory text for the app
           box(HTML("<p><strong>Littering in Groenlo</strong><br/></p>
                    <p align='justify'>Welcome to the littering app.
                    You can use this map to enter fieldwork data.<br/ > <br/ >
                    </p>
                    
                    <p><strong>Symbology</strong><br/ ></p>
                    <strong> Blue:</strong> Still needs assessment<br/ >
                    <strong>Green:</strong> Already assessed</p>
                  
                    <br/ ><br/ ></p>"), width = NULL, solidHeader = TRUE
                        ),
           box(width = NULL, sliderInput("time", "Time per sample:",
                                                            min = 0, max = 30,
                                                            value = 20),
               sliderInput("groups", "Number of groups:",
                           min = 1, max = 5, value = 3)), box(width = NULL, tableOutput("timeLeft"))
                    )
             )
    )
  )

