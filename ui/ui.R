#################### Defines ui logic for golf application ####################

library(shinydashboard)
library(leaflet)

source("ui/ui_helpers.R")

ui <- dashboardPage(
  
  dashboardHeader(
  ),
  
  # Sidebar layout with input and output definitions 
  dashboardSidebar(
  ),
  
  # Main panel for displaying outputs 
  dashboardBody(
    h2("Data Entry App for CMU Golf Team", align="center"), 
    br(),
    
    # Form for metadata entry
    fluidRow(
      column(3, dateInput("date", "Date:", value = Sys.Date(), width="100px")),
      column(9, textInput("tournament", "Tournament Name:", "", width="70%"))
    ),
    fluidRow(
      column(3, selectInput("round", "Round Number:", 1:3)),
      column(3, selectInput("hole", 
                            "Choose the hole:", 
                            list(`front half` = 1:9, `back half` = 10:18),
                            width="150px")),
      column(6, selectInput("player", "Player name:", c("p1", "p2", "p3", "p4", "p5")))
    ),
    br(), br(),
    
    # Leaflet map
    h5("Click anywhere to draw a circle", align="center"),
    leafletOutput("mymap", width="100%", height="500px"),
    br(), 
    
    # Submission buttons
    actionButton("clear", "Clear Markers"),
    actionButton("submit", "Submit Data")
  )
)
