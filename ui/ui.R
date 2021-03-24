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
    h2("My Map", align="center"),
    h5("Click anywhere to draw a circle", align="center"),
    leafletOutput("mymap", width="100%", height="500px"),
    actionButton("clear", "Clear Markers")
  )
)