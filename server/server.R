################## Defines server logic for golf application ##################

library(shiny)
library(sp)
library(tidyverse)  

source("server/server_helpers.R")

# This is the dataframe in which data from each click is stored
click_dataframe <- initialize_click_dataframe()

server <- function(input, output) {
  
  output$mymap <- renderLeaflet({
    x = 1 
    y = 2
    
    m = leaflet(initialize_spatial(), width="100%", height="100%") %>% 
      addTiles() %>%
      addCircleMarkers() %>%
      setView(lat = 40.47942168506459, lng=-79.85795114512402, zoom=17)
  })
  
  # Clicking to add a marker
  observeEvent(input$mymap_click, {
    
    click <- input$mymap_click
    leafletProxy("mymap") %>% 
      addCircles(click$lng, click$lat, radius=2, color="red", group = "new_point")
    click_dataframe <<- click_dataframe %>% add_row(Longitude = click$lng, Latitude = click$lat)
    saveData(click_dataframe)
    
  })
  
  # When "clear" button is clicked
  observeEvent(input$clear, {
    leafletProxy("mymap") %>% 
      clearGroup("new_point")
  })
}