################## Defines server logic for golf application ##################

library(shiny)
library(sp)
library(tidyverse)  
library(leaflet)
library(leaflet.extras)

source("server/server_helpers.R")

# This is the dataframe in which data from each click is stored
click_dataframe <- initialize_click_dataframe()

server <- function(input, output) {
  
  output$mymap <- renderLeaflet({
    x = 1 
    y = 2
    
    m = leaflet(initialize_spatial(), width="100%", height="100%") %>% 
      addDrawToolbar(circleOptions=NA, markerOptions=NA, polygonOptions=NA, 
                     rectangleOptions=NA, polylineOptions=NA, circleMarkerOptions = NA) %>%
      addTiles() %>%
      addCircleMarkers(options = markerOptions(draggable = TRUE)) %>%
      setView(lat = 40.47942168506459, lng=-79.85795114512402, zoom=17)
  })
  
  # Clicking to add a marker
  observeEvent(input$mymap_click, {
    
    click <- input$mymap_click
    shot_num <- nrow(click_dataframe) + 1
    leafletProxy("mymap") %>% 
      addCircleMarkers(click$lng, click$lat, radius=4, color="red", group = "new_point",
                     layerId = shot_num, options = markerOptions(draggable = TRUE))
    click_dataframe <<- click_dataframe %>% add_row(Longitude = click$lng, Latitude = click$lat,
                                                    ShotId = shot_num)
    saveData(click_dataframe)
    
    # nested observe event for dragging markers after initializing them
    observeEvent(input$mymap_marker_dragend, {
      drag <- input$mymap_marker_dragend
      
      click_dataframe$Longitude[click_dataframe$ShotId == drag$id] = drag$lng
      click_dataframe$Latitude[click_dataframe$ShotId == drag$id] = drag$lat
      
      saveData(click_dataframe)
    })
    
  })
  
  # When "clear" button is clicked
  observeEvent(input$clear, {
    leafletProxy("mymap") %>% 
      clearGroup("new_point")
  })
}