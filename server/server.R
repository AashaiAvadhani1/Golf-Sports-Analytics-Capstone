################## Defines server logic for golf application ##################

library(shiny)
library(sp)
library(tidyverse)  
library(leaflet)
library(leaflet.extras)

source("server/server_helpers.R")

# This is the dataframe in which data from each click is stored
dataframe_column_names <- c(
  "Date",
  "Tournament Name",
  "Round",
  "Player",
  "Hole",
  "Shot",
  "Latitude",
  "Longitude"
)
click_dataframe <- initialize_click_dataframe(dataframe_column_names)
shot_num <- 0

server <- function(input, output) {
  
  output$mymap <- renderLeaflet({
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
    shot_num <<- shot_num + 1
    
    leafletProxy("mymap") %>% 
      addCircleMarkers(click$lng, click$lat, radius=4, color="red", group = "new_point",
                     layerId = shot_num, options = markerOptions(draggable = TRUE))
    click_dataframe <<- click_dataframe %>% 
      add_shot(list(
        Date = input$date,
        `Tournament Name` = input$tournament,
        Round = input$round,
        Player = input$player,
        Hole = input$hole,
        Shot = shot_num,
        Latitude = click$lat,
        Longitude = click$lng
      ))
    
    # Nested observe event for dragging markers after initializing them
    observeEvent(input$mymap_marker_dragend, {
      drag <- input$mymap_marker_dragend
      
      update <- tibble(
        Date = input$date,
        `Tournament Name` = input$tournament,
        Round = input$round,
        Player = input$player,
        Hole = input$hole,
        Shot = drag$id,
        Latitude = drag$lat,
        Longitude = drag$lng
      )
      
      click_dataframe <<- click_dataframe %>% 
        update_shot(update, c("Latitude", "Longitude"))
    })
    
  })
  
  # When "clear" button is clicked
  observeEvent(input$clear, {
    leafletProxy("mymap") %>% 
      clearGroup("new_point")
  })
  
  # When "submit" button is clicked
  observeEvent(input$submit, {
    saveData(click_dataframe)
  })
}