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
  # Submission button only appears when all fields are filled
  output$metadata_submission <- renderUI({
    if (is_empty(input$tournament) || is_empty(input$player) || 
        is_empty(input$round) || is_empty(input$hole)) {
      return(NULL)
    } else {
      actionButton("submit_meta", "Submit Metadata")
    }
  })
  
  
  
  # When "submit-metadata" button is clicked
  observeEvent(input$submit_meta, {
    output$description <- renderText("Click anywhere to draw a circle")
    output$mymap <- renderLeaflet({
      m = leaflet(initialize_spatial(), width="100%", height="100%") %>%
        addDrawToolbar(circleOptions=NA, markerOptions=NA, polygonOptions=NA,
                       rectangleOptions=NA, polylineOptions=NA, circleMarkerOptions=NA) %>%
        addTiles() %>%
        setView(lat = 40.47942168506459, lng=-79.85795114512402, zoom=17)
    })
    output$map_buttons <- renderUI({
      fluidRow(
        column(2, actionButton("clear", "Clear Markers")),
        column(10, actionButton("submit_data", "Submit Markers"))
      )
    })
  })
  metadata <- eventReactive(input$submit_meta, {
    data <- list()
    data$date <- input$date
    data$tournament <- input$tournament
    data$round <- input$round
    data$player <- input$player
    data$hole <- input$hole
    data
  })
  
  # Clicking to add a marker
  observeEvent(input$mymap_click, {
    
    click <- input$mymap_click
    shot_num <<- shot_num + 1
    
    leafletProxy("mymap") %>%
      addCircleMarkers(click$lng, click$lat, radius=4, color="black", group="new_point",
                     layerId=shot_num, options=markerOptions(draggable = TRUE))
    
    click_dataframe <<- click_dataframe %>% 
      add_shot(list(
        Date = metadata()$date,
        `Tournament Name` = metadata()$tournament,
        Round = metadata()$round,
        Player = metadata()$player,
        Hole = metadata()$hole,
        Shot = shot_num,
        Latitude = click$lat,
        Longitude = click$lng
      ))
  })
  
  output$click_dataframe = DT::renderDataTable({
    click_dataframe
  })
  
  # Observe event for dragging markers after initializing them
  observeEvent(input$mymap_marker_dragend, {
    drag <- input$mymap_marker_dragend
    
    update <- tibble(
      Date = metadata()$date,
      `Tournament Name` = metadata()$tournament,
      Round = metadata()$round,
      Player = metadata()$player,
      Hole = metadata()$hole,
      Shot = drag$id,
      Latitude = drag$lat,
      Longitude = drag$lng
    )
    
    click_dataframe <<- click_dataframe %>% 
      update_shot(update, c("Latitude", "Longitude"))

  })

  # When "clear" button is clicked
  observeEvent(input$clear, {
    leafletProxy("mymap") %>% 
      clearGroup("new_point")
    click_dataframe <<- initialize_click_dataframe(dataframe_column_names)
  })
  
  # When "submit_data" button is clicked
  observeEvent(input$submit_data, {
    saveData(click_dataframe)
  })
}