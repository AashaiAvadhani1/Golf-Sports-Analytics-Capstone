################## Defines server logic for golf application ##################

library(shiny)
library(sp)
library(tidyverse)  
library(leaflet)
library(leaflet.extras)
library(DT)

source("server/server_helpers.R")

# This is the dataframe in which data from each click is stored
dataframe_column_names <- c(
  "Shot",
  "Latitude",
  "Longitude"
)
click_dataframe <- initialize_click_dataframe(dataframe_column_names)

server <- function(input, output) {
  ################## Main Tab Logic #######################
  
  # Metadata entry form 
  output$metadata_form <- metadata_form(input)

  # When "submit-metadata" button is clicked
  observeEvent(input$submit_meta, {
    hole_locations_filename <- str_interp("data/tournament_hole_locations/${metadata()$tournament}.csv")
    hole_locations <- load_data(hole_locations_filename)
    output$description <- renderText({
      dummy <- metadata()$date
      "Click anywhere to draw a circle"
    })
    output$mymap <- renderLeaflet({
      m = leaflet(initialize_spatial(), width="100%", height="100%") %>%
        addDrawToolbar(circleOptions=NA, markerOptions=NA, polygonOptions=NA,
                       rectangleOptions=NA, polylineOptions=NA, circleMarkerOptions=NA) %>%
        addProviderTiles('Esri.WorldImagery') %>%
        # setView(lat = 40.47942168506459, lng=-79.85795114512402, zoom=17)
        setView(
          lat = hole_locations[metadata()$hole, "Latitude", drop=TRUE],
          lng = hole_locations[metadata()$hole, "Longitude", drop=TRUE],
          zoom=17
        )
    })
    output$map_buttons <- renderUI({
      dummy <- metadata()$date
      fluidRow(
        column(2, actionButton("clear", "Clear Markers")),
        column(10, actionButton("submit_data", "Submit Markers"))
      )
    })
    click_dataframe <<- initialize_click_dataframe(dataframe_column_names)
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
    shot_num <- nrow(click_dataframe) + 1
    
    leafletProxy("mymap") %>%
      addCircleMarkers(click$lng, click$lat, radius=4, color="black", group="new_point",
                     layerId=shot_num, options=markerOptions(draggable = TRUE))
    
    click_dataframe <<- click_dataframe %>% 
      add_shot(list(
        Shot = shot_num,
        Latitude = click$lat,
        Longitude = click$lng
      ))
  })
  
  # Observe event for dragging markers after initializing them
  observeEvent(input$mymap_marker_dragend, {
    drag <- input$mymap_marker_dragend
    
    update <- tibble(
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
    folders_path <- c(
      "data",
      "shot_data",
      as.character(metadata()$date),
      as.character(metadata()$tournament),
      as.character(metadata()$player),
      str_interp("Round ${metadata()$round}")
    )
    file_name <- str_interp("hole_${metadata()$hole}.csv")
    save_data(click_dataframe, folders=folders_path, filename=file_name)
  })
  
  ################## Metadata Entry Tab Logic #######################
  
  # Button for adding a new player
  observeEvent(input$submit_new_player, {
    new_player_name <- input$new_player
    
    players <- load_data("data/players.csv")
    if(nrow(players) == 0) {
      players <- data.frame(new_player_name)
      colnames(players) <- "Players"
    } else {
      if(!(new_player_name %in% players$Players)) {
        players <- rbind(players, new_player_name)
      }
    }
    save_data(players, folders="data", filename="players.csv")
    updateTextInput(inputId="new_player", value="")
  })
  
  # Button for adding a new tournament
  observeEvent(input$submit_new_tournament, {
    new_tournament_name <- input$new_tournament
    
    # Add tournament name to tournaments list
    tournaments <- load_data("data/tournaments.csv")
    if(nrow(tournaments) == 0) {
      tournaments <- data.frame(new_tournament_name)
      colnames(tournaments) <- "Tournaments"
    } else {
      if(!(new_tournament_name %in% tournaments$Tournaments)) {
        tournaments <- rbind(tournaments, new_tournament_name)
      }
    }
    save_data(tournaments, folders="data", filename="tournaments.csv")
    
    # Save hole locations
    holes_filename <- paste0(new_tournament_name, ".csv")
    hole_locations <- data.frame(matrix(NA, nrow=0, ncol=3))
    names(hole_locations) <- c("Hole Number", "Latitude", "Longitude")
    for (hole_num in 1:18) {
      lat <- as.numeric(input[[str_interp("hole${hole_num}_lat")]])
      lon <- as.numeric(input[[str_interp("hole${hole_num}_lon")]])
      hole_locations <- hole_locations %>% 
        add_row(
          `Hole Number` = hole_num, 
          Latitude = lat, 
          Longitude = lon
        )
    }
    
    save_data(hole_locations, folders=c("data", "tournament_hole_locations"), filename=holes_filename)
    
    # Clear all inputs
    updateTextInput(inputId="new_tournament", value="")
    for (hole_num in 1:18) {
      updateTextInput(inputId=paste0("hole", hole_num, "_lat"), value="")
      updateTextInput(inputId=paste0("hole", hole_num, "_lon"), value="")
    }
  })
  
  ################## Reports Tab Logic #######################
  
  # Search form 
  output$search_form <- metadata_form(input, for_report=TRUE)
  
  # When "search" button is clicked
  observeEvent(input$search, {
    # This lets reports tab know what to render (to be changed maybe)
    output$click_dataframe <- renderDataTable(load_data(report_filepath()))
  })
  report_filepath <- eventReactive(input$search, {
    folders_path <- c(
      "data",
      "shot_data",
      as.character(input$date_report),
      as.character(input$tournament_report),
      as.character(input$player_report),
      str_interp("Round ${input$round_report}"),
      str_interp("hole_${input$hole_report}.csv")
    )
    paste0(folders_path, collapse="/")
  })
}