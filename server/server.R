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
  output$metadata_form <- renderUI({
    box(
      title = "Metadata Entry",
      width = "100%",
      fluidRow(
        column(3, dateInput("date", "Date:", value = Sys.Date(), width="100px")),
        column(9, selectInput("tournament", "Tournament name:", c("", load_data("data/tournaments.csv")$Tournaments), width="70%"))
      ),
      fluidRow(
        column(6, selectInput("player", "Player name:", c("", "Set Markers", load_data("data/players.csv")$Players))),
        column(3, selectInput("round", "Round Number:", c("", "NA (Setting markers)", 1:3))),
        column(3, selectInput("hole",
                              "Choose the hole:",
                              list(`not chosen` = "", `front half` = 1:9, `back half` = 10:18),
                              width="150px"))
      ),
      
      # Submission button only appears when all fields are filled
      renderUI({
        if (is_empty(input$tournament) || is_empty(input$player) || 
            is_empty(input$round) || is_empty(input$hole)) {
          return(NULL)
        } else {
          actionButton("submit_meta", "Submit Metadata")
        }
      })
    )
  })
  
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
  
  # This lets reports tab know what to render (to be changed maybe)
  output$click_dataframe <- renderDataTable({
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
    save_data(click_dataframe, folder="data", filename="shot_data.csv")
  })
  
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
    save_data(players, folder="data", filename="players.csv")
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
    save_data(tournaments, folder="data", filename="tournaments.csv")
    
    # Save hole locations
    holes_filename <- paste0(new_tournament_name, ".csv")
    hole_locations <- data.frame(matrix(NA, nrow=0, ncol=3))
    names(hole_locations) <- c("Hole Number", "Latitude", "Longitude")
    for(hole_num in 1:18) {
      lat <- as.numeric(input[[str_interp("hole${hole_num}_lat")]])
      lon <- as.numeric(input[[str_interp("hole${hole_num}_lon")]])
      print(lat)
      print(lon)
      hole_locations <- hole_locations %>% 
        add_row(
          `Hole Number` = hole_num, 
          Latitude = lat, 
          Longitude = lon
        )
    }
    
    save_data(hole_locations, folder="data/tournament_hole_locations", filename=holes_filename)
    
    # Clear all inputs
    updateTextInput(inputId="new_tournament", value="")
    for(hole_num in 1:18) {
      updateTextInput(inputId=paste0("hole", hole_num, "_lat"), value="")
      updateTextInput(inputId=paste0("hole", hole_num, "_lon"), value="")
    }
  })
}