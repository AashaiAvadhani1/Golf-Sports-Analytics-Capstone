################## Defines server logic for golf application ##################

library(shiny)
library(sp)
library(tidyverse)  
library(here)
library(leaflet)
library(leaflet.extras)
library(DT)

source("server/server_helpers.R")

click_dataframe <- initialize_click_dataframe()
pin_vector <- initialize_pin_vector()

server <- function(input, output) {
  ################## Main Tab Logic #######################
  
  # Metadata entry form 
  output$metadata_form <- shot_metadata_form(input)

  # When "submit-metadata" button is clicked
  observeEvent(input$submit_meta, {
    hole_locations_filename <- str_interp("data/tournament_hole_locations/${metadata()$tournament}.csv")
    hole_locations <- load_data(hole_locations_filename)
    output$description <- renderText({
      dummy <- metadata()$date
      "Click anywhere to draw a circle"
    })
    output$shot_input_map <- renderLeaflet({
      leaflet(width="100%", height="100%") %>%
        addDrawToolbar(circleOptions=NA, markerOptions=NA, polygonOptions=NA,
                       rectangleOptions=NA, polylineOptions=NA, circleMarkerOptions=NA) %>%
        addProviderTiles('Esri.WorldImagery') %>%
        # setView(lat = 40.47942168506459, lng=-79.85795114512402, zoom=17
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
    
    # populating markers
    file_to_check <- metadata_to_filepath(metadata())
    click_dataframe <<- initialize_click_dataframe(file_to_check)
    output$radio_buttons <- {
      dummy <- metadata()$date
      shot_type_vector <- click_dataframe %>% pull(`Shot Type`)
      create_radio_buttons(length(shot_type_vector), current_shots = shot_type_vector)
    }
    populate_map(leafletProxy("shot_input_map"), click_dataframe)
    
    # populating pin locations
    # metadata() %>%
    #   metadata_to_filepath(for_pins=TRUE) %>% 
    #   initialize_pin_vector %>% 
    #   add_pin_to_map(leafletProxy("shot_input_map"), .)
    add_pin_to_map(leafletProxy("shot_input_map"), map_pin_vector())
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
  map_pin_vector <- eventReactive(input$submit_meta, {
    metadata() %>%
      metadata_to_filepath(for_pins=TRUE) %>% 
      initialize_pin_vector
  })
  
  # Clicking to add a marker
  observeEvent(input$shot_input_map_click, {
    
    click <- input$shot_input_map_click
    shot_num <- nrow(click_dataframe) + 1
    
    add_shot_to_map(leafletProxy("shot_input_map"), click$lng, click$lat, shot_num)
    
    click_dataframe <<- click_dataframe %>% 
      add_shot(list(
        Shot = shot_num,
        Latitude = click$lat,
        Longitude = click$lng
      ))
    
    output$radio_buttons <- create_radio_buttons(shot_num)
  })
  
  # Observe event for dragging markers after initializing them
  observeEvent(input$shot_input_map_marker_dragend, {
    drag <- input$shot_input_map_marker_dragend
    
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
    leafletProxy("shot_input_map") %>% 
      clearGroup("new_point")
    click_dataframe <<- initialize_click_dataframe()
    output$radio_buttons <- NULL
  })
  
  # When "submit_data" button is clicked
  observeEvent(input$submit_data, {
    click_dataframe <<- click_dataframe %>% 
      mutate(`Shot Type` = get_shot_type_vector(input, nrow(.)))
    folders_path <- c(
      "data",
      "shot_data",
      as.character(metadata()$date),
      as.character(metadata()$tournament),
      as.character(metadata()$player),
      str_interp("Round ${metadata()$round}")
    )
    file_name <- str_interp("Hole ${metadata()$hole}.csv")
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
  output$search_form <- report_metadata_form(input)
  
  # When "search" button is clicked
  observeEvent(input$search, {
    # This lets reports tab know what to render
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
      str_interp("Hole ${input$hole_report}.csv")
    )
    paste0(folders_path, collapse="/")
  })
  
  ############## Data Compilation Tab Logic ###################
  
  # Form to select
  output$compile_form <- renderUI({
    box(
      selectInput("date_compile", "Tournament Start Date:", c("", list.dirs(here("data/shot_data"), full.names=FALSE, recursive=FALSE))),
      selectInput("tournament_compile", "Tournament Name:", c("", load_data("data/tournaments.csv")$Tournaments)),
      selectInput("player_compile", "Player Name:", c("", load_data("data/players.csv")$Players)),
      selectInput("round_compile", "Round Number:", c("", 1:3)),
      actionButton("submit_compile", "Submit")
    )
  })
  
  # When "submit" button is clicked
  observeEvent(input$submit_compile, {
    if(dir.exists(compile_file_location())) {
      data <- rbind_all(compile_file_location())
      output$compile_message <- renderText({
        dummy <- paste0(compile_file_location(), " ", collapse="")
        str_interp(
          '<font color="green">Success! Your file can be found at ${compile_file_location()}/all_data.csv!</font>'
        )
      })
    }
    else {
      output$compile_message <- renderText({
        dummy <- paste0(compile_file_location(), " ", collapse="")
        '<font color="red">This data does not exist.</font>'
      })
    }
  })
  compile_file_location <- eventReactive(input$submit_compile, {
    inputs <- list()
    if(!is_empty(input$date_compile)) {
      inputs$date <- input$date_compile
    }
    if(!is_empty(input$tournament_compile)) {
      inputs$tournament <- input$tournament_compile
    }
    if(!is_empty(input$player_compile)) {
      inputs$player <- input$player_compile
    }
    if(!is_empty(input$round_compile)) {
      inputs$round <- input$round_compile
    }
    metadata_to_filepath(inputs)
  })

  #################### Pin Entry Tab Logic #########################
  
  # Metadata entry form 
  output$pin_form <- pin_metadata_form(input)
  
  # When "submit_pin_metadata" button is clicked
  observeEvent(input$submit_pin_metadata, {
    hole_locations_filename <- str_interp("data/tournament_hole_locations/${pin_metadata()$tournament}.csv")
    hole_locations <- load_data(hole_locations_filename)
    output$pin_description <- renderText({
      dummy <- pin_metadata()$date
      "Click/drag to drop pin"
    })
    output$pin_input_map <- renderLeaflet({
      leaflet(width="100%", height="100%") %>%
        addDrawToolbar(circleOptions=NA, markerOptions=NA, polygonOptions=NA,
                       rectangleOptions=NA, polylineOptions=NA, circleMarkerOptions=NA) %>%
        addProviderTiles('Esri.WorldImagery') %>%
        setView(
          lat = hole_locations[pin_metadata()$hole, "Latitude", drop=TRUE],
          lng = hole_locations[pin_metadata()$hole, "Longitude", drop=TRUE],
          zoom=17
        )
    })
    output$pin_buttons <- renderUI({
      dummy <- pin_metadata()$date
      fluidRow(
        column(3, actionButton("clear_pin", "Clear Pin")),
        column(9, actionButton("submit_pin", "Submit Pins"))
      )
    })
    
    # Checks if pin has been inputted previously, displays if it does
    pin_vector <<- pin_metadata() %>% 
      metadata_to_filepath(for_pins=TRUE) %>% 
      initialize_pin_vector
    add_pin_to_map(leafletProxy("pin_input_map"), pin_vector, draggable = TRUE)
  })
  pin_metadata <- eventReactive(input$submit_pin_metadata, {
    data <- list()
    data$date <- input$date_pin
    data$tournament <- input$tournament_pin
    data$round <- input$round_pin
    data$hole <- input$hole_pin
    data
  })
  
  # Clicking to set a pin
  observeEvent(input$pin_input_map_click, {
    click <- input$pin_input_map_click

    if (pin_vector %>% is.na %>% any) {
      pin_vector <<- c(Latitude = click$lat, Longitude = click$lng)
      add_pin_to_map(leafletProxy("pin_input_map"), pin_vector, draggable = TRUE)
    }
  })
  
  # Observe event for dragging pins after initializing them
  observeEvent(input$pin_input_map_marker_dragend, {
    drag <- input$pin_input_map_marker_dragend
    pin_vector <- c(Latitude = drag$lat, Longitude = drag$lng)
  })
  
  # When "clear_pin" button is clicked
  observeEvent(input$clear_pin, {
    leafletProxy("pin_input_map") %>% 
      clearGroup("pin")
    pin_vector <<- initialize_pin_vector()
  })
  
  # When "submit_pin" button is clicked
  observeEvent(input$submit_pin, {
    folders_path <- c(
      "data",
      "shot_data",
      as.character(pin_metadata()$date),
      as.character(pin_metadata()$tournament),
      as.character("Pin Locations"),
      str_interp("Round ${pin_metadata()$round}")
    )
    file_name <- str_interp("Hole ${pin_metadata()$hole}.csv")
    save_data(pin_vector %>% as.list %>% data.frame, folders=folders_path, filename=file_name)
  })
  
}
