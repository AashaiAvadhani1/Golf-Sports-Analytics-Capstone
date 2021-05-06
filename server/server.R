################## Defines server logic for golf application ##################

library(shiny)
library(sp)
library(tidyverse)  
library(here)
library(leaflet)
library(leaflet.extras)
library(DT)

source("server/server_helpers.R")

# This is the dataframe in which data from each click is stored
dataframe_column_names <- c(
  "Shot",
  "Latitude",
  "Longitude",
  "Shot Type"
)
click_dataframe <- initialize_click_dataframe(dataframe_column_names)
pin_dataframe <- initialize_click_dataframe(dataframe_column_names)

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
        # setView(lat = 40.47942168506459, lng=-79.85795114512402, zoom=17
        setView(
          lat = hole_locations[metadata()$hole, "Latitude", drop=TRUE],
          lng = hole_locations[metadata()$hole, "Longitude", drop=TRUE],
          zoom=17
        )
    })
    output$radio_buttons <- renderUI({
      dummy <- metadata()$date
      NULL
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
    click_dataframe <<- initialize_click_dataframe(dataframe_column_names, file_to_check)
    populate_map(leafletProxy("mymap"), click_dataframe)
    # populating pin locations
    pin_to_check <- pindata_to_filepath(metadata())
    pin_dataframe <<- initialize_click_dataframe(dataframe_column_names, pin_to_check)
    add_pin_to_map(leafletProxy("mymap"), pin_dataframe$Longitude, pin_dataframe$Latitude, 
                   pin_dataframe$Shot, draggable=FALSE)
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
    
    add_shot_to_map(leafletProxy("mymap"), click$lng, click$lat, shot_num)
    
    click_dataframe <<- click_dataframe %>% 
      add_shot(list(
        Shot = shot_num,
        Latitude = click$lat,
        Longitude = click$lng
      ))
    
    output$radio_buttons <- create_radio_buttons(shot_num)
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
  output$search_form <- metadata_form(input, for_report=TRUE)
  
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

  ################## Pin Location Tab Logic #######################
  # Metadata entry form 
  output$pin_form <- pin_form(input)
  
  # When "submit-metadata" button is clicked
  observeEvent(input$submit_pin, {
    hole_locations_filename <- str_interp("data/tournament_hole_locations/${pindata()$tournament}.csv")
    hole_locations <- load_data(hole_locations_filename)
    output$pin_description <- renderText({
      dummy <- pindata()$date
      "Click anywhere to draw a Pin Location"
    })
    output$pinmap <- renderLeaflet({
      m = leaflet(initialize_spatial(), width="100%", height="100%") %>%
        addDrawToolbar(circleOptions=NA, markerOptions=NA, polygonOptions=NA,
                       rectangleOptions=NA, polylineOptions=NA, circleMarkerOptions=NA) %>%
        addProviderTiles('Esri.WorldImagery') %>%
        # setView(lat = 40.47942168506459, lng=-79.85795114512402, zoom=17)
        setView(
          lat = hole_locations[pindata()$hole, "Latitude", drop=TRUE],
          lng = hole_locations[pindata()$hole, "Longitude", drop=TRUE],
          zoom=17
        )
    })
    output$pin_buttons <- renderUI({
      dummy <- pindata()$date
      fluidRow(
        column(3, actionButton("clear", "Clear Pin Locations")),
        column(9, actionButton("submit_data", "Submit Pins"))
      )
    })
    pin_to_check <- pindata_to_filepath(pindata())
    pin_dataframe <<- initialize_click_dataframe(dataframe_column_names, pin_to_check)
    add_pin_to_map(leafletProxy("pinmap"), pin_dataframe$Longitude, pin_dataframe$Latitude, 
                   pin_dataframe$Shot, draggable=TRUE)
  })
  pindata <- eventReactive(input$submit_pin, {
    data <- list()
    data$date <- input$date_pin
    data$tournament <- input$tournament_pin
    data$round <- input$round_pin
    data$hole <- input$hole_pin
    data
  })
  
  # Clicking to add a Pin Location
  observeEvent(input$pinmap_click, {
    
    click <- input$pinmap_click
    shot_num <- nrow(pin_dataframe) + 1
    
    if (shot_num > 1) {
      return(NULL)
    }
    
    add_pin_to_map(leafletProxy("pinmap"), click$lng, click$lat, shot_num)
    
    pin_dataframe <<- pin_dataframe %>% 
      add_shot(list(
        Shot = shot_num,
        Latitude = click$lat,
        Longitude = click$lng
      ))
  })
  
  # Observe event for dragging markers after initializing them
  observeEvent(input$pinmap_marker_dragend, {
    drag <- input$pinmap_marker_dragend
    
    update <- tibble(
      Shot = drag$id,
      Latitude = drag$lat,
      Longitude = drag$lng
    )
    
    pin_dataframe <<- pin_dataframe %>% 
      update_shot(update, c("Latitude", "Longitude"))
    
  })
  
  # When "clear" button is clicked
  observeEvent(input$clear, {
    leafletProxy("pinmap") %>% 
      clearGroup("new_point")
    pin_dataframe <<- initialize_click_dataframe(dataframe_column_names)
  })
  
  # When "submit_data" button is clicked
  observeEvent(input$submit_data, {
    print(pin_dataframe)
    folders_path <- c(
      "data",
      "shot_data",
      as.character(pindata()$date_pin),
      as.character(pindata()$tournament_pin),
      as.character("Pin Locations"),
      str_interp("Round ${pindata()$round_pin}")
    )
    file_name <- str_interp("hole_${pindata()$hole_pin}.csv")
    save_data(pin_dataframe, folders=folders_path, filename=file_name)
  })
  
}






