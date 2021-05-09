##################  Custom function library for server logic ###################

library(tidyverse)
library(here)
library(measurements)
library(geosphere)

### Initialization

# Initialize the click dataframe
initialize_click_dataframe <- function(file_to_check = "") {
  # Columns in the data frame
  col_names <- c(
    "Shot",
    "Latitude",
    "Longitude",
    "Shot Type",
    "Distance"
  )

  dataframe_click <- load_data(file_to_check, num_cols=length(col_names))
  names(dataframe_click) <- col_names
  dataframe_click %>%
    mutate(Shot = as.numeric(dataframe_click$Shot)) %>%
    mutate(Latitude = as.numeric(dataframe_click$Latitude)) %>%
    mutate(Longitude = as.numeric(dataframe_click$Longitude)) %>%
    mutate(`Shot Type` = as.character(dataframe_click$`Shot Type`)) %>%
    mutate(Distance = as.numeric(dataframe_click$Distance))
}

# Extracts pin location information from file or initializes empty vector
initialize_pin_vector <- function(file_to_check = "") {
  loaded_data <- load_data(file_to_check, num_cols = 2)
  if (nrow(loaded_data) == 0) {
    c(Latitude = NA, Longitude = NA)
  } else {
    pin_vector <- loaded_data %>% unlist %>% as.numeric
    names(pin_vector) <- c("Latitude", "Longitude")
    pin_vector
  }
}

# Checks if a pin vector is uninitialized
is_uninitialized_pin_vector <- function(pin_vector) {
  is.na(pin_vector[1]) && is.na(pin_vector[2])
}

### For updating the click dataframe

# To add a new shot
add_shot <- function(df, params) {
  df %>% {do.call(add_row, c(list(`.data` = .), lapply(params, as.numeric)))}
}


# To update a shot (after dragging)
update_shot <- function(df, update, cols_to_update) {
  cols_to_identify <- setdiff(colnames(update), cols_to_update)
  df %>% 
    rows_update(update %>% mutate_all(as.numeric), by=cols_to_identify, copy=TRUE)
}


### Loading/saving data

# Writes data from a dataframe to a csv file
save_data <- function(data, folders="data", filename="shot_data.csv") {
  folders_path <- ""
  # Create folders if they don't exist
  for (folder in folders) {
    folders_path <- paste0(folders_path, folder)
    if (!dir.exists(folders_path)) {
      dir.create(folders_path, showWarnings = FALSE)
    }
    folders_path <- paste0(folders_path, "/")
  }
  full_filepath <- paste0(folders_path, filename)
  # Create a unique file name
  filepath <- here(sprintf(full_filepath, as.integer(Sys.time()), digest::digest(data)))
  if (!file.exists(filepath)) {
    file.create(filepath, showWarnings = FALSE)
  }
  # Write the file to the local system
  write.csv(
    x = data,
    file = filepath, 
    row.names = FALSE, quote = TRUE
  )
}

# Loads data from a file into a data.frame
load_data <- function(path, num_cols=1, headers=TRUE) {
  filepath <- here(sprintf(path, as.integer(Sys.time()), digest::digest(data)))
  if (is_empty(path) || (!file.exists(filepath))) {
    data.frame(matrix(NA, nrow=0, ncol=num_cols))
  } else {
    read_csv(filepath, col_names=headers)
  }
}

# Reads all CSV files in a folder to a single dataframe
rbind_all <- function(path) {
  level_ids <- c("Date", "Tournament", "Player", "Round", "Hole")
  level_id <- level_ids[str_count(path, "/")]
  if (level_id != "Hole") {
    # Recurse
    for (dir in list.dirs(here(path), full.names=FALSE)[-1]) {
      new_path <- str_interp("${path}/${dir}")
      rbind_all(new_path)
    }
    
    pattern <- str_interp(".*/${path}/[^/]+/all_data\\.csv")
  } else {
    pattern <- "(.+\\.csv)(?<!all_data\\.csv)"
  }
  
  files <- grep(pattern, list.files(here(path), full.names=TRUE, recursive=T), perl=T, value=T)
  aggregated_data <- sapply(files, read_csv, simplify=FALSE) %>% 
    bind_rows(.id = level_id) %>% 
    mutate("{level_id}" := gsub(str_interp("(${here(path)})|(\\.csv)|(Hole )|(Round )|(all_data)"), "", .[[level_id]])) %>% 
    mutate("{level_id}" := gsub("/", "", .[[level_id]]))
  save_data(aggregated_data, unlist(strsplit(path, split="/")), "all_data.csv")
}

# Checks if data exists for the pin's metadata
check_if_data_exists <- function(pin_metadata) {
  date <- pin_metadata$date
  tournament <- pin_metadata$tournament
  round <- pin_metadata$round
  hole <- pin_metadata$hole
  
  pattern <- str_interp("${as.character(date)}/${tournament}/[^/]+(?<!Pin Locations)/Round ${round}/Hole ${hole}\\.csv")
  print(pattern)
  pin_files <- grep(pattern, list.files(here("data/shot_data"), recursive=T), perl=T, value=T)
  length(pin_files) > 0
}


### Map interaction

# To add a shot to the map
add_shot_to_map <- function(map, lon, lat, shot_num, dis) {
  map %>% addCircleMarkers(lon, lat, radius=4, color="black", group="new_point",
                           layerId=shot_num, options=markerOptions(draggable = TRUE),
                           label = paste("Shot ID: ", shot_num, "distance: ", dis))
}

# Add pin location to the map
add_pin_to_map <- function(map, pin, draggable=FALSE) {
  if (!(pin %>% is.na %>% any)) {
    map %>% addAwesomeMarkers(pin["Longitude"], pin["Latitude"], group="pin", 
                              options=markerOptions(draggable = draggable))
  }
}

# To populate the map using a dataframe
populate_map <- function(map, shot_df) {
  for (row in 1:(nrow(shot_df))) {
    shot <- shot_df %>% slice(row)
    add_shot_to_map(map, shot$Longitude, shot$Latitude, shot$Shot, shot$Distance)
  }
}

# Renders radio buttons based on number of clicks
create_radio_buttons <- function(num_clicks=1, current_shots=character(0)) {
  renderUI({
    if (num_clicks < 1) {
      NULL
    } else {
      box(
        title = "Use these buttons to select shot types. Select after plotting all shots on the map.",
        lapply(1:num_clicks, function(i) {
          selection <- if (i > length(current_shots)) {
            character(0)
          } else {
            current_shots[i]
          }
          radioButtons(
            str_interp("shot_${i}_type"), 
            str_interp("Shot ${i} Type:"),
            c(
              "Fairway" = "Fairway",
              "Green" = "Green",
              "Rough" = "Rough",
              "Sand" = "Sand",
              "Water" = "Water"
            ), 
            selected = selection,
            inline = TRUE
          )
        })
      )
    }
  })
}

# Extracts a shot type vector from input
get_shot_type_vector <- function(input, num_shots) {
  sapply(1:num_shots, function(shot_num) {
    input[[str_interp("shot_${shot_num}_type")]]
  }) %>% unlist
}

# Finding distance method using built in R method
pin_distance <- function(pin_vector, shot) {
  if (!(is.na(pin_vector["Latitude"])) && !(is.na(pin_vector["Longitude"]))) {
    distm(c(pin_vector["Longitude"], pin_vector["Latitude"]), 
          c(shot$lng, shot$lat), fun = distHaversine) %>% 
      conv_unit("m", "yd")
  } else {
    -1
  }
}

### Metadata forms

# Form for metadata on shot input tab
shot_metadata_form <- function(input) {
  renderUI({
    box(
      title = "Metadata Entry",
      width = "100%",
      fluidRow(
        column(3, dateInput("date", "Tournament Start Date:", value = Sys.Date(), width="100px")),
        column(9, selectInput("tournament", "Tournament Name:", c("", load_data("data/tournaments.csv")$Tournaments), width="70%"))
      ),
      fluidRow(
        column(6, selectInput("player", "Player Name:", c("", load_data("data/players.csv")$Players))),
        column(3, selectInput("round", "Round Number:", c("", 1:3))),
        column(3, selectInput("hole",
                             "Choose the hole:",
                             list(`not chosen` = "", `front half` = 1:9, `back half` = 10:18),
                             width="150px"))
      ),
      
      # Submission button only appears when all fields are filled
      renderUI({
        if (is_empty(input$tournament) || is_empty(input$player) || 
            is_empty(input$round) || is_empty(input$hole)) {
          NULL
        } else {
          button <- actionButton("submit_meta", "Submit Metadata")
        }
      })
    )
  })
}

# Form for metadata on reports tab
report_metadata_form <- function(input) {
  renderUI({
    box(
      title = "Metadata Entry",
      width = "100%",
      fluidRow(
        column(3, dateInput("date_report", "Tournament Start Date:", value = Sys.Date(), width="100px")),
        column(9, selectInput("tournament_report", "Tournament Name:", c("", load_data("data/tournaments.csv")$Tournaments), width="70%"))
      ),
      fluidRow(
        column(6, selectInput("player_report", "Player Name:", c("", load_data("data/players.csv")$Players))),
        column(3, selectInput("round_report", "Round Number:", c("", 1:3))),
        column(3, selectInput("hole_report",
                             "Choose the hole:",
                             list(`not chosen` = "", `front half` = 1:9, `back half` = 10:18),
                             width="150px"))
      ),
      
      # Submission button only appears when all fields are filled
      renderUI({
        if (is_empty(input$tournament_report) || is_empty(input$player_report) || 
            is_empty(input$round_report) || is_empty(input$hole_report)) {
          NULL
        } else {
          actionButton("search", "Search")
        }
      })
    )
  })
}

# Form for metadata on pin locations entry tab
pin_metadata_form <- function(input) {
  renderUI({
    box(
      title = "Metadata Entry",
      width = "100%",
      fluidRow(
        column(3, dateInput("date_pin", "Date:", value = Sys.Date(), width="100px")),
        column(9, selectInput("tournament_pin", "Tournament name:", c("", load_data("data/tournaments.csv")$Tournaments), width="70%"))
      ),
      fluidRow(
        column(6, selectInput("round_pin", "Round Number:", c("", 1:3))),
        column(6, selectInput("hole_pin",
                              "Choose the hole:",
                              list(`not chosen` = "", `front half` = 1:9, `back half` = 10:18),
                              width="150px"))
      ),
      
      # Submission button only appears when all fields are filled
      renderUI({
        if (is_empty(input$tournament_pin) || is_empty(input$round_pin) || 
            is_empty(input$hole_pin)) {
          NULL
        } else {
          actionButton("submit_pin_metadata", "Submit Metadata")
        }
      })
    )
  })
}

### Miscellaneous helper functions

# Checks if a string is empty
is_empty <- function(str) {
  str == ""
}

# To convert metadata into a filepath
metadata_to_filepath <- function(metadata, for_pins=FALSE) {
  round <- if (is.null(metadata$round)) {
    NULL
  } else {
    str_interp("Round ${metadata$round}")
  }
  hole <- if (is.null(metadata$hole)) {
    NULL
  } else {
    str_interp("Hole ${metadata$hole}.csv")
  }
  folders_path <- c(
    "data",
    "shot_data",
    as.character(metadata$date),
    as.character(metadata$tournament),
    ifelse(for_pins, "Pin Locations", as.character(metadata$player)),
    round,
    hole
  )
  paste0(folders_path, collapse="/")
}

# Creates a vector for folders from metadata
get_folders_vector <- function(date, tournament, player, round) {
  folders_vector <- c(
    date = date, 
    tournament = tournament, 
    player = player,
    round = round
  )
  if (!is_empty(folders_vector["round"])) {
    folders_vector["round"] <- str_interp("Round ${round}")
  }
  folders_vector[!is_empty(folders_vector)]
}

