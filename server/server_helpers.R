##################  Custom function library for server logic ###################

library(tidyverse)
library(here)

### Initialization

# Make a spatial data frame for initialization purposes
initialize_spatial <- function() {
  lats <- c(37,38,39)
  lons <- c(-94,-95,-96)
  df <- data.frame(cbind(lons,lats))
  coordinates(df) <-~ lons+lats
  df
}

# Initialize the click dataframe
initialize_click_dataframe <- function(col_names=c('Longitude', 'Latitude'),
                                       file_to_check = "") {
  dataframe_click <- load_data(file_to_check, num_cols=length(col_names))
  names(dataframe_click) <- col_names
  dataframe_click %>% mutate_all(as.numeric)
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
rbind_all <- function(path, pattern="*.csv") {
  files <- list.files(path = here(path), pattern = pattern, full.names=TRUE)
  aggregated_data <- sapply(files, read_csv, simplify=FALSE) %>% 
    bind_rows(.id = "Hole")
  aggregated_data
}


### Map interaction

# To add a shot to the map
add_shot_to_map <- function(map, lon, lat, shot_num) {
  map %>% addCircleMarkers(lon, lat, radius=4, color="black", group="new_point",
                     layerId=shot_num, options=markerOptions(draggable = TRUE))
}

# To populate the map using a dataframe
populate_map <- function(map, shot_df) {
  for(row in 1:(nrow(shot_df))) {
    shot <- shot_df %>% slice(row)
    add_shot_to_map(map, shot$Longitude, shot$Latitude, shot$Shot)
  }
}


### Miscellaneous helper functions

# Checks if a string is empty
is_empty <- function(str) {
  str == ""
}

# Checks if a dataframe is empty
is_empty_df <- function(df) {
  nrow(df) == 0
}

# Form for metadata
metadata_form <- function(input, for_report=FALSE) {
  if (for_report) {
    suffix <- "_report"
    button <- actionButton("search", "Search")
  } else {
    suffix <- ""
    button <- actionButton("submit_meta", "Submit Metadata")
  }
  date_label <- str_interp("date${suffix}")
  tournament_label <- str_interp("tournament${suffix}")
  player_label <- str_interp("player${suffix}")
  round_label <- str_interp("round${suffix}")
  hole_label <- str_interp("hole${suffix}")
  renderUI({
    box(
      title = "Metadata Entry",
      width = "100%",
      fluidRow(
        column(3, dateInput(date_label, "Tournament Start Date:", value = Sys.Date(), width="100px")),
        column(9, selectInput(tournament_label, "Tournament Name:", c("", load_data("data/tournaments.csv")$Tournaments), width="70%"))
      ),
      fluidRow(
        column(6, selectInput(player_label, "Player Name:", c("", "Set Markers", load_data("data/players.csv")$Players))),
        column(3, selectInput(round_label, "Round Number:", c("", 1:3))),
        column(3, selectInput(hole_label,
                              "Choose the hole:",
                              list(`not chosen` = "", `front half` = 1:9, `back half` = 10:18),
                              width="150px"))
      ),
      
      # Submission button only appears when all fields are filled
      renderUI({
        if (is_empty(input[[tournament_label]]) || is_empty(input[[player_label]]) || 
            is_empty(input[[round_label]]) || is_empty(input[[hole_label]])) {
          return(NULL)
        } else {
          button
        }
      })
    )
  })
}

# To convert metadata into a filepath
metadata_to_filepath <- function(metadata) {
  round <- if(is.null(metadata$round)) {
    NULL
  } else {
    str_interp("Round ${metadata$round}")
  }
  hole <- if(is.null(metadata$hole)) {
    NULL
  } else {
    str_interp("hole_${metadata$hole}.csv")
  }
  folders_path <- c(
    "data",
    "shot_data",
    as.character(metadata$date),
    as.character(metadata$tournament),
    as.character(metadata$player),
    round,
    hole
  )
  paste0(folders_path, collapse="/")
}





