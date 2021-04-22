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
initialize_click_dataframe <- function(col_names = c('Longitude', 'Latitude')) {
  dataframe_click <- data.frame(matrix(NA, nrow=0, ncol=length(col_names)))
  names(dataframe_click) <- col_names
  dataframe_click %>% mutate_all(as.character)
}


### For updating the click dataframe

# To add a new shot
add_shot <- function(df, params) {
  df %>% {do.call(add_row, c(list(`.data` = .), lapply(params, as.character)))}
}

# To update a shot (after dragging)
update_shot <- function(df, update, cols_to_update) {
  cols_to_identify <- setdiff(colnames(update), cols_to_update)
  df %>% 
    rows_update(update %>% mutate_all(as.character), by=cols_to_identify, copy=TRUE)
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
load_data <- function(path, headers=TRUE) {
  filepath <- here(sprintf(path, as.integer(Sys.time()), digest::digest(data)))
  if (!file.exists(filepath)) {
    data.frame()
  } else {
    read_csv(filepath, col_names=headers)
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
        column(3, dateInput(date_label, "Date:", value = Sys.Date(), width="100px")),
        column(9, selectInput(tournament_label, "Tournament name:", c("", load_data("data/tournaments.csv")$Tournaments), width="70%"))
      ),
      fluidRow(
        column(6, selectInput(player_label, "Player name:", c("", "Set Markers", load_data("data/players.csv")$Players))),
        column(3, selectInput(round_label, "Round Number:", c("", "NA (Setting markers)", 1:3))),
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