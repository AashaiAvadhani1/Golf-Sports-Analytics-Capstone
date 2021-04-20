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
save_data <- function(data, folder="data", filename="shot_data.csv") {
  full_filepath <- paste(c(folder, filename), collapse="/")
  # Create a unique file name
  filepath <- here(sprintf(full_filepath, as.integer(Sys.time()), digest::digest(data)))
  if(!file.exists(filepath)) {
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
  if(!file.exists(filepath)) {
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