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
saveData <- function(data, filename="shot_data.csv") {
  full_filepath <- paste("server/data", filename, sep="/")
  # Create a unique file name
  fileName <- sprintf(full_filepath, as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = here(fileName), 
    row.names = FALSE, quote = TRUE
  )
}

# Loads data from files (currently not being used)
loadData <- function(output_dir) {
  # Read all the files into a list
  files <- list.files(output_dir, full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
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