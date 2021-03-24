##################  Custom function library for server logic ###################

library(tidyverse)
library(here)

### Initialization

# Make a spatial data frame for initialization purposes
initialize_spatial <- function() {
  lats <- c(37.38,39)
  lons <- c(-94,-95,-96)
  df <- data.frame(cbind(lons,lats))
  coordinates(df) <-~ lons+lats
  df
}

# Initialize the click dataframe
initialize_click_dataframe <- function() {
  dataframe_click <- data.frame(matrix(NA, nrow=0, ncol=3))
  names(dataframe_click) <- c('Longitude', 'Latitude', 'ShotId')
  dataframe_click
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

