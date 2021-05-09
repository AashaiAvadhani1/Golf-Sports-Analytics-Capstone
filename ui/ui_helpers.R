####################  Custom function library for server ######################

library(tidyverse)

### Data Entry Tab (Main Page) 



### Metainformation Entry Tab

lat_long_hole <- function(hole_num) {
  fluidRow(
    column(6, textInput(str_interp("hole${hole_num}_lat"), str_interp("Hole ${hole_num} Latitude:"), "")),
    column(6, textInput(str_interp("hole${hole_num}_lon"), str_interp("Hole ${hole_num} Longitude:"), ""))
  )
}

new_player_box <- box(
  title = "Add New Player",
  width = "100%",
  textInput("new_player", "Enter New Player:", ""),
  actionButton("submit_new_player", "Submit New Player")
)

new_tournament_box <- box(
  title = "Add New Tournament",
  width = "100%",
  textInput("new_tournament", "Enter New Tournament:", ""),
  lapply(1:18, lat_long_hole),
  actionButton("submit_new_tournament", "Submit New Tournament")
)

### Reports Tab



### Data Complilation Tab



