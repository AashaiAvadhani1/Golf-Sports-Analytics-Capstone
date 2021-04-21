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
  title = "Add a new player",
  width = "100%",
  textInput("new_player", "Enter a new player:", ""),
  actionButton("submit_new_player", "Submit New Player")
)

new_tournament_box <- box(
  title = "Add a new tournament",
  width = "100%",
  textInput("new_tournament", "Enter a new tournament:", ""),
  lat_long_hole(1),
  lat_long_hole(2),
  lat_long_hole(3),
  lat_long_hole(4),
  lat_long_hole(5),
  lat_long_hole(6),
  lat_long_hole(7),
  lat_long_hole(8),
  lat_long_hole(9),
  lat_long_hole(10),
  lat_long_hole(11),
  lat_long_hole(12),
  lat_long_hole(13),
  lat_long_hole(14),
  lat_long_hole(15),
  lat_long_hole(16),
  lat_long_hole(17),
  lat_long_hole(18),
  actionButton("submit_new_tournament", "Submit New Tournament")
)

### Reports Tab



