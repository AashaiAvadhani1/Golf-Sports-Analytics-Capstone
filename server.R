################## Defines server logic for golf application ##################

library(shiny)
library(sp)
library(tidyverse)  
library(leaflet)
library(leaflet.extras)
library(oce)
library(raster)
library(grid)

source("server/server_helpers.R")

# This is the dataframe in which data from each click is stored
dataframe_column_names <- c(
  "Date",
  "Tournament Name",
  "Round",
  "Player",
  "Hole",
  "Shot",
  "Latitude",
  "Longitude"
)
click_dataframe <- initialize_click_dataframe(dataframe_column_names)
shot_num <- 0

server <- function(input, output) {
  # Submission button only appears when all fields are filled
  output$metadata_submission <- renderUI({
    if (is_empty(input$tournament) || is_empty(input$player) || 
        is_empty(input$round) || is_empty(input$hole)) {
      return(NULL)
    } else {
      actionButton("submit_meta", "Submit Metadata")
    }
  })
  
  
  
  # When "submit-metadata" button is clicked
  observeEvent(input$submit_meta, {
    output$description <- renderText("Click anywhere to draw a circle")
    data <- reactiveValues(lat = NULL, lon = NULL)

    
    my_circle <- circleGrob(x = 0.5, y = 0.5, r = 0.5,
                            gp = gpar(col = "gray", lty = 3))
    output$mymap <- renderLeaflet({
      m = leaflet(initialize_spatial(), width="100%", height="100%") %>%
      
        
        addDrawToolbar(circleOptions=NA, markerOptions= NA,
                       rectangleOptions= markerOptions(draggable = TRUE), polylineOptions=NA, circleMarkerOptions=NA,
                       polygonOptions = drawPolygonOptions(showArea = TRUE, shapeOptions = drawShapeOptions(fill = FALSE))) %>%
        
        #addTiles() %>%
        #addGraticule(interval = 1) %>%
        setView(lat = 40.47942168506459, lng=-79.85795114512402, zoom=17) %>%
        
        Sr1 = Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2)))
        Sr2 = Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)))
        Sr3 = Polygon(cbind(c(4, 4, 5, 10, 4), c(5, 3, 2, 5, 5)))
        Sr4 = Polygon(cbind(c(5, 6, 6, 5, 5), c(4, 4, 3, 3, 4)), hole = TRUE)
        Srs1 = Polygons(list(Sr1), "s1")
        Srs2 = Polygons(list(Sr2), "s2")
        Srs3 = Polygons(list(Sr4, Sr3), "s3/4")
        SpP = SpatialPolygons(list(Srs1, Srs2, Srs3), 1:3)
        leaflet(height = "300px") %>% addPolygons(data = SpP)
        
        
        
      #m %>% addTiles() %>% addPolygons(opacity = 1,lat = 40.47942168506459, lng=-79.85795114512402,fillOpacity = 0.5, smoothFactor = 0.5, color="black",weight = 0.5)
      #get the 4 coordinates from the polygon
      #anchor has to be the center of the green, lower point of the green
    
    
    output$map_buttons <- renderUI({
      fluidRow(
        column(2, actionButton("clear", "Clear Markers")),
        column(10, actionButton("submit", "Submit Markers")), 
        column(20, actionButton("gridLocation", "Submit Grid Location"))
      )
    })
    })
  }
  )
    
  metadata <- eventReactive(input$submit_meta, {
    data <- list()
    data$date <- input$date
    data$tournament <- input$tournament
    data$round <- input$round
    data$player <- input$player
    data$hole <- input$hole
    data
  })
  
  
  #have a diffrent obeserve event for the grid markers ( )
  
  
  # Clicking to add a marker
  #make user input the fairway crosss to the green
  #
  observeEvent(input$mymap_click, {
  
    click <- input$mymap_click
    shot_num <<- shot_num + 1
    
    temp <- input$leafmap_draw_all_features 
    #print(click)

    #using the layerID, we can manually plot the lines using the markers 
    #have a new dropdown for the grid, "set green markers" and save those points of the green 
    #make a different toolbar for setting the green markers for the grid
    leafletProxy("mymap") %>%
      addCircleMarkers(click$lng, click$lat, radius=4, color="black", group="new_point",
                     layerId=shot_num, options=markerOptions(draggable = TRUE)) %>%    addTiles()
      #addGraticule()
    
    click_dataframe <<- click_dataframe %>% 
      add_shot(list(
        Date = metadata()$date,
        `Tournament Name` = metadata()$tournament,
        Round = metadata()$round,
        Player = metadata()$player,
        Hole = metadata()$hole,
        Shot = shot_num,
        Latitude = click$lat,
        Longitude = click$lng
      ))
  })
  
  
  observeEvent(input$map_draw_new_feature, {
    click_lat <- input$map_draw_new_feature$geometry$coordinates[[2]]
    click_lon <- input$map_draw_new_feature$geometry$coordinates[[1]]
    data$lat <- c(data$lat,click_lat)
    data$lon <- c(data$lon,click_lon)
  })
  
  
  # Clicking to add a grid
 # observeEvent(input$mymap_click, {
    
  #  grd <- st_sf(geom=st_make_grid(qk_sf), crs=4326)
    
   # leafletProxy("mymap") %>%
    #  addTiles() %>% 
     # addGraticule()
    
    #sfc = st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,0)))))
    #plot(st_make_grid(sfc, cellsize = .1, square = FALSE))
    
    #p <- as(r, 'SpatialPolygonsDataFrame')
    #r <- disaggregate(r, 2)
    #values(r) <- 1:ncell(r)
    #plot(r)
    #plot(p, add=TRUE)
    
   # }
  #)
  
  
  
  
  # Observe event for dragging markers after initializing them
  observeEvent(input$mymap_marker_dragend, {
    drag <- input$mymap_marker_dragend
    
    update <- tibble(
      Date = metadata()$date,
      `Tournament Name` = metadata()$tournament,
      Round = metadata()$round,
      Player = metadata()$player,
      Hole = metadata()$hole,
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
  })
  
  # When "submit" button is clicked
  observeEvent(input$submit, {
    saveData(click_dataframe)
  })
}