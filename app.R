library(shiny)
library(sp)
library(shinydashboard)
library(leaflet)
library(tidyverse)  
library(here)


#### Make a spatial data frame 
lats<-c(37.38,39)
lons<-c(-94,-95,-96)
df<-data.frame(cbind(lons,lats))
coordinates(df)<-~lons+lats

#### Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(
  ),
  
  # Sidebar layout with input and output definitions 
  dashboardSidebar(
  ),
  
  # Main panel for displaying outputs 
  dashboardBody(
    h2("My Map", align="center"),
    h5("Click anywhere to draw a circle", align="center"),
    leafletOutput("mymap", width="100%", height="500px"),
    actionButton("clear", "Clear Markers")
  )
)

saveData <- function(data) {
  # Create a unique file name
  fileName <- sprintf("shot_data.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = here(fileName), 
    row.names = FALSE, quote = TRUE
  )
}

loadData <- function() {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}


#df <- data.frame(matrix(NA, nrow=0, ncol=2))
dataframe_click <- data.frame(matrix(NA, nrow=0, ncol=2))
names(dataframe_click) <- c('Longitude', 'Latitude')

#### Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$mymap <- renderLeaflet({
    x=1 
    y=2
    print(x)
    
    
    m = leaflet(df,width="100%",height="100%") %>% 
      addTiles()    %>%
      addCircleMarkers() %>%
      setView(lat = 40.47942168506459, lng=-79.85795114512402, zoom=17)
  })
  
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  #initialize a dataframe
  observeEvent(input$mymap_click, {
    
    #df = data.frame(name = input$mymap_click, used_shiny = input$used_shiny)
    click <- input$mymap_click #stores the data of each click, might have to populate a dataframe for every click
    text<-paste("Latitude ", round(click$lat,2), "Longtitude ", round(click$lng,2))
    
    proxy <- leafletProxy("mymap")
    
    ## This displays the pin drop circle
    proxy %>% 
      #clearGroup("new_point") %>% #remove this line
      #clearMarkers(layerId=input$mymap_click$id) %>%
      #addPopups(click$lng, click$lat) %>%  #try uncommenting this 
      addCircles(click$lng, click$lat, radius=2, color="red", group = "new_point")
    
    marker_long = click$lng
    marker_lat = click$lat

    dataframe_click <<- dataframe_click %>% add_row(Longitude = click$lng, Latitude = click$lat)
    saveData(dataframe_click)

  })
  
  #code to clear all markers
  observeEvent(input$clear, {
    leafletProxy("mymap") %>% 
      clearGroup("new_point")
  })
  
  print(nrow(dataframe_click))
  #saveData(dataframe_click)

  
}

# Run the application 
shinyApp(ui = ui, server = server)