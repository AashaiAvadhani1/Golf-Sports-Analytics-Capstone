#################### Defines ui logic for golf application ####################

library(shinydashboard)
library(leaflet)
library(DT)

source("ui/ui_helpers.R")

ui <- dashboardPage(
  
  dashboardHeader(
  ),
  
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Golf-Map", tabName = "Golf-Map", icon = icon("dashboard")),
      menuItem("Information Entry", tabName = "Metainfo", icon = icon("golf-ball")),
      menuItem("Report", tabName = "Report", icon = icon("th"))
    )
  ),
  
  # Main panel for displaying outputs 
  dashboardBody(
    tabItems( 
      
      # Data entry tab content
      tabItem(
        tabName = "Golf-Map",
        # Form for metadata entry
        
        h2("Data Entry App for CMU Golf Team", align="center"), 
        br(),
        uiOutput("metadata_form"),
        br(), br(),
        
        # Leaflet map
        textOutput("description"),
        leafletOutput("mymap", width="100%", height="500px"),
        br(), 
        
        
        # Map interaction buttons
        uiOutput("map_buttons")
      ),
      
      # Metainformation entry tab content
      tabItem(
        tabName = "Metainfo",
        h2("New Metainformation Entry", align="center"), 
        br(),
        fluidRow(
          column(12, box(
            title = "Add a new player",
            width = "100%",
            textInput("new_player", "Enter a new player:", ""),
            actionButton("submit_new_player", "Submit New Player")
          ))
        ),
        fluidRow(
          column(12, box(
            title = "Add a new tournament",
            width = "100%",
            textInput("new_tournament", "Enter a new tournament:", ""),
            fluidRow(
              column(6, textInput("hole1_lat", "Hole 1 Latitude:", "")),
              column(6, textInput("hole1_lon", "Hole 1 Longitude:", ""))
            ),
            fluidRow(
              column(6, textInput("hole2_lat", "Hole 2 Latitude:", "")),
              column(6, textInput("hole2_lon", "Hole 2 Longitude:", ""))
            ),
            fluidRow(
              column(6, textInput("hole3_lat", "Hole 3 Latitude:", "")),
              column(6, textInput("hole3_lon", "Hole 3 Longitude:", ""))
            ),
            fluidRow(
              column(6, textInput("hole4_lat", "Hole 4 Latitude:", "")),
              column(6, textInput("hole4_lon", "Hole 4 Longitude:", ""))
            ),
            fluidRow(
              column(6, textInput("hole5_lat", "Hole 5 Latitude:", "")),
              column(6, textInput("hole5_lon", "Hole 5 Longitude:", ""))
            ),
            fluidRow(
              column(6, textInput("hole6_lat", "Hole 6 Latitude:", "")),
              column(6, textInput("hole6_lon", "Hole 6 Longitude:", ""))
            ),
            fluidRow(
              column(6, textInput("hole7_lat", "Hole 7 Latitude:", "")),
              column(6, textInput("hole7_lon", "Hole 7 Longitude:", ""))
            ),
            fluidRow(
              column(6, textInput("hole8_lat", "Hole 8 Latitude:", "")),
              column(6, textInput("hole8_lon", "Hole 8 Longitude:", ""))
            ),
            fluidRow(
              column(6, textInput("hole9_lat", "Hole 9 Latitude:", "")),
              column(6, textInput("hole9_lon", "Hole 9 Longitude:", ""))
            ),
            fluidRow(
              column(6, textInput("hole10_lat", "Hole 10 Latitude:", "")),
              column(6, textInput("hole10_lon", "Hole 10 Longitude:", ""))
            ),
            fluidRow(
              column(6, textInput("hole11_lat", "Hole 11 Latitude:", "")),
              column(6, textInput("hole11_lon", "Hole 11 Longitude:", ""))
            ),
            fluidRow(
              column(6, textInput("hole12_lat", "Hole 12 Latitude:", "")),
              column(6, textInput("hole12_lon", "Hole 12 Longitude:", ""))
            ),
            fluidRow(
              column(6, textInput("hole13_lat", "Hole 13 Latitude:", "")),
              column(6, textInput("hole13_lon", "Hole 13 Longitude:", ""))
            ),
            fluidRow(
              column(6, textInput("hole14_lat", "Hole 14 Latitude:", "")),
              column(6, textInput("hole14_lon", "Hole 14 Longitude:", ""))
            ),
            fluidRow(
              column(6, textInput("hole15_lat", "Hole 15 Latitude:", "")),
              column(6, textInput("hole15_lon", "Hole 15 Longitude:", ""))
            ),
            fluidRow(
              column(6, textInput("hole16_lat", "Hole 16 Latitude:", "")),
              column(6, textInput("hole16_lon", "Hole 16 Longitude:", ""))
            ),
            fluidRow(
              column(6, textInput("hole17_lat", "Hole 17 Latitude:", "")),
              column(6, textInput("hole17_lon", "Hole 17 Longitude:", ""))
            ),
            fluidRow(
              column(6, textInput("hole18_lat", "Hole 18 Latitude:", "")),
              column(6, textInput("hole18_lon", "Hole 18 Longitude:", ""))
            ),
            actionButton("submit_new_tournament", "Submit New Tournament")
          ))
        )
      ),
      
      # Report tab content
      tabItem(
        tabName = "Report",
        h2("Report", align="center"),
        dataTableOutput("click_dataframe")
      )    
    )
  )
)
