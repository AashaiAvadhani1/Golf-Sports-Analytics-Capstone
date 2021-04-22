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
          column(12, new_player_box)
        ),
        fluidRow(
          column(12, new_tournament_box)
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
