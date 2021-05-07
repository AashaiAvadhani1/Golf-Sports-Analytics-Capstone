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
      menuItem("Player + Tournament Entry", tabName = "Metainfo", icon = icon("golf-ball")),
      menuItem("Tournament Pin Locations", tabName = "Pin-Locations", icon = icon("map-marker-alt")),
      menuItem("Golf Map", tabName = "Golf-Map", icon = icon("dashboard")),
      menuItem("Data Compilation", tabName = "Data-Compilation", icon = icon("file-alt")),
      menuItem("Report", tabName = "Report", icon = icon("th"))
    )
  ),
  
  # Main panel for displaying outputs 
  dashboardBody(
    tabItems( 
      
      # Metainformation entry tab content
      tabItem(
        tabName = "Metainfo",
        h2("New Player/Tournament Entry", align="center"), 
        br(),
        fluidRow(
          column(12, new_player_box)
        ),
        fluidRow(
          column(12, new_tournament_box)
        )
      ),
      
      # Pin location entry tab content
      tabItem(
        tabName = "Pin-Locations",
        h2("Pin Location Input", align="center"),
        uiOutput("pin_form"),
        br(), br(),
        
        # Leaflet map
        textOutput("pin_description"),
        leafletOutput("pin_input_map", width="100%", height="500px"),
        br(),
        
        # Map interaction buttons
        uiOutput("pin_buttons")
      ),
      
      # Data entry tab content
      tabItem(
        tabName = "Golf-Map",
        h2("Data Entry App for CMU Golf Team", align="center"), 
        br(),
        uiOutput("metadata_form"),
        br(), br(),
        
        # Leaflet map
        textOutput("description"),
        leafletOutput("shot_input_map", width="100%", height="500px"),
        br(), 
        fluidRow(
          column(12, uiOutput("radio_buttons"))
        ),
        br(),
        
        # Map interaction buttons
        uiOutput("map_buttons")
      ),
      
      # Data Compilation tab content
      tabItem(
        tabName = "Data-Compilation",
        h2("Data Compilation", align="center"),
        br(),
        h3("Use the dropdowns to select the data that you want to compile:"),
        fluidRow(
          column(12, uiOutput("compile_form"))
        ),
        htmlOutput("compile_message")
      ),
      
      
      # Report tab content
      tabItem(
        tabName = "Report",
        h2("Report", align="center"),
        uiOutput("search_form"),
        dataTableOutput("click_dataframe")
      )
    )
  )
)
