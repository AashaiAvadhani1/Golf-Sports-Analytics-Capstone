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
        fluidRow(
          column(3, dateInput("date", "Date:", value = Sys.Date(), width="100px")),
          column(9, textInput("tournament", "Tournament Name:", "", width="70%"))
        ),
        fluidRow(
          column(6, selectInput("player", "Player name:", c("", "Set Markers", "p1", "p2", "p3", "p4", "p5"))),
          column(3, selectInput("round", "Round Number:", c("", "NA (Setting markers)", 1:3))),
          column(3, selectInput("hole",
                                "Choose the hole:",
                                list(`not chosen` = "", `front half` = 1:9, `back half` = 10:18),
                                width="150px"))
        ),
        
        uiOutput("metadata_submission"),
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
            textInput("new_player", "Enter a new player:", ""),
            actionButton("submit_new_player", "Submit New Player")
          ))
        ),
        fluidRow(
          column(12, box(
            title = "Add a new tournament",
            textInput("new_tournament", "Enter a new tournament:", ""),
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
