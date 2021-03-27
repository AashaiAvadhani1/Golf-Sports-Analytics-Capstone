#################### Defines ui logic for golf application ####################

library(shinydashboard)
library(leaflet)

source("ui/ui_helpers.R")

ui <- dashboardPage(
  
  dashboardHeader(
  ),
  
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  
  # Main panel for displaying outputs 
  dashboardBody(
    tabItems( 
      # First tab content
      tabItem(tabName = "dashboard",
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
      
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Report"),
              DT::dataTableOutput("mytable")
      )    
    )


  )
)
