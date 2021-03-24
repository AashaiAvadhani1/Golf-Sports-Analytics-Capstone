library(shiny)

source('ui/ui.R', local = TRUE)
source('server/server.R')

# Run the application 
shinyApp(ui = ui, server = server)