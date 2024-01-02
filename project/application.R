library(shiny)

# Load the separated UI and server files
source("global.R")
source("ui.R")
source("server.R")

# Run the Shiny app
shinyApp(ui = ui, server = server)
