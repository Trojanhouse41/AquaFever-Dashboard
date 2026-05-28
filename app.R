# ==============================================================================
# FILE: app.R
# PROJECT: AquaFever
# PURPOSE: Main Application Entry Point
# ==============================================================================

# Load required libraries
library(shiny)
library(shinydashboard)

# Run the application
shinyApp(
  ui = ui,
  server = server
)