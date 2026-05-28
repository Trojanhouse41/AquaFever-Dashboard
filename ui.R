# ==============================================================================
# FILE: ui.R
# PROJECT: AquaFever - Rainfall-Driven Malaria Monitoring System
# STUDENT: Sandra Adhiambo (24/05568)
# PURPOSE: User Interface - Dashboard Layout (SRS Section 3.1.1)
# ==============================================================================

# --- LOAD REQUIRED LIBRARIES (Critical for UI functions) ---
library(shiny)
library(shinydashboard)
library(shinyjs)
library(plotly)      # Required for plotlyOutput()
library(leaflet)     # Required for leafletOutput()
library(DT)          # Required for dataTableOutput()

# --- USER INTERFACE DEFINITION ---
ui <- dashboardPage(
  skin = "blue",
  
  # 1. Header (SRS 3.1.1)
  dashboardHeader(
    title = "🦟 AquaFever",
    titleWidth = 250
  ),
  
  # 2. Sidebar (SRS 3.1.1)
  dashboardSidebar(
    width = 300,
    
    sidebarMenu(
      id = "tabs",
      
      menuItem("📊 Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("📈 Forecast", tabName = "forecast", icon = icon("chart-line")),
      menuItem("🗺️ Map", tabName = "map", icon = icon("map")),
      menuItem("📋 Model Performance", tabName = "models", icon = icon("cogs")),
      menuItem("💾 Export", tabName = "export", icon = icon("download"))
    ),
    
    hr(),
    
    # File Upload Section (SRS 3.2.1)
    box(title = "📁 Data Upload", width = NULL, status = "primary", solidHeader = TRUE,
        fileInput("malaria_file", "Upload Malaria Data (CSV)",
                  accept = c(".csv", ".xlsx"),
                  placeholder = "malaria_data.csv"),
        
        fileInput("rainfall_file", "Upload Rainfall Data (CSV)",
                  accept = c(".csv", ".xlsx"),
                  placeholder = "rainfall_data.csv"),
        
        actionButton("load_data", "Load Uploaded Data", 
                     class = "btn-success", 
                     style = "width:100%; margin-top:10px;")
    ),
    
    # Analysis Controls (SRS 3.2.4)
    box(title = "⚙️ Analysis Controls", width = NULL, status = "info", solidHeader = TRUE,
        selectInput("county_select", "Select County:",
                    choices = c("All Counties", ""),
                    multiple = FALSE),
        
        numericInput("forecast_horizon", "Forecast Horizon (months):",
                     min = 1, max = 12, value = 6, step = 1),
        
        actionButton("run_analysis", "▶ Run Analysis", 
                     class = "btn-primary",
                     style = "width:100%; margin-top:10px; font-size:16px;")
    ),
    
    # Sample Data Button
    actionButton("load_sample", "📄 Load Sample Data", 
                 style = "width:100%; margin:5px;"),
    
    # Help Box
    box(title = "ℹ️ Help", width = NULL, status = "warning", solidHeader = TRUE,
        helpText("Upload CSV files with columns: county, year, month, cases (malaria) or rainfall_mm"),
        helpText("Or click 'Load Sample Data' to use example datasets.")
    )
  ),
  
  # 3. Main Body (SRS 3.1.1)
  dashboardBody(
    useShinyjs(),
    
    # Custom CSS (Optional styling)
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      # --- TAB 1: DASHBOARD (SRS 3.2.3 EDA) ---
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "📊 Data Overview", width = 12, status = "primary", solidHeader = TRUE,
                    tabsetPanel(
                      tabPanel("Summary Statistics",
                               tableOutput("summary_stats")),
                      tabPanel("Data Preview",
                               DT::dataTableOutput("data_preview")),
                      tabPanel("Correlation Analysis",
                               plotlyOutput("correlation_plot", height = "400px"))
                    )
                )
              ),
              fluidRow(
                box(title = "📈 Time Series", width = 12, status = "info", solidHeader = TRUE,
                    plotlyOutput("timeseries_plot", height = "500px")
                )
              ),
              fluidRow(
                box(title = "📊 Distribution", width = 6, status = "success", solidHeader = TRUE,
                    plotlyOutput("county_histogram", height = "400px")
                ),
                box(title = "🌧️ Rainfall Distribution", width = 6, status = "warning", solidHeader = TRUE,
                    plotlyOutput("rainfall_dist", height = "400px")
                )
              )
      ),
      
      # --- TAB 2: FORECAST (SRS 3.2.4) ---
      tabItem(tabName = "forecast",
              fluidRow(
                box(title = "📈 Malaria Forecast", width = 12, status = "primary", solidHeader = TRUE,
                    plotlyOutput("forecast_plot", height = "600px")
                )
              ),
              fluidRow(
                box(title = "📊 Forecast Summary", width = 12, status = "info", solidHeader = TRUE,
                    tableOutput("forecast_summary")
                )
              )
      ),
      
      # --- TAB 3: MAP (SRS 3.2.5) ---
      tabItem(tabName = "map",
              fluidRow(
                box(title = "🗺️ Malaria Hotspot Map", width = 12, status = "danger", solidHeader = TRUE,
                    leafletOutput("malaria_map", height = "700px")
                )
              )
      ),
      
      # --- TAB 4: MODEL PERFORMANCE (SRS 3.2.4) ---
      tabItem(tabName = "models",
              fluidRow(
                box(title = "📊 Model Performance Metrics", width = 12, status = "primary", solidHeader = TRUE,
                    tableOutput("model_metrics")
                )
              ),
              fluidRow(
                box(title = "📈 Actual vs Predicted", width = 6, status = "info", solidHeader = TRUE,
                    plotlyOutput("model_comparison", height = "400px")
                ),
                box(title = "🎯 Variable Importance", width = 6, status = "success", solidHeader = TRUE,
                    plotlyOutput("var_importance", height = "400px")
                )
              )
      ),
      
      # --- TAB 5: EXPORT (SRS 3.2.6) ---
      tabItem(tabName = "export",
              fluidRow(
                box(title = "💾 Download Results", width = 12, status = "warning", solidHeader = TRUE,
                    fluidRow(
                      column(4,
                             downloadButton("download_chart_ts", "📊 Download Time Series", 
                                            class = "btn-primary", style = "width:100%; margin:5px;")),
                      column(4,
                             downloadButton("download_chart_corr", "📈 Download Correlation", 
                                            class = "btn-info", style = "width:100%; margin:5px;")),
                      column(4,
                             downloadButton("download_data", "📑 Download Cleaned Data", 
                                            class = "btn-success", style = "width:100%; margin:5px;"))
                    ),
                    hr(),
                    fluidRow(
                      column(12,
                             downloadButton("download_report", "📄 Download Full Report (PDF)", 
                                            class = "btn-danger", style = "width:100%; margin:5px; font-size:16px;"))
                    )
                )
              )
      )
    )
  )
)