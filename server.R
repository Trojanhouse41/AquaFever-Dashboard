# ==============================================================================
# FILE: server.R
# PROJECT: AquaFever - Rainfall-Driven Malaria Monitoring System
# STUDENT: Sandra Adhiambo (24/05568)
# PURPOSE: Server Logic - Data Processing & Analysis (SRS Section 3.2)
# ==============================================================================

# --- LOAD REQUIRED LIBRARIES ---
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(leaflet)
library(tidyverse)
# NOTE: caret is NOT loaded here to avoid compilation errors (SRS 3.5 Simplicity)
# We use randomForest and forecast directly as per simplified methodology

# --- SOURCE CUSTOM SCRIPTS (SRS 3.5 Maintainability) ---
source("scripts/data_processing.R")
source("scripts/predictive_models.R")
source("scripts/visualization.R")

# --- SERVER FUNCTION ---
server <- function(input, output, session) {
  
  # Reactive Values to Store Data and Results (SRS 3.5 Privacy)
  # Data exists only in memory during session
  data_values <- reactiveValues(
    malaria_raw = NULL,
    rainfall_raw = NULL,
    malaria_clean = NULL,
    rainfall_clean = NULL,
    master_data = NULL,
    rf_results = NULL,
    arima_results = NULL,
    correlation_stats = NULL,
    forecast_data = NULL
  )
  
  # ==========================================================================
  # 1. DATA INGESTION MODULE (SRS 3.2.1)
  # ==========================================================================
  
  # Load Sample Data from /data folder
  observeEvent(input$load_sample, {
    tryCatch({
      malaria_path <- file.path("data", "sample_malaria.csv")
      rainfall_path <- file.path("data", "sample_rainfall.csv")
      
      if (!file.exists(malaria_path) || !file.exists(rainfall_path)) {
        showNotification("⚠️ Sample data files not found in /data folder!", type = "error", duration = 5)
        return()
      }
      
      # Read and Clean Data (SRS 3.2.2)
      data_values$malaria_raw <- read.csv(malaria_path, stringsAsFactors = FALSE)
      data_values$rainfall_raw <- read.csv(rainfall_path, stringsAsFactors = FALSE)
      
      data_values$malaria_clean <- clean_malaria_data(data_values$malaria_raw)
      data_values$rainfall_clean <- clean_rainfall_data(data_values$rainfall_raw)
      
      # Merge Datasets (SRS 3.2.2)
      data_values$master_data <- merge_datasets(
        data_values$malaria_clean,
        data_values$rainfall_clean
      )
      
      # Create Features (SRS 3.2.4)
      data_values$master_data <- create_features(data_values$master_data)
      
      # Update County Selector
      counties <- unique(data_values$master_data$county)
      updateSelectInput(session, "county_select", 
                        choices = c("All Counties", counties))
      
      showNotification("✅ Sample data loaded successfully!", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("❌ Error loading sample:", e$message), 
                       type = "error", duration = 5)
    })
  })
  
  # Load Uploaded Data (SRS 3.2.1)
  observeEvent(input$load_data, {
    req(input$malaria_file, input$rainfall_file)
    
    tryCatch({
      # Validate and Load Malaria Data
      data_values$malaria_raw <- validate_uploaded_file(
        input$malaria_file,
        c("county", "year", "month", "cases")
      )
      
      # Validate and Load Rainfall Data
      data_values$rainfall_raw <- validate_uploaded_file(
        input$rainfall_file,
        c("county", "year", "month", "rainfall_mm")
      )
      
      # Clean Data (SRS 3.2.2)
      data_values$malaria_clean <- clean_malaria_data(data_values$malaria_raw)
      data_values$rainfall_clean <- clean_rainfall_data(data_values$rainfall_raw)
      
      # Merge Datasets (SRS 3.2.2)
      data_values$master_data <- merge_datasets(
        data_values$malaria_clean,
        data_values$rainfall_clean
      )
      
      # Create Features (SRS 3.2.4)
      data_values$master_data <- create_features(data_values$master_data)
      
      # Update County Selector
      counties <- unique(data_values$master_data$county)
      updateSelectInput(session, "county_select", 
                        choices = c("All Counties", counties))
      
      showNotification("✅ Data loaded successfully!", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("❌ Error loading data:", e$message), 
                       type = "error", duration = 5)
    })
  })
  
  # ==========================================================================
  # 2. PREDICTIVE MODELING MODULE (SRS 3.2.4)
  # ==========================================================================
  
  # Run Analysis (SRS 3.2.3, 3.2.4, 3.2.5)
  observeEvent(input$run_analysis, {
    req(data_values$master_data)
    
    withProgress(message = "Running Analysis...", value = 0, {
      tryCatch({
        # Calculate Correlation (SRS 3.2.3)
        data_values$correlation_stats <- calculate_correlation(data_values$master_data)
        incProgress(0.2, detail = "Calculating correlations...")
        
        # Train Random Forest Model (SRS 3.2.4)
        # Uses simplified randomForest package (no caret dependency)
        data_values$rf_results <- train_random_forest(data_values$master_data)
        incProgress(0.4, detail = "Training Random Forest model...")
        
        # Train ARIMA Models for Selected County (SRS 3.2.4)
        if (input$county_select != "All Counties") {
          data_values$arima_results <- train_arima_county(
            data_values$master_data,
            input$county_select,
            input$forecast_horizon
          )
          data_values$forecast_data <- data_values$arima_results$forecast
        }
        incProgress(0.3, detail = "Training ARIMA models...")
        
        incProgress(0.1, detail = "Complete!")
        
        showNotification("✅ Analysis completed successfully!", type = "message", duration = 3)
        
      }, error = function(e) {
        showNotification(paste("❌ Error during analysis:", e$message), 
                         type = "error", duration = 5)
      })
    })
  })
  
  # ==========================================================================
  # 3. VISUALIZATION & OUTPUT MODULES (SRS 3.2.3, 3.2.5)
  # ==========================================================================
  
  # Output: Summary Statistics (SRS 3.2.3)
  output$summary_stats <- renderTable({
    req(data_values$master_data)
    generate_summary_stats(data_values$master_data)
  }, digits = 2)
  
  # Output: Data Preview (SRS 3.2.3)
  output$data_preview <- DT::renderDataTable({
    req(data_values$master_data)
    datatable(
      head(data_values$master_data, 10),
      options = list(pageLength = 10),
      class = 'cell-border stripe'
    )
  })
  
  # Output: Correlation Plot (SRS 3.2.3)
  output$correlation_plot <- renderPlotly({
    req(data_values$master_data)
    create_correlation_plot(data_values$master_data)
  })
  
  # Output: Time Series Plot (SRS 3.2.5)
  output$timeseries_plot <- renderPlotly({
    req(data_values$master_data)
    county <- if (input$county_select == "All Counties") NULL else input$county_select
    create_timeseries_plot(data_values$master_data, county)
  })
  
  # Output: County Histogram (SRS 3.2.5)
  output$county_histogram <- renderPlotly({
    req(data_values$master_data)
    p <- ggplot2::ggplot(data_values$master_data, ggplot2::aes(x = county, y = cases)) +
      ggplot2::geom_bar(stat = "summary", fun = "sum", fill = "#3498DB") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Total Cases by County", x = "County", y = "Total Cases") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    plotly::ggplotly(p)
  })
  
  # Output: Rainfall Distribution (SRS 3.2.5)
  output$rainfall_dist <- renderPlotly({
    req(data_values$master_data)
    p <- ggplot2::ggplot(data_values$master_data, ggplot2::aes(x = rainfall_mm)) +
      ggplot2::geom_histogram(fill = "#27AE60", bins = 20) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Rainfall Distribution", x = "Rainfall (mm)", y = "Frequency")
    plotly::ggplotly(p)
  })
  
  # Output: Forecast Plot (SRS 3.2.4)
  output$forecast_plot <- renderPlotly({
    req(data_values$master_data)
    
    county <- if (input$county_select == "All Counties") NULL else input$county_select
    
    # Get forecast if available
    forecast_obj <- NULL
    if (!is.null(data_values$arima_results) && !is.null(county)) {
      forecast_obj <- data_values$arima_results$forecast
    }
    
    create_timeseries_plot(data_values$master_data, county)
  })
  
  # Output: Forecast Summary
  output$forecast_summary <- renderTable({
    req(data_values$arima_results)
    
    result <- data_values$arima_results
    data.frame(
      Metric = c("County", "Model", "RMSE", "AIC", "BIC"),
      Value = c(
        result$county,
        "ARIMA",
        as.character(result$metrics$RMSE),
        as.character(result$metrics$AIC),
        as.character(result$metrics$BIC)
      ),
      stringsAsFactors = FALSE
    )
  })
  
  # Output: Malaria Map (SRS 3.2.5)
  output$malaria_map <- renderLeaflet({
    req(data_values$master_data)
    
    # Check if shapefile exists (SRS 3.4 Design Constraints)
    shapefile_path <- "www/maps/kenya_counties.shp"
    
    create_malaria_map(data_values$master_data, shapefile_path)
  })
  
  # Output: Model Metrics (SRS 3.2.4)
  output$model_metrics <- renderTable({
    req(data_values$rf_results)
    
    data.frame(
      Model = "Random Forest",
      Metric = c("RMSE", "MAE", "R-squared"),
      Value = c(
        data_values$rf_results$metrics$RMSE,
        data_values$rf_results$metrics$MAE,
        data_values$rf_results$metrics$R_squared
      ),
      stringsAsFactors = FALSE
    )
  })
  
  # Output: Model Comparison (SRS 3.2.4)
  output$model_comparison <- renderPlotly({
    req(data_values$rf_results)
    comparison <- data.frame(
      Actual = data_values$rf_results$actual,
      Predicted = data_values$rf_results$predictions
    )
    p <- ggplot2::ggplot(comparison, ggplot2::aes(x = Actual, y = Predicted)) +
      ggplot2::geom_point(alpha = 0.6, color = "#8E44AD", size = 3) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "#E74C3C", linetype = "dashed") +
      ggplot2::labs(title = "Actual vs Predicted", 
                    subtitle = paste0("R² = ", data_values$rf_results$metrics$R_squared)) +
      ggplot2::theme_minimal()
    plotly::ggplotly(p)
  })
  
  # Output: Variable Importance (SRS 3.2.4)
  output$var_importance <- renderPlotly({
    req(data_values$rf_results)
    
    # Extract variable importance from RF model
    if (!is.null(data_values$rf_results$variable_importance)) {
      var_imp <- data_values$rf_results$variable_importance
      p <- ggplot2::ggplot(var_imp, ggplot2::aes(x = reorder(variable, IncMSE), y = IncMSE)) +
        ggplot2::geom_bar(stat = "identity", fill = "#3498DB") +
        ggplot2::coord_flip() +
        ggplot2::labs(title = "Variable Importance", x = "Variable", y = "% Inc MSE") +
        ggplot2::theme_minimal()
      plotly::ggplotly(p)
    } else {
      # Fallback if importance not available
      plotly::plot_ly() %>% 
        plotly::add_text(text = "Variable importance not available", x = 0, y = 0)
    }
  })
  
  # ==========================================================================
  # 4. EXPORT & REPORTING MODULE (SRS 3.2.6)
  # ==========================================================================
  
  # Download Cleaned Data (SRS 3.2.6)
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("AquaFever_CleanedData_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(data_values$master_data)
      write.csv(data_values$master_data, file, row.names = FALSE)
    }
  )
  
  # Download Time Series Chart (SRS 3.2.6)
  output$download_chart_ts <- downloadHandler(
    filename = function() {
      paste0("AquaFever_TimeSeries_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(data_values$master_data)
      # Render static plot for download
      p <- ggplot2::ggplot(data_values$master_data, ggplot2::aes(x = date, y = cases)) +
        ggplot2::geom_line() + ggplot2::theme_minimal()
      ggplot2::ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  # Download Correlation Chart (SRS 3.2.6)
  output$download_chart_corr <- downloadHandler(
    filename = function() {
      paste0("AquaFever_Correlation_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(data_values$master_data)
      p <- ggplot2::ggplot(data_values$master_data, ggplot2::aes(x = rainfall_mm, y = cases)) +
        ggplot2::geom_point() + ggplot2::theme_minimal()
      ggplot2::ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  # Download Report (SRS 3.2.6)
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("AquaFever_Report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Simple text report for now (SRS 3.2.6)
      # Can be expanded with rmarkdown later
      lines <- c(
        "AquaFever Analysis Report",
        paste("Date:", Sys.Date()),
        "",
        "Summary:",
        paste("Counties Analyzed:", paste(unique(data_values$master_data$county), collapse = ", ")),
        paste("Total Records:", nrow(data_values$master_data)),
        "",
        "Model Performance:",
        paste("Random Forest R²:", data_values$rf_results$metrics$R_squared),
        paste("Random Forest RMSE:", data_values$rf_results$metrics$RMSE)
      )
      writeLines(lines, con = file)
    }
  )
}