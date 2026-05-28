# ==============================================================================
# FILE: scripts/visualization.R
# PROJECT: AquaFever - Rainfall-Driven Malaria Monitoring System
# STUDENT: Sandra Adhiambo (24/05568)
# PURPOSE: Interactive Visualizations (SRS Section 3.2.5)
# ==============================================================================

library(ggplot2)
library(plotly)
library(leaflet)
library(tidyverse)

#' Create interactive time series plot (SRS 3.2.5)
#' @param df Master dataframe
#' @param county Selected county (NULL for all)
#' @return Interactive plotly object
create_timeseries_plot <- function(df, county = NULL) {
  # Filter data
  plot_data <- if (!is.null(county) && county != "All Counties") {
    df %>% filter(county == county)
  } else {
    df
  }
  
  # Create base plot
  p <- ggplot(plot_data, aes(x = date, y = cases)) +
    geom_line(color = "#2C3E50", size = 1) +
    geom_point(color = "#3498DB", size = 2, alpha = 0.6) +
    labs(
      title = "Malaria Cases Over Time",
      subtitle = if (!is.null(county) && county != "All Counties") county else "All Counties",
      x = "Date",
      y = "Confirmed Cases"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Convert to interactive (SRS 3.2.5)
  ggplotly(p, tooltip = c("x", "y")) %>%
    layout(hovermode = "x unified")
}

#' Create correlation scatter plot (SRS 3.2.3 EDA)
#' @param df Master dataframe
#' @return Interactive plotly object
create_correlation_plot <- function(df) {
  # Remove NA values for correlation
  clean_df <- df %>% select(rainfall_mm, cases) %>% drop_na()
  
  if (nrow(clean_df) < 3) {
    return(plotly::plot_ly() %>% plotly::add_text(text = "Insufficient data for correlation"))
  }
  
  # Calculate correlation
  cor_val <- cor(clean_df$rainfall_mm, clean_df$cases, method = "pearson")
  r_squared <- cor_val^2
  
  p <- ggplot(clean_df, aes(x = rainfall_mm, y = cases)) +
    geom_point(alpha = 0.6, color = "#27AE60", size = 3) +
    geom_smooth(method = "lm", se = TRUE, color = "#C0392B", fill = "#E74C3C") +
    labs(
      title = "Rainfall vs Malaria Cases",
      subtitle = paste0("Correlation (R²) = ", round(r_squared, 3)),
      x = "Monthly Rainfall (mm)",
      y = "Malaria Cases"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11)
    )
  
  ggplotly(p, tooltip = c("x", "y"))
}

#' Create malaria map - FALLBACK VERSION (No shapefile required)
#' This ensures the app works immediately without external GIS files
#' @param df Master dataframe
#' @param shapefile_path Path to county shapefile (not used in fallback)
#' @return Leaflet map object
create_malaria_map <- function(df, shapefile_path) {
  
  # Prepare data by county (SRS 3.2.5)
  plot_data <- df %>%
    group_by(county) %>%
    summarise(
      avg_cases = mean(cases, na.rm = TRUE),
      total_cases = sum(cases, na.rm = TRUE),
      mean_rainfall = mean(rainfall_mm, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Coordinates for the 4 target counties (Proposal Section 1)
  # Kisumu, Homa Bay, Migori (Lake Victoria Basin) + Nairobi (Highland)
  county_coords <- data.frame(
    county = c("Kisumu", "Homa Bay", "Migori", "Nairobi"),
    lng = c(34.75, 34.55, 34.45, 36.82),
    lat = c(-0.10, -0.55, -1.05, -1.29)
  )
  
  # Join data with coordinates
  map_data <- county_coords %>%
    left_join(plot_data, by = "county")
  
  # Handle missing data (if a county has no data)
  map_data$avg_cases[is.na(map_data$avg_cases)] <- 0
  map_data$total_cases[is.na(map_data$total_cases)] <- 0
  
  # Color coding based on transmission level (Proposal Section 1)
  # Red = High (>200), Orange = Moderate (100-200), Green = Low (<100)
  map_data <- map_data %>%
    mutate(
      color = case_when(
        avg_cases > 200 ~ "red",
        avg_cases > 100 ~ "orange",
        TRUE ~ "green"
      ),
      risk_level = case_when(
        avg_cases > 200 ~ "High",
        avg_cases > 100 ~ "Moderate",
        TRUE ~ "Low"
      )
    )
  
  # Create interactive map (SRS 3.2.5)
  leaflet(map_data) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers(
      lng = ~lng,
      lat = ~lat,
      radius = ~ifelse(avg_cases > 200, 15, ifelse(avg_cases > 100, 12, 10)),
      color = ~color,
      fillColor = ~color,
      fillOpacity = 0.7,
      weight = 1,
      label = ~paste0(
        "<strong>", county, "</strong><br/>",
        "Risk Level: ", risk_level, "<br/>",
        "Avg Cases: ", round(avg_cases, 1), "<br/>",
        "Total Cases: ", total_cases, "<br/>",
        "Mean Rainfall: ", round(mean_rainfall, 1), " mm"
      ),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "12px",
        direction = "auto"
      )
    ) %>%
    addLegend(
      "bottomright",
      colors = c("red", "orange", "green"),
      labels = c("High (>200)", "Moderate (100-200)", "Low (<100)"),
      title = "Malaria Risk Level"
    ) %>%
    setView(lng = 35.5, lat = -0.5, zoom = 7)
}

#' Create model performance visualization (SRS 3.2.4)
#' @param rf_results Random Forest results
#' @return Plotly object
create_model_performance_plot <- function(rf_results) {
  # Create comparison dataframe
  comparison <- data.frame(
    Actual = rf_results$actual,
    Predicted = rf_results$predictions
  )
  
  p <- ggplot(comparison, aes(x = Actual, y = Predicted)) +
    geom_point(alpha = 0.6, color = "#8E44AD", size = 3) +
    geom_abline(slope = 1, intercept = 0, color = "#E74C3C", linetype = "dashed") +
    labs(
      title = "Model Performance: Actual vs Predicted",
      subtitle = paste0("R² = ", rf_results$metrics$R_squared),
      x = "Actual Cases",
      y = "Predicted Cases"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11)
    )
  
  ggplotly(p)
}