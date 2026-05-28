# ==============================================================================
# FILE: scripts/data_processing.R
# PROJECT: AquaFever
# PURPOSE: Data ingestion, validation, cleaning, and merging (SRS 3.2.1-3.2.2)
# ==============================================================================

library(tidyverse)

#' Validate uploaded file
#' @param file_input Shiny fileInput object
#' @param expected_columns Character vector of required column names
#' @return Validated dataframe or error
validate_uploaded_file <- function(file_input, expected_columns) {
  if (is.null(file_input$datapath)) {
    stop("No file uploaded")
  }
  
  # Check file extension (SRS 3.1.2)
  ext <- tolower(tools::file_ext(file_input$name))
  if (!ext %in% c("csv", "xlsx")) {
    stop("Invalid file type. Please upload CSV or Excel file only.")
  }
  
  # Read file
  df <- if (ext == "csv") {
    read.csv(file_input$datapath, stringsAsFactors = FALSE)
  } else {
    readxl::read_excel(file_input$datapath)
  }
  
  # Check required columns (SRS 3.1.2)
  missing_cols <- setdiff(expected_columns, colnames(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  return(df)
}

#' Clean malaria data (SRS 3.2.2)
#' @param df_raw Raw malaria dataframe
#' @return Cleaned dataframe
clean_malaria_data <- function(df_raw) {
  df_clean <- df_raw %>%
    # Remove duplicates
    distinct() %>%
    # Convert county to title case and trim whitespace
    mutate(county = str_to_title(str_trim(county))) %>%
    # Create date column from year and month
    mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
    # Ensure cases is numeric and non-negative
    mutate(cases = as.numeric(cases)) %>%
    filter(!is.na(cases), cases >= 0) %>%
    # Remove rows with missing critical values (SRS 3.2.2)
    drop_na(county, year, month, cases) %>%
    # Select and order columns
    select(county, year, month, date, cases)
  
  return(df_clean)
}

#' Clean rainfall data (SRS 3.2.2)
#' @param df_raw Raw rainfall dataframe
#' @return Cleaned dataframe
clean_rainfall_data <- function(df_raw) {
  df_clean <- df_raw %>%
    distinct() %>%
    mutate(county = str_to_title(str_trim(county))) %>%
    mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
    mutate(rainfall_mm = as.numeric(rainfall_mm)) %>%
    filter(!is.na(rainfall_mm), rainfall_mm >= 0) %>%
    drop_na(county, year, month, rainfall_mm) %>%
    select(county, year, month, date, rainfall_mm)
  
  return(df_clean)
}

#' Merge malaria and rainfall datasets (SRS 3.2.2)
#' @param df_malaria Cleaned malaria data
#' @param df_rainfall Cleaned rainfall data
#' @return Merged dataframe
merge_datasets <- function(df_malaria, df_rainfall) {
  df_master <- df_malaria %>%
    full_join(df_rainfall, by = c("county", "date")) %>%
    arrange(county, date)
  
  return(df_master)
}

#' Create features for modeling (SRS 3.2.4)
#' @param df Master dataframe
#' @return Dataframe with engineered features
create_features <- function(df) {
  df <- df %>%
    group_by(county) %>%
    mutate(
      # Lag variables for rainfall (SRS 3.2.4)
      rainfall_lag1 = lag(rainfall_mm, 1),
      rainfall_lag2 = lag(rainfall_mm, 2),
      rainfall_lag3 = lag(rainfall_mm, 3),
      
      # Rolling averages
      rainfall_3mo_avg = zoo::rollapply(rainfall_mm, 
                                        width = 3, 
                                        FUN = mean, 
                                        fill = NA, 
                                        align = "right"),
      
      # Month as factor for seasonality
      month_factor = factor(month, levels = 1:12, labels = month.abb)
    ) %>%
    ungroup()
  
  return(df)
}

#' Generate summary statistics (SRS 3.2.3 EDA)
#' @param df Master dataframe
#' @return Summary statistics dataframe
generate_summary_stats <- function(df) {
  summary_df <- df %>%
    group_by(county) %>%
    summarise(
      total_cases = sum(cases, na.rm = TRUE),
      mean_cases = mean(cases, na.rm = TRUE),
      max_cases = max(cases, na.rm = TRUE),
      min_cases = min(cases, na.rm = TRUE),
      mean_rainfall = mean(rainfall_mm, na.rm = TRUE),
      total_months = n(),
      .groups = "drop"
    )
  
  return(summary_df)
}