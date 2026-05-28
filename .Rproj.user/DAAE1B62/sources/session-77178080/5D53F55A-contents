# ==============================================================================
# FILE: scripts/predictive_models.R
# PROJECT: AquaFever
# PURPOSE: Machine learning models for forecasting (SRS 3.2.4)
# ==============================================================================

library(randomForest)
library(forecast)

#' Train Random Forest model (SRS 3.2.4) - SIMPLIFIED VERSION
#' @param df Master dataframe with features
#' @return List containing model, metrics, and predictions
train_random_forest <- function(df) {
  # Prepare data - select predictors and target
  df_model <- df %>%
    dplyr::select(cases, rainfall_mm, rainfall_lag1, rainfall_lag2, rainfall_3mo_avg) %>%
    stats::na.omit()  # Remove rows with NA
  
  if (nrow(df_model) < 20) {
    stop("Insufficient data for Random Forest model. Need at least 20 complete records.")
  }
  
  # Set seed for reproducibility
  set.seed(42)
  
  # Simple 80/20 split
  n <- nrow(df_model)
  train_size <- floor(0.8 * n)
  train_idx <- sample(1:n, train_size)
  
  train_data <- df_model[train_idx, ]
  test_data <- df_model[-train_idx, ]
  
  # Train Random Forest (SRS 3.2.4)
  rf_model <- randomForest::randomForest(
    cases ~ .,
    data = train_data,
    ntree = 100,
    mtry = 2,  # Number of variables tried at each split
    importance = TRUE
  )
  
  # Make predictions
  predictions <- predict(rf_model, newdata = test_data)
  
  # Calculate metrics (SRS 3.2.4)
  actual <- test_data$cases
  rmse <- sqrt(mean((actual - predictions)^2))
  mae <- mean(abs(actual - predictions))
  ss_res <- sum((actual - predictions)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r_squared <- 1 - (ss_res / ss_tot)
  
  # Get variable importance
  importance_df <- as.data.frame(randomForest::importance(rf_model))
  importance_df$variable <- rownames(importance_df)
  
  return(list(
    model = rf_model,
    metrics = list(
      RMSE = round(rmse, 2),
      MAE = round(mae, 2),
      R_squared = round(r_squared, 3)
    ),
    variable_importance = importance_df,
    predictions = predictions,
    actual = actual
  ))
}

#' Train ARIMA model for a specific county (SRS 3.2.4)
#' @param df Master dataframe
#' @param county County name
#' @param forecast_horizon Number of months to forecast
#' @return List containing model and forecast
train_arima_county <- function(df, county, forecast_horizon = 6) {
  # Filter county data
  county_data <- df %>%
    dplyr::filter(county == !!county) %>%
    dplyr::arrange(date)
  
  if (nrow(county_data) < 24) {
    stop(paste("Insufficient data for ARIMA in", county, 
               ". Need at least 24 months of data."))
  }
  
  # Create time series object
  ts_cases <- ts(county_data$cases, frequency = 12)
  
  # Fit ARIMA model (SRS 3.2.4)
  arima_model <- forecast::auto.arima(
    ts_cases,
    seasonal = TRUE,
    stepwise = TRUE,
    approximation = FALSE
  )
  
  # Generate forecast
  forecast_result <- forecast::forecast(arima_model, h = forecast_horizon)
  
  # Calculate in-sample RMSE
  rmse <- sqrt(mean(residuals(arima_model)^2, na.rm = TRUE))
  
  return(list(
    model = arima_model,
    forecast = forecast_result,
    metrics = list(
      RMSE = round(rmse, 2),
      AIC = round(AIC(arima_model), 2),
      BIC = round(BIC(arima_model), 2)
    ),
    county = county
  ))
}

#' Calculate correlation between rainfall and cases (SRS 3.2.3 EDA)
#' @param df Master dataframe
#' @return Correlation statistics
calculate_correlation <- function(df) {
  # Remove NA values
  clean_df <- stats::na.omit(df[, c("rainfall_mm", "cases")])
  
  if (nrow(clean_df) < 3) {
    return(list(
      correlation = NA,
      r_squared = NA,
      p_value = NA,
      significant = FALSE
    ))
  }
  
  cor_test <- stats::cor.test(clean_df$rainfall_mm, clean_df$cases, method = "pearson")
  
  return(list(
    correlation = round(cor_test$estimate, 3),
    r_squared = round(cor_test$estimate^2, 3),
    p_value = cor_test$p.value,
    significant = cor_test$p.value < 0.05
  ))
}