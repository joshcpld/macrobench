library(tidyverse)

# Runs the VAR model for a given country and returns validated forecasts.
# Args:
#   country: string matching a key in models_config (e.g. "US", "Australia")
# Returns:
#   tibble with columns: quarter (Date), variable (chr), forecast (dbl)

# last_history_date: the last observed quarter for this country (Date).
# Passed to the model function so it knows where to start forecasting.
# Stubs can ignore it; real VAR models should use it as the forecast origin.
run_forecast <- function(country, last_history_date = NULL) {

  if (!country %in% names(models_config)) {
    stop(sprintf("Country '%s' not found in models_config. Available: %s",
                 country, paste(names(models_config), collapse = ", ")))
  }

  model_fn <- models_config[[country]]

  # Call model with last_history_date if the function accepts it
  result <- if ("last_history_date" %in% names(formals(model_fn))) {
    model_fn(last_history_date = last_history_date)
  } else {
    model_fn()
  }

  # ---- Validate output contract ----
  required_cols <- c("quarter", "variable", "forecast")
  missing_cols  <- setdiff(required_cols, names(result))

  if (length(missing_cols) > 0) {
    stop(sprintf("Model for '%s' is missing columns: %s",
                 country, paste(missing_cols, collapse = ", ")))
  }

  if (!inherits(result$quarter, "Date")) {
    stop(sprintf("Model for '%s': 'quarter' column must be of type Date."))
  }

  if (!is.numeric(result$forecast)) {
    stop(sprintf("Model for '%s': 'forecast' column must be numeric."))
  }

  result %>%
    select(quarter, variable, forecast) %>%
    arrange(variable, quarter)

}

# Convenience wrapper: run all countries and return a combined tibble.
# history_df: optional tibble(country, variable, quarter, value) used to
# derive last_history_date per country for real VAR models.
run_all_forecasts <- function(history_df = NULL) {
  names(models_config) %>%
    map_dfr(function(co) {
      last_date <- if (!is.null(history_df)) {
        history_df %>%
          dplyr::filter(country == co) %>%
          pull(quarter) %>%
          max(na.rm = TRUE)
      } else NULL

      run_forecast(co, last_history_date = last_date) %>% mutate(country = co)
    })
}
