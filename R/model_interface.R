library(tidyverse)

# Runs the VAR model for a given country and returns validated forecasts.
# Args:
#   country: string matching a key in models_config (e.g. "US", "Australia")
# Returns:
#   tibble with columns: quarter (Date), variable (chr), forecast (dbl)

run_forecast <- function(country) {

  if (!country %in% names(models_config)) {
    stop(sprintf("Country '%s' not found in models_config. Available: %s",
                 country, paste(names(models_config), collapse = ", ")))
  }

  model_fn <- models_config[[country]]
  result   <- model_fn()

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

# Convenience wrapper: run all countries and return a combined tibble
run_all_forecasts <- function() {
  names(models_config) %>%
    map_dfr(~ run_forecast(.x) %>% mutate(country = .x))
}
