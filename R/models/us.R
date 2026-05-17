library(tidyverse)

# VAR model for the United States
# Returns quarterly forecasts for all endogenous variables
# Replace the stub values below with actual VAR/BVAR estimation

var_us <- function() {

  # Forecast horizon: 8 quarters starting from current quarter
  start_q  <- lubridate::floor_date(Sys.Date(), "quarter")
  quarters <- seq(start_q, by = "3 months", length.out = 8)

  # ---- GDP (QoQ %, SA) — modest recovery path ----
  gdp <- c(0.42, 0.45, 0.48, 0.50, 0.52, 0.53, 0.55, 0.55)

  # ---- Inflation (CPI QoQ %) — gradual disinflation toward target ----
  inflation <- c(0.68, 0.63, 0.60, 0.58, 0.57, 0.56, 0.55, 0.55)

  # ---- Unemployment rate (%, level) — mild drift up then stable ----
  unemployment <- c(4.2, 4.3, 4.4, 4.4, 4.4, 4.3, 4.3, 4.2)

  tibble(quarter = quarters, variable = "GDP",          forecast = gdp)          %>%
    bind_rows(tibble(quarter = quarters, variable = "Inflation",    forecast = inflation))    %>%
    bind_rows(tibble(quarter = quarters, variable = "Unemployment", forecast = unemployment))

}
