library(tidyverse)

# VAR model for Australia
# Returns quarterly forecasts for all endogenous variables
# Replace the stub values below with actual VAR/BVAR estimation

var_aus <- function() {

  # Forecast horizon: 8 quarters starting from current quarter
  start_q  <- lubridate::floor_date(Sys.Date(), "quarter")
  quarters <- seq(start_q, by = "3 months", length.out = 8)

  # ---- GDP (QoQ %, SA) — subdued near-term, gradual pickup ----
  gdp <- c(0.35, 0.38, 0.42, 0.45, 0.48, 0.50, 0.52, 0.53)

  # ---- Inflation (CPI QoQ %) — returning toward 2-3% band ----
  inflation <- c(0.72, 0.65, 0.60, 0.58, 0.57, 0.56, 0.55, 0.55)

  # ---- Unemployment rate (%, level) — modest rise then stabilise ----
  unemployment <- c(4.2, 4.3, 4.3, 4.4, 4.4, 4.3, 4.3, 4.2)

  tibble(quarter = quarters, variable = "GDP",          forecast = gdp)          %>%
    bind_rows(tibble(quarter = quarters, variable = "Inflation",    forecast = inflation))    %>%
    bind_rows(tibble(quarter = quarters, variable = "Unemployment", forecast = unemployment))

}
