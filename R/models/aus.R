library(tidyverse)

# VAR model for Australia
# Returns quarterly forecasts for all endogenous variables
# Replace the stub values below with actual VAR/BVAR estimation

var_aus <- function() {

  # Forecast horizon: 8 quarters starting from 2025 Q1
  quarters <- seq(as.Date("2025-01-01"), by = "3 months", length.out = 8)

  # ---- GDP (quarter-on-quarter %, seasonally adjusted) ----
  gdp <- c(0.40, 0.42, 0.45, 0.48, 0.50, 0.52, 0.53, 0.55)

  # ---- Inflation (CPI, quarter-on-quarter %) ----
  inflation <- c(0.80, 0.72, 0.65, 0.62, 0.60, 0.58, 0.57, 0.56)

  # ---- Unemployment rate (%, level) ----
  unemployment <- c(4.1, 4.1, 4.2, 4.2, 4.3, 4.2, 4.2, 4.1)

  tibble(quarter = quarters, variable = "GDP",          forecast = gdp)          %>%
    bind_rows(tibble(quarter = quarters, variable = "Inflation",    forecast = inflation))    %>%
    bind_rows(tibble(quarter = quarters, variable = "Unemployment", forecast = unemployment))

}
