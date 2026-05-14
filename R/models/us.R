library(tidyverse)

# VAR model for the United States
# Returns quarterly forecasts for all endogenous variables
# Replace the stub values below with actual VAR/BVAR estimation

var_us <- function() {

  # Forecast horizon: 8 quarters starting from 2025 Q1
  quarters <- seq(as.Date("2025-01-01"), by = "3 months", length.out = 8)

  # ---- GDP (quarter-on-quarter %, seasonally adjusted) ----
  gdp <- c(0.55, 0.50, 0.48, 0.45, 0.52, 0.55, 0.58, 0.60)

  # ---- Inflation (CPI, quarter-on-quarter %) ----
  inflation <- c(0.70, 0.65, 0.62, 0.60, 0.58, 0.58, 0.57, 0.56)

  # ---- Unemployment rate (%, level) ----
  unemployment <- c(4.2, 4.3, 4.3, 4.4, 4.4, 4.5, 4.4, 4.3)

  tibble(quarter = quarters, variable = "GDP",          forecast = gdp)          %>%
    bind_rows(tibble(quarter = quarters, variable = "Inflation",    forecast = inflation))    %>%
    bind_rows(tibble(quarter = quarters, variable = "Unemployment", forecast = unemployment))

}
