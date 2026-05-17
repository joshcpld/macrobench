library(tidyverse)
library(lubridate)
library(fredr)
library(readabs)

# ---- Series registry ----
# All series stored as levels. Transformations (QoQ, YoY, etc.) applied in app.
#
# US — all via FRED:
#   GDPC1:          Real GDP, chained 2017 $B, quarterly SA
#   CPIAUCSL:       CPI all urban consumers, index 1982-84=100, monthly SA
#   UNRATE:         Unemployment rate %, monthly SA
#
# Australia — all via ABS:
#   A2304402X:  GDP chain volume measure, $M, quarterly SA  (ABS 5206.0)
#   A2325846C:  CPI all groups price index, quarterly        (ABS 6401.0)
#   A84423050A: Unemployment rate %, monthly SA              (ABS 6202.0)

us_fred_series <- list(
  GDP          = "GDPC1",
  Inflation    = "CPIAUCSL",
  Unemployment = "UNRATE"
)

aus_abs_series <- list(
  GDP          = "A2304402X",
  Inflation    = "A2325846C",
  Unemployment = "A84423050A"
)

# ---- Helper: monthly -> quarterly average ----
to_quarterly <- function(df) {
  df %>%
    mutate(quarter = floor_date(date, "quarter")) %>%
    group_by(quarter) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    rename(date = quarter)
}

# ---- Country fetchers ----

fetch_history_us <- function(start_date) {

  api_key <- Sys.getenv("FRED_API_KEY")
  if (nchar(api_key) > 0 && api_key != "your_key_here") fredr_set_key(api_key)

  map_dfr(names(us_fred_series), function(var_name) {

    raw <- fredr(series_id = us_fred_series[[var_name]],
                 observation_start = as.Date(start_date))

    # Monthly series resampled to quarterly averages
    processed <- if (var_name %in% c("Inflation", "Unemployment")) {
      to_quarterly(raw)
    } else {
      raw
    }

    processed %>%
      filter(!is.na(value)) %>%
      transmute(
        quarter  = as.Date(floor_date(date, "quarter")),
        variable = var_name,
        value    = round(value, 4)
      )
  })
}

fetch_history_aus <- function(start_date) {

  # All three Australian series via ABS read_abs_series()
  map_dfr(names(aus_abs_series), function(var_name) {

    raw <- read_abs_series(aus_abs_series[[var_name]])

    # Unemployment is monthly — resample to quarterly average
    processed <- if (var_name == "Unemployment") to_quarterly(raw) else raw

    processed %>%
      dplyr::filter(date >= as.Date(start_date), !is.na(value)) %>%
      transmute(
        quarter  = as.Date(floor_date(date, "quarter")),
        variable = var_name,
        value    = round(value, 4)
      )
  })
}

# ---- Public interface ----
# Returns tibble(quarter, variable, value) with level data.
# GDP in local $M or $B, CPI as index, unemployment as rate %.
# QoQ / YoY transformations applied downstream in the app.

fetch_history <- function(country, start_date = as.Date("1900-01-01")) {
  switch(country,
    US        = fetch_history_us(start_date),
    Australia = fetch_history_aus(start_date),
    stop(sprintf("No history fetcher registered for country: '%s'", country))
  )
}

fetch_all_history <- function(start_date = as.Date("1900-01-01")) {
  c("US", "Australia") %>%
    map_dfr(~ fetch_history(.x, start_date) %>% mutate(country = .x))
}
