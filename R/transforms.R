library(tidyverse)

# ---- Chart transformations ----
# Applied to the combined history + forecast level series before plotting.
# The judgement table always operates in QoQ terms — transformations are display only.

# Compute QoQ % change from a levels series
to_qoq <- function(values) {
  (values / lag(values, 1) - 1) * 100
}

# Compute through-the-year % change (same quarter vs same quarter prior year)
to_tty <- function(values) {
  (values / lag(values, 4) - 1) * 100
}

# Chain QoQ forecast growth rates onto the last known history level
chain_levels <- function(last_level, qoq_rates) {
  accumulate(qoq_rates / 100, ~ .x * (1 + .y), .init = last_level)[-1]
}

# Apply a display transformation to a combined levels tibble.
# Args:
#   df        : tibble with columns quarter (Date), value (dbl), type (chr)
#   transform : one of "QoQ", "TTY", "Annual", "Levels"
# Returns:
#   tibble at appropriate frequency (quarterly for QoQ/TTY/Levels, annual for Annual)

apply_transform <- function(df, transform, variable = NULL) {

  if (transform == "Levels") return(df)

  hist     <- df %>% dplyr::filter(type == "History") %>% arrange(quarter)
  fc_types <- df %>% dplyr::filter(type != "History") %>% pull(type) %>% unique()

  # Unemployment: Annual = calendar-year average of the rate level (not TTY)
  is_rate <- !is.null(variable) && variable == "Unemployment"

  # Process one series at a time so lag() always operates on a continuous sequence
  process_type <- function(type_name) {

    if (type_name == "History") {
      series <- hist
    } else {
      series <- bind_rows(hist, df %>% dplyr::filter(type == type_name)) %>% arrange(quarter)
    }

    if (transform == "Annual") {
      fc_yrs <- if (type_name != "History") {
        df %>% dplyr::filter(type == type_name) %>% mutate(yr = year(quarter)) %>% pull(yr) %>% unique()
      } else NULL

      if (is_rate) {
        # Average the level directly, no TTY step
        series %>%
          mutate(yr = year(quarter)) %>%
          { if (!is.null(fc_yrs)) dplyr::filter(., yr %in% fc_yrs) else . } %>%
          group_by(yr) %>%
          summarise(
            quarter = as.Date(paste0(first(yr), "-01-01")),
            value   = mean(value, na.rm = TRUE),
            type    = type_name,
            .groups = "drop"
          ) %>%
          dplyr::filter(!is.na(value)) %>%
          select(-yr)
      } else {
        series %>%
          mutate(tty = to_tty(value), yr = year(quarter)) %>%
          { if (!is.null(fc_yrs)) dplyr::filter(., yr %in% fc_yrs) else . } %>%
          group_by(yr) %>%
          summarise(
            quarter = as.Date(paste0(first(yr), "-01-01")),
            value   = mean(tty, na.rm = TRUE),
            type    = type_name,
            .groups = "drop"
          ) %>%
          dplyr::filter(!is.na(value)) %>%
          select(-yr)
      }

    } else {
      series %>%
        arrange(quarter) %>%
        mutate(value = if (transform == "QoQ") to_qoq(value) else to_tty(value)) %>%
        dplyr::filter(type == type_name)
    }
  }

  map_dfr(c("History", fc_types), process_type)
}
