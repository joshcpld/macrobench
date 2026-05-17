library(tidyverse)

# ---- File paths ----
history_path    <- function(country) file.path("input", paste0("history_",   country, ".csv"))
forecasts_path  <- function(country) file.path("input", paste0("forecasts_", country, ".csv"))
judgements_path <- function()        file.path("input", "judgements.csv")
meta_path       <- function()        file.path("input", "meta.csv")

vintages_dir <- function() {
  dir <- file.path("input", "vintages")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  dir
}

vintage_path <- function(name) file.path(vintages_dir(), paste0(name, ".csv"))

# ---- Read ----

read_history <- function() {
  files <- list.files("input", pattern = "^history_.*\\.csv$", full.names = TRUE)
  if (length(files) == 0) return(tibble(quarter = as.Date(character()), variable = character(),
                                        value = numeric(), country = character()))
  map_dfr(files, read_csv,
          col_types = cols(quarter = col_date(), variable = col_character(),
                           value = col_double(), country = col_character()),
          show_col_types = FALSE)
}

read_forecasts <- function() {
  files <- list.files("input", pattern = "^forecasts_.*\\.csv$", full.names = TRUE)
  if (length(files) == 0) return(tibble(quarter = as.Date(character()), variable = character(),
                                        forecast = numeric(), country = character()))
  map_dfr(files, read_csv,
          col_types = cols(quarter = col_date(), variable = col_character(),
                           forecast = col_double(), country = col_character()),
          show_col_types = FALSE)
}

read_judgements <- function() {
  path <- judgements_path()
  if (!file.exists(path)) return(tibble(country = character(), variable = character(),
                                        quarter = as.Date(character()), judgement = numeric()))
  read_csv(path, col_types = cols(country = col_character(), variable = col_character(),
                                  quarter = col_date(), judgement = col_double()),
           show_col_types = FALSE)
}

read_meta <- function() {
  path <- meta_path()
  if (!file.exists(path)) return(list(last_data_update = NA, last_model_run = NA))
  meta <- read_csv(path, show_col_types = FALSE)
  list(
    last_data_update = meta$last_data_update[1],
    last_model_run   = meta$last_model_run[1]
  )
}

# ---- Write ----

save_history <- function(history_df) {
  for (co in unique(history_df$country)) {
    history_df %>%
      dplyr::filter(country == co) %>%
      write_csv(history_path(co))
  }
}

save_forecasts <- function(forecasts_df) {
  for (co in unique(forecasts_df$country)) {
    forecasts_df %>%
      dplyr::filter(country == co) %>%
      write_csv(forecasts_path(co))
  }
}

save_judgements <- function(judgements_df) {
  write_csv(judgements_df, judgements_path())
}

save_meta <- function(field, value) {
  path <- meta_path()
  meta <- if (file.exists(path)) {
    read_csv(path, show_col_types = FALSE)
  } else {
    tibble(last_data_update = NA_character_, last_model_run = NA_character_)
  }
  meta[[field]] <- as.character(value)
  write_csv(meta, path)
}

# ---- Vintage I/O ----

# Save a named vintage: Final QoQ = model forecast + current judgements
# forecasts_df  : tibble(country, variable, quarter, forecast)
# judgements_list: named list of adjustment vectors keyed "Country_Variable"
save_vintage <- function(forecasts_df, judgements_list, name) {
  final_df <- forecasts_df %>%
    arrange(country, variable, quarter) %>%
    group_by(country, variable) %>%
    group_modify(function(fc, keys) {
      key <- paste(keys$country, keys$variable, sep = "_")
      adj <- judgements_list[[key]]
      if (is.null(adj) || length(adj) != nrow(fc)) adj <- rep(0, nrow(fc))
      fc %>% mutate(forecast = forecast + adj)
    }) %>%
    ungroup()

  write_csv(final_df, vintage_path(name))
}

list_vintages <- function() {
  files <- list.files(vintages_dir(), pattern = "\\.csv$", full.names = FALSE)
  tools::file_path_sans_ext(files)
}

read_vintage <- function(name) {
  path <- vintage_path(name)
  if (!file.exists(path)) return(tibble(country = character(), variable = character(),
                                        quarter = as.Date(character()), forecast = numeric()))
  read_csv(path, col_types = cols(country = col_character(), variable = col_character(),
                                  quarter = col_date(), forecast = col_double()),
           show_col_types = FALSE)
}

# ---- Judgement helpers ----

# Convert the reactive judgements list to a tidy tibble for saving
judgements_to_df <- function(judgements_list, forecasts_df) {
  forecasts_df %>%
    arrange(country, variable, quarter) %>%
    group_by(country, variable) %>%
    mutate(judgement = {
      key <- paste(unique(country), unique(variable), sep = "_")
      adj <- judgements_list[[key]]
      if (is.null(adj) || length(adj) != n()) rep(0, n()) else adj
    }) %>%
    ungroup() %>%
    select(country, variable, quarter, judgement)
}

# Convert stored judgements tibble back to named list for reactiveValues
df_to_judgements_list <- function(judgements_df, forecasts_df) {
  forecasts_df %>%
    arrange(country, variable, quarter) %>%
    group_by(country, variable) %>%
    group_map(function(fc, keys) {
      co  <- keys$country
      var <- keys$variable
      key <- paste(co, var, sep = "_")

      adj <- judgements_df %>%
        dplyr::filter(country == co, variable == var) %>%
        # Align by quarter — default 0 for any missing quarter
        right_join(fc %>% select(quarter), by = "quarter") %>%
        arrange(quarter) %>%
        pull(judgement) %>%
        replace_na(0)

      list(key = key, values = adj)
    }) %>%
    set_names(map_chr(., "key")) %>%
    map("values")
}

# After a data update, drop judgements for quarters now in history
roll_judgements <- function(judgements_df, new_history_df) {
  history_quarters <- new_history_df %>%
    select(country, variable, quarter)

  judgements_df %>%
    anti_join(history_quarters, by = c("country", "variable", "quarter"))
}
