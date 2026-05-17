library(tidyverse)
library(lubridate)
library(fredr)
library(readabs)

# ########################################
#
# 00_setup.R
# Run from RStudio to initialise or refresh all data.
# Downloads history from FRED and ABS, runs models, writes to input/.
# Re-run whenever you want to update data or re-estimate models.
#
# ########################################

readRenviron(".Renviron")

source("R/config.R")
source("R/model_interface.R")
source("R/fetch_history.R")
source("R/data_io.R")

# ########################################
# 1. Download historical data
# ########################################

cat("Fetching historical data from FRED and ABS...\n")
history <- fetch_all_history()
history %>%
  group_by(country, variable) %>%
  summarise(from = min(quarter), to = max(quarter), n = n(), .groups = "drop") %>%
  print()

save_history(history)
save_meta("last_data_update", Sys.time())
cat("History saved to input/.\n\n")

# ########################################
# 2. Run forecast models
# ########################################

cat("Running VAR models...\n")
forecasts <- run_all_forecasts(history_df = history)
save_forecasts(forecasts)
save_meta("last_model_run", Sys.time())
cat("Forecasts saved to input/.\n\n")

# ########################################
# 3. Initialise judgements if not present
# ########################################

if (!file.exists("input/judgements.csv")) {
  forecasts %>%
    select(country, variable, quarter) %>%
    mutate(judgement = 0) %>%
    write_csv("input/judgements.csv")
  cat("Judgements initialised to zero.\n")
} else {
  cat("Existing judgements.csv preserved.\n")
}

cat("\nSetup complete. Launch app with: shiny::runApp()\n")
