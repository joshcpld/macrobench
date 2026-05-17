setwd("c:/Users/joshc/Documents/git/forecasting_tool")
readRenviron(".Renviron")
suppressPackageStartupMessages({ library(tidyverse); library(lubridate); library(fredr) })
source("R/config.R"); source("R/model_interface.R"); source("R/fetch_history.R"); source("R/data_io.R")

api_key <- Sys.getenv("FRED_API_KEY")
if (nchar(api_key) > 0) fredr_set_key(api_key)
us <- fetch_history_us(Sys.Date() - 365*10) %>% mutate(country = "US")
# 41 quarters: 2016 Q1 through 2026 Q1 (latest released quarter for Australia)
quarters <- seq(as.Date("2016-01-01"), by = "3 months", length.out = 41)
n <- length(quarters)
aus <- bind_rows(
  tibble(quarter=quarters, variable="GDP",          value=round(seq(550000,700000,length.out=n),0), country="Australia"),
  tibble(quarter=quarters, variable="Inflation",    value=round(seq(90,132,length.out=n),1),        country="Australia"),
  tibble(quarter=quarters, variable="Unemployment", value=round(seq(5.8,4.2,length.out=n),1),       country="Australia")
)
save_history(bind_rows(us, aus))
save_meta("last_data_update", Sys.time())
forecasts <- run_all_forecasts(); save_forecasts(forecasts); save_meta("last_model_run", Sys.time())
if (!file.exists("input/judgements.csv")) {
  forecasts %>% select(country,variable,quarter) %>% mutate(judgement=0) %>% write_csv("input/judgements.csv")
}
cat("Done. Forecast quarters:\n"); print(forecasts %>% group_by(country,variable) %>% summarise(from=min(quarter),to=max(quarter),.groups="drop"))
