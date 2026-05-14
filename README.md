# Macro Forecasting Tool

An R Shiny app for viewing, judging, and exporting macroeconomic forecasts. Designed to give non-programmers a clean interface to model-driven forecasts — no R knowledge required to use it.

---

## What it does

- Runs country-level VAR/BVAR models and displays quarterly forecast profiles
- Shows real historical data alongside model forecasts in a single chart
- Lets users add in-app judgement adjustments to any quarter
- Exports the final (model + judgement) profile to Excel

Current coverage: **United States** and **Australia**, across **GDP**, **Inflation**, and **Unemployment**.

---

## Architecture

One VAR model per country. Each model returns forecasts for all variables simultaneously, which is the natural output of a multivariate model. The app is agnostic to what's inside each model — it only cares about the output format.

```
app.R                        # Shiny UI and server
R/
  config.R                   # Registry: country -> model function
  model_interface.R          # run_forecast(country), run_all_forecasts()
  fetch_history.R            # Historical data from FRED (US) and ABS/FRED (Australia)
  models/
    us.R                     # US VAR model
    aus.R                    # Australia VAR model
input/                       # Raw data inputs for models
output/                      # Downloaded exports
papers/                      # Reference papers and documentation
```

**Data model:** all history is stored as levels (GDP in local currency, CPI as an index, unemployment as a rate). Growth transformations (QoQ, YoY) are computed in the app at render time. Model forecasts are expressed as quarter-on-quarter growth rates.

---

## Setup

### 1. Install R packages

```r
install.packages(c(
  "shiny", "bslib", "plotly", "rhandsontable",
  "tidyverse", "openxlsx", "fredr", "readabs"
))
```

### 2. FRED API key

Historical data for US series (and Australian CPI) is pulled from [FRED](https://fred.stlouisfed.org/docs/api/api_key.html). Register for a free key and add it to `.Renviron` in the repo root:

```
FRED_API_KEY=your_key_here
```

Restart your R session after saving.

### 3. Run the app

```r
shiny::runApp()
```

---

## Adding a new country

1. Create `R/models/{country}.R` with a function returning `tibble(quarter, variable, forecast)` in QoQ % (or level for unemployment)
2. Add an entry to `models_config` in `R/config.R`
3. Add historical series to `fetch_history.R` under `fetch_history_{country}()`

No other changes required.

## Adding a new variable

Update the country model to return an additional variable in the long-format output. The app picks it up automatically via the `models_config` registry.

---

## Data sources

| Country | Variable | Source | Series |
|---|---|---|---|
| US | GDP | FRED | GDPC1 |
| US | Inflation | FRED | CPIAUCSL |
| US | Unemployment | FRED | UNRATE |
| Australia | GDP | ABS 5206.0 | A2304402X |
| Australia | Inflation | FRED / OECD | AUSCPIALLQINMEI |
| Australia | Unemployment | ABS 6202.0 | A84423050A |
