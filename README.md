# MacroBench

R Shiny app for producing, judging and exporting model agnostic macroeconomic forecasts across countries and variables.

---

## What it does

- Runs country-level VAR/BVAR models and displays quarterly forecast profiles 
- Shows real historical data alongside model forecasts in an interactive chart
- Lets users apply in-app judgement adjustments to individual quarters
- Supports forecast vintaging: save named snapshots and overlay past forecasts on charts
- Exports the final (model + judgement) forecast profile to Excel
- Designed to be extended: adding a new country or variable requires changes to at most three lines of configuration — everything downstream updates automatically

Current coverage: **United States** and **Australia**, across **GDP**, **Inflation**, and **Unemployment**.

---

## Screenshots

Forecasts and overlays (Australia, GDP):

![Australia GDP: chart with history, model and baseline](output/screenshots/aus%20gdp.png)

App shell:

![Navbar and variable tabs](output/screenshots/landing%20page.png)

---

## Modularity

The model output contract — `tibble(quarter, variable, forecast)` — is the only interface between the models and the app. Everything downstream (charts, judgement tables, vintaging, export) picks up new content automatically.

### Adding a new country

1. Create `R/models/{country}.R` with a function `var_{country}(last_history_date = NULL)` returning `tibble(quarter, variable, forecast)` in QoQ % (levels for unemployment)
2. Add an entry to `models_config` in `R/config.R`
3. Add a `fetch_history_{country}()` function in `R/fetch_history.R`

No other changes required.

### Adding a new variable

1. Update each country model to return rows for the new variable in the long-format output
2. Add the variable name to `variables` in `app.R`
3. Add a `variable_transforms` entry in `app.R` specifying which display transforms apply (e.g. QoQ/TTY/Annual/Levels for a growth series, or Levels/Annual for a rate)

Example — adding core inflation:

```r
# In app.R
variables <- c("GDP", "Inflation", "Core_Inflation", "Unemployment")

variable_transforms <- list(
  GDP            = c("QoQ", "TTY", "Annual", "Levels"),
  Inflation      = c("QoQ", "TTY", "Annual", "Levels"),
  Core_Inflation = c("QoQ", "TTY", "Annual", "Levels"),
  Unemployment   = c("Levels", "Annual")
)
```

Charts, judgement tables, vintaging and export all pick up the new variable with no further changes.

---

## Architecture

One VAR model per country. Each model returns forecasts for all variables simultaneously, which is the natural output of a multivariate model. The app is agnostic to the internal specification of each model — it only requires a standard output format.

```
app.R                        # Shiny UI and server
00_setup.R                   # First-run and refresh script (run from RStudio)
R/
  config.R                   # Registry: country -> model function
  model_interface.R          # run_forecast(country), run_all_forecasts()
  fetch_history.R            # Historical data from FRED (US) and ABS (Australia)
  transforms.R               # QoQ, TTY, Annual, Levels display transforms
  data_io.R                  # CSV read/write for history, forecasts, judgements, vintages
  models/
    us.R                     # US VAR model (stub)
    aus.R                    # Australia VAR model (stub)
input/                       # CSVs: history, forecasts, judgements, vintages/
output/                      # Downloaded exports
papers/                      # Reference papers and documentation
```

**Data model:** all history is stored as levels (GDP in local currency, CPI as an index, unemployment as a rate %). Growth transformations (QoQ, TTY, Annual) are computed in the app at render time. Model forecasts are stored as quarter-on-quarter growth rates, except unemployment which is forecast in levels.

---

## Setup

### 1. Install R packages

```r
install.packages(c(
  "shiny", "bslib", "plotly", "rhandsontable",
  "tidyverse", "openxlsx", "fredr", "readabs", "lubridate"
))
```

### 2. FRED API key

Historical data for US series is pulled from [FRED](https://fred.stlouisfed.org/docs/api/api_key.html). Register for a free key and add it to `.Renviron` in the repo root:

```
FRED_API_KEY=your_key_here
```

Restart your R session after saving.

### 3. Initialise data

Run `00_setup.R` from RStudio (not the terminal — ABS downloads require RStudio's SSL handling):

```r
source("00_setup.R")
```

This downloads full history from FRED and ABS, runs the model stubs, and writes all CSVs to `input/`. Re-run whenever you want to refresh data or re-estimate models.

### 4. Launch the app

```r
shiny::runApp()
```

---

## Data panel

The **Data** tab in the navbar exposes:

| Button | What it does |
|---|---|
| Update Data | Re-fetches history from FRED and ABS, rolls judgements forward |
| Re-run Models | Auto-vintages current forecasts, then re-runs all country models |
| Save Vintage | Opens a naming dialog and saves a snapshot of the current Final forecast to `input/vintages/` |
| Export to Excel | Downloads Final forecasts for all countries as a single workbook (one sheet per country, one column per variable) |

To overlay past vintages on charts, use the **Overlay baselines** selector on each variable page.

---

## Data sources

| Country | Variable | Source | Series |
|---|---|---|---|
| US | GDP | FRED | GDPC1 |
| US | Inflation | FRED | CPIAUCSL |
| US | Unemployment | FRED | UNRATE |
| Australia | GDP | ABS 5206.0 | A2304402X |
| Australia | Inflation | ABS 6401.0 | A2325846C |
| Australia | Unemployment | ABS 6202.0 | A84423050A |

---

## Planned features

- **Real VAR/BVAR models**: replace stubs with estimated models using `vars` or `BVAR` packages
- **Forecast evaluation panel**: MAE/RMSE table comparing saved vintages against realised outcomes
- **Scenario analysis**: lock one variable and propagate shocks through the system via the model structure
