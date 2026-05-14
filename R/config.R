# Source all country model files
source(file.path("R", "models", "us.R"))
source(file.path("R", "models", "aus.R"))

# ---- Model registry ----
# Maps country name -> model function (one VAR per country)
# To add a country: source its model file above, add one entry below

models_config <- list(
  US        = var_us,
  Australia = var_aus
)
