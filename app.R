library(shiny)
library(bslib)
library(plotly)
library(rhandsontable)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(fredr)
library(readabs)

# ---- Load app modules ----
readRenviron(".Renviron")
source("R/config.R")
source("R/model_interface.R")
source("R/fetch_history.R")
source("R/transforms.R")
source("R/data_io.R")

# ---- Static config ----
countries  <- names(models_config)
variables  <- c("GDP", "Inflation", "Unemployment")
transforms <- c("QoQ", "TTY", "Annual", "Levels")

# Unemployment is a rate level — QoQ/TTY % changes are not meaningful
variable_transforms <- list(
  GDP          = c("QoQ", "TTY", "Annual", "Levels"),
  Inflation    = c("QoQ", "TTY", "Annual", "Levels"),
  Unemployment = c("Levels", "Annual")
)

transform_labels <- c(
  QoQ    = "QoQ %",
  TTY    = "Through the year %",
  Annual = "Annual average %",
  Levels = "Rate %"
)

# ---- Helpers ----

fmt_quarter <- function(d) paste0(year(d), " Q", quarter(d))

chain_levels <- function(last_level, qoq_rates) {
  purrr::accumulate(qoq_rates / 100, ~ .x * (1 + .y), .init = last_level)[-1]
}

# Fixed muted palette for vintage overlay lines
vintage_palette <- c("#1a73e8", "#f9c80e", "#00b050", "#ff5722", "#9c27b0")

build_vintage_levels <- function(history, vintage_df, country, variable) {
  # Label history as "History" so apply_transform can use it for lag calculations
  hist <- history %>%
    dplyr::filter(country == !!country, variable == !!variable) %>%
    select(quarter, value) %>%
    arrange(quarter) %>%
    mutate(type = "History")

  if (nrow(hist) == 0) return(tibble())

  last_hist_date <- max(hist$quarter)
  last_level     <- tail(hist$value, 1)

  fc <- vintage_df %>%
    dplyr::filter(country == !!country, variable == !!variable) %>%
    dplyr::filter(quarter > last_hist_date) %>%
    arrange(quarter)

  if (nrow(fc) == 0) return(tibble())

  bind_rows(
    hist,
    tibble(quarter = fc$quarter, value = chain_levels(last_level, fc$forecast), type = "Vintage")
  )
}

build_levels <- function(history, forecasts, country, variable, judgements_list) {

  hist <- history %>%
    dplyr::filter(country == !!country, variable == !!variable) %>%
    select(quarter, value) %>%
    arrange(quarter) %>%
    mutate(type = "History")

  if (nrow(hist) == 0) return(tibble())

  last_hist_date <- max(hist$quarter)
  last_level     <- tail(hist$value, 1)

  fc <- forecasts %>%
    dplyr::filter(country == !!country, variable == !!variable) %>%
    dplyr::filter(quarter > last_hist_date) %>%
    arrange(quarter)

  if (nrow(fc) == 0) return(hist)

  key <- paste(country, variable, sep = "_")
  adj <- judgements_list[[key]]
  if (is.null(adj) || length(adj) != nrow(fc)) adj <- rep(0, nrow(fc))

  model_levels <- chain_levels(last_level, fc$forecast)
  result <- bind_rows(
    hist,
    tibble(quarter = fc$quarter, value = model_levels, type = "Model")
  )

  # Only add Final line when at least one judgement is non-zero
  if (any(adj != 0)) {
    final_levels <- chain_levels(last_level, fc$forecast + adj)
    result <- bind_rows(
      result,
      tibble(quarter = fc$quarter, value = final_levels, type = "Final")
    )
  }

  result
}

fmt_ts <- function(ts) {
  if (is.na(ts) || is.null(ts)) return("Never")
  format(as.POSIXct(ts), "%d %b %Y %H:%M")
}

# ---- UI ----

make_country_tab <- function(variable, country) {
  pid         <- paste(country, variable, sep = "_")
  var_trans   <- variable_transforms[[variable]]
  default_t   <- if ("QoQ" %in% var_trans) "QoQ" else "Levels"
  nav_panel(
    title = country,
    br(),
    fluidRow(
      column(4,
        div(style = "display:flex; align-items:center; gap:10px;",
            strong("View:"),
            radioButtons(paste0("transform_", pid), NULL,
                         choices = var_trans, selected = default_t, inline = TRUE))
      ),
      column(4,
        div(style = "display:flex; align-items:center; gap:8px;",
            strong("Date range:"),
            dateInput(paste0("xmin_", pid), NULL, value = Sys.Date() - 365 * 15,
                      format = "yyyy", startview = "year", width = "110px"),
            span("to"),
            dateInput(paste0("xmax_", pid), NULL, value = Sys.Date() + 365 * 2,
                      format = "yyyy", startview = "year", width = "110px"))
      ),
      column(4,
        div(style = "display:flex; align-items:center; gap:8px;",
            strong("Y-axis:"),
            numericInput(paste0("ymin_", pid), NULL, value = NA, width = "90px"),
            span("to"),
            numericInput(paste0("ymax_", pid), NULL, value = NA, width = "90px"),
            actionButton(paste0("yreset_", pid), "Auto",
                         class = "btn-sm btn-outline-secondary"))
      )
    ),
    plotlyOutput(paste0("chart_", pid), height = "420px"),
    br(),
    h5("Quarterly judgement adjustments", style = "color:#555; margin-bottom:4px;"),
    p("Additive QoQ % adjustments on top of the model forecast. Changes are saved automatically.",
      style = "font-size:0.85em; color:#888; margin-bottom:8px;"),
    rHandsontableOutput(paste0("table_", pid)),
    br()
  )
}

make_variable_panel <- function(variable) {
  nav_panel(
    title = variable,
    # Global vintage selector — sits above country tabs, applies to all charts
    div(
      style = "display:flex; align-items:center; gap:12px; padding:0.5rem 0 0.25rem 0;",
      strong("Overlay baselines:", style = "white-space:nowrap; font-size:0.9em;"),
      div(style = "flex:1; max-width:500px;",
          selectInput(paste0("vintage_overlay_", variable), NULL,
                      choices  = list_vintages(),
                      selected = character(0),
                      multiple = TRUE,
                      width    = "100%"))
    ),
    do.call(navset_tab, c(
      list(id = paste0("tabs_", variable)),
      map(countries, ~ make_country_tab(variable, .x))
    ))
  )
}

ui <- do.call(
  page_navbar,
  c(
    list(
      title    = "MacroBench",
      theme    = bs_theme(
        bg        = "#ffffff",
        fg        = "#1a1a2e",
        primary   = "#2c6fad",
        navbar_bg = "#1e2d3d",
        base_font = font_google("Inter")
      ),
      fillable = FALSE,
      padding  = "1.5rem",
      # Data management panel in navbar
      nav_panel(
        title = "Data",
        icon  = shiny::icon("database"),
        br(),
        h4("Data management"),
        p("Historical data and forecasts are stored in ", code("input/"), " as CSV files.
           Use the buttons below to refresh data or re-estimate models.",
          style = "color:#555;"),
        hr(),
        fluidRow(
          column(6,
            h5("Historical data"),
            p(textOutput("last_data_update"), style = "color:#888; font-size:0.9em;"),
            p("Re-downloads all historical data from FRED and ABS and updates the CSVs.
               Judgements for quarters now in history are automatically removed.",
              style = "font-size:0.85em; color:#aaa;"),
            actionButton("btn_update_data", "Update Data",
                         class = "btn-primary", icon = icon("download"))
          ),
          column(6,
            h5("Forecast models"),
            p(textOutput("last_model_run"), style = "color:#888; font-size:0.9em;"),
            p("Re-runs all country VAR models and overwrites the forecast CSVs.
               Existing judgements are preserved and aligned to the new forecast horizon.",
              style = "font-size:0.85em; color:#aaa;"),
            actionButton("btn_rerun_models", "Re-run Models",
                         class = "btn-warning", icon = icon("play"))
          )
        ),
        hr(),
        fluidRow(
          column(6,
            h5("Vintages"),
            p("Save a named snapshot of the current Final forecast (model + judgements).
               Vintages are stored in ", code("input/vintages/"), " as CSV files.",
              style = "font-size:0.85em; color:#aaa;"),
            actionButton("btn_save_vintage", "Save Vintage",
                         class = "btn-success", icon = icon("floppy-disk"))
          ),
          column(3,
            h5("Export Final forecast"),
            p("One sheet per country. Columns: Quarter + one per variable (Final QoQ %).",
              style = "font-size:0.85em; color:#aaa;"),
            downloadButton("btn_export_all", "Export to Excel (.xlsx)",
                           class = "btn-outline-primary")
          )
        ),
        br(),
        verbatimTextOutput("data_log")
      )
    ),
    map(variables, make_variable_panel)
  )
)

# ---- Server ----

server <- function(input, output, session) {

  # ---- Reactive data stores ----
  rv <- reactiveValues(
    history   = read_history(),
    forecasts = read_forecasts(),
    log       = character(0)
  )

  judgements <- reactiveValues()

  # Initialise judgements from file on session start
  observe({
    stored <- read_judgements()
    fc     <- rv$forecasts
    jlist  <- df_to_judgements_list(stored, fc)
    for (key in names(jlist)) judgements[[key]] <- jlist[[key]]
  })

  # ---- Data panel outputs ----
  output$last_data_update <- renderText({
    paste("Last updated:", fmt_ts(read_meta()$last_data_update))
  })
  output$last_model_run <- renderText({
    paste("Last run:", fmt_ts(read_meta()$last_model_run))
  })
  output$data_log <- renderText({
    paste(rv$log, collapse = "\n")
  })

  # ---- Update Data button ----
  observeEvent(input$btn_update_data, {
    rv$log <- c(rv$log, paste0("[", Sys.time(), "] Fetching data from FRED and ABS..."))
    withProgress(message = "Downloading data...", {
      tryCatch({
        new_history <- fetch_all_history()
        save_history(new_history)
        save_meta("last_data_update", Sys.time())

        # Roll judgements: drop entries now covered by history
        old_judgements <- read_judgements()
        rolled         <- roll_judgements(old_judgements, new_history)
        save_judgements(rolled)

        rv$history <- new_history
        rv$log <- c(rv$log, paste0("[", Sys.time(), "] Data updated successfully."))
      }, error = function(e) {
        msg <- e$message
        rv$log <- c(rv$log, paste0("[", Sys.time(), "] ERROR: ", msg))
        if (grepl("ABS|abs|Time Series Directory|readabs", msg, ignore.case = TRUE)) {
          rv$log <- c(rv$log, paste0("[", Sys.time(), "] ABS fetch failed — ",
            "run 00_setup.R from RStudio instead (SSL issue in terminal environment)."))
        }
      })
    })
  })

  # ---- Re-run Models button ----
  observeEvent(input$btn_rerun_models, {
    rv$log <- c(rv$log, paste0("[", Sys.time(), "] Re-running VAR models..."))
    withProgress(message = "Running models...", {
      tryCatch({
        # Auto-vintage current forecasts before overwriting
        auto_name <- paste0("auto_", format(Sys.time(), "%Y-%m-%d_%H%M%S"))
        save_vintage(rv$forecasts, reactiveValuesToList(judgements), auto_name)
        rv$log <- c(rv$log, paste0("[", Sys.time(), "] Auto-vintage saved: ", auto_name))

        new_forecasts <- run_all_forecasts(history_df = rv$history)
        save_forecasts(new_forecasts)
        save_meta("last_model_run", Sys.time())
        rv$forecasts <- new_forecasts
        rv$log <- c(rv$log, paste0("[", Sys.time(), "] Models re-run successfully."))
      }, error = function(e) {
        rv$log <- c(rv$log, paste0("[", Sys.time(), "] ERROR: ", e$message))
      })
    })
  })

  # ---- Save Vintage button: show naming modal ----
  observeEvent(input$btn_save_vintage, {
    showModal(modalDialog(
      title = "Save Vintage",
      textInput("vintage_name_input", "Vintage name",
                value = format(Sys.Date(), "%Y-%m-%d"),
                placeholder = "e.g. May_2026_baseline"),
      p("Saves the current Final forecast (model + judgements) for all countries and variables.",
        style = "font-size:0.85em; color:#888;"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_vintage_confirm", "Save", class = "btn-success")
      )
    ))
  })

  observeEvent(input$btn_vintage_confirm, {
    name <- trimws(input$vintage_name_input)
    name <- gsub("\\s+", "_", name)   # spaces -> underscores
    removeModal()

    if (nchar(name) == 0) {
      rv$log <- c(rv$log, paste0("[", Sys.time(), "] ERROR: Vintage name cannot be empty."))
      return()
    }

    tryCatch({
      save_vintage(rv$forecasts, reactiveValuesToList(judgements), name)
      folder <- normalizePath(vintages_dir(), winslash = "/")
      rv$log <- c(rv$log, paste0("[", Sys.time(), "] Vintage '", name,
                                 "' saved to: ", folder))
    }, error = function(e) {
      rv$log <- c(rv$log, paste0("[", Sys.time(), "] ERROR saving vintage: ", e$message))
    })
  })

  # ---- Export all Final forecasts to Excel ----
  output$btn_export_all <- downloadHandler(
    filename = function() {
      paste0("forecasts_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      wb <- createWorkbook()
      jlist <- reactiveValuesToList(judgements)

      for (co in countries) {
        # Build wide table: Quarter + one column per variable (Final QoQ)
        wide <- map(variables, function(var) {
          fc <- rv$forecasts %>%
            dplyr::filter(country == co, variable == var) %>%
            arrange(quarter)
          key <- paste(co, var, sep = "_")
          adj <- jlist[[key]]
          if (is.null(adj) || length(adj) != nrow(fc)) adj <- rep(0, nrow(fc))
          tibble(
            Quarter        = fmt_quarter(fc$quarter),
            !!var         := round(fc$forecast + adj, 3)
          )
        }) %>%
          reduce(full_join, by = "Quarter")

        addWorksheet(wb, co)
        writeData(wb, co, wide)
      }

      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # ---- Render chart + table + download for each panel ----
  walk(countries, function(country) {
    walk(variables, function(variable) {
      local({
        co  <- country
        var <- variable
        pid <- paste(co, var, sep = "_")

        levels_data <- reactive({
          build_levels(rv$history, rv$forecasts, co, var,
                       reactiveValuesToList(judgements))
        })

        # Chart
        output[[paste0("chart_", pid)]] <- renderPlotly({
          df        <- levels_data()
          transform <- input[[paste0("transform_", pid)]] %||% "QoQ"
          if (nrow(df) == 0) return(plot_ly() %>%
            plotly::layout(title = list(text = "No data — run 00_setup.R first")))

          transformed <- apply_transform(df, transform, variable = var) %>%
            dplyr::filter(!is.na(value))

          hist_df  <- transformed %>% dplyr::filter(type == "History")
          model_df <- transformed %>% dplyr::filter(type == "Model")
          final_df <- transformed %>% dplyr::filter(type == "Final")

          has_final <- nrow(final_df) > 0

          # Silent connectors: history -> first forecast point, no hover
          make_connector <- function(fc_sub, col) {
            if (nrow(hist_df) > 0 && nrow(fc_sub) > 0)
              bind_rows(tail(hist_df, 1), head(fc_sub, 1))
            else tibble()
          }
          conn_model <- make_connector(model_df)
          conn_final <- if (has_final) make_connector(final_df) else tibble()

          y_label <- if (var == "Unemployment" && transform == "Annual") "Annual average rate %" else transform_labels[[transform]]
          last_hist_date <- hist_df %>% pull(quarter) %>% max()
          is_annual      <- transform == "Annual"
          x_fmt          <- if (is_annual) "%Y" else "%Y Q%q"
          mode           <- if (is_annual) "lines+markers" else "lines"

          p <- plot_ly() %>%
            add_trace(
              data = hist_df, x = ~quarter, y = ~value,
              type = "scatter", mode = mode, name = "History",
              line   = list(color = "#000000", width = 2.5),
              marker = list(color = "#000000", size = 6),
              hovertemplate = paste0("<b>%{x|", x_fmt, "}</b><br>", y_label, ": %{y:.2f}<extra>History</extra>")
            ) %>%
            add_trace(
              data = model_df, x = ~quarter, y = ~value,
              type = "scatter", mode = mode, name = "Model",
              line   = list(color = "#cc0000", width = 2, dash = "dash"),
              marker = list(color = "#cc0000", size = 5),
              hovertemplate = paste0("<b>%{x|", x_fmt, "}</b><br>", y_label, ": %{y:.2f}<extra>Model</extra>")
            ) %>%
            add_trace(
              data = conn_model, x = ~quarter, y = ~value,
              type = "scatter", mode = "lines",
              line = list(color = "#cc0000", width = 2, dash = "dash"),
              showlegend = FALSE, hoverinfo = "skip", name = ""
            )

          # Final trace only when judgements are active
          if (has_final) {
            p <- p %>%
              add_trace(
                data = final_df, x = ~quarter, y = ~value,
                type = "scatter", mode = mode, name = "Final",
                line   = list(color = "#000000", width = 2.5),
                marker = list(color = "#000000", size = 6),
                hovertemplate = paste0("<b>%{x|", x_fmt, "}</b><br>", y_label, ": %{y:.2f}<extra>Final</extra>")
              ) %>%
              add_trace(
                data = conn_final, x = ~quarter, y = ~value,
                type = "scatter", mode = "lines",
                line = list(color = "#000000", width = 2.5),
                showlegend = FALSE, hoverinfo = "skip", name = ""
              )
          }

          # Vintage overlays — driven by per-variable selector above the tabs
          selected_vintages <- input[[paste0("vintage_overlay_", var)]] %||% character(0)
          for (i in seq_along(selected_vintages)) {
            vname   <- selected_vintages[[i]]
            vcol    <- vintage_palette[((i - 1) %% length(vintage_palette)) + 1]
            vdf_raw <- read_vintage(vname)
            vdf     <- build_vintage_levels(rv$history, vdf_raw, co, var)

            if (nrow(vdf) > 0) {
              vt  <- apply_transform(vdf, transform, variable = var) %>% dplyr::filter(!is.na(value))
              vfc <- vt %>% dplyr::filter(type == "Vintage")

              # Connector: last actual history point -> first vintage forecast point
              vconn <- if (nrow(hist_df) > 0 && nrow(vfc) > 0) {
                bind_rows(tail(hist_df, 1), head(vfc, 1))
              } else NULL

              if (nrow(vfc) > 0) {
                p <- p %>%
                  add_trace(
                    data = vfc, x = ~quarter, y = ~value,
                    type = "scatter", mode = mode, name = vname,
                    line   = list(color = vcol, width = 1.5, dash = "dot"),
                    marker = list(color = vcol, size = 4),
                    opacity = 0.7,
                    hovertemplate = paste0("<b>%{x|", x_fmt, "}</b><br>", y_label,
                                           ": %{y:.2f}<extra>", vname, "</extra>")
                  )
                if (!is.null(vconn)) {
                  p <- p %>%
                    add_trace(
                      data = vconn, x = ~quarter, y = ~value,
                      type = "scatter", mode = "lines",
                      line = list(color = vcol, width = 1.5, dash = "dot"),
                      opacity = 0.7,
                      showlegend = FALSE, hoverinfo = "skip", name = ""
                    )
                }
              }
            }
          }

          p %>%
            plotly::layout(
              # uirevision tied to transform so axis zoom resets on transform change,
              # but persists when only judgements or vintages change
              uirevision = paste(transform, co, var),
              xaxis  = list(
                title    = "",
                showgrid = FALSE,
                range    = list(
                  as.numeric(as.POSIXct(input[[paste0("xmin_", pid)]])) * 1000,
                  as.numeric(as.POSIXct(input[[paste0("xmax_", pid)]])) * 1000
                )
              ),
              yaxis  = list(
                title     = y_label,
                gridcolor = "#eeeeee",
                zeroline  = FALSE,
                autorange = if (!is.na(input[[paste0("ymin_", pid)]]) &&
                                !is.na(input[[paste0("ymax_", pid)]])) FALSE else TRUE,
                range     = if (!is.na(input[[paste0("ymin_", pid)]]) &&
                                !is.na(input[[paste0("ymax_", pid)]])) {
                  list(input[[paste0("ymin_", pid)]], input[[paste0("ymax_", pid)]])
                } else {
                  NULL
                }
              ),
              legend = list(orientation = "h", x = 0, y = 1.1),
              hovermode     = "x unified",
              plot_bgcolor  = "#ffffff",
              paper_bgcolor = "#ffffff",
              shapes = list(list(
                type = "line",
                x0 = last_hist_date, x1 = last_hist_date,
                y0 = 0, y1 = 1, yref = "paper",
                line = list(color = "#bbbbbb", width = 1, dash = "dot")
              ))
            ) %>%
            config(displayModeBar = FALSE)
        })

        # Table
        table_data <- reactive({
          fc  <- rv$forecasts %>%
            dplyr::filter(country == co, variable == var) %>% arrange(quarter)
          adj <- judgements[[pid]]
          if (is.null(adj)) adj <- rep(0, nrow(fc))
          tibble(
            Quarter       = fmt_quarter(fc$quarter),
            `Model QoQ %` = round(fc$forecast, 3),
            Judgement     = round(adj, 3),
            `Final QoQ %` = round(fc$forecast + adj, 3)
          )
        })

        output[[paste0("table_", pid)]] <- renderRHandsontable({
          rhandsontable(table_data(), rowHeaders = FALSE, stretchH = "all") %>%
            hot_col("Quarter",       readOnly = TRUE) %>%
            hot_col("Model QoQ %",   readOnly = TRUE) %>%
            hot_col("Judgement",     type = "numeric", format = "0.000") %>%
            hot_col("Final QoQ %",   readOnly = TRUE) %>%
            hot_cols(columnSorting = FALSE)
        })

        # Y-axis auto reset
        observeEvent(input[[paste0("yreset_", pid)]], {
          updateNumericInput(session, paste0("ymin_", pid), value = NA)
          updateNumericInput(session, paste0("ymax_", pid), value = NA)
        })

        # Save judgements on edit
        observeEvent(input[[paste0("table_", pid)]], {
          edited <- hot_to_r(input[[paste0("table_", pid)]])
          if (!is.null(edited)) {
            judgements[[pid]] <- edited$Judgement
            # Persist to CSV
            fc <- rv$forecasts %>%
              dplyr::filter(country == co, variable == var) %>% arrange(quarter)
            updated <- read_judgements() %>%
              dplyr::filter(!(country == co & variable == var)) %>%
              bind_rows(tibble(country = co, variable = var,
                               quarter = fc$quarter, judgement = edited$Judgement))
            save_judgements(updated)
          }
        })

      })
    })
  })
}

shinyApp(ui, server)
