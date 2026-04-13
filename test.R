# NYC Air Quality Dashboard
# Author: Chenchong Xia, Luobing Wang
# Course: SYSEN 5460

# packages ---------------------------------------------
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(DT)
library(broom)
library(lubridate)
library(bslib)
library(bsicons)
library(sf)
library(leaflet)

# County name lookup table -----------------------------
county_lookup <- c(
  "34003" = "Bergen",
  "34017" = "Hudson",
  "34019" = "Hunterdon",
  "34023" = "Middlesex",
  "34027" = "Morris",
  "34029" = "Gloucester",
  "34031" = "Passaic",
  "34039" = "Ocean",
  "36005" = "Bronx",
  "36047" = "Kings/Brooklyn",
  "36061" = "New York/Manhattan",
  "36081" = "Queens",
  "36085" = "Richmond/Staten Island",
  "36087" = "Rockland",
  "36103" = "Suffolk",
  "36119" = "Westchester"
)

# UI ---------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(
    bootswatch  = "cosmo",
    primary     = "#2C7BB6",
    secondary   = "#4DAC26",
    base_font   = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  
  # Custom CSS for minor polish
  tags$head(tags$style(HTML("
    .navbar, .title-bar {background-color: #1a4f7a;}
    .sidebar-panel-title {font-weight: 700; font-size: 1.1rem;}
    .well {border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08);}
    .main-title {
      background: linear-gradient(90deg, #2C7BB6, #1a4f7a);
      color: white;
      padding: 18px 24px;
      border-radius: 8px;
      margin-bottom: 20px;
      font-size: 1.4rem;
      font-weight: 700;
      letter-spacing: 0.3px;
    }
    .section-header {
      border-left: 4px solid #2C7BB6;
      padding-left: 10px;
      margin-top: 10px;
      margin-bottom: 12px;
      font-weight: 600;
      color: #1a4f7a;
    }
    .analysis-text {
      background: #f0f6fb;
      border-radius: 6px;
      padding: 12px 16px;
      margin-bottom: 8px;
      font-size: 0.95rem;
      line-height: 1.6;
      color: #2c3e50;
    }
    .caption-text {
      color: #666;
      font-size: 0.85rem;
      font-style: italic;
      text-align: center;
      margin-top: 6px;
    }
  "))),
  
  # Title bar
  div(class = "main-title",
      "\U0001F4CD NYC Air Quality Dashboard",
      tags$span(style = "font-size:0.85rem; font-weight:400; margin-left:16px; opacity:0.85;",
                "by Luobing Wang & Chenchong Xia | SYSEN 5460")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      tags$p(class = "sidebar-panel-title", "\u2699\ufe0f Controls"),
      tags$hr(),
      
      radioButtons("tab", "View:",
                   choices = c(
                     "\U0001F4CA Overview"          = "overview",
                     "\U0001F4C8 Temporal Analysis" = "temporal",
                     "\U0001F5FA\ufe0f Spatial Analysis"  = "spatial"
                   ),
                   selected = "overview"),
      tags$hr(),
      
      selectInput(
        inputId  = "year_range",
        label    = tags$b("Select Year(s):"),
        choices  = c(2018, 2019, 2024, 2025),
        selected = c(2024, 2025),
        multiple = TRUE
      ),
      tags$hr(),
      
      uiOutput("county_selector"),
      tags$hr(),
      
      tags$small(
        style = "color:#555;",
        tags$b("Note: "), "All concentrations are in micrograms per cubic meter (μg/m³).",
        tags$br(),
        "Data available for 2018, 2019, 2024, and 2025 only."
      )
    ),
    
    mainPanel(
      width = 9,
      
      # ── Overview ──────────────────────────────────────
      conditionalPanel("input.tab == 'overview'",
                       tags$h4(class = "section-header", "Key Metrics"),
                       div(class = "analysis-text", textOutput("summary_sentence")),
                       div(class = "analysis-text", textOutput("data_note")),
                       br(),
                       tags$h5(class = "section-header", "Detailed Statistics"),
                       fluidRow(
                         value_box(
                           title    = "Mean PM2.5",
                           value    = textOutput("text_mean"),
                           showcase = bs_icon("calculator-fill"),
                           class    = "bg-primary"
                         ),
                         value_box(
                           title    = "Standard Error",
                           value    = textOutput("text_se"),
                           showcase = bs_icon("plus-slash-minus"),
                           class    = "bg-success"
                         ),
                         value_box(
                           title    = "Observations (N)",
                           value    = textOutput("text_n"),
                           showcase = bs_icon("database-fill"),
                           class    = "bg-info"
                         )
                       )
      ),
      
      # ── Temporal ──────────────────────────────────────
      conditionalPanel("input.tab == 'temporal'",
                       tags$h4(class = "section-header", "Hourly PM2.5 Time Series"),
                       plotlyOutput("ts_plot", height = "340px"),
                       br(),
                       tags$h4(class = "section-header", "Monthly Average Trend"),
                       plotlyOutput("monthly_trend", height = "340px"),
                       br(),
                       div(class = "analysis-text", textOutput("trend_text"))
      ),
      
      # ── Spatial ───────────────────────────────────────
      conditionalPanel("input.tab == 'spatial'",
                       tags$h4(class = "section-header", "Proximity Analysis"),
                       div(class = "analysis-text", textOutput("spatial_summary_sentence")),
                       
                       tags$h4(class = "section-header", "Spatial Interpolation"),
                       div(class = "analysis-text", textOutput("interpolation_insight_text")),
                       
                       tags$h4(class = "section-header", "Hotspot Identification"),
                       div(class = "analysis-text", textOutput("hotspot_analysis_text")),
                       
                       br(),
                       fluidRow(
                         value_box(
                           value    = textOutput("concentration_diff_value"),
                           title    = "Congestion vs Remote PM2.5 Difference",
                           showcase = bs_icon("geo-alt-fill"),
                           class    = "bg-warning"
                         )
                       ),
                       
                       br(),
                       tags$h4(class = "section-header",
                               "Spatial Regression: Distance to Congestion Zone vs PM2.5"),
                       fluidRow(
                         column(6, plotOutput("spatial_scatter", height = "380px")),
                         column(6, leafletOutput("spatial_map",   height = "380px"))
                       ),
                       tags$p(class = "caption-text",
                              "Left: Regression of PM2.5 on distance to congestion zone. ",
                              "Right: Interactive map of mean PM2.5 per monitoring site.")
      )
    )
  )
)

# Server -------------------------------------------------
server <- function(input, output, session) {
  
  ## Load data ---------------------------------
  raw_data <- readRDS("merged_air_quality_sf.rds") %>%
    mutate(
      datetime    = lubridate::ymd_hms(datetime, tz = "UTC"),
      county_id   = as.character(county),
      county_name = dplyr::recode(as.character(county),
                                  !!!county_lookup,
                                  .default = as.character(county))
    )
  
  ## County selector UI ------------------------
  output$county_selector <- renderUI({
    ids_in_data <- sort(unique(raw_data$county_id))
    ids_in_data <- ids_in_data[!is.na(ids_in_data) & ids_in_data != ""]
    
    choice_names  <- ifelse(ids_in_data %in% names(county_lookup),
                            county_lookup[ids_in_data],
                            ids_in_data)
    choices_named <- setNames(ids_in_data, choice_names)
    
    tagList(
      tags$b("Filter by County:"),
      checkboxGroupInput(
        inputId  = "selected_counties",
        label    = NULL,
        choices  = choices_named,
        selected = ids_in_data
      )
    )
  })
  
  ## processed_data() --------------------------
  processed_data <- reactive({
    req(input$selected_counties)
    
    raw_data %>%
      mutate(
        pollutant = trimws(pollutant),
        unit      = trimws(unit)
      ) %>%
      filter(
        pollutant %in% c("PM2.5", "pm2.5"),
        unit %in% c("UG/M3", "ug/m3", "µg/m³"),
        !is.na(value),
        year %in% input$year_range,
        !is.na(datetime),
        county_id %in% input$selected_counties
      ) %>%
      mutate(
        display_value = value,
        unit_label    = "μg/m³"
      )
  })
  
  ## spatial_analysis() ------------------------
  spatial_analysis <- reactive({
    df_tbl <- processed_data() %>%
      st_drop_geometry() %>%
      filter(!is.na(dist))
    
    if (nrow(df_tbl) == 0)
      return(list(congestion_avg = NA, remote_avg = NA, diff_value = NA))
    
    congestion_avg <- df_tbl %>%
      filter(dist <= 50) %>%
      summarise(avg = mean(display_value, na.rm = TRUE)) %>%
      pull(avg)
    
    remote_avg <- df_tbl %>%
      filter(dist > 50) %>%
      summarise(avg = mean(display_value, na.rm = TRUE)) %>%
      pull(avg)
    
    list(
      congestion_avg = congestion_avg,
      remote_avg     = remote_avg,
      diff_value     = congestion_avg - remote_avg
    )
  })
  
  ## spatial_interpolation_analysis() ----------
  spatial_interpolation_analysis <- reactive({
    df <- processed_data()
    
    sites_mean <- df %>%
      filter(!is.na(display_value), !st_is_empty(geometry)) %>%
      group_by(aqs_id_full) %>%
      summarise(display_value = mean(display_value, na.rm = TRUE), .groups = "drop")
    
    if (nrow(sites_mean) < 3)
      return(list(error = "Insufficient monitoring sites for interpolation analysis (need ≥ 3)."))
    
    sites_m     <- st_transform(sites_mean, 3857)
    coords      <- st_coordinates(sites_m)
    values      <- sites_m$display_value
    dist_matrix <- as.matrix(dist(coords))
    
    mean_distance <- mean(dist_matrix[upper.tri(dist_matrix)], na.rm = TRUE)
    mean_conc     <- mean(values, na.rm = TRUE)
    cv            <- ifelse(mean_conc != 0,
                            sqrt(var(values, na.rm = TRUE)) / abs(mean_conc), 0)
    
    n <- nrow(sites_m)
    dist_vec <- numeric()
    diff_vec <- numeric()
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        d <- dist_matrix[i, j]
        v <- abs(values[i] - values[j])
        if (!is.na(d) && !is.na(v) && d > 0) {
          dist_vec <- c(dist_vec, d)
          diff_vec <- c(diff_vec, v)
        }
      }
    }
    
    spatial_corr <- if (length(dist_vec) > 2)
      cor(dist_vec, diff_vec, use = "complete.obs") else 0
    
    list(
      mean_distance       = mean_distance,
      concentration_cv    = cv,
      spatial_correlation = spatial_corr,
      n_sites             = nrow(sites_m)
    )
  })
  
  ## spatial_hotspot_analysis() ----------------
  spatial_hotspot_analysis <- reactive({
    sites_mean <- processed_data() %>%
      filter(!is.na(display_value), !st_is_empty(geometry)) %>%
      group_by(aqs_id_full) %>%
      summarise(display_value = mean(display_value, na.rm = TRUE), .groups = "drop")
    
    if (nrow(sites_mean) < 3)
      return(list(error = "Insufficient monitoring sites for hotspot analysis (need ≥ 3)."))
    
    values     <- sites_mean$display_value
    mean_val   <- mean(values, na.rm = TRUE)
    sd_val     <- sd(values, na.rm = TRUE)
    threshold  <- mean_val + sd_val
    n_hotspots <- sum(values > threshold, na.rm = TRUE)
    n_total    <- length(values)
    
    list(
      mean_concentration = mean_val,
      sd_concentration   = sd_val,
      threshold          = threshold,
      n_hotspots         = n_hotspots,
      n_total            = n_total,
      hotspot_percentage = round(n_hotspots / n_total * 100, 1)
    )
  })
  
  ## Outputs — spatial text --------------------
  output$spatial_summary_sentence <- renderText({
    res <- spatial_analysis()
    if (is.na(res$diff_value)) {
      "No valid spatial data found for the selected years and counties."
    } else {
      paste0(
        "Sites within 50 m of congestion areas averaged ",
        round(res$congestion_avg, 2), " μg/m³, compared to ",
        round(res$remote_avg, 2), " μg/m³ for sites beyond 50 m. ",
        "Difference: ", round(res$diff_value, 2), " μg/m³."
      )
    }
  })
  
  output$interpolation_insight_text <- renderText({
    res <- spatial_interpolation_analysis()
    if (!is.null(res$error)) return(res$error)
    
    heterogeneity <- ifelse(res$concentration_cv > 0.3, "high",
                            ifelse(res$concentration_cv > 0.15, "moderate", "low"))
    dependency    <- ifelse(abs(res$spatial_correlation) > 0.3, "moderate",
                            ifelse(abs(res$spatial_correlation) > 0.1, "weak", "minimal"))
    
    paste0(
      "Average inter-site distance: ", round(res$mean_distance / 1000, 1), " km. ",
      "Concentration CV: ", round(res$concentration_cv, 2),
      " (", heterogeneity, " spatial heterogeneity). ",
      "Spatial correlation: ", round(res$spatial_correlation, 2),
      " (", dependency, " spatial dependency) across ",
      res$n_sites, " monitoring sites."
    )
  })
  
  output$hotspot_analysis_text <- renderText({
    res <- spatial_hotspot_analysis()
    if (!is.null(res$error)) return(res$error)
    
    spread <- ifelse(res$hotspot_percentage > 25, "widespread",
                     ifelse(res$hotspot_percentage > 10, "moderate", "limited"))
    
    paste0(
      "Threshold: mean + 1 SD = ", round(res$threshold, 2), " μg/m³. ",
      res$n_hotspots, " of ", res$n_total,
      " sites (", res$hotspot_percentage, "%) classified as hotspots — ",
      spread, " spatial clustering."
    )
  })
  
  output$concentration_diff_value <- renderText({
    res <- spatial_analysis()
    if (is.na(res$diff_value)) "N/A" else paste0(round(res$diff_value, 2), " μg/m³")
  })
  
  ## summary_stats() ---------------------------
  summary_stats <- reactive({
    data  <- processed_data() %>% st_drop_geometry()
    n_val <- nrow(data)
    if (n_val == 0) return(NULL)
    
    list(
      mean = round(mean(data$display_value, na.rm = TRUE), 1),
      se   = round(sd(data$display_value,   na.rm = TRUE) / sqrt(n_val), 2),
      n    = n_val
    )
  })
  
  output$summary_sentence <- renderText({
    s <- summary_stats()
    if (is.null(s))
      "No valid PM2.5 data found for the selected years and counties."
    else
      paste0("Average PM2.5: ", s$mean, " μg/m³  (SE = ", s$se, " μg/m³)")
  })
  
  output$data_note <- renderText({
    yrs <- sort(unique(processed_data() %>% st_drop_geometry() %>% pull(year)))
    if (length(yrs) == 0) "No data available."
    else paste("Years included:", paste(yrs, collapse = ", "))
  })
  
  output$text_mean <- renderText({
    s <- summary_stats()
    if (is.null(s)) "N/A" else paste0(s$mean, " μg/m³")
  })
  output$text_se <- renderText({
    s <- summary_stats()
    if (is.null(s)) "N/A" else paste0(s$se, " μg/m³")
  })
  output$text_n <- renderText({
    s <- summary_stats()
    if (is.null(s)) "0" else format(s$n, big.mark = ",")
  })
  
  ## output$ts_plot ----------------------------
  output$ts_plot <- renderPlotly({
    data <- processed_data() %>% st_drop_geometry()
    if (nrow(data) == 0 || all(is.na(data$datetime))) return(NULL)
    
    data <- data %>%
      mutate(
        label = paste0(
          "Monitor: ", aqs_id_full, "<br>",
          "PM2.5: ", round(display_value, 2), " μg/m³<br>",
          "County: ", county_name, "<br>",
          "Time: ", format(datetime, "%Y-%m-%d %H:%M", tz = "UTC")
        )
      )
    
    p <- ggplot(data, aes(x = datetime, y = display_value,
                          group = aqs_id_full, text = label)) +
      geom_line(alpha = 0.55, color = "#2C7BB6") +
      labs(x = "Date / Time (UTC)", y = "PM2.5 (μg/m³)",
           title = "Hourly PM2.5 — All Selected Sites") +
      scale_x_datetime(
        limits = c(min(data$datetime, na.rm = TRUE),
                   max(data$datetime, na.rm = TRUE)),
        expand = c(0.005, 0.005)
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "label")
  })
  
  ## monthly_data() ----------------------------
  monthly_data <- reactive({
    processed_data() %>%
      st_drop_geometry() %>%
      group_by(year, month) %>%
      summarise(mean_pm25 = mean(display_value, na.rm = TRUE), .groups = "drop") %>%
      arrange(year, month) %>%
      mutate(
        date_month = lubridate::make_date(year, month, 1),
        time_index = row_number()
      )
  })
  
  ## output$monthly_trend ----------------------
  output$monthly_trend <- renderPlotly({
    data <- monthly_data()
    if (nrow(data) < 2) return(NULL)
    
    model <- lm(mean_pm25 ~ time_index, data = data)
    pred  <- predict(model, interval = "confidence")
    data$fit <- pred[, "fit"]
    data$lwr <- pred[, "lwr"]
    data$upr <- pred[, "upr"]
    
    p <- ggplot(data, aes(x = date_month)) +
      geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "#d9534f", alpha = 0.15) +
      geom_line(aes(y = fit), color = "#d9534f", linewidth = 1) +
      geom_point(aes(y = mean_pm25), color = "#2C7BB6", size = 2.5) +
      labs(x = "Month", y = "Mean PM2.5 (μg/m³)",
           title = "Monthly Mean PM2.5 with Linear Trend (95% CI)") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 40, hjust = 1))
    
    ggplotly(p)
  })
  
  ## output$trend_text -------------------------
  output$trend_text <- renderText({
    data <- monthly_data()
    if (nrow(data) < 2)
      return("Need at least 2 months of data to compute a linear trend.")
    
    ct        <- summary(lm(mean_pm25 ~ time_index, data = data))$coefficients
    slope_est <- ct["time_index", "Estimate"]
    slope_p   <- ct["time_index", "Pr(>|t|)"]
    
    if (slope_p < 0.05) {
      dir <- if (slope_est > 0) "significant increasing" else "significant decreasing"
      paste0("Linear trend: ", dir, " (slope = ",
             format(abs(slope_est), digits = 3), " μg/m³/month, p < 0.05).")
    } else {
      "Linear trend: No statistically significant trend detected (p ≥ 0.05)."
    }
  })
  
  ## output$spatial_scatter --------------------
  output$spatial_scatter <- renderPlot({
    agg <- processed_data() %>%
      st_drop_geometry() %>%
      filter(!is.na(dist)) %>%
      group_by(aqs_id_full) %>%
      summarise(
        mean_pm25 = mean(display_value, na.rm = TRUE),
        dist_km   = mean(dist, na.rm = TRUE) / 1000,
        .groups   = "drop"
      )
    
    if (nrow(agg) < 2) return(NULL)
    
    ggplot(agg, aes(x = dist_km, y = mean_pm25)) +
      geom_point(color = "#2C7BB6", alpha = 0.75, size = 3.5) +
      geom_smooth(method = "lm", color = "#d9534f", fill = "#f5c6c6", se = TRUE) +
      labs(
        title = "PM2.5 vs Distance to Congestion Zone",
        x     = "Distance to Congestion Zone (km)",
        y     = "Mean PM2.5 (μg/m³)"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold", size = 13))
  })
  
  ## output$spatial_map ------------------------
  output$spatial_map <- renderLeaflet({
    data <- processed_data()
    if (nrow(data) == 0) return(NULL)
    
    map_data <- data %>%
      group_by(aqs_id_full) %>%
      summarise(
        mean_pm25    = mean(display_value, na.rm = TRUE),
        county_label = first(county_name),
        geometry     = first(geometry),
        .groups      = "drop"
      ) %>%
      st_sf() %>%
      st_transform(4326)
    
    pal <- colorBin("RdYlGn", domain = map_data$mean_pm25,
                    reverse = TRUE, bins = 6)
    
    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        radius      = ~pmax(sqrt(mean_pm25) * 2, 4),
        color       = "white",
        weight      = 1.5,
        fillColor   = ~pal(mean_pm25),
        fillOpacity = 0.85,
        popup       = ~paste0(
          "<b>", county_label, "</b><br>",
          "AQS ID: ", aqs_id_full, "<br>",
          "Mean PM2.5: <b>", round(mean_pm25, 2), " μg/m³</b>"
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = ~mean_pm25,
                title = "Mean PM2.5<br>(μg/m³)", opacity = 0.9)
  })
}

# Run app
shinyApp(ui = ui, server = server)