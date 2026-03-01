# Load required libraries
library(shiny)
library(bslib)
library(readr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(scales)
library(ggpubr)
library(plotly)
library(patchwork)

# Mobile-friendly: viewport + responsive plot and touch targets
# Script sends window width so server can stack plots on narrow screens (shareY kept)
mobile_head <- tags$head(
  tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, maximum-scale=1"),
  tags$script(HTML("
    function sendWidth() {
      if (typeof Shiny !== 'undefined')
        Shiny.setInputValue('plot_width', window.innerWidth, {priority: 'event'});
    }
    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', sendWidth);
    } else {
      sendWidth();
    }
    window.addEventListener('resize', sendWidth);
  ")),
  tags$style(HTML("
    /* Responsive plot container */
    #snowPlot-container { width: 100%; min-height: 320px; }
    @media (max-width: 768px) {
      .shiny-plot-output, .plotly { height: min(70vh, 500px) !important; min-height: 280px !important; }
      .form-group label { font-size: 1rem; }
      .checkbox { min-height: 28px; padding: 6px 0; }
      .shiny-input-container { padding: 4px 0; }
      .btn { min-height: 44px; padding: 10px 16px; font-size: 1rem; }
      .form-control { min-height: 44px; font-size: 16px; }
    }
  "))
)

# Define UI (bslib: collapsible sidebar on mobile)
ui <- page_sidebar(
  title = "Snowfall by Season",
  mobile_head,
  sidebar = sidebar(
    position = "left",
    open = "desktop",
    helpText("Snow accumulation (in.) from October 1st for each winter season"),
    h4("City"),
    uiOutput("city_ui"),
    h4("Highlighted Seasons"),
    uiOutput("highlight_controls"),
    h4("Statistical Overlays (from full dataset)"),
    checkboxInput("show_median", "Show Median", value = FALSE),
    checkboxInput("show_mean", "Show Mean", value = TRUE),
    checkboxInput("show_min_max", "Show Min/Max", value = FALSE),
    checkboxInput("show_ci_50", "Show 50% Confidence Interval", value = FALSE),
    checkboxInput("show_ci_95", "Show 95% Confidence Interval", value = FALSE),
    sliderInput("season_range", "Season Range:",
                min = 1936, max = as.numeric(year(Sys.Date())),
                value = c(1936, as.numeric(year(Sys.Date()))), step = 1, sep = ""),
    downloadButton("downloadPlot", "Download Plot")
  ),
  div(
    id = "snowPlot-container",
    plotlyOutput("snowPlot", height = "70vh")
  ),
  br(),
  textOutput("dataSource")
)

# Per-city y-axis max (inches) for readable scales; default for unknown cities
CITY_YMAX <- c(
  "Boston, MA" = 115, "Buffalo, NY" = 120, "Chicago, IL" = 60, "Cleveland, OH" = 70,
  "Denver, CO" = 80, "Detroit, MI" = 55, "Milwaukee, WI" = 80,
  "Minneapolis-St Paul, MN" = 90, "New York, NY" = 35, "Philadelphia, PA" = 65,
  "Pittsburgh, PA" = 55, "Raleigh, NC" = 25, "Salt Lake City, UT" = 90,
  "Seattle, WA" = 25, "Washington, D.C." = 40
)
DEFAULT_YMAX <- 80

# Define server logic
server <- function(input, output, session) {

  # Debounce plot width so the plot doesn't keep re-rendering on load/resize
  plot_width_debounced <- debounce(reactive(input$plot_width), millis = 400)

  # Raw data: load CSV; prefer multicity so dropdown has all cities
  raw_snow_data <- reactive({
    data_url <- Sys.getenv("SNOW_DATA_URL", "")
    if (!nzchar(data_url)) {
      data_url <- "https://raw.githubusercontent.com/marcohamins/Snowfall/main/BostonSnow/snow_multicity.csv"
    }
    data_path <- "snow_multicity.csv"
    data_path_legacy <- "USW00014739_2_24_25.csv"
    tryCatch({
      if (nzchar(data_url)) {
        snowfalldata <- read_csv(data_url, show_col_types = FALSE)
      } else if (file.exists(data_path)) {
        snowfalldata <- read_csv(data_path, show_col_types = FALSE)
      } else {
        snowfalldata <- read_csv(data_path_legacy, show_col_types = FALSE)
      }
      snowfalldata$DATE <- as.Date(snowfalldata$DATE)
      if (!"CITY" %in% names(snowfalldata)) {
        snowfalldata$CITY <- "Boston, MA"
      }
      snowfalldata
    }, error = function(e) {
      data_path <- "snow_multicity.csv"
      data_path_legacy <- "USW00014739_2_24_25.csv"
      if (file.exists(data_path)) {
        snowfalldata <- read_csv(data_path, show_col_types = FALSE)
      } else if (file.exists(data_path_legacy)) {
        snowfalldata <- read_csv(data_path_legacy, show_col_types = FALSE)
      } else {
        warning("Sample data used. Set SNOW_DATA_URL or ensure snow_multicity.csv (or USW00014739_2_24_25.csv) in app directory.")
        start_date <- as.Date("2010-01-01")
        end_date <- Sys.Date()
        num_days <- as.numeric(difftime(end_date, start_date, units = "days")) + 1
        snowfalldata <- data.frame(
          STATION = "USW00014739",
          DATE = seq.Date(start_date, end_date, by = "day"),
          LATITUDE = 42.36, LONGITUDE = -71.01, ELEVATION = 3.2,
          NAME = "BOSTON LOGAN INTERNATIONAL AIRPORT, MA US",
          CITY = "Boston, MA",
          PRCP = runif(num_days, 0, 1) * (runif(num_days) > 0.8),
          SNOW = runif(num_days, 0, 4) * (runif(num_days) > 0.9),
          SNWD = 0, TMAX = NA_real_, TMIN = NA_real_
        )
      }
      snowfalldata$DATE <- as.Date(snowfalldata$DATE)
      if (!"CITY" %in% names(snowfalldata)) snowfalldata$CITY <- "Boston, MA"
      snowfalldata
    })
  })

  # City dropdown: choices from data (or single city for legacy)
  output$city_ui <- renderUI({
    raw <- raw_snow_data()
    if (is.null(raw) || nrow(raw) == 0) return(selectInput("city", "City", choices = "Boston, MA", selected = "Boston, MA"))
    cities <- sort(unique(raw$CITY))
    selectInput("city", "City", choices = cities, selected = if (!is.null(input$city) && input$city %in% cities) input$city else cities[1])
  })

  # Read and process data (from URL if SNOW_DATA_URL set, else local CSV)
  snowfall_data <- reactive({
    raw <- raw_snow_data()
    req(raw, input$city)
    snowfalldata <- raw %>% filter(CITY == input$city)
    if (nrow(snowfalldata) == 0) {
      snowfalldata <- raw
      if (nrow(snowfalldata) == 0) return(NULL)
    }
    tryCatch({
      max_date <- max(snowfalldata$DATE, na.rm = TRUE)
      yr <- as.numeric(format(max_date, "%Y"))
      mo <- as.numeric(format(max_date, "%m"))
      if (mo >= 7) {
        year <- as.character(yr + 1)
        preyear <- as.character(yr)
      } else {
        year <- as.character(yr)
        preyear <- as.character(yr - 1)
      }
    }, error = function(e) {
      preyear <- as.character(as.numeric(format(Sys.Date(), "%Y")) - 1)
      year <- format(Sys.Date(), "%Y")
    })

    currentYearRange <- paste0(preyear, "-", year)
    preYearRange <- paste0(as.numeric(preyear) - 1, "-", as.numeric(year) - 1)
    if (!"year" %in% colnames(snowfalldata)) {
      snowfalldata$year <- year(snowfalldata$DATE)
    }
    processed_data <- snowfalldata %>%
      mutate(SNOW = ifelse(is.na(SNOW), 0, SNOW)) %>%
      mutate(doy = yday(DATE)) %>%
      mutate(sameyeardate = as.Date("2021-12-31") + doy) %>%
      mutate(daysinceOct1 = ifelse(sameyeardate < as.Date("2022-09-30"),
                                   as.Date(sameyeardate) - as.Date("2021-09-30"),
                                   as.Date(sameyeardate) - as.Date("2022-09-30"))) %>%
      mutate(datesinceOct1 = as.Date("2021-09-30") + daysinceOct1) %>%
      mutate(yearRange = ifelse(sameyeardate < as.Date("2022-09-30"),
                                paste0(as.character(as.numeric(year) - 1), "-", year),
                                paste0(year, "-", as.character(as.numeric(year) + 1)))) %>%
      group_by(yearRange) %>%
      mutate(cum_sum = cumsum(SNOW)) %>%
      mutate(cum_sum_prec = cumsum(PRCP)) %>%
      mutate(cum_sum = if_else(datesinceOct1 <= 1, 0, cum_sum)) %>%
      mutate(impYears = ifelse(yearRange == currentYearRange, currentYearRange,
                               ifelse(yearRange == "2014-2015", "2014-2015", "Historical data"))) %>%
      mutate(impYears2 = ifelse(yearRange == currentYearRange, currentYearRange,
                                 ifelse(yearRange == preYearRange, preYearRange, "Historical data"))) %>%
      mutate(iscurrentyear = ifelse(yearRange == currentYearRange, currentYearRange, "Historical data")) %>%
      mutate(snowYear = as.numeric(substr(yearRange, 6, 9)) * 1.0)
    return(processed_data)
  })

  # Dynamic labels for current/previous season checkboxes (e.g. "2025-2026", "2024-2025")
  output$highlight_controls <- renderUI({
    data <- snowfall_data()
    all_years <- unique(data$yearRange)
    if (length(all_years) == 0) {
      return(tagList(
        checkboxInput("highlight_current", "Current year", value = TRUE),
        checkboxInput("highlight_previous", "Previous year", value = TRUE)
      ))
    }
    current <- all_years[which.max(as.numeric(substr(all_years, 6, 9)))]
    prev_idx <- which.max(as.numeric(substr(all_years, 6, 9))) - 1
    previous <- if (prev_idx >= 1) all_years[prev_idx] else all_years[1]
    tagList(
      checkboxInput("highlight_current", current, value = TRUE),
      checkboxInput("highlight_previous", previous, value = TRUE)
    )
  })
  
  add_plotting_elements <- reactive({
    data <- snowfall_data()
    
    all_years <- unique(data$yearRange)
    
    
    current_year <- all_years[which.max(as.numeric(substr(all_years, 6, 9)))]
    prev_idx <- which.max(as.numeric(substr(all_years, 6, 9))) - 1
    prev_year <- ifelse(prev_idx > 0 && prev_idx <= length(all_years),
                        all_years[prev_idx], 
                        all_years[1])
    
    # Prepare data based on user selections
    if (input$highlight_current && input$highlight_previous) {
      data <- data %>%
        mutate(highlight = ifelse(yearRange == current_year, "Current Year",
                                  ifelse(yearRange == prev_year, "Previous Year", "Historical")))
      data$highlight = factor(data$highlight,levels = c("Historical","Previous Year","Current Year"))
    } else if (input$highlight_current) {
      data <- data %>%
        mutate(highlight = ifelse(yearRange == current_year, "Current Year", "Historical"))
      data$highlight = factor(data$highlight,levels = c("Historical","Current Year"))
    } else if (input$highlight_previous) {
      data <- data %>%
        mutate(highlight = ifelse(yearRange == prev_year, "Previous Year", "Historical"))
      data$highlight = factor(data$highlight,levels = c("Historical","Previous Year"))
    } else {
      # If no highlights selected, still need the column for plotting
      data <- data %>%
        mutate(highlight = "Historical")
    }
    
    # Adjust line sizes and opacity based on user input
    data <- data %>%
      mutate(
        line_alpha = case_when(
          highlight == "Current Year" ~ 1.0,
          highlight == "Previous Year" ~ 0.8,
          TRUE ~ 0.4
        )
      )
    
  })
  
  
  filtered_data <- reactive({
    data <- add_plotting_elements()
    
    # Filter by season range
    data %>%
      filter(snowYear >= input$season_range[1] & snowYear <= input$season_range[2])
  })
  
  # Create the plot
  snowPlot <- reactive({
    # Get data
    data <- add_plotting_elements()
    data_subset <- filtered_data()
    
    # Check if data exists and has required columns
    req(data)
    req("yearRange" %in% colnames(data))
    req("datesinceOct1" %in% colnames(data))
    req("cum_sum" %in% colnames(data))
    
    # Determine current and previous years
    all_years <- unique(data$yearRange)
    if(length(all_years) == 0) {
      return(ggplot() + 
               geom_text(aes(x = 0, y = 0), label = "No data available") + 
               theme_minimal())
    }
    
    
    # Smaller axis font on mobile; when stacked, second plot gets its own y-axis
    w <- plot_width_debounced()
    is_mobile <- !is.null(w) && w < 768
    base_main <- if (is_mobile) 11 else 16
    base_pt <- if (is_mobile) 12 else 20

    # Per-city y-axis max for readable scale
    y_max <- if (!is.null(input$city) && input$city %in% names(CITY_YMAX)) CITY_YMAX[[input$city]] else DEFAULT_YMAX

    # Create base plot
    p <- ggplot(data_subset, aes(x = datesinceOct1,
                          group = yearRange, 
                          text = paste("Year:", yearRange, 
                                       "<br>Date:", format(datesinceOct1, "%b %d"),
                                       "<br>Snow:", round((cum_sum/10)*0.393701, 1), "in"))) +
      scale_size_identity() +
      scale_alpha_identity() +
      labs(x = "Month", y = "Snow accumulation (in.)") +
      scale_x_date(date_labels = "%b", 
                   limits = c(as.Date("2021-10-01"), as.Date("2022-05-30")),
                   breaks = "1 months") +
      ylim(c(0, y_max)) +
      theme_pubr(base_size = base_main)
    
    p1 <- data_subset %>% group_by(snowYear,highlight) %>% 
      summarise(maxsnow = (max(cum_sum)/10)*0.393701, .groups="drop") %>% 
      ggplot(mapping=aes(x=snowYear, y=maxsnow,
                         text = paste0("Season ending: ", snowYear,
                                       "<br>Total snow: ", round(maxsnow, 1), " in"))) +
      xlab("Year") +
      coord_cartesian(xlim = c(1934,as.numeric(year(Sys.Date()))), ylim = c(0, y_max)) +
      theme_pubr(base_size = base_pt) +
      guides(col=guide_legend("")) 
    
    # Add color scale: explicit mapping so Current/Previous Year stay red and blue (not grey)
    if (input$highlight_current && input$highlight_previous) {
      p <- p + 
        geom_line(mapping = aes(y = (cum_sum/10)*0.393701, color = highlight, alpha = line_alpha)) +
        scale_color_manual(values = c("Historical" = "gray", "Previous Year" = "red", "Current Year" = "blue", "Mean" = "black")) +
        guides(col = guide_legend(""))
    } else if (input$highlight_current && !input$highlight_previous) {
      p <- p + geom_line(mapping = aes(y = (cum_sum/10)*0.393701, color = highlight, alpha = line_alpha)) +
        scale_color_manual(values = c("Historical" = "gray", "Current Year" = "blue", "Mean" = "black")) +
        guides(col = guide_legend(""))
    } else if (input$highlight_previous && !input$highlight_current) {
      p <- p + geom_line(mapping = aes(y = (cum_sum/10)*0.393701, color = highlight, alpha = line_alpha)) +
        scale_color_manual(values = c("Historical" = "gray", "Previous Year" = "red", "Mean" = "black")) +
        guides(col = guide_legend(""))
    } else {
      p <- p + geom_line(mapping=aes(y = (cum_sum/10)*0.393701, color = as.numeric(snowYear), alpha = line_alpha)) +
        scale_color_viridis_c(name = "Year",option = "B") +
        guides(alpha = "none")
    }
    
    # Apply consistent color scheme based on user selections
    if (input$highlight_current && input$highlight_previous) {
      p1 <- p1 + geom_point(aes(col=highlight)) + scale_color_manual(values = c("gray", "red", "blue"))
    } else if (input$highlight_current && !input$highlight_previous) {
      p1 <- p1 + geom_point(aes(col=highlight)) + scale_color_manual(values = c(
        if (input$highlight_current) "gray" else "gray", 
        "blue"
      ))
    } else if (input$highlight_previous && !input$highlight_current) {
      p1 <- p1 + geom_point(aes(col=highlight)) + scale_color_manual(values = c(
        if (input$highlight_current) "gray" else "gray", 
        "red"
      ))
    }else {
      p1 <- p1 + geom_point(aes(col=snowYear)) + scale_color_viridis_c(name = "Year",option = "B") +
        guides(alpha = "none")
    }
    # No legend for points plot. Both plots show y-axis; second plot label is end-of-season total
    p1 <- p1 + theme(legend.position = "none") +
      ylab("End of season total snowfall (in.)")
    
    # Calculate statistics for each day across all years
    stats_data <- data %>%
      filter(datesinceOct1 >= as.Date("2021-10-01") & 
               datesinceOct1 <= as.Date("2022-05-30")) %>%
      group_by(datesinceOct1) %>%
      summarise(
        mean_snow = mean((cum_sum/10)*0.393701, na.rm = TRUE),
        median_snow = median((cum_sum/10)*0.393701, na.rm = TRUE),
        min_snow = min((cum_sum/10)*0.393701, na.rm = TRUE),
        max_snow = max((cum_sum/10)*0.393701, na.rm = TRUE),
        ci_lower50 = quantile((cum_sum/10)*0.393701, 0.25, na.rm = TRUE),
        ci_upper50 = quantile((cum_sum/10)*0.393701, 0.75, na.rm = TRUE),
        ci_lower95 = quantile((cum_sum/10)*0.393701, 0.025, na.rm = TRUE),
        ci_upper95 = quantile((cum_sum/10)*0.393701, 0.975, na.rm = TRUE)
      ) |> 
      arrange(datesinceOct1)
    
    # Add statistical overlays based on user selections
    if (input$show_mean) {
      p <- p + geom_line(data = stats_data,
                         aes(x = datesinceOct1, y = mean_snow, group = 1, color = "Mean"),
                         size = 1.2, linetype = "solid", inherit.aes = FALSE)
      p1 <- p1 + geom_hline(data = stats_data[stats_data$datesinceOct1 == max(stats_data$datesinceOct1),], 
                         aes(yintercept = mean_snow,group = 1),
                         color = "black", size = 1.2, linetype = "solid", inherit.aes = FALSE)
    }
    
    if (input$show_median) {
      p <- p + geom_line(data = stats_data, 
                         aes(x = datesinceOct1, y = median_snow,group = 1),
                         color = "black", size = 1.2, linetype = "dashed", inherit.aes = FALSE)
      
      p1 <- p1 + geom_hline(data = stats_data[stats_data$datesinceOct1 == max(stats_data$datesinceOct1),], 
                            aes(yintercept = median_snow,group = 1),
                            color = "black", size = 1.2, linetype = "solid", inherit.aes = FALSE)
    }
    
    if (input$show_min_max) {
      p <- p + geom_line(data = stats_data, 
                         aes(x = datesinceOct1, y = min_snow,group = 1),
                         color = "darkblue", size = 0.8, linetype = "dotted", inherit.aes = FALSE) +
        geom_line(data = stats_data, 
                  aes(x = datesinceOct1, y = max_snow,group = 1),
                  color = "darkblue", size = 0.8, linetype = "dotted", inherit.aes = FALSE)
      
      p1 <- p1 + geom_hline(data = stats_data[stats_data$datesinceOct1 == max(stats_data$datesinceOct1),], 
                         aes(yintercept = min_snow,group = 1),
                         color = "darkblue", size = 0.8, linetype = "dotted", inherit.aes = FALSE) +
        geom_hline(data = stats_data[stats_data$datesinceOct1 == max(stats_data$datesinceOct1),], 
                  aes(yintercept = max_snow,group = 1),
                  color = "darkblue", size = 0.8, linetype = "dotted", inherit.aes = FALSE)
    }
    
    if (input$show_ci_95) {
      p <- p + 
        geom_line(data = stats_data, 
                  aes(x = datesinceOct1, y = ci_upper95, group = 1),
                  color = "lightgray", size = 0.8, linetype = "solid", inherit.aes = FALSE) +
        geom_line(data = stats_data, 
                  aes(x = datesinceOct1, y = ci_lower95, group = 1),
                  color = "lightgray", size = 0.8, linetype = "solid", inherit.aes = FALSE) +
        geom_ribbon(data = stats_data,
                    aes(x = datesinceOct1, 
                        ymin = ci_lower95, ymax = ci_upper95,group=1),
                    fill = "lightgray", alpha = 0.3, inherit.aes = FALSE)
      
      holddf = rbind(stats_data[stats_data$datesinceOct1 == max(stats_data$datesinceOct1),],
            stats_data[stats_data$datesinceOct1 == max(stats_data$datesinceOct1),])
      holddf$snowYear = c(0,3000)
      p1 <- p1 + 
        geom_hline(data = holddf, 
                  aes(yintercept = ci_upper95, group = 1),
                  color = "lightgray", size = 0.8, linetype = "solid", inherit.aes = FALSE) +
        geom_hline(data = holddf, 
                  aes(yintercept = ci_lower95, group = 1),
                  color = "lightgray", size = 0.8, linetype = "solid", inherit.aes = FALSE) +
        geom_ribbon(data = holddf,
                    aes(x = snowYear, 
                        ymin = ci_lower95, ymax = ci_upper95,group=1),
                    fill = "lightgray", alpha = 0.3, inherit.aes = FALSE)
    }
    
    if (input$show_ci_50) {
      p <- p + 
        geom_line(data = stats_data, 
                  aes(x = datesinceOct1, y = ci_upper50, group = 1),
                  color = "darkgray", size = 0.8, linetype = "solid", inherit.aes = FALSE) +
        geom_line(data = stats_data, 
                  aes(x = datesinceOct1, y = ci_lower50, group = 1),
                  color = "darkgray", size = 0.8, linetype = "solid", inherit.aes = FALSE) +
        geom_ribbon(data = stats_data,
                    aes(x = datesinceOct1, 
                        ymin = ci_lower50, ymax = ci_upper50,group=1),
                    fill = "darkgray", alpha = 0.3, inherit.aes = FALSE)
      
      holddf = rbind(stats_data[stats_data$datesinceOct1 == max(stats_data$datesinceOct1),],
                     stats_data[stats_data$datesinceOct1 == max(stats_data$datesinceOct1),])
      holddf$snowYear = c(0,3000)
      p1 <- p1 + 
        geom_hline(data = holddf, 
                   aes(yintercept = ci_upper50, group = 1),
                   color = "lightgray", size = 0.8, linetype = "solid", inherit.aes = FALSE) +
        geom_hline(data = holddf, 
                   aes(yintercept = ci_lower50, group = 1),
                   color = "lightgray", size = 0.8, linetype = "solid", inherit.aes = FALSE) +
        geom_ribbon(data = holddf,
                    aes(x = snowYear, 
                        ymin = ci_lower50, ymax = ci_upper50,group=1),
                    fill = "lightgray", alpha = 0.3, inherit.aes = FALSE)
    }
    
    # Add legend
    p <- p + theme(legend.position = "bottom",
                   legend.title = element_blank())
    
    # Return list of plots (not patchwork) so ggplotly keeps legend on first plot
    return(list(p, p1))
  })
  
  # Render the plot with Plotly for interactivity (with error handling)
  # Update your renderPlotly function
  output$snowPlot <- renderPlotly({
    # Stack plots vertically on narrow screens (mobile), side-by-side on desktop; always share y-axis
    w <- plot_width_debounced()
    nrows <- if (is.null(w) || w >= 768) 1 else 2
    margin <- if (nrows == 2) 0.08 else 0.05

    tryCatch({
      plots <- snowPlot()
      p_main <- plots[[1]]
      p_yearly <- plots[[2]]

      pl_main <- ggplotly(p_main, tooltip = "text")
      pl_yearly <- ggplotly(p_yearly, tooltip = "text")
      n_traces_main <- length(pl_main$x$data)

      out <- subplot(pl_main, pl_yearly, nrows = nrows, shareY = TRUE, margin = margin) %>%
        layout(showlegend = TRUE,
               legend = list(orientation = "h", y = -0.12, x = 0.5, xanchor = "center"),
               margin = list(b = 80))

      for (i in seq_len(n_traces_main)) {
        out$x$data[[i]]$showlegend <- TRUE
      }
      for (i in (n_traces_main + 1):length(out$x$data)) {
        out$x$data[[i]]$showlegend <- FALSE
      }
      out
      
    }, error = function(e) {
      # Return a simple plot with error message
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Error creating plot:", e$message)) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
      
      ggplotly(p)
    })
  })
  
  # Display data source information
  output$dataSource <- renderText({
    data <- snowfall_data()
    if (is.null(data) || nrow(data) == 0) return("Data source: NOAA GHCN Daily")
    station_name <- data$NAME[1]
    paste0("Data source: NOAA ", station_name)
  })
  
  # Download handler for the plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("boston-snowfall-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      tryCatch({
        plots <- snowPlot()
        combined_plot <- plots[[1]] + plots[[2]]
        ggsave(file, plot = combined_plot, device = "png", width = 10, height = 9, dpi = 300)
      }, error = function(e) {
        # Create a simple error message plot if the main plot fails
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "Error generating plot") +
          theme_void() +
          xlim(0, 1) + ylim(0, 1)
        ggsave(file, plot = p, device = "png", width = 10, height = 9, dpi = 300)
      })
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)