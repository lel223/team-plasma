# BikeFlow QC — Cornell Six Sigma Hackathon, Team Plasma
# Phase 2 — outage-duration prioritization and neighborhood statistics
# Synthetic demonstration; no live bikeshare data are used.
# Requires: install.packages("shiny")

library(shiny)

# ---- 1. Reproducible network and 7-day simulated dataset ----
set.seed(5300)
TIMEZONE <- "America/New_York"
INTERVAL_HOURS <- 0.25
N_STATIONS <- 20L
N_INTERVALS <- 7L * 24L * 4L

stations <- data.frame(
  station_id = sprintf("ST-%02d", seq_len(N_STATIONS)),
  neighborhood = rep(c("Residential", "Downtown", "Mixed"), length.out = N_STATIONS),
  capacity = sample(15:30, N_STATIONS, replace = TRUE),
  stringsAsFactors = FALSE
)

times <- seq(
  from = as.POSIXct("2026-10-09 00:00:00", tz = TIMEZONE),
  by = "15 min", length.out = N_INTERVALS
)

simulate_station <- function(id, area, capacity) {
  bikes <- integer(N_INTERVALS)
  current <- round(capacity * runif(1, 0.35, 0.65))
  rebalance <- logical(N_INTERVALS)

  for (i in seq_len(N_INTERVALS)) {
    hour <- as.numeric(format(times[i], "%H", tz = TIMEZONE)) +
      as.numeric(format(times[i], "%M", tz = TIMEZONE)) / 60
    weekday <- as.integer(format(times[i], "%u", tz = TIMEZONE)) <= 5L

    # Explicit simulation assumption: daily scheduled redistribution at 04:00.
    if (hour == 4) {
      current <- max(0L, min(capacity, round(capacity / 2) + sample(-3:3, 1)))
      rebalance[i] <- TRUE
    }

    rentals <- 0.5
    returns <- 0.5
    if (weekday && hour >= 7 && hour < 10) {
      if (area == "Residential") { rentals <- 2.8; returns <- 0.5 }
      if (area == "Downtown")    { rentals <- 0.5; returns <- 2.8 }
      if (area == "Mixed")       { rentals <- 1.6; returns <- 1.1 }
    } else if (weekday && hour >= 16 && hour < 19) {
      if (area == "Residential") { rentals <- 0.5; returns <- 2.8 }
      if (area == "Downtown")    { rentals <- 2.8; returns <- 0.5 }
      if (area == "Mixed")       { rentals <- 1.1; returns <- 1.6 }
    } else if (hour >= 10 && hour < 16) {
      rentals <- 1.1
      returns <- 1.0
    }

    # Rentals cannot exceed bikes; returns cannot exceed open docks.
    # Flows are treated as external arrivals/departures, not closed-system routing.
    taken <- min(current, rpois(1, rentals))
    room_after_rentals <- capacity - (current - taken)
    returned <- min(room_after_rentals, rpois(1, returns))
    current <- current - taken + returned
    bikes[i] <- current
  }

  data.frame(
    timestamp = times,
    interval = seq_len(N_INTERVALS),
    station_id = id,
    neighborhood = area,
    capacity = capacity,
    bikes = bikes,
    docks = capacity - bikes,
    scheduled_rebalance = rebalance,
    stringsAsFactors = FALSE
  )
}

network <- do.call(rbind, lapply(seq_len(nrow(stations)), function(i) {
  simulate_station(stations$station_id[i], stations$neighborhood[i], stations$capacity[i])
}))
network$unusable <- network$bikes == 0L | network$docks == 0L
network$usable <- !network$unusable
network$hour <- as.integer(format(network$timestamp, "%H", tz = TIMEZONE))
network$day <- as.character(as.Date(network$timestamp, tz = TIMEZONE))

# Count consecutive unusable observations for each station.
# Duration is a 15-minute sample-based estimate, not continuous telemetry.
network$unusable_run_hours <- numeric(nrow(network))
for (station in stations$station_id) {
  indices <- which(network$station_id == station)
  consecutive <- 0L
  for (j in indices) {
    consecutive <- if (network$unusable[j]) consecutive + 1L else 0L
    network$unusable_run_hours[j] <- consecutive * INTERVAL_HOURS
  }
}

# Invariants of the synthetic data generator.
stopifnot(
  nrow(network) == N_STATIONS * N_INTERVALS,
  all(network$bikes >= 0L),
  all(network$docks >= 0L),
  all(network$bikes + network$docks == network$capacity),
  all(!is.na(network$bikes)),
  all(network$unusable_run_hours[!network$unusable] == 0),
  all(network$unusable_run_hours[network$unusable] >= INTERVAL_HOURS)
)

# Exploratory p-chart using the first snapshot from each hour (n = 20).
# Independence / constant-probability assumptions can be violated by time trends.
hourly_sample <- network[(network$interval - 1L) %% 4L == 0L, ]
hourly_counts <- aggregate(unusable ~ interval, data = hourly_sample, FUN = sum)
hourly_counts$p <- hourly_counts$unusable / N_STATIONS
p_bar <- mean(hourly_sample$unusable)
p_se <- sqrt(p_bar * (1 - p_bar) / N_STATIONS)
p_ucl <- min(1, p_bar + 3 * p_se)
p_lcl <- max(0, p_bar - 3 * p_se)

# ---- 2. User interface ----
ui <- fluidPage(
  tags$head(tags$style(HTML("\n    body {background:#f3f6fb; color:#17324d; font-family:Arial,sans-serif;}\n    .headline {background:#153451; color:white; padding:22px 25px; border-radius:12px; margin-bottom:20px;}\n    .headline h2 {margin:0 0 6px; font-weight:700;}\n    .headline p {margin:0; color:#d9e7f2;}\n    .metric {background:white; border:1px solid #e3e9ef; border-radius:10px;\n             padding:16px; margin-bottom:16px; min-height:116px;}\n    .metric .value {font-size:28px; color:#126b9a; font-weight:700;}\n    .metric .label {font-size:13px; color:#53677a;}\n    .panel {background:white; border:1px solid #e3e9ef; border-radius:10px;\n            padding:15px; margin-bottom:15px;}\n    .panel h4 {margin-top:0; font-weight:700;}\n    .note {font-size:12px; color:#5f7184; line-height:1.5;}\n    .control-label {font-weight:600;}\n  "))),
  div(class = "headline",
      h2("BikeFlow QC"),
      p("Bikeshare availability, statistical monitoring, and rebalancing priorities"),
      p("DEMONSTRATION ONLY — reproducible simulated data, not live station telemetry")),
  sidebarLayout(
    sidebarPanel(width = 3,
      h4("Analysis controls"),
      selectInput("day", "Select day", choices = unique(network$day),
                  selected = tail(unique(network$day), 1)),
      selectInput("station", "Station occupancy plot", choices = stations$station_id,
                  selected = stations$station_id[1]),
      sliderInput("warn", "Warning threshold (bikes or open docks)",
                  min = 1, max = 5, value = 2, step = 1),
      hr(),
      p(class = "note", "A station is unusable when it has zero bikes or zero open docks."),
      p(class = "note", "Synthetic demand includes commuting peaks and a scheduled 04:00 redistribution."),
      downloadButton("download", "Download simulated CSV")
    ),
    mainPanel(width = 9,
      fluidRow(
        column(3, div(class = "metric", div(class = "value", textOutput("kpi_stations")),
                      div(class = "label", "Stations monitored"))),
        column(3, div(class = "metric", div(class = "value", textOutput("kpi_availability")),
                      div(class = "label", "Usable station-observations, selected day"))),
        column(3, div(class = "metric", div(class = "value", textOutput("kpi_critical")),
                      div(class = "label", "Full or empty at last snapshot"))),
        column(3, div(class = "metric", div(class = "value", textOutput("kpi_hours")),
                      div(class = "label", "Unusable station-hours, selected day")))
      ),
      fluidRow(
        column(6, div(class = "panel", h4("Station occupancy over time"),
                      plotOutput("occupancy", height = "280px"))),
        column(6, div(class = "panel", h4("Unusable stations by hour"),
                      plotOutput("rush", height = "280px")))
      ),
      div(class = "panel", h4("Dispatch priority — most recent snapshot on selected day"),
          p(class = "note", "Critical stations are ranked by current continuous unusable duration, then by bikes needed to reach half capacity. Positive bike movement means deliver; negative means remove. Durations are estimates from 15-minute snapshots, not live outage timers."),
          tableOutput("dispatch")),
      div(class = "panel", h4("Neighborhood service availability — selected day"),
          p(class = "note", "Availability is the proportion of 15-minute station snapshots at which a station can both rent and accept a bike. This is an operational performance metric, not Cp or Cpk."),
          tableOutput("area_availability")),
      div(class = "panel", h4("Exploratory p-chart: network unusability (all 7 days)"),
          plotOutput("pchart", height = "280px"),
          p(class = "note", "Each point is the fraction unusable among 20 stations at one hourly snapshot. Limits use p-bar +/- 3 sqrt[p-bar(1-p-bar)/20], clipped to [0,1]. Stations share demand patterns and observations are serially dependent, so these limits are descriptive screening thresholds, NOT proof of statistical control or special causes.")),
      div(class = "panel", h4("Methods and definitions"),
          p("Availability = usable station-observations / total station-observations."),
          p("Unusable station-hours = number of unusable 15-minute station-observations multiplied by 0.25 hours."),
          p("Current outage duration = consecutive unusable snapshots up through the most recent reading, multiplied by 0.25 hours; a sampled estimate."),
          p("Warnings occur when a station has at most the selected number of bikes or open docks, but is not yet full or empty."),
          p(class = "note", "No machine learning, GPS optimization, causal attribution, or real-world predictive claims are made."))
    )
  )
)

# ---- 3. Reactive metrics, analytics, and controls ----
server <- function(input, output, session) {
  selected_day <- reactive({
    network[network$day == input$day, , drop = FALSE]
  })

  latest <- reactive({
    df <- selected_day()
    df <- df[df$timestamp == max(df$timestamp), , drop = FALSE]
    warn <- input$warn
    df$status <- ifelse(df$bikes == 0, "EMPTY",
                  ifelse(df$docks == 0, "FULL",
                  ifelse(df$bikes <= warn | df$docks <= warn, "WATCH", "OK")))
    df$action <- ifelse(df$status == "EMPTY", "Deliver bikes",
                 ifelse(df$status == "FULL", "Remove bikes",
                 ifelse(df$status == "WATCH", "Inspect / rebalance", "None")))
    df$move_bikes <- round(df$capacity / 2) - df$bikes
    df$priority <- ifelse(df$status %in% c("EMPTY", "FULL"), 1,
                   ifelse(df$status == "WATCH", 2, 3))
    df[order(df$priority, -df$unusable_run_hours,
             -abs(df$move_bikes), df$station_id), , drop = FALSE]
  })

  output$kpi_stations <- renderText(N_STATIONS)
  output$kpi_availability <- renderText({
    sprintf("%.1f%%", 100 * mean(selected_day()$usable))
  })
  output$kpi_critical <- renderText({
    sum(latest()$status %in% c("EMPTY", "FULL"))
  })
  output$kpi_hours <- renderText({
    sprintf("%.1f h", sum(selected_day()$unusable) * INTERVAL_HOURS)
  })

  output$occupancy <- renderPlot({
    df <- selected_day()
    df <- df[df$station_id == input$station, , drop = FALSE]
    plot(df$timestamp, df$bikes, type = "l", lwd = 2.5, col = "#1976a3",
         ylim = c(0, max(df$capacity)), xlab = "Time on selected day",
         ylab = "Available bikes", main = input$station)
    abline(h = c(0, unique(df$capacity)), col = "#d15b57", lty = 2)
    legend("topright", legend = c("Bikes", "Empty / full limits"),
           col = c("#1976a3", "#d15b57"), lty = c(1, 2), bty = "n", cex = 0.8)
  })

  output$rush <- renderPlot({
    df <- selected_day()
    profile <- aggregate(unusable ~ hour, data = df, FUN = mean)
    barplot(100 * profile$unusable, names.arg = sprintf("%02d", profile$hour),
            col = "#3e93b7", border = NA, ylim = c(0, 100),
            xlab = "Hour of day (local)", ylab = "% of unusable station-observations",
            cex.names = 0.65)
  })

  output$dispatch <- renderTable({
    df <- latest()
    df <- df[df$status != "OK", , drop = FALSE]
    if (nrow(df) == 0) return(data.frame(Message = "No dispatch alerts at this snapshot"))
    result <- df[, c("station_id", "neighborhood", "bikes", "docks",
                     "capacity", "status", "unusable_run_hours", "action", "move_bikes"),
                 drop = FALSE]
    names(result) <- c("Station", "Area", "Bikes", "Open docks", "Capacity",
                       "Status", "Current outage (h)", "Suggested action",
                       "Bike movement (+ deliver)")
    result
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)

  output$area_availability <- renderTable({
    df <- selected_day()
    by_area <- aggregate(usable ~ neighborhood, data = df, FUN = mean)
    bad <- aggregate(unusable ~ neighborhood, data = df, FUN = sum)
    output_table <- data.frame(
      Area = by_area$neighborhood,
      `Availability (%)` = sprintf("%.1f", 100 * by_area$usable),
      `Unusable station-hours` = sprintf("%.2f", bad$unusable * INTERVAL_HOURS),
      check.names = FALSE
    )
    output_table[order(as.numeric(output_table[["Availability (%)"]])), , drop = FALSE]
  }, striped = TRUE, spacing = "s", rownames = FALSE)

  output$pchart <- renderPlot({
    values <- hourly_counts$p
    plot(seq_along(values), values * 100, type = "b", pch = 16,
         cex = 0.45, lwd = 1, col = "#237ba4", ylim = c(0, 100),
         xlab = "Hourly sample (7 days)", ylab = "Unusable stations (%)")
    abline(h = 100 * p_bar, col = "#1b8a65", lwd = 2)
    abline(h = 100 * c(p_lcl, p_ucl), col = "#c6584e", lty = 2, lwd = 2)
    legend("topright", legend = c("Observed", "Center line", "Illustrative 3-sigma limits"),
           col = c("#237ba4", "#1b8a65", "#c6584e"),
           pch = c(16, NA, NA), lty = c(1, 1, 2), bty = "n", cex = 0.75)
  })

  output$download <- downloadHandler(
    filename = function() "bikeflow_simulated_data.csv",
    content = function(file) write.csv(network, file, row.names = FALSE)
  )
}

shinyApp(ui = ui, server = server)
