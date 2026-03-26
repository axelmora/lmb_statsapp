# ======================================================
# LMB Stats App (Modernized)
# ======================================================

library(shiny)
library(bslib)
library(shinyWidgets)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(reactable)
library(plotly)
library(ggplot2)
library(scales)
library(thematic)
library(htmltools)

thematic::thematic_shiny(font = "Inter")

options(shiny.sanitize.errors = TRUE)

safe_read_rds <- function(name) {
  paths <- c(
    file.path("data", "cache", paste0(name, ".rds")),
    file.path("cache", paste0(name, ".rds"))
  )

  for (p in paths) {
    if (file.exists(p)) {
      out <- tryCatch(readRDS(p), error = function(e) NULL)
      if (!is.null(out)) return(out)
    }
  }

  data.frame()
}

find_col <- function(df, candidates = character(), regex = NULL) {
  nms <- names(df)
  if (!length(nms)) return(NULL)

  for (cand in candidates) {
    m <- nms[tolower(nms) == tolower(cand)]
    if (length(m)) return(m[[1]])
  }

  if (!is.null(regex)) {
    m <- nms[grepl(regex, nms, ignore.case = TRUE)]
    if (length(m)) return(m[[1]])
  }

  NULL
}

numeric_cols <- function(df, max_n = 12) {
  out <- names(df)[vapply(df, is.numeric, logical(1))]
  out <- out[!tolower(out) %in% c("x", "y", "id", "rank")]
  head(out, max_n)
}

`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y

# ---- Data registry ----
datasets <- list(
  team_hitting = safe_read_rds("team_hitting"),
  team_pitching = safe_read_rds("team_pitching"),
  team_fielding = safe_read_rds("team_fielding"),
  hitting = safe_read_rds("hitting"),
  pitching = safe_read_rds("pitching"),
  fielding = safe_read_rds("fielding"),
  rosters = safe_read_rds("rosters"),
  trans = safe_read_rds("trans"),
  game_logs = safe_read_rds("game_logs"),
  attendance = safe_read_rds("lmb_att_24"),
  pace = safe_read_rds("lmb_pace_24"),
  pace_venue = safe_read_rds("lmb_pace_venue_24"),
  guts = safe_read_rds("woba_fipc"),
  park_factors = safe_read_rds("park_factors"),
  standings = safe_read_rds("stan_lmb")
)

team_col <- find_col(datasets$team_hitting, c("Team", "team", "Club"), "team")
season_col <- find_col(datasets$team_hitting, c("Year", "Season", "year"), "year|season")
name_col <- find_col(datasets$hitting, c("Name", "Player", "Jugador"), "name|player|jugador")

all_seasons <- if (!is.null(season_col)) sort(unique(datasets$team_hitting[[season_col]])) else "All"
all_teams <- if (!is.null(team_col)) sort(unique(datasets$team_hitting[[team_col]])) else "All"

ui <- page_navbar(
  title = div(icon("baseball"), " LMB Stats Lab"),
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#0d6efd"),

  nav_panel(
    "Overview",
    layout_column_wrap(
      width = 1 / 4,
      value_box(
        title = "Players",
        value = textOutput("kpi_players"),
        showcase = bsicons::bs_icon("person-badge"),
        theme = value_box_theme(bg = "#0d6efd", fg = "white")
      ),
      value_box(
        title = "Teams",
        value = textOutput("kpi_teams"),
        showcase = bsicons::bs_icon("people"),
        theme = value_box_theme(bg = "#198754", fg = "white")
      ),
      value_box(
        title = "Games Logged",
        value = textOutput("kpi_games"),
        showcase = bsicons::bs_icon("calendar-event"),
        theme = value_box_theme(bg = "#6f42c1", fg = "white")
      ),
      value_box(
        title = "Seasons",
        value = textOutput("kpi_seasons"),
        showcase = bsicons::bs_icon("graph-up-arrow"),
        theme = value_box_theme(bg = "#fd7e14", fg = "white")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Top Team Offense Snapshot"),
        plotlyOutput("overview_team_metric", height = "420px") %>% withSpinner()
      ),
      card(
        card_header("Attendance & Pace Signals"),
        plotlyOutput("overview_attendance", height = "420px") %>% withSpinner()
      )
    )
  ),

  nav_panel(
    "Player Explorer",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        pickerInput("player_dataset", "Dataset", choices = c("Hitting", "Pitching", "Fielding")),
        uiOutput("player_season_ui"),
        uiOutput("player_metric_ui"),
        sliderInput("player_top_n", "Top N", min = 5, max = 50, value = 20)
      ),
      card(
        card_header("Leaderboard"),
        plotlyOutput("player_leaderboard", height = "420px") %>% withSpinner()
      ),
      card(
        card_header("Detailed table"),
        reactableOutput("player_table")
      )
    )
  ),

  nav_panel(
    "Team Explorer",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        pickerInput("team_dataset", "Dataset", choices = c("Hitting", "Pitching", "Fielding")),
        pickerInput("team_filter", "Team", choices = c("All", all_teams), selected = "All"),
        pickerInput("team_season", "Season", choices = c("All", all_seasons), selected = "All"),
        uiOutput("team_metric_ui")
      ),
      card(
        card_header("Team trends"),
        plotlyOutput("team_trend", height = "420px") %>% withSpinner()
      ),
      card(
        card_header("Team records"),
        reactableOutput("team_table")
      )
    )
  ),

  nav_panel(
    "Game Center",
    layout_columns(
      col_widths = c(12, 6, 6),
      card(card_header("Game logs"), reactableOutput("game_logs_table")),
      card(card_header("Rosters"), reactableOutput("rosters_table")),
      card(card_header("Transactions"), reactableOutput("transactions_table"))
    )
  ),

  nav_panel(
    "League Insights",
    layout_columns(
      col_widths = c(6, 6, 12),
      card(card_header("Standings"), reactableOutput("standings_table")),
      card(card_header("Park factors"), reactableOutput("pf_table")),
      card(card_header("Guts constants"), reactableOutput("guts_table"))
    )
  )
)

server <- function(input, output, session) {

  player_data <- reactive({
    switch(input$player_dataset,
      "Hitting" = datasets$hitting,
      "Pitching" = datasets$pitching,
      datasets$fielding
    )
  })

  team_data <- reactive({
    switch(input$team_dataset,
      "Hitting" = datasets$team_hitting,
      "Pitching" = datasets$team_pitching,
      datasets$team_fielding
    )
  })

  output$kpi_players <- renderText({
    df <- datasets$hitting
    nm <- find_col(df, c("Name", "Player"), "name|player")
    comma(if (!is.null(nm)) dplyr::n_distinct(df[[nm]]) else nrow(df))
  })

  output$kpi_teams <- renderText({
    df <- datasets$team_hitting
    tc <- find_col(df, c("Team"), "team")
    comma(if (!is.null(tc)) dplyr::n_distinct(df[[tc]]) else nrow(df))
  })

  output$kpi_games <- renderText({
    comma(nrow(datasets$game_logs))
  })

  output$kpi_seasons <- renderText({
    sc <- find_col(datasets$team_hitting, c("Year", "Season"), "year|season")
    if (is.null(sc) || !nrow(datasets$team_hitting)) return("n/a")
    vals <- sort(unique(datasets$team_hitting[[sc]]))
    paste0(min(vals), " – ", max(vals))
  })

  output$player_season_ui <- renderUI({
    df <- player_data()
    sc <- find_col(df, c("Year", "Season"), "year|season")
    if (is.null(sc)) return(NULL)
    pickerInput("player_season", "Season", choices = c("All", sort(unique(df[[sc]]))), selected = "All")
  })

  output$player_metric_ui <- renderUI({
    df <- player_data()
    choices <- numeric_cols(df)
    if (!length(choices)) return(helpText("No numeric metrics available for this dataset."))
    pickerInput("player_metric", "Metric", choices = choices, selected = choices[[1]])
  })

  output$team_metric_ui <- renderUI({
    df <- team_data()
    choices <- numeric_cols(df)
    if (!length(choices)) return(helpText("No numeric metrics available for this dataset."))
    pickerInput("team_metric", "Metric", choices = choices, selected = choices[[1]])
  })

  filtered_player_data <- reactive({
    df <- player_data()
    sc <- find_col(df, c("Year", "Season"), "year|season")
    if (!is.null(sc) && !is.null(input$player_season) && input$player_season != "All") {
      df <- df[df[[sc]] == input$player_season, , drop = FALSE]
    }
    df
  })

  output$player_leaderboard <- renderPlotly({
    df <- filtered_player_data()
    req(nrow(df), input$player_metric)

    metric <- input$player_metric
    nm <- find_col(df, c("Name", "Player", "Jugador"), "name|player|jugador") %||% names(df)[1]

    plot_df <- df %>%
      filter(!is.na(.data[[metric]])) %>%
      arrange(desc(.data[[metric]])) %>%
      slice_head(n = input$player_top_n)

    p <- ggplot(plot_df, aes(x = reorder(.data[[nm]], .data[[metric]]), y = .data[[metric]], fill = .data[[metric]])) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = NULL, y = metric) +
      scale_fill_viridis_c(option = "C") +
      theme_minimal(base_size = 13)

    ggplotly(p, tooltip = c("x", "y"))
  })

  output$player_table <- renderReactable({
    reactable(
      filtered_player_data(),
      searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      compact = TRUE,
      defaultPageSize = 25,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(25, 50, 100)
    )
  })

  filtered_team_data <- reactive({
    df <- team_data()
    tc <- find_col(df, c("Team"), "team")
    sc <- find_col(df, c("Year", "Season"), "year|season")

    if (!is.null(tc) && input$team_filter != "All") df <- df[df[[tc]] == input$team_filter, , drop = FALSE]
    if (!is.null(sc) && input$team_season != "All") df <- df[df[[sc]] == input$team_season, , drop = FALSE]

    df
  })

  output$team_trend <- renderPlotly({
    df <- filtered_team_data()
    req(nrow(df), input$team_metric)

    tc <- find_col(df, c("Team"), "team") %||% names(df)[1]
    sc <- find_col(df, c("Year", "Season"), "year|season") %||% names(df)[2]
    metric <- input$team_metric

    p <- ggplot(df, aes(x = .data[[sc]], y = .data[[metric]], color = .data[[tc]], group = .data[[tc]])) +
      geom_line(linewidth = 1, alpha = 0.85) +
      geom_point(size = 2) +
      labs(x = sc, y = metric, color = "Team") +
      theme_minimal(base_size = 13)

    ggplotly(p, tooltip = c("x", "y", "color"))
  })

  output$team_table <- renderReactable({
    reactable(
      filtered_team_data(),
      searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      defaultPageSize = 20
    )
  })

  output$overview_team_metric <- renderPlotly({
    df <- datasets$team_hitting
    req(nrow(df))
    tc <- find_col(df, c("Team"), "team") %||% names(df)[1]
    metric <- numeric_cols(df, 1)
    req(length(metric))

    plot_df <- df %>%
      group_by(.data[[tc]]) %>%
      summarise(value = mean(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(value)) %>%
      slice_head(n = 12)

    p <- ggplot(plot_df, aes(x = reorder(.data[[tc]], value), y = value, fill = value)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = NULL, y = metric, subtitle = "Average by team") +
      scale_fill_viridis_c(option = "B") +
      theme_minimal(base_size = 13)

    ggplotly(p, tooltip = c("x", "y"))
  })

  output$overview_attendance <- renderPlotly({
    att <- datasets$attendance
    pv <- datasets$pace_venue
    req(nrow(att) || nrow(pv))

    att_num <- numeric_cols(att, 1)
    pv_num <- numeric_cols(pv, 1)

    if (length(att_num) && length(pv_num)) {
      d1 <- data.frame(source = "Attendance", value = mean(att[[att_num]], na.rm = TRUE))
      d2 <- data.frame(source = "Pace", value = mean(pv[[pv_num]], na.rm = TRUE))
      plot_df <- bind_rows(d1, d2)
    } else {
      plot_df <- data.frame(source = c("Attendance", "Pace"), value = c(nrow(att), nrow(pv)))
    }

    p <- ggplot(plot_df, aes(source, value, fill = source)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = round(value, 2)), vjust = -0.2, size = 5) +
      theme_minimal(base_size = 14) +
      labs(x = NULL, y = "Signal")

    ggplotly(p, tooltip = c("x", "y"))
  })

  output$game_logs_table <- renderReactable({
    reactable(datasets$game_logs, searchable = TRUE, striped = TRUE, highlight = TRUE, defaultPageSize = 25)
  })

  output$rosters_table <- renderReactable({
    reactable(datasets$rosters, searchable = TRUE, striped = TRUE, highlight = TRUE, defaultPageSize = 20)
  })

  output$transactions_table <- renderReactable({
    reactable(datasets$trans, searchable = TRUE, striped = TRUE, highlight = TRUE, defaultPageSize = 20)
  })

  output$standings_table <- renderReactable({
    reactable(datasets$standings, searchable = TRUE, striped = TRUE, highlight = TRUE, defaultPageSize = 20)
  })

  output$pf_table <- renderReactable({
    reactable(datasets$park_factors, searchable = TRUE, striped = TRUE, highlight = TRUE, defaultPageSize = 20)
  })

  output$guts_table <- renderReactable({
    reactable(datasets$guts, searchable = TRUE, striped = TRUE, highlight = TRUE, defaultPageSize = 20)
  })
}

shinyApp(ui, server)
