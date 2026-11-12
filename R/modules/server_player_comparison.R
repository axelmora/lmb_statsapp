# ======================================================
# Player Comparison - Server Module
# ======================================================

playerComparisonServer <- function(
  id,
  hit_cp,
  pit_cp,
  hit_cp_cum,
  pit_cp_cum
) {

  moduleServer(id, function(input, output, session) {

    # --------------------------------------------------
    # HELPERS
    # --------------------------------------------------

    get_player_seasons <- function(data, player_name) {

      if (is.null(player_name) || length(player_name) == 0 || player_name == "") {
        return(character(0))
      }

      seasons <- data %>%
        dplyr::filter(Name == player_name) %>%
        dplyr::pull(Year) %>%
        as.character() %>%
        unique() %>%
        stats::na.omit()

      seasons[order(as.numeric(seasons), decreasing = TRUE)]
    }

    get_player_row <- function(data, player_name, season = NULL) {

      if (is.null(season)) {
        data %>% dplyr::filter(Name == player_name)
      } else {
        data %>%
          dplyr::filter(
            Name == player_name,
            as.character(Year) == as.character(season)
          ) %>%
          dplyr::select(-dplyr::any_of("Year"))
      }
    }

    # Convert two player records into a statistics-by-player table.
    transpose_comparison <- function(comparison_data) {

      comparison_data <- as.data.frame(comparison_data)

      transposed <- data.table::transpose(
        data.table::as.data.table(comparison_data),
        keep.names = "Stats",
        make.names = "Name"
      )

      player_cols <- setdiff(names(transposed), "Stats")

      list(
        data = transposed,
        player_cols = player_cols
      )
    }

    # --------------------------------------------------
    # HITTERS: PLAYER INPUTS AND SEASONS
    # --------------------------------------------------

    hitter_names <- sort(intersect(
      unique(hit_cp$Name),
      unique(hit_cp_cum$Name)
    ))

    observe({
      updateSelectizeInput(
        session, "player1",
        choices = hitter_names,
        selected = "",
        server = TRUE
      )

      updateSelectizeInput(
        session, "player2",
        choices = hitter_names,
        selected = "",
        server = TRUE
      )
    })

    observeEvent(input$player1, {
      seasons <- get_player_seasons(hit_cp, input$player1)

      updateSelectInput(
        session,
        "season1",
        choices = seasons,
        selected = if (length(seasons) > 0) seasons[1] else character(0)
      )
    }, ignoreInit = TRUE)

    observeEvent(input$player2, {
      seasons <- get_player_seasons(hit_cp, input$player2)

      updateSelectInput(
        session,
        "season2",
        choices = seasons,
        selected = if (length(seasons) > 0) seasons[1] else character(0)
      )
    }, ignoreInit = TRUE)

    selected_players <- reactive({

      req(input$player1, input$player2, input$hitter_mode)

      if (identical(input$hitter_mode, "career")) {

        comparison_data <- dplyr::bind_rows(
          get_player_row(hit_cp_cum, input$player1),
          get_player_row(hit_cp_cum, input$player2)
        )

      } else {

        req(input$season1, input$season2)

        comparison_data <- dplyr::bind_rows(
          get_player_row(hit_cp, input$player1, input$season1),
          get_player_row(hit_cp, input$player2, input$season2)
        )
      }

      comparison_data
    })

    output$hitter_comparison_table <- renderReactable({

      req(input$player1, input$player2, input$hitter_mode)

      if (identical(input$player1, input$player2)) {
        showNotification(
          "⚠ Please select two different players to compare!",
          type = "warning",
          duration = 5
        )
        return(NULL)
      }

      comparison_data <- selected_players()
      req(nrow(comparison_data) == 2)

      transposed <- transpose_comparison(comparison_data)
      trans_data <- transposed$data
      player_cols <- transposed$player_cols

      # Hitter statistics are numeric; NaN values are retained as NA.
      trans_data[, (player_cols) := lapply(.SD, function(x) {
        suppressWarnings(as.numeric(as.character(x)))
      }), .SDcols = player_cols]

      trans_data <- trans_data[
        , c(player_cols[1], "Stats", player_cols[2]),
        with = FALSE
      ]

      reactable(
        trans_data,
        columns = c(
          setNames(
            lapply(player_cols, function(col_name) {
              colDef(
                align = "center",
                style = function(value, index) {
                  opponent_col <- if (col_name == player_cols[1]) {
                    player_cols[2]
                  } else {
                    player_cols[1]
                  }

                  opponent_value <- trans_data[[opponent_col]][index]

                  if (is.na(value) || is.na(opponent_value)) return("")

                  if (value > opponent_value) {
                    "background-color: #D4EDDA;"
                  } else {
                    ""
                  }
                }
              )
            }),
            player_cols
          ),
          list(
            Stats = colDef(name = "Statistic", align = "center")
          )
        ),
        pagination = FALSE,
        defaultColDef = colDef(align = "center"),
        bordered = TRUE,
        highlight = TRUE
      )
    })

    # --------------------------------------------------
    # PITCHERS: PLAYER INPUTS AND SEASONS
    # --------------------------------------------------

    pitcher_names <- sort(intersect(
      unique(pit_cp$Name),
      unique(pit_cp_cum$Name)
    ))

    observe({
      updateSelectizeInput(
        session, "pitcher1",
        choices = pitcher_names,
        selected = "",
        server = TRUE
      )

      updateSelectizeInput(
        session, "pitcher2",
        choices = pitcher_names,
        selected = "",
        server = TRUE
      )
    })

    observeEvent(input$pitcher1, {
      seasons <- get_player_seasons(pit_cp, input$pitcher1)

      updateSelectInput(
        session,
        "pitcher_season1",
        choices = seasons,
        selected = if (length(seasons) > 0) seasons[1] else character(0)
      )
    }, ignoreInit = TRUE)

    observeEvent(input$pitcher2, {
      seasons <- get_player_seasons(pit_cp, input$pitcher2)

      updateSelectInput(
        session,
        "pitcher_season2",
        choices = seasons,
        selected = if (length(seasons) > 0) seasons[1] else character(0)
      )
    }, ignoreInit = TRUE)

    selected_pitchers <- reactive({

      req(input$pitcher1, input$pitcher2, input$pitcher_mode)

      if (identical(input$pitcher_mode, "career")) {

        comparison_data <- dplyr::bind_rows(
          get_player_row(pit_cp_cum, input$pitcher1),
          get_player_row(pit_cp_cum, input$pitcher2)
        )

      } else {

        req(input$pitcher_season1, input$pitcher_season2)

        comparison_data <- dplyr::bind_rows(
          get_player_row(pit_cp, input$pitcher1, input$pitcher_season1),
          get_player_row(pit_cp, input$pitcher2, input$pitcher_season2)
        )
      }

      comparison_data
    })

    output$pitcher_comparison_table <- renderReactable({

      req(input$pitcher1, input$pitcher2, input$pitcher_mode)

      if (identical(input$pitcher1, input$pitcher2)) {
        showNotification(
          "⚠ Please select two different pitchers to compare!",
          type = "warning",
          duration = 5
        )
        return(NULL)
      }

      comparison_data <- selected_pitchers()
      req(nrow(comparison_data) == 2)

      transposed <- transpose_comparison(comparison_data)
      trans_data_p <- transposed$data
      pitcher_cols <- transposed$player_cols

      # W-L remains character; all other pitcher statistics are numeric.
      non_numeric_stats <- c("W-L")
      numeric_stats <- setdiff(trans_data_p$Stats, non_numeric_stats)

      for (col in pitcher_cols) {
        trans_data_p[
          Stats %in% numeric_stats,
          (col) := suppressWarnings(as.numeric(as.character(get(col))))
        ]
      }

      trans_data_p <- trans_data_p[
        , c(pitcher_cols[1], "Stats", pitcher_cols[2]),
        with = FALSE
      ]

      lower_is_better <- c("ERA", "WHIP", "FIP", "BB%")

      parse_wl <- function(wl) {
        parts <- strsplit(as.character(wl), "-", fixed = TRUE)[[1]]

        if (length(parts) != 2) return(NA_real_)

        w <- suppressWarnings(as.numeric(parts[1]))
        l <- suppressWarnings(as.numeric(parts[2]))

        if (is.na(w) || is.na(l) || (w + l) == 0) {
          return(NA_real_)
        }

        w / (w + l)
      }

      reactable(
        trans_data_p,
        columns = c(
          setNames(
            lapply(pitcher_cols, function(col_name) {
              colDef(
                align = "center",
                style = function(value, index) {
                  stat_name <- trans_data_p$Stats[index]

                  opponent_col <- if (col_name == pitcher_cols[1]) {
                    pitcher_cols[2]
                  } else {
                    pitcher_cols[1]
                  }

                  opponent_value <- trans_data_p[[opponent_col]][index]

                  if (is.na(value) || is.na(opponent_value)) return("")

                  if (stat_name == "W-L") {
                    wp <- parse_wl(value)
                    opponent_wp <- parse_wl(opponent_value)

                    if (is.na(wp) || is.na(opponent_wp)) return("")

                    if (wp > opponent_wp) {
                      "background-color: #D4EDDA;"
                    } else {
                      ""
                    }

                  } else if (stat_name %in% lower_is_better) {
                    if (value < opponent_value) {
                      "background-color: #D4EDDA;"
                    } else {
                      ""
                    }

                  } else {
                    if (value > opponent_value) {
                      "background-color: #D4EDDA;"
                    } else {
                      ""
                    }
                  }
                }
              )
            }),
            pitcher_cols
          ),
          list(
            Stats = colDef(name = "Statistic", align = "center")
          )
        ),
        pagination = FALSE,
        defaultColDef = colDef(align = "center"),
        bordered = TRUE,
        highlight = TRUE
      )
    })
  })
}
