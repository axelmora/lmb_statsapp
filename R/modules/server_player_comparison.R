
# ======================================================
# Player Comparison - Server Module
# ======================================================

playerComparisonServer <- function(id, hit_cp, pit_cp) {
  
  moduleServer(id, function(input, output, session) {
    
    # --------------------------------------------------
    # HELPER: Obtener temporadas disponibles por jugador
    # --------------------------------------------------
    
    get_player_seasons <- function(data, player_name) {
      
      if (is.null(player_name) || player_name == "") {
        return(character(0))
      }
      
      seasons <- data %>%
        dplyr::filter(Name == player_name) %>%
        dplyr::pull(Year) %>%
        as.character() %>%
        unique() %>%
        na.omit()
      
      # Ordenar temporadas de la más reciente a la más antigua
      seasons <- seasons[
        order(as.numeric(seasons), decreasing = TRUE)
      ]
      
      seasons
    }
    
    
    # ==================================================
    # HITTERS
    # ==================================================
    
    hitter_names <- sort(unique(hit_cp$Name))
    
    # Inicializar selectores de jugadores
    observe({
      
      updateSelectizeInput(
        session,
        "player1",
        choices = hitter_names,
        selected = "",
        server = TRUE
      )
      
      updateSelectizeInput(
        session,
        "player2",
        choices = hitter_names,
        selected = "",
        server = TRUE
      )
      
    })
    
    
    # --------------------------------------------------
    # Temporadas disponibles: Hitter 1
    # --------------------------------------------------
    
    observeEvent(input$player1, {
      
      seasons <- get_player_seasons(
        hit_cp,
        input$player1
      )
      
      updateSelectInput(
        session,
        "season1",
        choices = seasons,
        selected = if (length(seasons) > 0) seasons[1] else character(0)
      )
      
    }, ignoreInit = TRUE)
    
    
    # --------------------------------------------------
    # Temporadas disponibles: Hitter 2
    # --------------------------------------------------
    
    observeEvent(input$player2, {
      
      seasons <- get_player_seasons(
        hit_cp,
        input$player2
      )
      
      updateSelectInput(
        session,
        "season2",
        choices = seasons,
        selected = if (length(seasons) > 0) seasons[1] else character(0)
      )
      
    }, ignoreInit = TRUE)
    
    
    # --------------------------------------------------
    # Datos seleccionados de Hitters
    # --------------------------------------------------
    
    selected_players <- reactive({
      
      req(
        input$player1,
        input$player2,
        input$season1,
        input$season2
      )
      
      hit_cp %>%
        dplyr::filter(
          (Name == input$player1 & as.character(Year) == input$season1) |
            (Name == input$player2 & as.character(Year) == input$season2)
        ) %>%
        dplyr::select(-dplyr::any_of("Year"))
      
    })
    
    
    # --------------------------------------------------
    # Hitter Comparison Table
    # --------------------------------------------------
    
    output$hitter_comparison_table <- renderReactable({
      
      req(
        input$player1,
        input$player2,
        input$season1,
        input$season2
      )
      
      if (input$player1 == input$player2) {
        
        showNotification(
          "⚠ Please select two different players to compare!",
          type = "warning",
          duration = 5
        )
        
        return(NULL)
      }
      
      comparison_data <- selected_players()
      
      req(nrow(comparison_data) == 2)
      
      trans_data <- data.table::transpose(
        comparison_data,
        keep.names = "Stats",
        make.names = "Name"
      )
      
      player_cols <- colnames(trans_data)[-1]
      
      # Convertir las columnas de jugadores a numeric
      trans_data[, (player_cols) := lapply(
        .SD,
        as.numeric
      ), .SDcols = player_cols]
      
      # Reordenar columnas
      trans_data <- trans_data[
        ,
        c(player_cols[1], "Stats", player_cols[2]),
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
                  
                  opponent_col <- if (
                    col_name == player_cols[1]
                  ) {
                    player_cols[2]
                  } else {
                    player_cols[1]
                  }
                  
                  opponent_value <- trans_data[[opponent_col]][index]
                  
                  if (
                    is.na(value) ||
                    is.na(opponent_value)
                  ) {
                    return("")
                  }
                  
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
            Stats = colDef(
              name = "Statistic",
              align = "center"
            )
          )
          
        ),
        
        pagination = FALSE,
        defaultColDef = colDef(align = "center"),
        bordered = TRUE,
        highlight = TRUE
      )
      
    })
    
    
    # ==================================================
    # PITCHERS
    # ==================================================
    
    pitcher_names <- sort(unique(pit_cp$Name))
    
    
    # Inicializar selectores de pitchers
    observe({
      
      updateSelectizeInput(
        session,
        "pitcher1",
        choices = pitcher_names,
        selected = "",
        server = TRUE
      )
      
      updateSelectizeInput(
        session,
        "pitcher2",
        choices = pitcher_names,
        selected = "",
        server = TRUE
      )
      
    })
    
    
    # --------------------------------------------------
    # Temporadas disponibles: Pitcher 1
    # --------------------------------------------------
    
    observeEvent(input$pitcher1, {
      
      seasons <- get_player_seasons(
        pit_cp,
        input$pitcher1
      )
      
      updateSelectInput(
        session,
        "pitcher_season1",
        choices = seasons,
        selected = if (length(seasons) > 0) seasons[1] else character(0)
      )
      
    }, ignoreInit = TRUE)
    
    
    # --------------------------------------------------
    # Temporadas disponibles: Pitcher 2
    # --------------------------------------------------
    
    observeEvent(input$pitcher2, {
      
      seasons <- get_player_seasons(
        pit_cp,
        input$pitcher2
      )
      
      updateSelectInput(
        session,
        "pitcher_season2",
        choices = seasons,
        selected = if (length(seasons) > 0) seasons[1] else character(0)
      )
      
    }, ignoreInit = TRUE)
    
    
    # --------------------------------------------------
    # Datos seleccionados de Pitchers
    # --------------------------------------------------
    
    selected_pitchers <- reactive({
      
      req(
        input$pitcher1,
        input$pitcher2,
        input$pitcher_season1,
        input$pitcher_season2
      )
      
      pit_cp %>%
        dplyr::filter(
          (Name == input$pitcher1 & as.character(Year) == input$pitcher_season1) |
            (Name == input$pitcher2 & as.character(Year) == input$pitcher_season2)
        ) %>%
        dplyr::select(-dplyr::any_of("Year"))
      
    })
    
    
    # --------------------------------------------------
    # Pitcher Comparison Table
    # --------------------------------------------------
    
    output$pitcher_comparison_table <- renderReactable({
      
      req(
        input$pitcher1,
        input$pitcher2,
        input$pitcher_season1,
        input$pitcher_season2
      )
      
      if (input$pitcher1 == input$pitcher2) {
        
        showNotification(
          "⚠ Please select two different pitchers to compare!",
          type = "warning",
          duration = 5
        )
        
        return(NULL)
      }
      
      comparison_data <- selected_pitchers()
      
      req(nrow(comparison_data) == 2)
      
      trans_data_p <- data.table::transpose(
        comparison_data,
        keep.names = "Stats",
        make.names = "Name"
      )
      
      pitcher_cols <- colnames(trans_data_p)[-1]
      
      # Estadísticas que no deben convertirse a numeric
      non_numeric_stats <- c("W-L")
      
      numeric_stats <- setdiff(
        trans_data_p$Stats,
        non_numeric_stats
      )
      
      # Convertir únicamente estadísticas numéricas
      for (col in pitcher_cols) {
        
        trans_data_p[
          Stats %in% numeric_stats,
          (col) := as.numeric(get(col))
        ]
        
      }
      
      # Reordenar columnas
      trans_data_p <- trans_data_p[
        ,
        c(pitcher_cols[1], "Stats", pitcher_cols[2]),
        with = FALSE
      ]
      
      # Estadísticas donde un valor menor es mejor
      lower_is_better <- c(
        "ERA",
        "WHIP",
        "FIP",
        "BB%"
      )
      
      
      reactable(
        trans_data_p,
        
        columns = c(
          
          setNames(
            lapply(pitcher_cols, function(col_name) {
              
              colDef(
                align = "center",
                
                style = function(value, index) {
                  
                  stat_name <- trans_data_p$Stats[index]
                  
                  opponent_col <- if (
                    col_name == pitcher_cols[1]
                  ) {
                    pitcher_cols[2]
                  } else {
                    pitcher_cols[1]
                  }
                  
                  opponent_value <- trans_data_p[[opponent_col]][index]
                  
                  if (
                    is.na(value) ||
                    is.na(opponent_value)
                  ) {
                    return("")
                  }
                  
                  
                  # Comparación de W-L mediante Win Percentage
                  if (stat_name == "W-L") {
                    
                    parse_wl <- function(wl) {
                      
                      parts <- strsplit(
                        as.character(wl),
                        "-"
                      )[[1]]
                      
                      w <- as.numeric(parts[1])
                      l <- as.numeric(parts[2])
                      
                      if (
                        length(parts) != 2 ||
                        is.na(w) ||
                        is.na(l) ||
                        (w + l) == 0
                      ) {
                        return(NA_real_)
                      }
                      
                      w / (w + l)
                      
                    }
                    
                    wp1 <- tryCatch(
                      parse_wl(value),
                      error = function(e) NA_real_
                    )
                    
                    wp2 <- tryCatch(
                      parse_wl(opponent_value),
                      error = function(e) NA_real_
                    )
                    
                    if (is.na(wp1) || is.na(wp2)) {
                      return("")
                    }
                    
                    if (wp1 > wp2) {
                      "background-color: #D4EDDA;"
                    } else {
                      ""
                    }
                    
                    
                    # Estadísticas donde menor es mejor
                  } else if (stat_name %in% lower_is_better) {
                    
                    if (value < opponent_value) {
                      "background-color: #D4EDDA;"
                    } else {
                      ""
                    }
                    
                    
                    # Resto de estadísticas: mayor es mejor
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
            Stats = colDef(
              name = "Statistic",
              align = "center"
            )
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
