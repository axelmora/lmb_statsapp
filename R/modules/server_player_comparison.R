# ======================================================
# Player Comparison - Server Module
# ======================================================
playerComparisonServer <- function(id, hit_cp, pit_cp) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- HITTERS ---
    player_data <- reactive({
      hit_cp
    })
    
    observe({
      updateSelectInput(session, "player1", choices = sort(unique(player_data()$Name)))
      updateSelectInput(session, "player2", choices = sort(unique(player_data()$Name)))
    })
    
    selected_players <- reactive({
      req(input$player1, input$player2)
      player_data() %>%
        filter(Name %in% c(input$player1, input$player2))
    })
    
    output$hitter_comparison_table <- renderReactable({
      req(input$player1, input$player2)
      if (input$player1 == input$player2) {
        showNotification("⚠ Please select two different players to compare!", type = "warning", duration = 5)
        return(NULL)
      }
      
      trans_data <- data.table::transpose(selected_players(), keep.names = "Stats", make.names = "Name")
      trans_data[, -1] <- lapply(trans_data[, -1], as.numeric)
      
      player_cols <- colnames(trans_data)[2:3]
      trans_data <- trans_data[, c(player_cols[1], "Stats", player_cols[2])]
      
      reactable(
        trans_data,
        columns = c(
          setNames(
            lapply(player_cols, function(col_name) {
              colDef(
                align = "center",
                style = function(value, index) {
                  opponent_col <- ifelse(col_name == player_cols[1], player_cols[2], player_cols[1])
                  if (is.na(value) || is.na(trans_data[index, opponent_col])) return("")
                  if (value > trans_data[index, opponent_col]) "background-color: #D4EDDA;" else ""
                }
              )
            }),
            player_cols
          ),
          list(Stats = colDef(name = "Statistic", align = "center"))
        ),
        pagination = FALSE,
        defaultColDef = colDef(align = "center"),
        bordered = TRUE,
        highlight = TRUE
      )
    })
    
    # --- PITCHERS ---
    pitcher_data <- reactive({
      pit_cp
    })
    
    observe({
      updateSelectInput(session, "pitcher1", choices = sort(unique(pitcher_data()$Name)))
      updateSelectInput(session, "pitcher2", choices = sort(unique(pitcher_data()$Name)))
    })
    
    selected_pitchers <- reactive({
      req(input$pitcher1, input$pitcher2)
      pitcher_data() %>% filter(Name %in% c(input$pitcher1, input$pitcher2))
    })
    
    output$pitcher_comparison_table <- renderReactable({
      req(input$pitcher1, input$pitcher2)
      if (input$pitcher1 == input$pitcher2) {
        showNotification("⚠ Please select two different pitchers to compare!", type = "warning", duration = 5)
        return(NULL)
      }
      
      trans_data_p <- data.table::transpose(selected_pitchers(), keep.names = "Stats", make.names = "Name")
      
      pitcher_cols <- colnames(trans_data_p)[2:3]
      trans_data_p <- trans_data_p[, c(pitcher_cols[1], "Stats", pitcher_cols[2])]
      
      non_numeric_stats <- c("W-L")
      numeric_stats <- setdiff(trans_data_p$Stats, non_numeric_stats)
      
      for (col in pitcher_cols) {
        trans_data_p[[col]] <- ifelse(
          trans_data_p$Stats %in% numeric_stats,
          as.numeric(trans_data_p[[col]]),
          trans_data_p[[col]]
        )
      }
      
      lower_is_better <- c("ERA", "WHIP", "FIP", "BB%")
      
      reactable(
        trans_data_p,
        columns = c(
          setNames(
            lapply(pitcher_cols, function(col_name) {
              colDef(
                align = "center",
                style = function(value, index) {
                  stat_name <- trans_data_p$Stats[index]
                  opponent_col <- ifelse(col_name == pitcher_cols[1], pitcher_cols[2], pitcher_cols[1])
                  opponent_value <- trans_data_p[index, opponent_col]
                  
                  if (is.na(value) || is.na(opponent_value)) return("")
                  
                  if (stat_name == "W-L") {
                    parse_wl <- function(wl) {
                      parts <- strsplit(wl, "-")[[1]]
                      w <- as.numeric(parts[1])
                      l <- as.numeric(parts[2])
                      return(w / (w + l))
                    }
                    wp1 <- tryCatch(parse_wl(value), error = function(e) NA)
                    wp2 <- tryCatch(parse_wl(opponent_value), error = function(e) NA)
                    if (is.na(wp1) || is.na(wp2)) return("")
                    if (wp1 > wp2) "background-color: #D4EDDA;" else ""
                  } else if (stat_name %in% lower_is_better) {
                    if (value < opponent_value) "background-color: #D4EDDA;" else ""
                  } else {
                    if (value > opponent_value) "background-color: #D4EDDA;" else ""
                  }
                }
              )
            }),
            pitcher_cols
          ),
          list(Stats = colDef(name = "Statistic", align = "center"))
        ),
        pagination = FALSE,
        defaultColDef = colDef(align = "center"),
        bordered = TRUE,
        highlight = TRUE
      )
    })
  })
}
