ui_game_pace <- function(id) {
  ns <- NS(id)
  fluidRow( 
    valueBox(
      value = textOutput(ns("hits9")),
      subtitle = 'Hits per 9 innings',
      color = "primary"
    ),
    valueBox(
      value = textOutput(ns("runs9")),
      subtitle = "Runs per 9 innings",
      color = "primary"
    ),
    valueBox(
      value = textOutput(ns("pitches")),
      subtitle = "Pitches per pitcher",
      color = "primary"
    ),
    valueBox(
      value = textOutput(ns("time_pitch")),
      subtitle = "Time per pitch (Sec)",
      color = "primary"
    ),
    valueBox(
      value = textOutput(ns("time_pa")),
      subtitle = "Time per PA (Sec)",
      color = "primary"
    ),
    valueBox(
      value = textOutput(ns("time_game")),
      subtitle = "Game time",
      color = "primary"
    ),     
    reactableOutput(ns("game_pace_table"))
  )
}

server_game_pace <- function(id, pace_venue_data, pace_data) {
  moduleServer(id, function(input, output, session) {
    output$game_pace_table <- renderReactable({
      reactable(
        pace_venue_data,
        searchable = FALSE,
        sortable = TRUE,
        striped = TRUE,
        compact = TRUE,
        defaultPageSize = 21,
        columns = reactable_numeric_columns(pace_venue_data, digits = 3),
        defaultColDef = reactable_lmb_coldef(minWidth = 85, maxWidth = 150),
        theme = reactable_lmb_theme()
      )
    })
  
  output$hits9 <- renderText({
    pace_data$`Hits/9in`
  })
  
  output$runs9 <- renderText({
    pace_data$`Runs/9in`
  })
  
  output$pitches <- renderText({
    pace_data$`Pitches/Pitcher`
  })
  
  output$time_pitch <- renderText({
    pace_data$`Time/Pitch`
  })
  
  output$time_pa <- renderText({
    pace_data$`Time/PA`
  })
  
  output$time_game <- renderText({
    format(pace_data$`Time/9inGame`)
  })
    
  })
}
