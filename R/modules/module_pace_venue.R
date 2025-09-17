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
    DTOutput(ns("game_pace_table"))
  )
}

server_game_pace <- function(id, pace_venue_data, pace_data) {
  moduleServer(id, function(input, output, session) {
    output$game_pace_table <- renderDT({
    datatable(
      pace_venue_data
      ,escape = FALSE
      ,rownames = FALSE
      ,options = list(
        dom = 't'
        ,pageLength = 21
        ,scrollX = FALSE
        ,columnDefs = list(list(targets = 0, width = '150x')
                           ,list(targets = 1, width = '200px')
                           ,list(targets = c(2:7), width = '5px')
                           ,list(targets = "_all", className = 'dt-left')
        )
        ,scrollX = FALSE
      )
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