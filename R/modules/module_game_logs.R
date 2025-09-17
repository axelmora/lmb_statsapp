ui_game_logs <- function(id) {
  ns <- NS(id)
  fluidRow(column(12, DTOutput(ns("game_logs_table"))))
}

server_game_logs <- function(id, gl_data) {
  moduleServer(id, function(input, output, session) {
    output$game_logs_table <- renderDT({
    datatable(
      gl_data
      ,escape = FALSE
      ,rownames = FALSE
      ,options = list(
        dom = 'tip'
        ,pageLength = 30
        ,scrollX = FALSE
        ,columnDefs = list(list(targets = 0, width = '100x')
                           ,list(targets = c(2,4), width = '10px')
                           ,list(targets = c(2,4,8), className = 'dt-center')
                           ,list(targets = c(1,3,5,9), className = 'dt-left')
                           ,list(targets = c(1,3,5), width = '250px')
                           ,list(targets = c(6,7), width = '50px')
                           ,list(targets = 8, width = '50px')
                           ,list(targets = 9, width = '50px')
                           ,list(targets = "_all", className = 'dt-left')
        )
      )
    )
  })
  })
}