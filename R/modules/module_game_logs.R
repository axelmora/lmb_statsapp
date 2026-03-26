ui_game_logs <- function(id) {
  ns <- NS(id)
  fluidRow(column(12, reactableOutput(ns("game_logs_table"))))
}

server_game_logs <- function(id, gl_data) {
  moduleServer(id, function(input, output, session) {
    output$game_logs_table <- renderReactable({
      reactable(
        gl_data,
        searchable = TRUE,
        sortable = TRUE,
        striped = TRUE,
        highlight = TRUE,
        compact = TRUE,
        defaultPageSize = 30,
        columns = reactable_numeric_columns(gl_data, digits = 3),
        defaultColDef = reactable_lmb_coldef(minWidth = 90, maxWidth = 220),
        theme = reactable_lmb_theme()
      )
    })
  })
}
