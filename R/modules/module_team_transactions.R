ui_transactions <- function(id) {
  ns <- NS(id)
  fluidRow(column(12, reactableOutput(ns("transactions_table"))))
}

server_transactions <- function(id, trans_data, filters) {
  moduleServer(id, function(input, output, session) {
    output$transactions_table <- renderReactable({
      req(filters$team())
      df <- trans_data
      if (filters$team() != "All") df <- df %>% filter(Team == filters$team())
      reactable(df, searchable = TRUE, sortable = TRUE)
    })
  })
}