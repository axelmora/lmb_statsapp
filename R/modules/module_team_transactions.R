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
      reactable(df, searchable = TRUE, 
                sortable       = TRUE,
                highlight      = TRUE,
                striped        = TRUE,
                compact        = TRUE,
                defaultPageSize = 25,
                columns = list(
                  Date = colDef(width = 100),
                  Type = colDef(width = 150),
                  Team = colDef(width = 230),
                  `Player Name` = colDef(width = 200),
                  Description = colDef(width = 600)
                )
      )
    })
  })
}