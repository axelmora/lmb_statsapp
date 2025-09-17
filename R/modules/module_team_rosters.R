# R/modules/module_team_rosters.R
ui_rosters <- function(id) {
  ns <- NS(id)
  fluidRow(column(12, reactableOutput(ns("rosters_table"))))
}

server_rosters <- function(id, rosters_data, filters) {
  moduleServer(id, function(input, output, session) {
    output$rosters_table <- renderReactable({
      req(filters$team())
      df <- rosters_data
      if (filters$team() != "All") df <- df %>% filter(Team == filters$team())
      reactable(df, searchable = TRUE, sortable = TRUE)
    })
  })
}



