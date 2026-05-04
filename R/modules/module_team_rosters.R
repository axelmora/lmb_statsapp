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
      reactable(df, 
                searchable = TRUE, 
                sortable = TRUE,
                groupBy = "position_group",
                highlight      = TRUE,
                striped        = TRUE,
                compact        = TRUE,
                defaultPageSize = 25,
                columns = list(
                  Number = colDef(width = 70),
                  Name = colDef(width = 200),
                  Team = colDef(width = 230),
                  `Birth Country` = colDef(width = 200)
                )
      )
    })
  })
}



