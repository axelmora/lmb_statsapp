ui_team_stats <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      12,
      tabsetPanel(
        id = ns("tabsetpanel2"),
        tabPanel("Hitting",  reactableOutput(ns("hitting_table"))),
        tabPanel("Pitching", reactableOutput(ns("pitching_table"))),
        tabPanel("Fielding", reactableOutput(ns("fielding_table")))
      )
    )
  )
}
