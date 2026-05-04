# ======================================================
# Player Comparison - UI Module
# ======================================================
playerComparisonUI <- function(id) {
  ns <- NS(id)
  
  tabsetPanel(
    id = ns("comparison_tabs"),
    tabPanel(
      "Hitters",
      fluidRow(
        column(6, selectizeInput(ns("player1"), "Select Player 1", choices = NULL,
                                 options = list(placeholder = 'Type a name...'))),
        column(6, selectizeInput(ns("player2"), "Select Player 2", choices = NULL,
                                 options = list(placeholder = 'Type a name...')))
      ),
      reactableOutput(ns("hitter_comparison_table"))
    ),
    tabPanel(
      "Pitchers",
      fluidRow(
        column(6, selectizeInput(ns("pitcher1"), "Select Pitcher 1", choices = NULL,
                                 options = list(placeholder = 'Type a name...'))),
        column(6, selectizeInput(ns("pitcher2"), "Select Pitcher 2", choices = NULL,
                                 options = list(placeholder = 'Type a name...')))
      ),
      reactableOutput(ns("pitcher_comparison_table"))
    )
  )
}
