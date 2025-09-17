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
        column(6, selectInput(ns("player1"), "Select Player 1", choices = NULL)),
        column(6, selectInput(ns("player2"), "Select Player 2", choices = NULL))
      ),
      reactableOutput(ns("hitter_comparison_table"))
    ),
    tabPanel(
      "Pitchers",
      fluidRow(
        column(6, selectInput(ns("pitcher1"), "Select Pitcher 1", choices = NULL)),
        column(6, selectInput(ns("pitcher2"), "Select Pitcher 2", choices = NULL))
      ),
      reactableOutput(ns("pitcher_comparison_table"))
    )
  )
}
