# ======================================================
# Player Stats - UI
# ======================================================
ui_player_stats <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "players",
    fluidRow(
      column(
        width = 12,
        tabsetPanel(
          id = "tabsetpanel1",
            tabPanel(
              "Hitting",
              reactableOutput(ns("player_hit_table"))
            ),
            tabPanel(
              "Pitching",
              reactableOutput(ns("player_pit_table"))
            ),
            tabPanel(
              "Fielding",
              reactableOutput(ns("player_fie_table"))
            )
        ) 
      )
    )
  )
}

