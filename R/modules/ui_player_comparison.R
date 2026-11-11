
# ======================================================
# Player Comparison - UI Module
# ======================================================

playerComparisonUI <- function(id) {
  
  ns <- NS(id)
  
  tabsetPanel(
    id = ns("comparison_tabs"),
    
    # --------------------------------------------------
    # HITTERS
    # --------------------------------------------------
    tabPanel(
      "Hitters",
      
      fluidRow(
        column(
          6,
          selectizeInput(
            ns("player1"),
            "Select Player 1",
            choices = NULL,
            options = list(placeholder = "Type a name...")
          ),
          selectInput(
            ns("season1"),
            "Season",
            choices = NULL
          )
        ),
        
        column(
          6,
          selectizeInput(
            ns("player2"),
            "Select Player 2",
            choices = NULL,
            options = list(placeholder = "Type a name...")
          ),
          selectInput(
            ns("season2"),
            "Season",
            choices = NULL
          )
        )
      ),
      
      reactableOutput(ns("hitter_comparison_table"))
    ),
    
    # --------------------------------------------------
    # PITCHERS
    # --------------------------------------------------
    tabPanel(
      "Pitchers",
      
      fluidRow(
        column(
          6,
          selectizeInput(
            ns("pitcher1"),
            "Select Pitcher 1",
            choices = NULL,
            options = list(placeholder = "Type a name...")
          ),
          selectInput(
            ns("pitcher_season1"),
            "Season",
            choices = NULL
          )
        ),
        
        column(
          6,
          selectizeInput(
            ns("pitcher2"),
            "Select Pitcher 2",
            choices = NULL,
            options = list(placeholder = "Type a name...")
          ),
          selectInput(
            ns("pitcher_season2"),
            "Season",
            choices = NULL
          )
        )
      ),
      
      reactableOutput(ns("pitcher_comparison_table"))
    )
  )
}
