# R/modules/server_team_stats.R
server_team_stats <- function(id, data_hitting, data_pitching, data_fielding, filters) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$hitting_table <- renderReactable({
      req(filters$team(), filters$season())
      df <- data_hitting
      if (filters$team() != "All") df <- df %>% filter(Team == filters$team())
      if (filters$season() != "All") df <- df %>% filter(Season == filters$season())
      reactable(df,
                sortable = TRUE,
                highlight      = TRUE,
                striped        = TRUE,
                compact        = TRUE,
                defaultPageSize = 20,
                defaultColDef  = colDef(
                  minWidth = 65
                ),
                columns = list(
                  Team = colDef(sticky = "left",
                                style = list(borderRight = "1px solid #eee"),
                                headerStyle = list(borderRight = "1px solid #eee"),
                                width = 230)
                )
      )
    })
    
    output$pitching_table <- renderReactable({
      req(filters$team(), filters$season())
      df <- data_pitching
      if (filters$team() != "All") df <- df %>% filter(Team == filters$team())
      if (filters$season() != "All") df <- df %>% filter(Season == filters$season())
      reactable(df,
                sortable = TRUE,
                highlight      = TRUE,
                striped        = TRUE,
                compact        = TRUE,
                defaultPageSize = 20,
                defaultColDef  = colDef(
                  minWidth = 65
                ),
                columns = list(
                  Team = colDef(sticky = "left",
                                style = list(borderRight = "1px solid #eee"),
                                headerStyle = list(borderRight = "1px solid #eee"),
                                width = 230)
                )
      )
    })
    
    output$fielding_table <- renderReactable({
      req(filters$team(), filters$season())
      df <- data_fielding
      if (filters$team() != "All") df <- df %>% filter(Team == filters$team())
      if (filters$season() != "All") df <- df %>% filter(Season == filters$season())
      reactable(df,
                sortable = TRUE,
                highlight      = TRUE,
                striped        = TRUE,
                compact        = TRUE,
                defaultPageSize = 20,
                defaultColDef  = colDef(
                  minWidth = 65
                ),
                columns = list(
                  Team = colDef(sticky = "left",
                                style = list(borderRight = "1px solid #eee"),
                                headerStyle = list(borderRight = "1px solid #eee"),
                                width = 230)
                )
      )
    })
  })
}


