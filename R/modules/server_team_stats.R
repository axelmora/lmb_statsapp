# R/modules/server_team_stats.R
server_team_stats <- function(id, data_hitting, data_pitching, data_fielding, filters) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$hitting_table <- renderReactable({
      req(filters$team(), filters$season())
      df <- data_hitting
      if (filters$team() != "All") df <- df %>% filter(Team == filters$team())
      if (filters$season() != "All") df <- df %>% filter(Year == filters$season())
      reactable(df, searchable = TRUE, sortable = TRUE)
    })
    
    output$pitching_table <- renderReactable({
      req(filters$team(), filters$season())
      df <- data_pitching
      if (filters$team() != "All") df <- df %>% filter(Team == filters$team())
      if (filters$season() != "All") df <- df %>% filter(Year == filters$season())
      reactable(df, searchable = TRUE, sortable = TRUE)
    })
    
    output$fielding_table <- renderReactable({
      req(filters$team(), filters$season())
      df <- data_fielding
      if (filters$team() != "All") df <- df %>% filter(Team == filters$team())
      if (filters$season() != "All") df <- df %>% filter(Year == filters$season())
      reactable(df, searchable = TRUE, sortable = TRUE)
    })
  })
}


