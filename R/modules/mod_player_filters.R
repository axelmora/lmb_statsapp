hitting <- readRDS("data/cache/hitting.rds")
seasons <- sort(unique(hitting$Year))
current_teams <- sort(unique(hitting$Team[hitting$Year == max(seasons)]))

playerFiltersUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("season"), "Select Season:", choices = c("All", seasons), selected = max(seasons)),
    selectInput(ns("team_player"), "Select Team:", choices = c("All", current_teams), selected = "All"),
    selectInput(ns("players_qualified"), "Qualified:",choices  = c("All", "YES", "NO"),selected = "YES")
  )
}

playerFiltersServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Update team choices whenever the season selection changes
    observeEvent(input$season, {
      teams_for_season <- if (input$season == "All") {
        sort(unique(hitting$Team))
      } else {
        sort(unique(hitting$Team[hitting$Year == input$season]))
      }
      
      updateSelectInput(session, "team_player", choices = c("All", teams_for_season), selected = "All")
    })
    
    list(
      season = reactive(input$season),
      team_player   = reactive(input$team_player),
      qualified_filter = reactive(input$players_qualified)
    )
  })
}