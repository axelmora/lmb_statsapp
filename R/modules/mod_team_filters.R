team_hitting <- readRDS("data/cache/team_hitting.rds")
seasons <- sort(unique(team_hitting$Season))
current_teams <- sort(unique(team_hitting$Team[team_hitting$Season == max(seasons)]))

teamFiltersUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("season"), "Select Season:", choices = c("All", seasons), selected = max(seasons)),
    selectInput(ns("team"), "Select Team:", choices = c("All", current_teams), selected = "All")
  )
}

teamFiltersServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Update team choices whenever the season selection changes
    observeEvent(input$season, {
      teams_for_season <- if (input$season == "All") {
        sort(unique(team_hitting$Team))
      } else {
        sort(unique(team_hitting$Team[team_hitting$Season == input$season]))
      }
      
      updateSelectInput(session, "team", choices = c("All", teams_for_season), selected = "All")
    })
    
    list(
      season = reactive(input$season),
      team   = reactive(input$team)
    )
  })
}


