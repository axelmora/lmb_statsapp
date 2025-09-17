team_hitting <- readRDS("data/cache/team_hitting.rds")  
teams   <- sort(unique(team_hitting$Team))
seasons <- sort(unique(team_hitting$Year))

teamFiltersUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("team"), "Select Team:", choices = c("All",teams), selected = "All"),
    selectInput(ns("season"), "Select Season:", choices = c("All",seasons), selected = max(seasons))
  )
}

teamFiltersServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    list(
      team = reactive(input$team),
      season = reactive(input$season)
    )
  })
}


