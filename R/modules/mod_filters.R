# ======================================================
# Universal Filters Module
# ======================================================
# Requires a `teams_ref` data frame loaded in app.R with columns:
#   Season   — numeric or character year (must match the Season/Year col in each dataset)
#   TeamId   — neutral key used for filtering (e.g. "MTY")
#   Team     — full name shown when use_abbr = FALSE (e.g. "Sultanes de Monterrey")
#   Abbr     — abbreviation shown when use_abbr = TRUE  (e.g. "MTY")
#
# Usage in app.R:
#   filtersUI("player_filters", use_abbr = TRUE,  show_qualified = TRUE)
#   filtersUI("team_filters",   use_abbr = FALSE, show_qualified = FALSE)
#
#   filters_player <- filtersServer("player_filters", teams_ref, use_abbr = TRUE)
#   filters_team   <- filtersServer("team_filters",   teams_ref, use_abbr = FALSE)
#
# Each server returns:
#   $season()          — selected season ("All" or a value)
#   $team_id()         — selected TeamId  ("All" or a TeamId value)
#   $qualified_filter()— "All" / "YES" / "NO"  (NULL if show_qualified = FALSE)
# ======================================================

filtersUI <- function(id, use_abbr = FALSE, show_qualified = FALSE) {
  ns <- NS(id)
  
  tagList(
    selectInput(ns("season"), "Select Season:",
                choices  = NULL,   # populated by server on init
                selected = NULL),
    selectInput(ns("team"), "Select Team:",
                choices  = c("All"),
                selected = "All"),
    if (show_qualified)
      selectInput(ns("qualified"), "Qualified:",
                  choices  = c("All", "YES", "NO"),
                  selected = "YES")
  )
}


filtersServer <- function(id, teams_ref, use_abbr = FALSE, show_qualified = FALSE) {
  moduleServer(id, function(input, output, session) {
    
    seasons <- sort(unique(teams_ref$Season))
    
    # Helper: build the label→value named vector for the team dropdown.
    # `label` is what the user sees; `value` is always TeamId (neutral key).
    team_choices <- function(season_val) {
      df <- if (season_val == "All") teams_ref else teams_ref[teams_ref$Season == season_val, ]
      df <- df[!duplicated(df$TeamId), ]
      df <- df[order(if (use_abbr) df$Abbr else df$Team), ]
      label <- if (use_abbr) df$Abbr else df$Team
      stats::setNames(df$TeamId, label)   # named vector: shown label → TeamId value
    }
    
    # Populate season dropdown on init
    updateSelectInput(session, "season",
                      choices  = c("All", seasons),
                      selected = max(seasons))
    
    # Cascade: update team dropdown whenever season changes
    observeEvent(input$season, {
      req(input$season)
      choices <- team_choices(input$season)
      updateSelectInput(session, "team",
                        choices  = c("All", choices),
                        selected = "All")
    })
    
    # Return neutral keys so every server module works the same way
    list(
      season          = reactive(input$season),
      team_id         = reactive(input$team),       # always a TeamId or "All"
      qualified_filter = if (show_qualified) reactive(input$qualified) else reactive("All")
    )
  })
}