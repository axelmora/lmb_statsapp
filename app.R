# ======================================================
# LMB Shiny App - Main Entrypoint
# ======================================================

# ---- Load Required Packages ----
library(shiny)
library(bs4Dash)
library(shinythemes)
library(dplyr)
library(reactable)
library(ggplot2)
library(data.table)
library(DT)
library(gt)
library(plotly)
library(shinyWidgets)
library(shinyjs)

options(
  shiny.sanitize.errors = TRUE,
  shiny.suppressDuplicateIDs = TRUE  # <--- suppress duplicate-ID messages
)

team_hitting <- readRDS("data/cache/team_hitting.rds")
team_pitching <- readRDS("data/cache/team_pitching.rds")
team_fielding <- readRDS("data/cache/team_fielding.rds")
hitting <- readRDS("data/cache/hitting.rds")
pitching <- readRDS("data/cache/pitching.rds")
fielding <- readRDS("data/cache/fielding.rds")
hitting_cp <- readRDS("data/cache/hitting_cp.rds")
pitching_cp <- readRDS("data/cache/pitching_cp.rds")
rosters <- readRDS("data/cache/rosters.rds")
trans <- readRDS("data/cache/trans.rds")
game_logs <- readRDS("data/cache/game_logs.rds")
sur_matrix <- readRDS("data/cache/hth_sur_matrix.rds")
nte_matrix <- readRDS("data/cache/hth_nte_matrix.rds")
sur_std <- readRDS("data/cache/stan_sur.rds")
nte_std <- readRDS("data/cache/stan_nte.rds")
lmb_std <- readRDS("data/cache/stan_lmb.rds")
lmb_att <- readRDS("data/cache/lmb_att_24.rds")
lmb_pace <- readRDS("data/cache/lmb_pace_24.rds")
lmb_pace_venue <- readRDS("data/cache/lmb_pace_venue_24.rds")
guts <- readRDS("data/cache/woba_fipc.rds")
pf <- readRDS("data/cache/park_factors.rds")
nte_leader_long <- readRDS("data/cache/nte_leader_long.rds")
team_colors_nte <- readRDS("data/cache/team_colors_nte.rds")
sur_leader_long <- readRDS("data/cache/sur_leader_long.rds")
team_colors_sur <- readRDS("data/cache/team_colors_sur.rds")
# ---- Source Helpers & Modules ----
# helpers
source("R/helpers/data_cleaning.R")
source("R/helpers/plotting.R")
source("R/helpers/utils.R")
source("R/modules/mod_team_filters.R")
# modules (UI + server)
source("R/modules/ui_player_stats.R")
source("R/modules/server_player_stats.R")
source("R/modules/ui_team_stats.R")
source("R/modules/server_team_stats.R")
source("R/modules/ui_player_comparison.R")
source("R/modules/server_player_comparison.R")
source("R/modules/module_team_transactions.R")
source("R/modules/module_team_rosters.R")
source("R/modules/module_game_logs.R")
source("R/modules/module_standings.R")
source("R/modules/module_pace_venue.R")
source("R/modules/module_att.R")
source("R/modules/module_guts.R")
source("R/modules/module_reports.R")
source("R/modules/mod_chatbot.R")
# ... add more as needed

# ---- App UI ----
ui <- bs4DashPage(
  title = "LMB Analytics",
  
  header = bs4DashNavbar(
    skin = "light",
    status = "success",
    border = TRUE,
    sidebarIcon = icon("bars"),
    controlbarIcon = icon("th"),
    fixed = TRUE
  ),
  
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "success",
    title = "LMB Analytics",
    bs4SidebarMenu(
      id = "tabs",
      bs4SidebarMenuItem("Welcome", tabName = "welcome", icon = icon("home")),
      bs4SidebarMenuItem("Players", tabName = "player_hit", icon = icon("user")),
      bs4SidebarMenuItem("Teams", tabName = "teams_stats", icon = icon("users"), startExpanded = TRUE,
        bs4SidebarMenuSubItem("Team Stats",tabName = "team_stats",icon = icon("table")),
        bs4SidebarMenuSubItem("Rosters",tabName = "team_rosters",icon = icon("address-book")),
        bs4SidebarMenuSubItem("Transactions",tabName = "team_transactions",icon = icon("exchange-alt"))
      ),
      bs4SidebarMenuItem(
        "Player Comparison",tabName = "player_comparison",icon = icon("arrow-right-arrow-left")),
      bs4SidebarMenuItem("Game Logs",tabName = "game_logs",icon = icon("list-alt")),
      bs4SidebarMenuItem("Standings",tabName = "standings_menu",icon = icon("trophy"),startExpanded = TRUE,
        bs4SidebarMenuSubItem("Standings",tabName = "standings",icon = icon("list-ol")),
        bs4SidebarMenuSubItem("League Standing",tabName = "league_standing",icon = icon("globe")),
        bs4SidebarMenuSubItem("Head-to-Head",tabName = "head_to_head",icon = icon("exchange-alt")),
        bs4SidebarMenuSubItem("Standing Evolution",tabName = "standing_evolution",icon = icon("chart-line"))
      ),
      bs4SidebarMenuItem(
      "Extras",tabName = "extras_menu",icon = icon("star"),startExpanded = TRUE,
        bs4SidebarMenuSubItem("Game Pace",tabName = "game_pace",icon = icon("clock")),
        bs4SidebarMenuSubItem("Attendance",tabName = "attendance",icon = icon("users")),
        bs4SidebarMenuSubItem("Guts",tabName = "guts",icon = icon("fire"))
      ),
      bs4SidebarMenuItem("Reports",tabName = "reports",icon = icon("file-pdf")),
      bs4SidebarMenuItem("Chatbot", tabName = "chatbot", icon = icon("robot"))
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      bs4TabItem(tabName = "welcome", h3("Bienvenidos")),
      bs4TabItem(tabName = "player_hit",ui_player_stats("ui_player_stats_1")),
      bs4TabItem(tabName = "team_stats",ui_team_stats("ui_team_stats_1")),
      bs4TabItem(tabName = "team_rosters",ui_rosters("ui_rosters_1")),
      bs4TabItem(tabName = "team_transactions",ui_transactions("ui_transactions_1")),
      bs4TabItem(tabName = "player_comparison",playerComparisonUI("comparison")),
      bs4TabItem(tabName = "game_logs",h3("Game Logs"),ui_game_logs("ui_game_logs_1")),
      bs4TabItem(tabName = "standings", h3("Standings"), ui_zone_std("ui_zone_std_1")),
      bs4TabItem(tabName = "league_standing", h3("League Standing"), ui_lg_std("ui_lg_std_1")),
      bs4TabItem(tabName = "head_to_head", h3("Head-to-Head"), ui_h2h("ui_h2h_1")),
      bs4TabItem(tabName = "standing_evolution", h3("Standing Evolution"), ui_std_evol("ui_std_evol_1")),
      bs4TabItem(tabName = "game_pace",ui_game_pace("ui_game_pace_1")),
      bs4TabItem(tabName = "attendance",ui_att("ui_att_1")),
      bs4TabItem(tabName = "guts", ui_guts("ui_guts_1")),
      bs4TabItem(tabName = "reports",h3("Custom Reports"),ui_reports("ui_reports_1")),
      bs4TabItem(tabName = "chatbot", h3("Chatbot"), chatbotUI("chatmod"))
    )
  ),
  controlbar = bs4DashControlbar(
    id = "filters",
    skin = "light",
    title = "Filters",
    collapsed = FALSE,
    uiOutput("team_filters"),
    uiOutput("players_filters")
  )
)


# ---- App Server ----
server <- function(input, output, session) {
  # Dynamic filter UI based on dataset
  teams   <- sort(unique(team_hitting$Team))
  seasons <- sort(unique(team_hitting$Year))
  
  output$players_filters <- renderUI({
    if (input$tabs != "player_hit") return(NULL)
    tagList(
      selectInput("players_year", "Select Season:",
                  choices  = c("All", seasons),
                  selected = max(seasons))
    )
  })
   
  server_player_stats(
    id            = "ui_player_stats_1",
    datasets = list(
      hit = hitting,
      pit = pitching,
      fie = fielding
    ),
    season_filter = reactive(input$players_year)
  )

  filters <- teamFiltersServer("team_filters")

  output$team_filters <- renderUI({
    if (!input$tabs %in% c("team_stats", "team_rosters", "team_transactions")) return(NULL)
    teamFiltersUI("team_filters")   # <-- this renders the UI
  })
  
  server_team_stats("ui_team_stats_1",team_hitting,team_pitching,team_fielding, filters)
  server_rosters("ui_rosters_1", rosters, filters)
  server_transactions("ui_transactions_1", trans, filters)

  playerComparisonServer(
    id     = "comparison", 
    hit_cp = hitting_cp,
    pit_cp = pitching_cp)
  
  server_game_logs("ui_game_logs_1",game_logs)

  server_std("ui_zone_std_1",datasets = list(
      nte = nte_std,
      sur = sur_std
    ))
  server_h2h("ui_h2h_1",datasets = list(
      nte = nte_matrix,
      sur = sur_matrix
    ))
  server_std("ui_lg_std_1", datasets = list(
    lmb = lmb_std
  ))
  server_std_evol("ui_std_evol_1", datasets = list(
      nte = nte_leader_long,
      sur = sur_leader_long,
      colors_nte = team_colors_nte,
      colors_sur = team_colors_sur
  ))

  # Extras
  server_game_pace("ui_game_pace_1",lmb_pace_venue, lmb_pace)
  server_att("ui_att_1",lmb_att)
  server_guts("ui_guts_1",guts,pf)
  server_reports("ui_reports_1", player_data = hitting, team_data = team_hitting)

  chatbotServer(
    id = "chatmod",
    datasets = list(
      hitting = hitting,
      pitching = pitching,
      fielding = fielding,
      team_hitting = team_hitting,
      team_pitching = team_pitching,
      team_fielding = team_fielding,
      rosters = rosters,
      trans = trans,
      game_logs = game_logs
    )
  )
  
}

# ---- Run App ----
shinyApp(ui = ui, server = server)
