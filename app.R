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
library(ggimage)

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
teams_ref <- readRDS("data/cache/teams_ref.rds")
logos <- readRDS("data/cache/logos.rds")
# ---- Source Helpers & Modules ----
# helpers
source("R/helpers/data_cleaning.R")
source("R/helpers/plotting.R")
source("R/helpers/utils.R")
source("R/modules/mod_team_filters.R")
source("R/modules/mod_player_filters.R")
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
source("R/modules/mod_team_comparison.R")
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
        bs4SidebarMenuSubItem("Transactions",tabName = "team_transactions",icon = icon("shuffle"))
      ),
      bs4SidebarMenuItem(
        "Player Comparison",tabName = "player_comparison",icon = icon("greater-than-equal")),
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
      bs4SidebarMenuItem("Chatbot", tabName = "chatbot", icon = icon("robot")),
      bs4SidebarMenuItem("Team Comparison", tabName = "team_comp_plot", icon = icon("chart-area"))
    )
  ),
  body = bs4DashBody(
    tags$head(
      tags$style(HTML("

      /* ── Reactable Dark Mode Fix ── */
      .dark-mode .reactable {
        background-color: #1e2329 !important;
        color: #e0e0e0 !important;
      }

      /* Table wrapper and container */
      .dark-mode .rt-table,
      .dark-mode .rt-thead,
      .dark-mode .rt-tbody,
      .dark-mode .rt-tfoot {
        background-color: #1e2329 !important;
        color: #e0e0e0 !important;
      }

      /* Header cells */
      .dark-mode .rt-th,
      .dark-mode .rt-td {
        background-color: #1e2329 !important;
        color: #e0e0e0 !important;
        border-color: #3a3f4b !important;
      }

      /* Striped rows */
      .dark-mode .rt-tr-striped {
        background-color: #252b33 !important;
      }

      /* Row hover highlight */
      .dark-mode .rt-tr-highlight:hover .rt-td {
        background-color: #2e3540 !important;
      }

      /* Sticky columns — these get their own background so fix them explicitly */
      .dark-mode .rt-td.rt-sticky-left,
      .dark-mode .rt-th.rt-sticky-left {
        background-color: #1e2329 !important;
        color: #e0e0e0 !important;
        border-right-color: #3a3f4b !important;
      }

      /* Striped sticky cells */
      .dark-mode .rt-tr-striped .rt-td.rt-sticky-left {
        background-color: #252b33 !important;
      }

      /* Highlighted sticky cells on hover */
      .dark-mode .rt-tr-highlight:hover .rt-td.rt-sticky-left {
        background-color: #2e3540 !important;
      }

      /* Pagination bar */
      .dark-mode .rt-pagination,
      .dark-mode .rt-pagination-nav {
        background-color: #1e2329 !important;
        color: #e0e0e0 !important;
      }

      .dark-mode .rt-page-button {
        background-color: #2e3540 !important;
        color: #e0e0e0 !important;
        border-color: #3a3f4b !important;
      }

      .dark-mode .rt-page-button:hover {
        background-color: #3a4150 !important;
      }

      /* Search / filter inputs inside reactable */
      .dark-mode .rt-search,
      .dark-mode .rt-filter {
        background-color: #2e3540 !important;
        color: #e0e0e0 !important;
        border-color: #3a3f4b !important;
      }

      /* Footer cells */
      .dark-mode .rt-tfoot .rt-td {
        background-color: #181d23 !important;
        color: #aaaaaa !important;
        border-top: 1px solid #3a3f4b !important;
      }

      /* Page size select dropdown */
      .dark-mode .rt-page-size select {
        background-color: #2e3540 !important;
        color: #e0e0e0 !important;
        border-color: #3a3f4b !important;
      }
      

      .dark-mode .dataTables_wrapper {
        color: #e0e0e0 !important;
        background-color: #1e2329 !important;
      }
      

      .dark-mode table.dataTable,
      .dark-mode table.dataTable thead,
      .dark-mode table.dataTable tbody,
      .dark-mode table.dataTable tfoot {
        background-color: #1e2329 !important;
        color: #e0e0e0 !important;
      }
      

      .dark-mode table.dataTable thead th,
      .dark-mode table.dataTable thead td {
        background-color: #252b33 !important;
        color: #e0e0e0 !important;
        border-bottom: 2px solid #3a3f4b !important;
      }
      

      .dark-mode table.dataTable thead .sorting::before,
      .dark-mode table.dataTable thead .sorting::after,
      .dark-mode table.dataTable thead .sorting_asc::before,
      .dark-mode table.dataTable thead .sorting_asc::after,
      .dark-mode table.dataTable thead .sorting_desc::before,
      .dark-mode table.dataTable thead .sorting_desc::after {
        color: #888 !important;
        opacity: 0.6;
      }
      

      .dark-mode table.dataTable tbody tr {
        background-color: #1e2329 !important;
        color: #e0e0e0 !important;
      }
      

      .dark-mode table.dataTable.stripe tbody tr.odd,
      .dark-mode table.dataTable.display tbody tr.odd {
        background-color: #252b33 !important;
      }
      

      .dark-mode table.dataTable.hover tbody tr:hover,
      .dark-mode table.dataTable.display tbody tr:hover {
        background-color: #2e3540 !important;
      }
      

      .dark-mode table.dataTable.hover tbody tr:hover > *,
      .dark-mode table.dataTable.display tbody tr:hover > * {
        background-color: #2e3540 !important;
        color: #e0e0e0 !important;
      }
      

      .dark-mode table.dataTable tbody tr.selected > * {
        background-color: #1a3a5c !important;
        color: #ffffff !important;
        box-shadow: inset 0 0 0 9999px rgba(13, 110, 253, 0.25) !important;
      }
      

      .dark-mode table.dataTable tbody td {
        border-top: 1px solid #3a3f4b !important;
      }
      

      .dark-mode table.dataTable tfoot th,
      .dark-mode table.dataTable tfoot td {
        background-color: #181d23 !important;
        color: #aaaaaa !important;
        border-top: 1px solid #3a3f4b !important;
      }
      
      
      .dark-mode .dataTables_wrapper .dataTables_length label {
        color: #e0e0e0 !important;
      }
      
      .dark-mode .dataTables_wrapper .dataTables_length select {
        background-color: #2e3540 !important;
        color: #e0e0e0 !important;
        border: 1px solid #3a3f4b !important;
      }
      

      .dark-mode .dataTables_wrapper .dataTables_filter label {
        color: #e0e0e0 !important;
      }
      
      .dark-mode .dataTables_wrapper .dataTables_filter input {
        background-color: #2e3540 !important;
        color: #e0e0e0 !important;
        border: 1px solid #3a3f4b !important;
      }
      
      .dark-mode .dataTables_wrapper .dataTables_filter input::placeholder {
        color: #888 !important;
      }
      
      .dark-mode .dataTables_wrapper .dataTables_info {
        color: #aaaaaa !important;
      }
      

      .dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button {
        background-color: #2e3540 !important;
        color: #e0e0e0 !important;
        border: 1px solid #3a3f4b !important;
      }
      
      .dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background: #3a4150 !important;
        color: #ffffff !important;
        border-color: #4a5160 !important;
      }
      
      .dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button.current,
      .dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
        background: #1a3a5c !important;
        color: #ffffff !important;
        border-color: #2a5a8c !important;
      }
      
      .dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button.disabled,
      .dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button.disabled:hover {
        color: #555 !important;
        background-color: #1e2329 !important;
      }
      
      .dark-mode .dataTables_wrapper .dataTables_scrollHead,
      .dark-mode .dataTables_wrapper .dataTables_scrollBody,
      .dark-mode .dataTables_wrapper .dataTables_scrollFoot {
        background-color: #1e2329 !important;
      }
      
      .dark-mode table.dataTable thead input,
      .dark-mode table.dataTable thead select {
        background-color: #2e3540 !important;
        color: #e0e0e0 !important;
        border: 1px solid #3a3f4b !important;
      }
      
      .dark-mode .dataTables_scrollBody::-webkit-scrollbar {
        background-color: #1e2329;
      }
      
      .dark-mode .dataTables_scrollBody::-webkit-scrollbar-thumb {
        background-color: #3a4150;
        border-radius: 4px;
      }

    "))
    ),
    
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
      bs4TabItem(tabName = "chatbot", h3("Chatbot"), chatbotUI("chatmod")),
      bs4TabItem(tabName = "team_comp_plot", h3("Teams Hitting vs Batting Plot"), teamComparisonUI("team_comp"))
    )
  ),
  controlbar = bs4DashControlbar(
    id = "filters",
    skin = "light",
    title = "Filters",
    collapsed = FALSE,
    uiOutput("team_filters"),
    uiOutput("player_filters")
  )
)


# ---- App Server ----
server <- function(input, output, session) {
  
  filters_player <- playerFiltersServer("player_filters")
  
  output$player_filters <- renderUI({
    if (!input$tabs %in% c("player_hit")) return(NULL)
    playerFiltersUI("player_filters")   # <-- this renders the UI
  })
  
  server_player_stats("ui_player_stats_1",
                      datasets = list(
                        hit = hitting,
                        pit = pitching,
                        fie = fielding
                      ),
                      filters_player
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
  
  teamComparisonServer("team_comp", team_hitting, team_pitching, teams_ref, logos, lmb_std, current_season = reactive(2026L))
  
}

# ---- Run App ----
shinyApp(ui = ui, server = server)
