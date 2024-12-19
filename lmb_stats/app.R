# ------------------ LIBRARIES ------------------------------------------
library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(readr)
library(dplyr)
library(googlesheets4)

# ----------------- DATA PREPARATION -------------------------------------
# Set authentication token to be stored in a folder called `.secrets`
options(gargle_oauth_cache = ".secrets")

# Authenticate manually
gs4_auth()

# If successful, the previous step stores a token file.
# Check that a file has been created with:
list.files(".secrets/")

# Check that the non-interactive authentication works by first deauthorizing:
gs4_deauth()

# Authenticate using token. If no browser opens, the authentication works.
gs4_auth(cache = ".secrets", email = "lmb.stats.app@gmail.com")
gs_ids <- read_csv("gs_ids.csv")
woba_fipc <- as.data.frame(read_sheet(gs_ids$woba_fipc),stringAsFactors = FALSE)
park_factors <- as.data.frame(read_sheet(gs_ids$park_factors),stringAsFactors = FALSE)
lmb_hitting_standard <- as.data.frame(read_sheet(gs_ids$hitting_std),stringAsFactors = FALSE)
lmb_hitting_advanced <- as.data.frame(read_sheet(gs_ids$hitting_adv),stringAsFactors = FALSE)
lmb_pitching_standard <- as.data.frame(read_sheet(gs_ids$pitching_std),stringAsFactors = FALSE)
lmb_pitching_advanced <- as.data.frame(read_sheet(gs_ids$pitching_adv),stringAsFactors = FALSE)
lmb_fielding_standard <- as.data.frame(read_sheet(gs_ids$fielding_std),stringAsFactors = FALSE)
lmb_fielding_advanced <- as.data.frame(read_sheet(gs_ids$fielding_adv),stringAsFactors = FALSE)
lmb_hitting_team_standard <- as.data.frame(read_sheet(gs_ids$team_hitting_std),stringAsFactors = FALSE)
lmb_hitting_team_advanced <- as.data.frame(read_sheet(gs_ids$team_hitting_adv),stringAsFactors = FALSE)
lmb_pitching_team_standard <- as.data.frame(read_sheet(gs_ids$team_pitching_std),stringAsFactors = FALSE)
lmb_pitching_team_advanced <- as.data.frame(read_sheet(gs_ids$team_pitching_adv),stringAsFactors = FALSE)
lmb_fielding_team_standard <- as.data.frame(read_sheet(gs_ids$team_fielding_std),stringAsFactors = FALSE)
lmb_fielding_team_advanced <- as.data.frame(read_sheet(gs_ids$team_fielding_adv),stringAsFactors = FALSE)
league_pace <- as.data.frame(read_sheet(gs_ids$lmb_pace_24),stringAsFactors = FALSE)
teams_pace <- as.data.frame(read_sheet(gs_ids$lmb_pace_venue_24),stringAsFactors = FALSE)
lmb_att_24 <- as.data.frame(read_sheet(gs_ids$lmb_att_24),stringAsFactors = FALSE)
# ----------- SETUP -------------------------------------------------------
thematic::thematic_shiny(font = "auto")
theme_set(theme_bw(base_size = 10))

#-------------------- UI ---------------------------------------------------

ui <- page_navbar(
  input_dark_mode(id = "mode"),
  tags$head(
    tags$style(HTML("
      table.dataTable {
        font-size: 16px;
        padding: 0px;
      }
      table.dataTable td, table.dataTable th {
        padding: 0px;
      }
      .dataTable td {

      }
      .dataTable thead {
        
      }
       .dataTables_paginate {
        font-size: 6px;  /* Smaller font size */
        padding: 0px;     /* Less padding */
      }
      .dataTables_paginate .paginate_button {
        padding: 0px; /* Smaller padding for page buttons */
        font-size: 6px;   /* Smaller font size for page buttons */
      }
      .dataTables_info {
        font-size: 10px;  /* Smaller font size for information text */
        padding: 0px;     /* Less padding */
      }
    ")
    )
  ),
  title = "LMB Stats App",
  theme = bs_theme(
    bootswatch = "cerulean", version = 5),
  nav_menu(
    title = "Player Stats",
    nav_panel(
      title = "Hitting",
      layout_column_wrap(
      height = "5px",
      selectInput("year_h","Season",
                  choices = c(2024,2023,2022,2021,2019), 
                  selected = 2024),
      selectInput("player_name_h","Player Name",
                                  choices = c("All", sort(lmb_hitting_standard$Name)), 
                                  selected = "All")
      ),
      navset_card_tab(
        full_screen = TRUE,
        title = "Hitting",
        nav_panel(
          "Standard Stats",
            DTOutput("hitting_std")
        ),
        nav_panel(
          "Advanced Stats",
            DTOutput("hitting_adv")
        )
      )
    ),
    nav_panel(
      title = "Pitching", 
      selectInput("player_name_p","Player Name",
                  choices = c("All", sort(lmb_pitching_standard$Name)), 
                  selected = "All"),
      navset_card_tab(
        full_screen = TRUE,
        title = "Pitching",
        nav_panel(
          "Standard Stats",
          DTOutput("pitching_std")
        ),
        nav_panel(
          "Advanced Stats",
          DTOutput("pitching_adv")
        )
      )
    ),
    nav_panel(
      title = "Fielding", 
      selectInput("player_name_f","Player Name",
                  choices = c("All", sort(lmb_fielding_standard$Name)), 
                  selected = "All"),
      navset_card_tab(
        full_screen = TRUE,
        title = "Fielding",
        nav_panel(
          "Standard Stats",
          DTOutput("fielding_std")
        ),
        nav_panel(
          "Advanced Stats",
          DTOutput("fielding_adv")
        )
      )
    )
  ),
  nav_menu(
    title = "Team Stats",
    nav_panel(
      title = "Hitting",
      navset_card_tab(
        full_screen = TRUE,
        title = "Hitting",
        nav_panel(
          "Standard Stats",
          DTOutput("hitting_team_std")
        ),
        nav_panel(
          "Advanced Stats",
          DTOutput("hitting_team_adv")
        )
      )
    ),
    nav_panel(
      title = "Pitching", 
      navset_card_tab(
        full_screen = TRUE,
        title = "Pitching",
        nav_panel(
          "Standard Stats",
          DTOutput("pitching_team_std")
        ),
        nav_panel(
          "Advanced Stats",
          DTOutput("pitching_team_adv")
        )
      )
    ),
    nav_panel(
      title = "Fielding", 
      navset_card_tab(
        full_screen = TRUE,
        title = "Fielding",
        nav_panel(
          "Standard Stats",
          DTOutput("fielding_team_std")
        ),
        nav_panel(
          "Advanced Stats",
          DTOutput("fielding_team_adv")
        )
      )
    )
  ),
  nav_menu(
    title = "Misc Stats",
    nav_panel(
      title = "Game Pace",
      layout_column_wrap(
        height = "20px",
        value_box(
          title = 'Hits per 9 innings',
          value = textOutput("hits9")
        ),
        value_box(
          title = 'Runs per 9 innings',
          value = textOutput("runs9")
        ),
        value_box(
          title = 'Pitches per pitcher',
          value = textOutput("pitches")
        ),
        value_box(
          title = 'Time per pitch (Sec)',
          value = textOutput("time_pitch")
        ),
        value_box(
          title = 'Time per PA (Sec)',
          value = textOutput("time_pa")
        ),
        value_box(
          title = 'Game time',
          value = textOutput("time_game")
        ),
      ),
      card(
        card_header(
          "Game Pace by Venue/Team"),
        card_body(
          DTOutput("teams_pace_dt"))
      )
    ),
    nav_panel(
      title = "Attendance",
      layout_column_wrap(
        height = "20px",
        value_box(
          title = 'Avg Attendance',
          value = textOutput("lmb_att_avg")
        ),
        value_box(
          title = 'Avg Attendance Pct',
          value = textOutput("lmb_cap_pct")
        ),
        value_box(
          title = 'Max Attendance',
          value = textOutput("lmb_max_att")
        ),
      ),
      card(
        card_header(
          "Attendance"),
        card_body(
          DTOutput("lmb_att_dt"))
      )
    ),
    nav_panel(
      title = "GUTS",
      card(
        max_height = 250,
        card_header(
          "wOBA and FIP constant"),
        card_body(
          DTOutput("woba_fip_dt"))
      ),
      card(
        card_header(
          "Park Factors 2024",
          max_height = 250),
        card_body(
          DTOutput("pf_dt"))
      )
    )
  ),
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a("X", href = "https://x.com/axelmora93")),
    nav_item(tags$a("LinkedIn", href = "https://linkedin.com/in/axelmora"))

  )
)


# ------ SERVER --------------------------------------------------

server <- function(input, output, session) {
  
  filtered_hitting_std <- reactive({
    lmb_hitting_standard %>%
      filter(if (input$year_h != 9999) Year %in% input$year_h else TRUE) %>%
      filter(if (input$player_name_h != "All") Name %in% input$player_name_h else TRUE)
  })
  filtered_hitting_adv <- reactive({
    lmb_hitting_advanced %>%
      filter(if (input$player_name_h != "All") Name %in% input$player_name_h else TRUE)
  })
  filtered_pitching_std <- reactive({
    lmb_pitching_standard %>%
      filter(if (input$player_name_p != "All") Name %in% input$player_name_p else TRUE) 
  })
  filtered_pitching_adv <- reactive({
    lmb_pitching_advanced %>%
      filter(if (input$player_name_p != "All") Name %in% input$player_name_p else TRUE) 
  })
  filtered_fielding_std <- reactive({
    lmb_fielding_standard %>%
      filter(if (input$player_name_f != "All") Name %in% input$player_name_f else TRUE) 
  })
  filtered_fielding_adv <- reactive({
    lmb_fielding_advanced %>%
      filter(if (input$player_name_f != "All") Name %in% input$player_name_f else TRUE) 
  })
  
  output$hitting_std <- renderDT({
    datatable(
      filtered_hitting_std(),
      rownames = FALSE,
      options = list(
        dom = 'tip',
        pageLength = 20,
        columnDefs = list(list(targets = 0, width = '5px')
                          ,list(targets = 1, width = '180px')
                          ,list(targets = c(2:22), width = '5px')
                          ,list(targets = "_all", className = 'dt-left')
                          ),
        order = list(5, 'desc')
        ,scrollX = FALSE
      )
    ) %>%
      formatRound(columns = 20:22, digits = 3)
  })
  
  output$hitting_adv <- renderDT({
    datatable(
      filtered_hitting_adv(),
      rownames = FALSE,
      options = list(
        dom = 'tip',
        pageLength = 20,
        columnDefs = list(list(targets = 0, width = '5px')
                          ,list(targets = 1, width = '180px')
                          ,list(targets = c(2:21), width = '5px')
                          ,list(targets = "_all", className = 'dt-left')
                          ),
        order = list(5, 'desc')
        ,scrollX = FALSE
      )
    ) 
  })
  
  output$pitching_std <- renderDT({
    datatable(
      filtered_pitching_std(),
      rownames = FALSE,
      options = list(
        dom = 'tip',
        pageLength = 20, 
        columnDefs = list(list(targets = 0, width = '5px')
                           ,list(targets = 1, width = '180px')
                           ,list(targets = c(3:22), width = '5px')
                           ,list(targets = "_all", className = 'dt-left')
                          )
        ,order = list(6, 'desc')
        ,scrollX = FALSE
      )
    )%>%
      formatRound(columns = 7, digits = 2)
  })
  
  output$pitching_adv <- renderDT({
    datatable(
      filtered_pitching_adv(),
      rownames = FALSE,
      options = list(
        dom = 'tip',
        pageLength = 20, 
        columnDefs = list(list(targets = 0, width = '5px')
                          ,list(targets = 1, width = '180px')
                          #,list(targets = c(3:25), width = '5px')
                          ,list(targets = "_all", className = 'dt-left')
                          )
        ,order = list(6, 'desc')
        ,scrollX = FALSE
      )
    )
  })
  
  output$fielding_std <- renderDT({
    datatable(
      filtered_fielding_std(),
      rownames = FALSE,
      options = list(
        dom = 'tip',
        pageLength = 20,
        columnDefs = list(list(targets = 0, width = '5px')
                          ,list(targets = 1, width = '180px')
                          ,list(targets = c(3:17), width = '5px')
                          ,list(targets = "_all", className = 'dt-left')
                          )
        ,order = list(6, 'desc')
        ,scrollX = FALSE
      )
    )%>%
      formatRound(columns = c(11,16), digits = 3)
  })
  
  output$fielding_adv <- renderDT({
    datatable(
      filtered_fielding_adv(),
      rownames = FALSE,
      options = list(
        dom = 'tip',
        pageLength = 20,
        columnDefs = list(list(targets = 0, width = '5px')
                          ,list(targets = 1, width = '180px')
                          ,list(targets = c(3:12), width = '5px')
                          ,list(targets = "_all", className = 'dt-left')
                          )
        ,order = list(6, 'desc')
        ,scrollX = FALSE
      )
    )
  })
  
  output$hitting_team_std <- renderDT({
    datatable(
      lmb_hitting_team_standard,
      rownames = FALSE,
      options = list(
        dom = 'tip',
        pageLength = 20,
        columnDefs = list(list(targets = 0, width = '5px')
                          ,list(targets = 1, width = '180px')
                          #,list(targets = c(2:22), width = '5px')
                          ,list(targets = "_all", className = 'dt-left')
        ),
        order = list(1, 'asc')
        ,scrollX = FALSE
      )
    ) 
    #%>%
    #  formatRound(columns = 20:22, digits = 3)
  })
  
  output$hitting_team_adv <- renderDT({
    datatable(
      lmb_hitting_team_advanced,
      rownames = FALSE,
      options = list(
        dom = 'tip',
        pageLength = 20,
        columnDefs = list(list(targets = 0, width = '5px')
                          ,list(targets = 1, width = '180px')
                          #,list(targets = c(2:17), width = '5px')
                          ,list(targets = "_all", className = 'dt-left')
        ),
        order = list(1, 'asc')
        ,scrollX = FALSE
      )
    ) 
  })
  
  output$pitching_team_std <- renderDT({
    datatable(
      lmb_pitching_team_standard,
      rownames = FALSE,
      options = list(
        dom = 'tip',
        pageLength = 20, 
        columnDefs = list(list(targets = 0, width = '5px')
                          ,list(targets = 1, width = '180px')
                          #,list(targets = c(3:22), width = '5px')
                          ,list(targets = "_all", className = 'dt-left')
        )
        ,order = list(1, 'asc')
        ,scrollX = FALSE
      )
    )
    #%>%
    #  formatRound(columns = 7, digits = 2)
  })
  
  output$pitching_team_adv <- renderDT({
    datatable(
      lmb_pitching_team_advanced,
      rownames = FALSE,
      options = list(
        dom = 'tip',
        pageLength = 20, 
        columnDefs = list(list(targets = 0, width = '5px')
                          ,list(targets = 1, width = '180px')
                          #,list(targets = c(3:23), width = '5px')
                          ,list(targets = "_all", className = 'dt-left')
        )
        ,order = list(1, 'asc')
        ,scrollX = FALSE
      )
    )
  })
  
  output$fielding_team_std <- renderDT({
    datatable(
      lmb_fielding_team_standard,
      rownames = FALSE,
      options = list(
        dom = 'tip',
        pageLength = 20,
        columnDefs = list(list(targets = 0, width = '5px')
                          ,list(targets = 1, width = '180px')
                          #,list(targets = c(3:17), width = '5px')
                          ,list(targets = "_all", className = 'dt-left')
        )
        ,order = list(1, 'asc')
        ,scrollX = FALSE
      )
    )
    #%>%
    #  formatRound(columns = c(11,16), digits = 3)
  })
  
  output$fielding_team_adv <- renderDT({
    datatable(
      lmb_fielding_team_advanced,
      rownames = FALSE,
      options = list(
        dom = 'tip',
        pageLength = 20,
        columnDefs = list(list(targets = 0, width = '5px')
                          ,list(targets = 1, width = '180px')
                          #,list(targets = c(3:12), width = '5px')
                          ,list(targets = "_all", className = 'dt-left')
        )
        ,order = list(1, 'asc')
        ,scrollX = FALSE
      )
    )
  })
  
  output$teams_pace_dt <- renderDT({
    datatable(
      teams_pace,
      rownames = FALSE,
      options = list(
        dom = 't'
        ,pageLength = 21
        ,columnDefs = list(list(targets = 0, width = '150x')
                          ,list(targets = 1, width = '200px')
                          ,list(targets = c(2:7), width = '5px')
                          ,list(targets = "_all", className = 'dt-left')
        )
        ,scrollX = FALSE
      )
    )
  })
  
  output$lmb_att_dt <- renderDT({
    datatable(
      lmb_att_24,
      rownames = FALSE,
      options = list(
        dom = 't'
        ,pageLength = 20
        ,columnDefs = list(list(targets = 0, width = '150x')
                           ,list(targets = 1, width = '200px')
                           ,list(targets = c(2:8), width = '5px')
                           ,list(targets = "_all", className = 'dt-left')
        )
        ,scrollX = FALSE
      )
    )
  })
  
  output$woba_fip_dt <- renderDT({
    datatable(
      woba_fipc,
      rownames = FALSE,
      options = list(
        dom = 't'
        ,scrollX = FALSE
      )
    )%>%
      formatRound(columns = 2:11, digits = 3)
  })
  
  output$pf_dt <- renderDT({
    datatable(
      park_factors,
      rownames = FALSE,
      options = list(
        dom = 't'
        ,pageLength = 20
        ,scrollX = FALSE
      )
    )
  })
  
  output$hits9 <- renderText({
    league_pace$`Hits/9in`
  })
  
  output$runs9 <- renderText({
    league_pace$`Runs/9in`
  })
  
  output$pitches <- renderText({
    league_pace$`Pitches/Pitcher`
  })
  
  output$time_pitch <- renderText({
    league_pace$`Time/Pitch`
  })
  
  output$time_pa <- renderText({
    league_pace$`Time/PA`
  })
  
  output$time_game <- renderText({
    format(league_pace$`Time/9inGame`)
  })
  
  output$lmb_att_avg <- renderText({
    round(sum(lmb_att_24$`Total Home Attendance`)/sum(lmb_att_24$`Home Openings`),1)
  })
  
  output$lmb_cap_pct <- renderText({
    round(((sum(lmb_att_24$`Total Home Attendance`)/sum(lmb_att_24$`Home Openings`))*100)/mean(lmb_att_24$Capacity),1)
  })
  
  output$lmb_max_att <- renderText({
    max(lmb_att_24$`High Home Attendance`)
  })
}


# ---------------- RUN ------------------------------------------------------

shinyApp(ui, server)