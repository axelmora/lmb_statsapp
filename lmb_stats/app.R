# ------------------ LIBRARIES ------------------------------------------
library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(readr)
library(dplyr)
library(tidyr)
library(googlesheets4)
library(data.table)
library(future)
library(promises)
library(reactable)
library(shinyWidgets)
library(baseballr)

plan(multisession)
# ----------------- DATA PREPARATION -------------------------------------
# Set authentication token to be stored in a folder called `.secrets`
options(gargle_oauth_cache = ".secrets")

# Authenticate manually
#gs4_auth()

# If successful, the previous step stores a token file.
# Check that a file has been created with:
list.files(".secrets/")

# Check that the non-interactive authentication works by first deauthorizing:
gs4_deauth()

# Authenticate using token. If no browser opens, the authentication works.
gs4_auth(cache = ".secrets", email = "lmb.stats.app@gmail.com")

hitters <- read_csv("hitters.csv")
pitchers <- read_csv("pitchers.csv")
fielders <- read_csv("fielders.csv")
teams <- read_csv("teams.csv")

# Function to load or cache Google Sheets data
load_or_cache_data <- function(sheet_id, file_path) {
  if (file.exists(file_path)) {
    # Load from cache
    message("Loading data from cache: ", file_path)
    print(Sys.time())
    readRDS(file_path)
  } else {
    # Fetch from Google Sheets and save to cache
    message("Fetching data from Google Sheets: ", sheet_id)
    data <- read_sheet(sheet_id)
    saveRDS(data, file_path)
    data
  }
}

# Batch load and cache all datasets
load_data <- function(gs_ids, cache_dir = "cache") {
  # Ensure cache directory exists
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir)
  }
  
  datasets <- list()
  for (name in names(gs_ids)) {
    file_path <- file.path(cache_dir, paste0(name, ".rds"))
    datasets[[name]] <- load_or_cache_data(gs_ids[[name]], file_path)
  }
  datasets
}

gs_ids <- list(
  woba_fipc = "1H8xuzPAjuNJxlWaBAk1fmo-lGH9Jfm0Zeyrcv0R5OPY",
  park_factors = "1VkbsNGuHrhZHYoMdxiWVjeUyc3kSMLafYpg0nLHHdcY",
  hitting = "1eVJ4dSg6KgATE8zmPxvriHyk6ZRZB94fuDWpdcloC1M",
  pitching = "1xlYBP_x1mfsuFDnMxk4gtPUIl7FTIJ0hfVRK3BPeeRQ",
  fielding = "1hVRO16lDvVHkYGva06sgG1715wYdEKHaIy2GwyC_K8U",
  team_hitting = "1gkOC_566Bp7XCuuyEsIkK7jT0V0TMn--Xbuv1i4Wdxc",
  team_pitching = "1uOckNtrCu811khLZ1CxSU7C2TLapZubH4g-Yh2XJTh4",
  team_fielding = "1GzUH0leAnoAM64ml-XQ2dR9IFL19u31YCY6Ta955xlQ",
  lmb_att_24 = "1uer8QQuM-x8VyxCDJBlCtqXofPHE-Dlkzzk64xzLFBQ",
  lmb_pace_24 = "1sJ_KjQgmKDUtLRh1MW0yHyQWTzllarKHXGzSGrLNVBk",
  lmb_pace_venue_24 = "1_zF8o6iYKrcpE0Cwgky4dpm7gXMt4nU1At7Z4j4qFJI",
  game_logs = "1NHm3ZAMeTpBag95kOpozo_8Ngkxh9VSXot4bbEQpkR8",
  hitting_cp = "1K-wOBfh9QW4ucEShjypBXQkti962Yajyf-Da5QEKbg0",
  pitching_cp = "1Bkbt91wCmAbAGSlMpMgx8dHcPhsyFjxVpi5eo9gy3do",
  rosters = "1zeor7gan9NlpzDx_Dh5h1soDpdSye-V-joHZwXH9F2Y",
  trans = "1_Wy1KW5AUH_NTbog_QJ9dFiq5Ujm719MBM0YZ0X7EZw"
)

refresh_data <- function(gs_ids) {
  cache_dir = "cache"
  datasets <- list()
  for (name in names(gs_ids)) {
    file_path <- file.path(cache_dir, paste0(name, ".rds"))
    print(gs_ids[[name]])
    data <- read_sheet(gs_ids[[name]])
    saveRDS(data, file_path)
    print(Sys.time())
  }
}

# ----------- SETUP -------------------------------------------------------
thematic::thematic_shiny(font = "auto")
theme_set(theme_bw(base_size = 10))

#-------------------- UI ---------------------------------------------------

ui <- page_navbar(
  #input_dark_mode(id = "mode"),
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
    bootswatch = "litera", version = 5),
  nav_menu(
    title = "Player Stats",
    nav_panel(
      title = "Hitting",
      layout_column_wrap(
        height = "5px",
        selectInput("year_h","Season",
                    choices = c(2025,2024,2023,2022,2021,2019), 
                    selected = 2025),
        selectInput("teams_h","Team",
                    choices = c("All", sort(teams$team_full_name)), 
                    selected = "All"),
        materialSwitch(inputId = "HQlf",
                       label = "Qualified Players", 
                       value = FALSE)
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
      layout_column_wrap(
        height = "5px",
      selectInput("year_p","Season",
                  choices = c(2025,2024,2023,2022,2021,2019), 
                  selected = 2025),
      selectInput("teams_p","Team",
                  choices = c("All", sort(teams$team_full_name)), 
                  selected = "All"),
      materialSwitch(inputId = "PQlf",
                     label = "Qualified Players", 
                     value = FALSE)
      ),
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
      layout_column_wrap(
        height = "5px",
      selectInput("year_f","Season",
                  choices = c(2025,2024,2023,2022,2021,2019), 
                  selected = 2025),
      selectInput("teams_f","Team",
                  choices = c("All", sort(teams$team_full_name)), 
                  selected = "All")
      ),
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
    title = "Player comparison",
  nav_panel(
    "Hitters",
    page_sidebar(
      sidebar = sidebar(
        title = "Players selection",
        selectInput("player1","Player 1 Name",
                    choices = c("All", sort(hitters$x)), 
                    selected = NULL),
        selectInput("player2","Player 2 Name",
                    choices = c("All", sort(hitters$x)), 
                    selected = NULL),
      ),
      card(
        full_screen = TRUE,
        card_header(
          "Player Statistics Comparison"),
        card_body(
          reactableOutput("hitter_comparison_table"))
          )
      )
    ),
  nav_panel(
    "Pitchers",
    page_sidebar(
      sidebar = sidebar(
        title = "Players selection",
        selectInput("pitcher1","Player 1 Name",
                    choices = c("All", sort(pitchers$x)), 
                    selected = NULL),
        selectInput("pitcher2","Player 2 Name",
                    choices = c("All", sort(pitchers$x)), 
                    selected = NULL),
      ),
      card(
        full_screen = TRUE,
        card_header(
          "Player Statistics Comparison"),
        card_body(
          reactableOutput("pitcher_comparison_table"))
      )
    )
  )
  ),
  nav_menu(
    title = "Team Stats",
    nav_panel(
      title = "Hitting",
      selectInput("year_h","Season",
                  choices = c(2025,2024,2023,2022,2021,2019), 
                  selected = 2025),
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
      selectInput("year_h","Season",
                  choices = c(2025,2024,2023,2022,2021,2019), 
                  selected = 2025),
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
      selectInput("year_h","Season",
                  choices = c(2025,2024,2023,2022,2021,2019), 
                  selected = 2025),
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
    ),
    nav_panel(
      title = "Rosters",
        selectInput("team_name","Team",
                    choices = sort(teams$team_full_name)
                    ,selected = "Acereros del Norte"),
      reactableOutput("roster")
    ),
    nav_panel(
      title = "Transactions",
      selectInput("team_name2","Team",
                  choices = c("All",teams$team_full_name)
                  ,selected = "All"),
      DTOutput("trans")
    )
  ),
  nav_panel(
    "Game Logs",
    DTOutput("game_logs")
  ),
  nav_menu(
    title = "Extras",
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
      title = "Guts!",
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
    nav_item(tags$a("LinkedIn", href = "https://linkedin.com/in/axelmora")),
    nav_panel(
      title = "About",
      textOutput("last_refresh_time")
    )
  )
)


# ------ SERVER --------------------------------------------------

server <- function(input, output, session) {

  refresh_timer <- reactiveTimer(intervalMs = 300000)
  
  observe({
    refresh_timer()
    
    current_time <- format(Sys.time(), "%H:%M")
    
    if (current_time %in% c("05:00","05:01","05:02","05:03","05:04","05:05","23:00","23:01","23:02","23:03","23:04","23:05")) {
      refresh_data(gs_ids)
      print("DATA REFRESHED")
    }
  })
  
  # observeEvent(refresh_timer(), {
  #  if (format(Sys.time(), "%H:%M") == "15:30") {  # Check if it's midnight
  #    refresh_data(gs_ids)  # Refresh the cache
  #    print("DATA REFRESHED")
  #  }
  #})
  
  datasets <- reactiveVal(NULL)
  
  observe({
    future({
      load_data(gs_ids)  # Load data with caching
    }) %...>% datasets %...!% (function(err) {
      showNotification(paste("Error loading data:", err$message), type = "error")
    })
  })
  
  ##### DATA Team AND YEAR FILTERS
  
  gm <- max(mlb_standings(season = 2025, league_id = 125) %>% select(team_records_games_played))
  qp <- gm*0.8
  qh <- gm*2.7

  filtered_team <- reactive({
    req(datasets())
    datasets()$rosters %>%
      filter(Team %in% input$team_name)
  })
  filtered_trans <- reactive({
    req(datasets())
    datasets()$trans %>%
      filter(if (input$team_name2 != "All") Team %in% input$team_name2 else TRUE)
  })
  filtered_hitting_std <- reactive({
    req(datasets())
    datasets()$hitting[,1:23] %>%
      filter(if (input$year_h != 9999) Year %in% input$year_h else TRUE) %>%
      filter(if (input$teams_h != "All") Team %in% teams$team_abbreviation[which(teams$team_full_name == input$teams_h)] else TRUE) %>%
      filter(if (input$HQlf == TRUE) PA >= qh else TRUE)
  })
  filtered_hitting_adv <- reactive({
    req(datasets())
    datasets()$hitting[,-c(8:23)] %>%
      filter(if (input$year_h != 9999) Year %in% input$year_h else TRUE) %>%
      filter(if (input$teams_h != "All") Team %in% teams$team_abbreviation[which(teams$team_full_name == input$teams_h)] else TRUE) %>%
      filter(if (input$HQlf == TRUE) PA >= qh else TRUE)
  })
  filtered_pitching_std <- reactive({
    req(datasets())
    datasets()$pitching[,1:24] %>%
      filter(if (input$year_p != 9999) Year %in% input$year_p else TRUE) %>%
      filter(if (input$teams_p != "All") Team %in% teams$team_abbreviation[which(teams$team_full_name == input$teams_p)] else TRUE) %>%
      filter(if (input$PQlf == TRUE) IP >= qp else TRUE)
  })
  filtered_pitching_adv <- reactive({
    req(datasets())
    datasets()$pitching[,-c(8:24)] %>%
      filter(if (input$year_p != 9999) Year %in% input$year_p else TRUE) %>%
      filter(if (input$teams_p != "All") Team %in% teams$team_abbreviation[which(teams$team_full_name == input$teams_p)] else TRUE) %>%
      filter(if (input$PQlf == TRUE) IP >= qp else TRUE)
  })
  filtered_fielding_std <- reactive({
    req(datasets())
    datasets()$fielding[,1:18] %>%
      filter(if (input$year_f != 9999) Year %in% input$year_f else TRUE) %>%
      filter(if (input$teams_h != "All") Team %in% teams$team_abbreviation[which(teams$team_full_name == input$teams_h)] else TRUE)
  })
  filtered_fielding_adv <- reactive({
    req(datasets())
    datasets()$fielding[,-c(8:18)] %>%
      filter(if (input$year_f != 9999) Year %in% input$year_f else TRUE) %>%
      filter(if (input$teams_h != "All") Team %in% teams$team_abbreviation[which(teams$team_full_name == input$teams_h)] else TRUE)
  })
  
  ######### FILTER YEAR DATA TEAMS
  
  filter_hitting_team_std <- reactive({
    req(datasets())
    datasets()$team_hitting[,1:22] %>%
      filter(if (input$year_p != 9999) Year %in% input$year_h else TRUE) 
  })
  filter_pitching_team_std <- reactive({
    req(datasets())
    datasets()$team_pitching[,c(1:21)] %>%
      filter(if (input$year_f != 9999) Year %in% input$year_p else TRUE) 
  })
  filter_fielding_team_std <- reactive({
    req(datasets())
    datasets()$team_fielding[,c(1:10)] %>%
      filter(if (input$year_f != 9999) Year %in% input$year_f else TRUE)
  })
  
  filter_hitting_team_adv <- reactive({
    req(datasets())
    datasets()$team_hitting[,-c(6:22)] %>%
      filter(if (input$year_p != 9999) Year %in% input$year_h else TRUE) 
  })
  filter_pitching_team_adv <- reactive({
    req(datasets())
    datasets()$team_pitching[-c(5:21)] %>%
      filter(if (input$year_f != 9999) Year %in% input$year_p else TRUE) 
  })
  filter_fielding_team_adv <- reactive({
    req(datasets())
    datasets()$team_fielding[,-c(4:10)] %>%
      filter(if (input$year_f != 9999) Year %in% input$year_f else TRUE)
  })
  
  ########## PLAYER DATA STATS
  
  output$hitting_std <- renderDT({
    req(filtered_hitting_std())
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
    req(filtered_hitting_adv())
    datatable(
      filtered_hitting_adv(),
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
    ) 
  })
  
  output$pitching_std <- renderDT({
    req(filtered_pitching_std())
    datatable(
      filtered_pitching_std(),
      rownames = FALSE,
      options = list(
        dom = 'tip',
        pageLength = 20, 
        columnDefs = list(list(targets = 0, width = '5px')
                          ,list(targets = 1, width = '180px')
                          ,list(targets = c(3:22), width = '10px')
                          ,list(targets = "_all", className = 'dt-left')
        )
        ,order = list(6, 'desc')
        ,scrollX = FALSE
      )
    )%>%
      formatRound(columns = 8, digits = 2) %>%
      formatRound(columns = 7, digits = 1)
  })
  
  output$pitching_adv <- renderDT({
    req(filtered_pitching_adv())
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
    req(filtered_fielding_std())
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
    req(filtered_fielding_adv())
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
  
  ############### TEAMS STATS ###########
  
  output$hitting_team_std <- renderDT({
    req(filter_hitting_team_std())
    datatable(
      filter_hitting_team_std(),
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
    req(filter_hitting_team_adv())
    datatable(
      filter_hitting_team_adv(),
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
    req(filter_pitching_team_std())
    datatable(
      filter_pitching_team_std(),
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
    req(filter_pitching_team_adv())
    datatable(
      filter_pitching_team_adv(),
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
    req(filter_fielding_team_std())
    datatable(
      filter_fielding_team_std(),
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
    req(filter_fielding_team_adv())
    datatable(
      filter_fielding_team_adv(),
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
  
  ############### MISC AND GUTS ###############
  
  output$teams_pace_dt <- renderDT({
    req(datasets()$lmb_pace_venue_24)
    datatable(
      datasets()$lmb_pace_venue_24,
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
    req(datasets()$lmb_att_24)
    datatable(
      datasets()$lmb_att_24[,-5],
      rownames = FALSE,
      options = list(
        dom = 't'
        ,pageLength = 20
        ,columnDefs = list(list(targets = 0, width = '150x')
                           ,list(targets = 1, width = '200px')
                           ,list(targets = c(2:7), width = '5px')
                           ,list(targets = "_all", className = 'dt-left')
        )
        ,scrollX = FALSE
      )
    ) %>%
      formatCurrency(3:7, currency = "", interval = 3, mark = ",", digits = 0)
  })
  
  output$woba_fip_dt <- renderDT({
    req(datasets()$woba_fipc)
    datatable(
      datasets()$woba_fipc,
      rownames = FALSE,
      options = list(
        dom = 't'
        ,scrollX = FALSE
      )
    )%>%
      formatRound(columns = 2:11, digits = 3)
  })
  
  output$pf_dt <- renderDT({
    req(datasets()$park_factors)
    datatable(
      datasets()$park_factors,
      rownames = FALSE,
      options = list(
        dom = 't'
        ,pageLength = 20
        ,scrollX = FALSE
      )
    )
  })
  
  output$game_logs <- renderDT({
    req(datasets()$game_logs)
    datatable(
      datasets()$game_logs
      ,escape = FALSE
      ,rownames = FALSE
      ,options = list(
        dom = 'tip'
        ,pageLength = 30
        ,scrollX = FALSE
        ,columnDefs = list(list(targets = 0, width = '50x')
                           ,list(targets = 1, width = '50px')
                           ,list(targets = c(3,5), width = '10px')
                           ,list(targets = c(1,3,5,9), className = 'dt-center')
                           ,list(targets = c(2,4,6), width = '250px')
                           ,list(targets = c(7,8), width = '20px')
                           ,list(targets = 9, width = '100px')
                           ,list(targets = 10, width = '100px')
                           ,list(targets = "_all", className = 'dt-left')
        )
      )
    )
  })
  
  output$roster <- renderReactable({
    req(filtered_team())
    reactable(
      filtered_team()[,c(1:7,9)],
      groupBy = "position_group",
      compact = TRUE,  
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE
    )
  })
  
  output$trans <- renderDT({
    req(filtered_trans())
    trans_data <- filtered_trans() %>%
      mutate(Date = format(as.Date(Date), "%Y-%m-%d"))
    datatable(
      trans_data
      ,escape = FALSE
      ,rownames = FALSE
      ,options = list(
        dom = 'tip'
        ,pageLength = 30
        ,scrollX = FALSE
        ,columnDefs = list(list(targets = c(0,1), width = '50x')
                           ,list(targets = 2, width = '250px')
                           ,list(targets = 3, width = '200px')
                           ,list(targets = "_all", className = 'dt-left')
        )
      )
    )
  })
  
  output$hits9 <- renderText({
    datasets()$lmb_pace_24$`Hits/9in`
  })
  
  output$runs9 <- renderText({
    datasets()$lmb_pace_24$`Runs/9in`
  })
  
  output$pitches <- renderText({
    datasets()$lmb_pace_24$`Pitches/Pitcher`
  })
  
  output$time_pitch <- renderText({
    datasets()$lmb_pace_24$`Time/Pitch`
  })
  
  output$time_pa <- renderText({
    datasets()$lmb_pace_24$`Time/PA`
  })
  
  output$time_game <- renderText({
    format(datasets()$lmb_pace_24$`Time/9inGame`)
  })
  
  output$lmb_att_avg <- renderText({
    round(sum(datasets()$lmb_att_24$`Total Home Attendance`)/
            sum(datasets()$lmb_att_24$`Home Openings`),1)
  })
  
  output$lmb_cap_pct <- renderText({
    round(((sum(datasets()$lmb_att_24$`Total Home Attendance`)/
              sum(datasets()$lmb_att_24$`Home Openings`))*100)/
            mean(datasets()$lmb_att_24$Capacity),1)
  })
  
  output$lmb_max_att <- renderText({
    max(datasets()$lmb_att_24$`High Home Attendance`)
  })
  
  
########## PLAYERS COMPARISSON #########
  player_data <- reactive({
    req(datasets())
    datasets()$hitting_cp
  })
  
  observe({
    updateSelectInput(session, "player1", choices = sort(unique(player_data()$Name)), selected = NULL)
    updateSelectInput(session, "player2", choices = sort(unique(player_data()$Name)), selected = NULL)
  })
  
  selected_players <- reactive({
    req(input$player1, input$player2)
    player_data() %>% 
      filter(Name %in% c(input$player1, input$player2))
  })
  
  output$hitter_comparison_table <- renderReactable({
    req(input$player1, input$player2) 
    
    # Validate if the same player is selected
    if (input$player1 == input$player2) {
      showNotification("⚠ Please select two different players to compare!", type = "warning", duration = 5)
      return(NULL)  # Prevents the table from rendering
    }
    
    trans_data <- transpose(selected_players(), keep.names = "Stats", make.names = "Name")
    
    # Ensure numeric conversion for comparisons
    trans_data[, -1] <- lapply(trans_data[, -1], as.numeric)
    
    # Get the player column names dynamically (assuming they are in columns 2 and 3)
    player_cols <- colnames(trans_data)[2:3]
    
    # Reorder columns: Move "Stats" to the second position
    trans_data <- trans_data[, c(player_cols[1], "Stats", player_cols[2])]
    
    reactable(trans_data,
              columns = c(
                setNames(
                  lapply(player_cols, function(col_name) {
                    colDef(
                      align = "center",
                      style = function(value, index) {
                        # Find the opponent column dynamically
                        opponent_col <- ifelse(col_name == player_cols[1], player_cols[2], player_cols[1])
                        if (is.na(value) || is.na(trans_data[index, opponent_col])) return("")
                        if (value > trans_data[index, opponent_col]) "background-color: #D4EDDA;" else ""
                      }
                    )
                  }),
                  player_cols
                ),
                list(Stats = colDef(name = "Statistic", align = "center"))  # Now placed in the second position
              ),
              pagination = FALSE,
              defaultColDef = colDef(align = "center"),
              bordered = TRUE,
              highlight = TRUE
    )
  })
  
  ####
  pitcher_data <- reactive({
    req(datasets())
    datasets()$pitching_cp
  })
  
  observe({
    updateSelectInput(session, "pitcher1", choices = sort(unique(pitcher_data()$Name)), selected = NULL)
    updateSelectInput(session, "pitcher2", choices = sort(unique(pitcher_data()$Name)), selected = NULL)
  })
  
  selected_pitchers <- reactive({
    req(input$pitcher1, input$pitcher2)
    pitcher_data() %>% 
      filter(Name %in% c(input$pitcher1, input$pitcher2))
  })
  
  output$pitcher_comparison_table <- renderReactable({
    req(input$pitcher1, input$pitcher2) 
    
    # Validate if the same pitcher is selected
    if (input$pitcher1 == input$pitcher2) {
      showNotification("⚠ Please select two different pitchers to compare!", type = "warning", duration = 5)
      return(NULL)  # Prevents the table from rendering
    }
    
    trans_data_p <- transpose(selected_pitchers(), keep.names = "Stats", make.names = "Name")
    
    # Ensure numeric conversion for comparisons
    trans_data_p[, -1] <- lapply(trans_data_p[, -1], as.numeric)
    
    # Get the pitcher column names dynamically (assuming they are in columns 2 and 3)
    pitcher_cols <- colnames(trans_data_p)[2:3]
    
    # Reorder columns: Move "Stats" to the second position
    trans_data_p <- trans_data_p[, c(pitcher_cols[1], "Stats", pitcher_cols[2])]
    
    lower_is_better <- c("ERA", "WHIP", "FIP", "BB%")
    
    reactable(trans_data_p,
              columns = c(
                setNames(
                  lapply(pitcher_cols, function(col_name) {
                    colDef(
                      align = "center",
                      style = function(value, index) {
                        stat_name <- trans_data_p$Stats[index]
                        opponent_col <- ifelse(col_name == pitcher_cols[1], pitcher_cols[2], pitcher_cols[1])
                        opponent_value <- trans_data_p[index, opponent_col]
                        
                        if (is.na(value) || is.na(opponent_value)) return("")
                        
                        if (stat_name %in% lower_is_better) {
                          if (value < opponent_value) "background-color: #D4EDDA;" else ""
                        } else {
                          if (value > opponent_value) "background-color: #D4EDDA;" else ""
                        }
                      }
                    )
                  }),
                  pitcher_cols
                ),
                list(Stats = colDef(name = "Statistic", align = "center"))
              ),
              pagination = FALSE,
              defaultColDef = colDef(align = "center"),
              bordered = TRUE,
              highlight = TRUE
    )
  })
  
  ####
  
  output$last_refresh_time <- renderText({
    paste()
  })
  
}


# ---------------- RUN ------------------------------------------------------

shinyApp(ui, server)