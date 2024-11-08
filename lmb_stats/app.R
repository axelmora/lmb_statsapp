# ------------------ LIBRARIES ------------------------------------------
library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(readr)
library(dplyr)

# ----------------- DATA PREPARATION -------------------------------------
lmb_hitting_standard <- read_csv("lmb_hitting_standard.csv")
lmb_hitting_standard <- as.data.frame(lmb_hitting_standard[2:24,drop = F], 
                                      stringAsFactors = FALSE)
#
lmb_hitting_advanced <- read_csv("lmb_hitting_advanced.csv")
lmb_hitting_advanced <- as.data.frame(lmb_hitting_advanced[2:19,drop = F], 
                                      stringAsFactors = FALSE)
#
lmb_pitching_standard <- read_csv("lmb_pitching_standard.csv")
lmb_pitching_standard <- as.data.frame(lmb_pitching_standard[2:24,drop = F], 
                                       stringAsFactors = FALSE)
#
lmb_pitching_advanced <- read_csv("lmb_pitching_advanced.csv")
lmb_pitching_advanced <- as.data.frame(lmb_pitching_advanced[2:26,drop = F], 
                                       stringAsFactors = FALSE)

lmb_fielding_standard <- read_csv("lmb_fielding_standard.csv")
lmb_fielding_standard <- as.data.frame(lmb_fielding_standard[2:19,drop = F], 
                                       stringAsFactors = FALSE)
#
lmb_fielding_advanced <- read_csv("lmb_fielding_advanced.csv")
lmb_fielding_advanced <- as.data.frame(lmb_fielding_advanced[2:14,drop = F], 
                                       stringAsFactors = FALSE)
#
lmb_hitting_team_standard <- read_csv("lmb_hitting_team_standard.csv")
lmb_hitting_team_standard <- as.data.frame(lmb_hitting_team_standard[2:23,drop = F], 
                                      stringAsFactors = FALSE)
#
lmb_hitting_team_advanced <- read_csv("lmb_hitting_team_advanced.csv")
lmb_hitting_team_advanced <- as.data.frame(lmb_hitting_team_advanced[2:18,drop = F], 
                                      stringAsFactors = FALSE)
#
lmb_pitching_team_standard <- read_csv("lmb_pitching_team_standard.csv")
lmb_pitching_team_standard <- as.data.frame(lmb_pitching_team_standard[2:22,drop = F], 
                                       stringAsFactors = FALSE)
#
lmb_pitching_team_advanced <- read_csv("lmb_pitching_team_advanced.csv")
lmb_pitching_team_advanced <- as.data.frame(lmb_pitching_team_advanced[2:23,drop = F], 
                                       stringAsFactors = FALSE)

lmb_fielding_team_standard <- read_csv("lmb_fielding_team_standard.csv")
lmb_fielding_team_standard <- as.data.frame(lmb_fielding_team_standard[2:11,drop = F], 
                                       stringAsFactors = FALSE)
#
lmb_fielding_team_advanced <- read_csv("lmb_fielding_team_advanced.csv")
lmb_fielding_team_advanced <- as.data.frame(lmb_fielding_team_advanced[2:14,drop = F], 
                                       stringAsFactors = FALSE)

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
      layout_sidebar(
        sidebar = sidebar(
          selectInput("player_name_h","Player Name",
                      choices = c("All", sort(lmb_hitting_standard$Name)), 
                      selected = "All")
          ),
        card(
          card_header(
            "Standard Stats"),
          card_body(
            DTOutput("hitting_std")),
        ),
        card(
          card_header(
            "Advanced Stats"),
          card_body(
            DTOutput("hitting_adv"))
        )
      )
    ),
    nav_panel(
      title = "Pitching", 
      layout_sidebar(
        sidebar = sidebar(
          selectInput("player_name_p","Player Name",
                      choices = c("All", sort(lmb_pitching_standard$Name)), 
                      selected = "All")
        ),
        card(
          card_header(
            "Standard Stats"),
          card_body(
            DTOutput("pitching_std"))
        ),
        card(
          card_header(
            "Advanced Stats"),
          card_body(
            DTOutput("pitching_adv"))
        )
      )
    ),
    nav_panel(
      title = "Fielding", 
      layout_sidebar(
        sidebar = sidebar(
          selectInput("player_name_f","Player Name",
                      choices = c("All", sort(lmb_fielding_standard$Name)), 
                      selected = "All")
        ),
        card(
          card_header(
            "Standard Stats"),
          card_body(
            DTOutput("fielding_std"))
        ),
        card(
          card_header(
            "Advanced Stats"),
          card_body(
            DTOutput("fielding_adv"))
        )
      )
    )
  ),
  nav_menu(
    title = "Team Stats",
    nav_panel(
      title = "Hitting",
        card(
          card_header(
            "Standard Stats"),
          card_body(
            DTOutput("hitting_team_std")),
        ),
        card(
          card_header(
            "Advanced Stats"),
          card_body(
            DTOutput("hitting_team_adv"))
        )
    ),
    nav_panel(
      title = "Pitching", 
        card(
          card_header(
            "Standard Stats"),
          card_body(
            DTOutput("pitching_team_std"))
        ),
        card(
          card_header(
            "Advanced Stats"),
          card_body(
            DTOutput("pitching_team_adv"))
        )
    ),
    nav_panel(
      title = "Fielding", 
        card(
          card_header(
            "Standard Stats"),
          card_body(
            DTOutput("fielding_team_std"))
        ),
        card(
          card_header(
            "Advanced Stats"),
          card_body(
            DTOutput("fielding_team_adv"))
        )
    )
  ),
  nav_panel(title = "League Stats", p("Third page content.")),
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
        pageLength = 8,
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
        pageLength = 8,
        columnDefs = list(list(targets = 0, width = '5px')
                          ,list(targets = 1, width = '180px')
                          ,list(targets = c(2:17), width = '5px')
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
        pageLength = 8, 
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
        pageLength = 8, 
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
        pageLength = 8,
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
        pageLength = 8,
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
        pageLength = 10,
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
        pageLength = 10,
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
        pageLength = 10, 
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
        pageLength = 10, 
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
        pageLength = 10,
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
        pageLength = 10,
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
}


# ---------------- RUN ------------------------------------------------------

shinyApp(ui, server)