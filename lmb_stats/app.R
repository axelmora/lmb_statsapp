library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(readr)

lmb_hitting_standard <- read_csv("lmb_hitting_standard.csv")
lmb_hitting_standard <- lmb_hitting_standard[2:24]
lmb_hitting_standard$Name <- factor(lmb_hitting_standard$Name)
lmb_hitting_standard$Team <- factor(lmb_hitting_standard$Team)
lmb_hitting_standard$POS <- factor(lmb_hitting_standard$POS)
#
lmb_hitting_advanced <- read_csv("lmb_hitting_advanced.csv")
lmb_hitting_advanced <- lmb_hitting_advanced[2:19]
lmb_hitting_advanced$Name <- factor(lmb_hitting_advanced$Name)
lmb_hitting_advanced$Team <- factor(lmb_hitting_advanced$Team)
lmb_hitting_advanced$POS <- factor(lmb_hitting_advanced$POS)
#
lmb_pitching_standard <- read_csv("lmb_pitching_standard.csv")
lmb_pitching_standard <- lmb_pitching_standard[2:24]
lmb_pitching_standard$Name <- factor(lmb_pitching_standard$Name)
lmb_pitching_standard$Team <- factor(lmb_pitching_standard$Team)
lmb_pitching_standard$POS <- factor(lmb_pitching_standard$POS)
#
lmb_pitching_advanced <- read_csv("lmb_pitching_advanced.csv")
lmb_pitching_advanced <- lmb_pitching_advanced[2:25]
lmb_pitching_advanced$Name <- factor(lmb_pitching_advanced$Name)
lmb_pitching_advanced$Team <- factor(lmb_pitching_advanced$Team)
lmb_pitching_advanced$POS <- factor(lmb_pitching_advanced$POS)


lmb_fielding_standard <- read_csv("lmb_fielding_standard.csv")
lmb_fielding_standard <- lmb_fielding_standard[2:19]
lmb_fielding_standard$Name <- factor(lmb_fielding_standard$Name)
lmb_fielding_standard$Team <- factor(lmb_fielding_standard$Team)
lmb_fielding_standard$POS <- factor(lmb_fielding_standard$POS)
#
lmb_fielding_advanced <- read_csv("lmb_fielding_advanced.csv")
lmb_fielding_advanced <- lmb_fielding_advanced[2:14]
lmb_fielding_advanced$Name <- factor(lmb_fielding_advanced$Name)
lmb_fielding_advanced$Team <- factor(lmb_fielding_advanced$Team)
lmb_fielding_advanced$POS <- factor(lmb_fielding_advanced$POS)

# Setup -------------------------------------------------------------------


# Turn on thematic for theme-matched plots
thematic::thematic_shiny(font = "auto")
theme_set(theme_bw(base_size = 14))

# UI ----------------------------------------------------------------------

ui <- page_navbar(
  title = "LMB Stats App",
  theme = bs_theme(
    bootswatch = "darkly", version = 5),
  nav_panel(
    title = "Player stats", 
    #layout_sidebar(
            navset_card_underline(
              title = "Standard Stats",
              nav_panel("Hitting", DTOutput("hitting_std")),
              nav_panel("Pitching", DTOutput("pitching_std")),
              nav_panel("Fielding", DTOutput("fielding_std"))
              ),
            navset_card_underline(
              title = "Advanced Stats",
              nav_panel("Hitting", DTOutput("hitting_adv")),
              nav_panel("Pitching", DTOutput("pitching_adv")),
              nav_panel("Fielding", DTOutput("fielding_adv"))
            )
     # )
  ),
  nav_panel(title = "Team Stats", p("Second page content.")),
  nav_panel(title = "League Stats", p("Third page content.")),
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a("X", href = "https://x.com/axelmora93")),
    nav_item(tags$a("LinkedIn", href = "https://linkedin.com/in/axelmora"))

  )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {
  output$hitting_std <- renderDT({
    datatable(
      lmb_hitting_standard,
      filter = "top",
      extensions = c('Buttons'),
      rownames = FALSE,
      options = list(
        dom = 'Brtip',
        buttons = 
          list('print', list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'
          )),
        pageLength = 10,
        columnDefs = list(list(targets = 0, width = '5px')
                          ,list(targets = 1, width = '180px')
                          ,list(targets = 2, width = '100px')
                          ,list(targets = c(2:22), width = '5px')
                          ),
        order = list(5, 'desc')
        ,scrollX = FALSE
      )
    ) %>%
      formatRound(columns = 20:22, digits = 3)
  })
  
  output$hitting_adv <- renderDT({
    datatable(
      lmb_hitting_advanced,
      filter = "top",
      extensions = c('Buttons'),
      rownames = FALSE,
      options = list(
        dom = 'Brtip',
        buttons = 
          list('print', list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'
          )),
        pageLength = 10,
        columnDefs = list(list(targets = 0, width = '5px')
                          ,list(targets = 1, width = '180px')
                          ,list(targets = 2, width = '100px')
                          ,list(targets = c(2:17), width = '5px')
        ),
        order = list(5, 'desc')
        ,scrollX = FALSE
      )
    ) 
  })
  
  output$pitching_std <- renderDT({
    datatable(
      lmb_pitching_standard,
      filter = "top",
      extensions = c('Buttons'),
      rownames = FALSE,
      options = list(
        dom = 'Brtip',
        buttons = 
          list('print', list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'
          )),
        pageLength = 10, 
        columnDefs = list(list(targets = 0, width = '5px')
                           ,list(targets = 1, width = '180px')
                           ,list(targets = 2, width = '100px')
                           ,list(targets = c(3:22), width = '5px')
        )
        ,order = list(6, 'desc')
        ,scrollX = FALSE
      )
    )%>%
      formatRound(columns = 7, digits = 2)
  })
  
  output$pitching_adv <- renderDT({
    datatable(
      lmb_pitching_advanced,
      filter = "top",
      extensions = c('Buttons'),
      rownames = FALSE,
      options = list(
        dom = 'Brtip',
        buttons = 
          list('print', list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'
          )),
        pageLength = 10, 
        columnDefs = list(list(targets = 0, width = '5px')
                          ,list(targets = 1, width = '180px')
                          ,list(targets = 2, width = '100px')
                          ,list(targets = c(3:23), width = '5px')
        )
        ,order = list(6, 'desc')
        ,scrollX = FALSE
      )
    )
  })
  
  output$fielding_std <- renderDT({
    datatable(
      lmb_fielding_standard,
      filter = "top",
      extensions = c('Buttons'),
      rownames = FALSE,
      options = list(
        dom = 'Brtip',
        buttons = 
          list('print', list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'
          )),
        pageLength = 10,
        columnDefs = list(list(targets = 0, width = '5px')
                          ,list(targets = 1, width = '180px')
                          ,list(targets = 2, width = '100px')
                          ,list(targets = c(3:17), width = '5px')
        )
        ,order = list(6, 'desc')
        ,scrollX = FALSE
      )
    )%>%
      formatRound(columns = c(11,16), digits = 3)
  })
  
  output$fielding_adv <- renderDT({
    datatable(
      lmb_fielding_advanced,
      filter = "top",
      extensions = c('Buttons'),
      rownames = FALSE,
      options = list(
        dom = 'Brtip',
        buttons = 
          list('print', list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'
          )),
        pageLength = 10,
        columnDefs = list(list(targets = 0, width = '5px')
                          ,list(targets = 1, width = '180px')
                          ,list(targets = 2, width = '100px')
                          ,list(targets = c(3:12), width = '5px')
        )
        ,order = list(6, 'desc')
        ,scrollX = FALSE
      )
    )
  })
}


# Shiny App ---------------------------------------------------------------

shinyApp(ui, server)