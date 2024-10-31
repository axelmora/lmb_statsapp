library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(readr)

lmb_hitting_2024_ind <- read_csv("lmb_hitting_2024.csv")
lmb_hitting_2024_ind <- lmb_hitting_2024_ind[2:25]
lmb_pitching_2024_ind <- read_csv("lmb_pitching_2024.csv")
lmb_pitching_2024_ind <- lmb_pitching_2024_ind[2:23]
lmb_fielding_2024_ind <- read_csv("lmb_fielding_2024.csv")
lmb_fielding_2024_ind <- lmb_fielding_2024_ind[2:16]
# Setup -------------------------------------------------------------------


# Turn on thematic for theme-matched plots
#thematic::thematic_shiny(font = "auto")
#theme_set(theme_bw(base_size = 16))

# Calculate column means for the value boxes
#tot <- sum(lmb_att_24_0801$`ASISTENCIA TOTAL CASA`)
#avg <- tot/(sum(lmb_att_24_0801$`APERTURAS EN CASA`))


# UI ----------------------------------------------------------------------

ui <- page_sidebar(
  title = "LMB Stats App",
  theme = bs_theme(
    bootswatch = "cerulean"),
  sidebar = sidebar(
    
  ),
  navset_card_underline(
    title = "Players Stats",
    nav_panel("Hitting Stats", DTOutput("hitting")),
    nav_panel("Pitching Stats", DTOutput("pitching")),
    nav_panel("Fielding Stats", DTOutput("fielding"))
  )
  
)


# Server ------------------------------------------------------------------

server <- function(input, output) {
  output$hitting <- renderDT({
    datatable(
      lmb_hitting_2024_ind,
      extensions = c('Buttons','FixedColumns'),
      rownames = FALSE,
      options = list(
        dom = 'Bfrtip',
        buttons = 
          list('print', list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'
          )),
        pageLength = 20, 
        responsive = TRUE,
        autoWidth = TRUE,
        fixedColumns = TRUE,
        columnDefs = list(list( targets = 1, width = '150px'),
                          list( targets = 0, width = '20px'),
                          list( targets = c(2:19), width = '10px')),
        scrollX = TRUE
      )
    )
  })
  
  output$fielding <- renderDT({
    datatable(
      lmb_fielding_2024_ind,
      extensions = c('Buttons','FixedColumns'),
      rownames = FALSE,
      options = list(
        dom = 'Bfrtip',
        buttons = 
          list('print', list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'
          )),
        pageLength = 20, 
        responsive = TRUE,
        autoWidth = TRUE,
        fixedColumns = TRUE,
        columnDefs = list(list( targets = 1, width = '150px'),
                          list( targets = 0, width = '20px'),
                          list( targets = c(2:19), width = '10px')),
        scrollX = TRUE
      )
    )
  })
  
  output$pitching <- renderDT({
    datatable(
      lmb_pitching_2024_ind,
      extensions = c('Buttons','FixedColumns'),
      rownames = FALSE,
      options = list(
        dom = 'Bfrtip',
        buttons = 
          list('print', list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'
          )),
        pageLength = 20, 
        responsive = TRUE,
        autoWidth = TRUE,
        fixedColumns = TRUE,
        columnDefs = list(list( targets = 1, width = '150px'),
                          list( targets = 0, width = '20px'),
                          list( targets = c(2:19), width = '10px')),
        scrollX = TRUE
      )
    )
  })
  
  
}


# Shiny App ---------------------------------------------------------------

shinyApp(ui, server)