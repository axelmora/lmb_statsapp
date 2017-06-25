library(RCurl)
library(XML)
library(xml2)
library(chron)
library(xts)
library(tidyverse)
library(dygraphs)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "LMB Stats ShinyApp"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Index", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Standings", tabName = "Standings", icon = icon("th"))
      )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard"
      ),
      tabItem(tabName = "Standings",
              tabBox(title = "Evolution standings",
                  width = 12,
                  tabPanel("General",
                    dygraphOutput("standings")
                  ),
                  tabPanel("Norte",
                    dygraphOutput("norte")
                  ),
                  tabPanel("Sur",
                    dygraphOutput("sur")
                  )
              )
      )
    )
  )
)

server <- function(input, output) { 
  
  
  
  output$standings <- renderDygraph({
    lmbts <- read.csv("lmbts.csv")
    lmb.ts <- xts(lmbts[,-1], order.by = as.Date(lmbts$DATE, "%Y-%m-%d"))
    
    dygraph(lmb.ts, main = "2017 LMB standings", ylab = "Win/Loss percentage") %>%
      dySeries("mxo", label = "Diablos Rojos") %>%
      dySeries("leo", label = "Bravos") %>%
      dySeries("vra", label = "Rojos del Aguila") %>%
      dySeries("vaq", label = "Vaqueros") %>%
      dySeries("mty", label = "Sultanes") %>%
      dySeries("mva", label = "Acereros") %>%
      dySeries("yuc", label = "Leones") %>%
      dySeries("cam", label = "Piratas") %>%
      dySeries("tab", label = "Olmecas") %>%
      dySeries("pue", label = "Pericos") %>%
      dySeries("oax", label = "Guerreros") %>%
      dySeries("qui", label = "Tigres") %>%
      dySeries("slt", label = "Saraperos") %>%
      dySeries("tij", label = "Toros") %>%
      dySeries("dur", label = "Generales") %>%
      dySeries("agu", label = "Rieleros") %>%
      dyLegend(show = "follow") %>%
      dyLimit(0.5, color = 'red') %>%
      dyRangeSelector()
  })  
  
  output$norte <- renderDygraph({
    lmbts_nte <- read.csv("lmbts_nte.csv")
    lmb.tsN <- xts(lmbts_nte[,-1], order.by = as.Date(lmbts_nte$DATE, "%Y-%m-%d"))
    dygraph(lmb.tsN, main = "2017 LMB standings", ylab = "Win/Loss percentage") %>%
      dySeries("mxo", label = "Diablos Rojos") %>%
      dySeries("vaq", label = "Vaqueros") %>%
      dySeries("mty", label = "Sultanes") %>%
      dySeries("mva", label = "Acereros") %>%
      dySeries("slt", label = "Saraperos") %>%
      dySeries("tij", label = "Toros") %>%
      dySeries("dur", label = "Generales") %>%
      dySeries("agu", label = "Rieleros") %>%
      dyLegend(show = "follow") %>%
      dyLimit(0.5, color = 'red') %>%
      dyRangeSelector()
  })
  
  output$sur <- renderDygraph({
    lmbts_sur <- read.csv("lmbts_sur.csv")
    lmb.tsS <- xts(lmbts_sur[,-1], order.by = as.Date(lmbts_sur$DATE, "%Y-%m-%d"))
    dygraph(lmb.tsS, main = "2017 LMB standings", ylab = "Win/Loss percentage") %>%
      dySeries("leo", label = "Bravos") %>%
      dySeries("vra", label = "Rojos del Aguila") %>%
      dySeries("yuc", label = "Leones") %>%
      dySeries("cam", label = "Piratas") %>%
      dySeries("tab", label = "Olmecas") %>%
      dySeries("pue", label = "Pericos") %>%
      dySeries("oax", label = "Guerreros") %>%
      dySeries("qui", label = "Tigres") %>%
      dyLegend(show = "follow") %>%
      dyLimit(0.5, color = 'red') %>%
      dyRangeSelector()
  })
}

shinyApp(ui, server)

