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
      menuItem("Index", tabName = "Index", icon = icon("dashboard")),
      menuItem("Standings", tabName = "Standings", icon = icon("th")),
      menuItem("Pyth Win Pct", tabName = "Pyth", icon = icon("th")),
      menuItem("Att and time", tabName = "Att", icon = icon("th"))
      )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Index",
              box(
                verbatimTextOutput("systime")
              )
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
      ),
      tabItem(tabName = "Pyth",
              tabBox(
                width = 12,
                tabPanel("AGS",
                         dygraphOutput("ags")),
                tabPanel("CAM",
                         dygraphOutput("cam")),
                tabPanel("DUR",
                         dygraphOutput("dur")),
                tabPanel("LEO",
                         dygraphOutput("leo")),
                tabPanel("MTY",
                         dygraphOutput("mty")),
                tabPanel("MVA",
                         dygraphOutput("mva")),
                tabPanel("MEX",
                         dygraphOutput("mex")),
                tabPanel("OAX",
                         dygraphOutput("oax")),
                tabPanel("PUE",
                         dygraphOutput("pue")),
                tabPanel("QUI",
                         dygraphOutput("qui")),
                tabPanel("SAL",
                         dygraphOutput("sal")),
                tabPanel("TAB",
                         dygraphOutput("tab")),
                tabPanel("TIJ",
                         dygraphOutput("tij")),
                tabPanel("LAG",
                         dygraphOutput("lag")),
                tabPanel("VER",
                         dygraphOutput("ver")),
                tabPanel("YUC",
                         dygraphOutput("yuc"))
              )
      ),
      tabItem(tabName = "Att",
              tabBox(
                
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
      dyOptions(colors= c("#FF0000","#66CDAA","#FF0000","#8B0000","#00008B",
                          "#0000FF","#FF4500","#000080","#FFFF00","#008000",
                          "#000000","#00008B","#40E0D0","#000000","#800080",
                          "#FFFF00")) %>%
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
      dyOptions(colors= c("#FF0000","#8B0000","#00008B",
                          "#0000FF","#40E0D0","#000000","#800080",
                          "#FFFF00")) %>%
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
      dyOptions(colors= c("#66CDAA","#FF0000","#FF4500","#000080","#FFFF00",
                          "#008000","#000000","#00008B")) %>%
      dyLimit(0.5, color = 'red') %>%
      dyRangeSelector()
  })
  
  output$ags <- renderDygraph({
    lmbts <- read.csv("lmbts.csv")
    lmb_exp <- read.csv("lmb_2017_exp.csv")
    ags.ts <- xts(lmbts[,2], order.by = as.Date(lmbts$DATE, "%Y-%m-%d"))
    dygraph(ags.ts, main = "2017 Rieleros Expected Winning Season", ylab = "Win/Loss percentage") %>%
      dyLegend(show = "follow") %>%
      dyOptions(colors = '#FFFF00') %>%
      dyLimit(lmb_exp$Pyth[10], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$cam <- renderDygraph({
    lmbts <- read.csv("lmbts.csv")
    lmb_exp <- read.csv("lmb_2017_exp.csv")
    cam.ts <- xts(lmbts[,3], order.by = as.Date(lmbts$DATE, "%Y-%m-%d"))
    dygraph(cam.ts, main = "2017 Piratas Expected Winning Season", ylab = "Win/Loss percentage") %>%
      dyLegend(show = "follow") %>%
      dyOptions(colors = '#000080') %>%
      dyLimit(lmb_exp$Pyth[9], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$dur <- renderDygraph({
    lmbts <- read.csv("lmbts.csv")
    lmb_exp <- read.csv("lmb_2017_exp.csv")
    dur.ts <- xts(lmbts[,4], order.by = as.Date(lmbts$DATE, "%Y-%m-%d"))
    dygraph(dur.ts, main = "2017 Generales Expected Winning Season", ylab = "Win/Loss percentage") %>%
      dyLegend(show = "follow") %>%
      dyOptions(colors = '#800080') %>%
      dyLimit(lmb_exp$Pyth[4], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$leo <- renderDygraph({
    lmbts <- read.csv("lmbts.csv")
    lmb_exp <- read.csv("lmb_2017_exp.csv")
    leo.ts <- xts(lmbts[,5], order.by = as.Date(lmbts$DATE, "%Y-%m-%d"))
    dygraph(leo.ts, main = "2017 Bravos Expected Winning Season", ylab = "Win/Loss percentage") %>%
      dyLegend(show = "follow") %>%
      dyOptions(colors = '#66CDAA') %>%
      dyLimit(lmb_exp$Pyth[2], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$mty <- renderDygraph({
    lmbts <- read.csv("lmbts.csv")
    lmb_exp <- read.csv("lmb_2017_exp.csv")
    mty.ts <- xts(lmbts[,6], order.by = as.Date(lmbts$DATE, "%Y-%m-%d"))
    dygraph(mty.ts, main = "2017 Sultanes Expected Winning Season", ylab = "Win/Loss percentage") %>%
      dyLegend(show = "follow") %>%
      dyOptions(colors = '#00008B') %>%
      dyLimit(lmb_exp$Pyth[13], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$mva <- renderDygraph({
    lmbts <- read.csv("lmbts.csv")
    lmb_exp <- read.csv("lmb_2017_exp.csv")
    mva.ts <- xts(lmbts[,7], order.by = as.Date(lmbts$DATE, "%Y-%m-%d"))
    dygraph(mva.ts, main = "2017 Acereros Expected Winning Season", ylab = "Win/Loss percentage") %>%
      dyLegend(show = "follow") %>%
      dyOptions(colors = '#0000FF') %>%
      dyLimit(lmb_exp$Pyth[1], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$mex <- renderDygraph({
    lmbts <- read.csv("lmbts.csv")
    lmb_exp <- read.csv("lmb_2017_exp.csv")
    mex.ts <- xts(lmbts[,8], order.by = as.Date(lmbts$DATE, "%Y-%m-%d"))
    dygraph(mex.ts, main = "2017 Diablos Rojos Expected Winning Season", ylab = "Win/Loss percentage") %>%
      dyLegend(show = "follow") %>%
      dyOptions(colors = '#FF0000') %>%
      dyLimit(lmb_exp$Pyth[3], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$oax <- renderDygraph({
    lmbts <- read.csv("lmbts.csv")
    lmb_exp <- read.csv("lmb_2017_exp.csv")
    oax.ts <- xts(lmbts[,9], order.by = as.Date(lmbts$DATE, "%Y-%m-%d"))
    dygraph(oax.ts, main = "2017 Guerreros Expected Winning Season", ylab = "Win/Loss percentage") %>%
      dyLegend(show = "follow") %>%
      dyOptions(colors = '#000000') %>%
      dyLimit(lmb_exp$Pyth[5], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$pue <- renderDygraph({
    lmbts <- read.csv("lmbts.csv")
    lmb_exp <- read.csv("lmb_2017_exp.csv")
    pue.ts <- xts(lmbts[,10], order.by = as.Date(lmbts$DATE, "%Y-%m-%d"))
    dygraph(pue.ts, main = "2017 Pericos Expected Winning Season", ylab = "Win/Loss percentage") %>%
      dyLegend(show = "follow") %>%
      dyOptions(colors = '#008000') %>%
      dyLimit(lmb_exp$Pyth[8], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$qui <- renderDygraph({
    lmbts <- read.csv("lmbts.csv")
    lmb_exp <- read.csv("lmb_2017_exp.csv")
    qui.ts <- xts(lmbts[,11], order.by = as.Date(lmbts$DATE, "%Y-%m-%d"))
    dygraph(qui.ts, main = "2017 Tigres Expected Winning Season", ylab = "Win/Loss percentage") %>%
      dyLegend(show = "follow") %>%
      dyOptions(colors = '#00008B') %>%
      dyLimit(lmb_exp$Pyth[14], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$sal <- renderDygraph({
    lmbts <- read.csv("lmbts.csv")
    lmb_exp <- read.csv("lmb_2017_exp.csv")
    sal.ts <- xts(lmbts[,12], order.by = as.Date(lmbts$DATE, "%Y-%m-%d"))
    dygraph(sal.ts, main = "2017 Saraperos Expected Winning Season", ylab = "Win/Loss percentage") %>%
      dyLegend(show = "follow") %>%
      dyOptions(colors = '#40E0D0') %>%
      dyLimit(lmb_exp$Pyth[12], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$tab <- renderDygraph({
    lmbts <- read.csv("lmbts.csv")
    lmb_exp <- read.csv("lmb_2017_exp.csv")
    tab.ts <- xts(lmbts[,13], order.by = as.Date(lmbts$DATE, "%Y-%m-%d"))
    dygraph(tab.ts, main = "2017 Olmecas Expected Winning Season", ylab = "Win/Loss percentage") %>%
      dyLegend(show = "follow") %>%
      dyOptions(colors = '#FFFF00') %>%
      dyLimit(lmb_exp$Pyth[7], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$tij <- renderDygraph({
    lmbts <- read.csv("lmbts.csv")
    lmb_exp <- read.csv("lmb_2017_exp.csv")
    tij.ts <- xts(lmbts[,14], order.by = as.Date(lmbts$DATE, "%Y-%m-%d"))
    dygraph(tij.ts, main = "2017 Toros Expected Winning Season", ylab = "Win/Loss percentage") %>%
      dyLegend(show = "follow") %>%
      dyOptions(colors = '#000000') %>%
      dyLimit(lmb_exp$Pyth[15], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$lag <- renderDygraph({
    lmbts <- read.csv("lmbts.csv")
    lmb_exp <- read.csv("lmb_2017_exp.csv")
    lag.ts <- xts(lmbts[,15], order.by = as.Date(lmbts$DATE, "%Y-%m-%d"))
    dygraph(lag.ts, main = "2017 Vaqueros Expected Winning Season", ylab = "Win/Loss percentage") %>%
      dyLegend(show = "follow") %>%
      dyOptions(colors = '#8B0000') %>%
      dyLimit(lmb_exp$Pyth[16], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$ver <- renderDygraph({
    lmbts <- read.csv("lmbts.csv")
    lmb_exp <- read.csv("lmb_2017_exp.csv")
    ver.ts <- xts(lmbts[,16], order.by = as.Date(lmbts$DATE, "%Y-%m-%d"))
    dygraph(ver.ts, main = "2017 Rojos del Aguila Expected Winning Season", ylab = "Win/Loss percentage") %>%
      dyLegend(show = "follow") %>%
      dyOptions(colors = '#FF0000') %>%
      dyLimit(lmb_exp$Pyth[11], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$yuc <- renderDygraph({
    lmbts <- read.csv("lmbts.csv")
    lmb_exp <- read.csv("lmb_2017_exp.csv")
    yuc.ts <- xts(lmbts[,17], order.by = as.Date(lmbts$DATE, "%Y-%m-%d"))
    dygraph(yuc.ts, main = "2017 Leones Expected Winning Season", ylab = "Win/Loss percentage") %>%
      dyLegend(show = "follow") %>%
      dyOptions(colors = '#FF4500') %>%
      dyLimit(lmb_exp$Pyth[6], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$systime <- renderPrint({
    Sys.time()
  })
}

shinyApp(ui, server)
