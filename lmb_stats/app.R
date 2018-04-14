library(RCurl)
library(XML)
library(xml2)
library(chron)
library(xts)
library(tidyverse)
library(dygraphs)
library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)


ui <- dashboardPage(
  dashboardHeader(title = "LMB StatsApp"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Index", tabName = "Index", icon = icon("dashboard")),
      menuItem("Standings", tabName = "Standings", icon = icon("th-list")),
      menuItem("Teams charts", tabName = "Pyth", icon = icon("stats", lib="glyphicon")),
      menuItem("Att and time", tabName = "Att", icon = icon("time", lib = "glyphicon")),
      menuItem("Batting Stats", tabName = "bat", icon = icon("stats", lib = "glyphicon")),
      menuItem("Pitching Stats", tabName = "pit", icon = icon("stats", lib = "glyphicon"))
      )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Index",
              box(
                width = 12,
                h2("Bienvenidos"),
                h3("lmb statsapp")
              ),
              tags$footer(paste0("Última actualización: ",Sys.time()))
              ),
      tabItem(tabName = "Standings",
              tabBox(title = "Standings",
                  width = 12,
                  tabPanel("Standing Norte",
                     dataTableOutput("stan_nte")
                  ),
                  tabPanel("Standing Sur",
                     dataTableOutput("stan_sur")
                  ),
                  #tabPanel("Gráfica general",
                  #  dygraphOutput("standings")
                  #),
                  tabPanel("Gráfica Zona Norte",
                    dygraphOutput("norte")
                  ),
                  tabPanel("Gráfica Zona Sur",
                    dygraphOutput("sur")
                  )
              )
      ),
      tabItem(tabName = "Pyth",
              h4("WL PCT evolution"),
              p(""),
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
                tabPanel("LAR",
                         dygraphOutput("lar")),
                tabPanel("YUC",
                         dygraphOutput("yuc"))
              )
      ),
      tabItem(tabName = "Att",
              h4("Promedio de asistencia y duración de juegos"),
              p(""),
              tabBox(
                width = 12,
                tabPanel("Time",
                         dygraphOutput("time")),
                tabPanel("Attendance",
                         dygraphOutput("att"))
              )
      ),
      tabItem(tabName = "bat",
              tabBox(
                width = 12,
                tabPanel("Batting",
                  dataTableOutput("bat")),
                tabPanel("Advanced",
                 dataTableOutput("bat2"))
              )),
      tabItem(tabName = "pit",
              tabBox(
                width = 12,
                tabPanel("Standard",
                  dataTableOutput("pit")),
                tabPanel("Advanced",
                  dataTableOutput("pit2"))
              )
              )
    )
  )
)

server <- function(input, output) { 

  output$standings <- renderDygraph({
    ts.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQCuLmj4gmK_L06zKI9nyc_EoiRNqVwl10H4CSDfGOsIjWHLok7OBCg_RGMA57C5sLjq1twOQMb488T/pub?gid=948498967&single=true&output=csv"
    LMBts <- read.csv(url(ts.u))
    lmb.ts <- xts(LMBts[,-1], order.by = as.Date(LMBts$DATE, "%Y-%m-%d"))
    
    dygraph(lmb.ts, main = "2018 LMB standings", ylab = "Win/Loss percentage") %>%
      dySeries("mxo", label = "Diablos Rojos") %>%
      dySeries("leo", label = "Bravos") %>%
      dySeries("lar", label = "Tecolotes") %>%
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
      #dyLegend(show = "follow") %>%
      dyOptions(colors= c("#FF0000","#66CDAA","#FF0000","#8B0000","#00008B",
                          "#0000FF","#FF4500","#000080","#FFFF00","#008000",
                          "#000000","#00008B","#40E0D0","#000000","#800080",
                          "#FFFF00")) %>%
      dyLimit(0.5, color = 'red') %>%
      dyRangeSelector()
  })  
  
  output$norte <- renderDygraph({
    ts.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQCuLmj4gmK_L06zKI9nyc_EoiRNqVwl10H4CSDfGOsIjWHLok7OBCg_RGMA57C5sLjq1twOQMb488T/pub?gid=948498967&single=true&output=csv"
    LMBts <- read.csv(url(ts.u))
    norte <- c(1,2,3,4,6,7,10,11,14)
    LMBts_nte <- LMBts[norte]
    lmb.tsN <- xts(LMBts_nte[,2:9], order.by = as.Date(LMBts_nte$DATE, "%Y-%m-%d"))
    dygraph(lmb.tsN, main = "2018 LMB standings", ylab = "Win/Loss percentage") %>%
      dySeries("lar", label = "Tecolotes") %>%
      dySeries("vaq", label = "Vaqueros") %>%
      dySeries("mty", label = "Sultanes") %>%
      dySeries("mva", label = "Acereros") %>%
      dySeries("slt", label = "Saraperos") %>%
      dySeries("tij", label = "Toros") %>%
      dySeries("dur", label = "Generales") %>%
      dySeries("agu", label = "Rieleros") %>%
      #dyLegend(show = "follow") %>%
      dyOptions(colors= c("#FF0000","#8B0000","#00008B",
                          "#0000FF","#40E0D0","#000000","#800080",
                          "#FFFF00")) %>%
      dyLimit(0.5, color = 'red') %>%
      dyRangeSelector()
  })
  
  output$sur <- renderDygraph({
    ts.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQCuLmj4gmK_L06zKI9nyc_EoiRNqVwl10H4CSDfGOsIjWHLok7OBCg_RGMA57C5sLjq1twOQMb488T/pub?gid=948498967&single=true&output=csv"
    LMBts <- read.csv(url(ts.u))
    sur <- c(1,5,8,9,12,13,15,16,17)
    LMBts_sur <- LMBts[sur]
    lmb.tsS <- xts(LMBts_sur[,2:9], order.by = as.Date(LMBts_sur$DATE, "%Y-%m-%d"))
    dygraph(lmb.tsS, main = "2018 LMB standings", ylab = "Win/Loss percentage") %>%
      dySeries("leo", label = "Bravos") %>%
      dySeries("mxo", label = "Diablos Rojos") %>%
      dySeries("yuc", label = "Leones") %>%
      dySeries("cam", label = "Piratas") %>%
      dySeries("tab", label = "Olmecas") %>%
      dySeries("pue", label = "Pericos") %>%
      dySeries("oax", label = "Guerreros") %>%
      dySeries("qui", label = "Tigres") %>%
      #dyLegend(show = "follow") %>%
      dyOptions(colors= c("#66CDAA","#FF0000","#FF4500","#000080","#FFFF00",
                          "#008000","#000000","#00008B")) %>%
      dyLimit(0.5, color = 'red') %>%
      dyRangeSelector()
  })
  
  output$ags <- renderDygraph({
    ts.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQCuLmj4gmK_L06zKI9nyc_EoiRNqVwl10H4CSDfGOsIjWHLok7OBCg_RGMA57C5sLjq1twOQMb488T/pub?gid=948498967&single=true&output=csv"
    LMBts <- read.csv(url(ts.u))
    aguts <- rename(select(LMBts,DATE,agu),WL_AVG = agu, DATE = DATE)
    #lmb_exp <- read.csv("LMB2018stan")
    agu.ts <- xts(aguts$WL_AVG, order.by = as.Date(aguts$DATE, "%Y-%m-%d"))
    dygraph(agu.ts, main = "2018 Rieleros Evolution WL PCT", ylab = "Win/Loss percentage") %>%
      #dyLegend(show = "follow") %>%
      dyOptions(colors = '#FFFF00') %>%
      #dyLimit(lmb_exp$Pyth[10], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$cam <- renderDygraph({
    ts.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQCuLmj4gmK_L06zKI9nyc_EoiRNqVwl10H4CSDfGOsIjWHLok7OBCg_RGMA57C5sLjq1twOQMb488T/pub?gid=948498967&single=true&output=csv"     
    LMBts <- read.csv(url(ts.u))
    camts <- rename(select(LMBts,DATE,cam),WL_AVG = cam, DATE = DATE)
    #lmb_exp <- read.csv("LMB2018stan")
    cam.ts <- xts(camts$WL_AVG, order.by = as.Date(camts$DATE, "%Y-%m-%d"))
    dygraph(cam.ts, main = "2017 Piratas Evolution WL PCT", ylab = "Win/Loss percentage") %>%
      #dyLegend(show = "follow") %>%
      dyOptions(colors = '#000080') %>%
      #dyLimit(lmb_exp$Pyth[9], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$dur <- renderDygraph({
    ts.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQCuLmj4gmK_L06zKI9nyc_EoiRNqVwl10H4CSDfGOsIjWHLok7OBCg_RGMA57C5sLjq1twOQMb488T/pub?gid=948498967&single=true&output=csv"     
    LMBts <- read.csv(url(ts.u))
    durts <- rename(select(LMBts,DATE,dur),WL_AVG = dur, DATE = DATE)
    #lmb_exp <- read.csv("LMB2018stan")
    dur.ts <- xts(durts$WL_AVG, order.by = as.Date(durts$DATE, "%Y-%m-%d"))
    dygraph(dur.ts, main = "2017 Generales Evolution WL PCT", ylab = "Win/Loss percentage") %>%
      #dyLegend(show = "follow") %>%
      dyOptions(colors = '#800080') %>%
      #dyLimit(lmb_exp$Pyth[4], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$leo <- renderDygraph({
    ts.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQCuLmj4gmK_L06zKI9nyc_EoiRNqVwl10H4CSDfGOsIjWHLok7OBCg_RGMA57C5sLjq1twOQMb488T/pub?gid=948498967&single=true&output=csv"     
    LMBts <- read.csv(url(ts.u))
    leots <- rename(select(LMBts,DATE,leo),WL_AVG = leo, DATE = DATE)
    #lmb_exp <- read.csv("LMB2018stan")
    leo.ts <- xts(leots$WL_AVG, order.by = as.Date(leots$DATE, "%Y-%m-%d"))
    dygraph(leo.ts, main = "2018 Bravos Evolution WL PCT", ylab = "Win/Loss percentage") %>%
      #dyLegend(show = "follow") %>%
      dyOptions(colors = '#66CDAA') %>%
      #dyLimit(lmb_exp$Pyth[2], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$mty <- renderDygraph({
    ts.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQCuLmj4gmK_L06zKI9nyc_EoiRNqVwl10H4CSDfGOsIjWHLok7OBCg_RGMA57C5sLjq1twOQMb488T/pub?gid=948498967&single=true&output=csv"     
    LMBts <- read.csv(url(ts.u))
    mtyts <- rename(select(LMBts,DATE,mty),WL_AVG = mty, DATE = DATE)
    #lmb_exp <- read.csv("LMB2018stan")
    mty.ts <- xts(mtyts$WL_AVG, order.by = as.Date(mtyts$DATE, "%Y-%m-%d"))
    dygraph(mty.ts, main = "2018 Sultanes Evolution WL PCT", ylab = "Win/Loss percentage") %>%
      #dyLegend(show = "follow") %>%
      dyOptions(colors = '#00008B') %>%
      #dyLimit(lmb_exp$Pyth[13], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$mva <- renderDygraph({
    ts.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQCuLmj4gmK_L06zKI9nyc_EoiRNqVwl10H4CSDfGOsIjWHLok7OBCg_RGMA57C5sLjq1twOQMb488T/pub?gid=948498967&single=true&output=csv"     
    LMBts <- read.csv(url(ts.u))
    mvats <- rename(select(LMBts,DATE,mva),WL_AVG = mva, DATE = DATE)
    #lmb_exp <- read.csv("LMB2018stan")
    mva.ts <- xts(mvats$WL_AVG, order.by = as.Date(mvats$DATE, "%Y-%m-%d"))
    dygraph(mva.ts, main = "2018 Acereros Evolution WL PCT", ylab = "Win/Loss percentage") %>%
      #dyLegend(show = "follow") %>%
      dyOptions(colors = '#0000FF') %>%
      #dyLimit(lmb_exp$Pyth[1], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$mex <- renderDygraph({
    ts.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQCuLmj4gmK_L06zKI9nyc_EoiRNqVwl10H4CSDfGOsIjWHLok7OBCg_RGMA57C5sLjq1twOQMb488T/pub?gid=948498967&single=true&output=csv"     
    LMBts <- read.csv(url(ts.u))
    mxots <- rename(select(LMBts,DATE,mxo),WL_AVG = mxo, DATE = DATE)
    #lmb_exp <- read.csv("LMB2018stan")
    mxo.ts <- xts(mxots$WL_AVG, order.by = as.Date(mxots$DATE, "%Y-%m-%d"))
    dygraph(mxo.ts, main = "2018 Diablos Rojos Evolution WL PCT", ylab = "Win/Loss percentage") %>%
      #dyLegend(show = "follow") %>%
      dyOptions(colors = '#FF0000') %>%
      #dyLimit(lmb_exp$Pyth[3], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$oax <- renderDygraph({
    ts.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQCuLmj4gmK_L06zKI9nyc_EoiRNqVwl10H4CSDfGOsIjWHLok7OBCg_RGMA57C5sLjq1twOQMb488T/pub?gid=948498967&single=true&output=csv"     
    LMBts <- read.csv(url(ts.u))
    oaxts <- rename(select(LMBts,DATE,oax),WL_AVG = oax, DATE = DATE)
    #lmb_exp <- read.csv("LMB2018stan")
    oax.ts <- xts(oaxts$WL_AVG, order.by = as.Date(oaxts$DATE, "%Y-%m-%d"))
    dygraph(oax.ts, main = "2018 Guerreros Evolution WL PCT", ylab = "Win/Loss percentage") %>%
      #dyLegend(show = "follow") %>%
      dyOptions(colors = '#000000') %>%
      #dyLimit(lmb_exp$Pyth[5], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$pue <- renderDygraph({
    ts.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQCuLmj4gmK_L06zKI9nyc_EoiRNqVwl10H4CSDfGOsIjWHLok7OBCg_RGMA57C5sLjq1twOQMb488T/pub?gid=948498967&single=true&output=csv"     
    LMBts <- read.csv(url(ts.u))
    puets <- rename(select(LMBts,DATE,pue),WL_AVG = pue, DATE = DATE)
    #lmb_exp <- read.csv("LMB2018stan")
    pue.ts <- xts(puets$WL_AVG, order.by = as.Date(puets$DATE, "%Y-%m-%d"))
    dygraph(pue.ts, main = "2018 Pericos Evolution WL PCT", ylab = "Win/Loss percentage") %>%
      #dyLegend(show = "follow") %>%
      dyOptions(colors = '#008000') %>%
      #dyLimit(lmb_exp$Pyth[8], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$qui <- renderDygraph({
    ts.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQCuLmj4gmK_L06zKI9nyc_EoiRNqVwl10H4CSDfGOsIjWHLok7OBCg_RGMA57C5sLjq1twOQMb488T/pub?gid=948498967&single=true&output=csv"     
    LMBts <- read.csv(url(ts.u))
    quits <- rename(select(LMBts,DATE,qui),WL_AVG = qui, DATE = DATE)
    #lmb_exp <- read.csv("LMB2018stan")
    qui.ts <- xts(quits$WL_AVG, order.by = as.Date(quits$DATE, "%Y-%m-%d"))
    dygraph(qui.ts, main = "2018 Tigres Evolution WL PCT", ylab = "Win/Loss percentage") %>%
      #dyLegend(show = "follow") %>%
      dyOptions(colors = '#00008B') %>%
      #dyLimit(lmb_exp$Pyth[14], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$sal <- renderDygraph({
    ts.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQCuLmj4gmK_L06zKI9nyc_EoiRNqVwl10H4CSDfGOsIjWHLok7OBCg_RGMA57C5sLjq1twOQMb488T/pub?gid=948498967&single=true&output=csv"     
    LMBts <- read.csv(url(ts.u))
    sltts <- rename(select(LMBts,DATE,slt),WL_AVG = slt, DATE = DATE)
    #lmb_exp <- read.csv("LMB2018stan")
    slt.ts <- xts(sltts$WL_AVG, order.by = as.Date(sltts$DATE, "%Y-%m-%d"))
    dygraph(slt.ts, main = "2018 Saraperos Evolution WL PCT", ylab = "Win/Loss percentage") %>%
      #dyLegend(show = "follow") %>%
      dyOptions(colors = '#40E0D0') %>%
      #dyLimit(lmb_exp$Pyth[12], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$tab <- renderDygraph({
    ts.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQCuLmj4gmK_L06zKI9nyc_EoiRNqVwl10H4CSDfGOsIjWHLok7OBCg_RGMA57C5sLjq1twOQMb488T/pub?gid=948498967&single=true&output=csv"     
    LMBts <- read.csv(url(ts.u))
    tabts <- rename(select(LMBts,DATE,tab),WL_AVG = tab, DATE = DATE)
    #lmb_exp <- read.csv("LMB2018stan")
    tab.ts <- xts(tabts$WL_AVG, order.by = as.Date(tabts$DATE, "%Y-%m-%d"))
    dygraph(tab.ts, main = "2017 Olmecas Expected Winning Season", ylab = "Win/Loss percentage") %>%
      #dyLegend(show = "follow") %>%
      dyOptions(colors = '#FFFF00') %>%
      #dyLimit(lmb_exp$Pyth[7], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$tij <- renderDygraph({
    ts.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQCuLmj4gmK_L06zKI9nyc_EoiRNqVwl10H4CSDfGOsIjWHLok7OBCg_RGMA57C5sLjq1twOQMb488T/pub?gid=948498967&single=true&output=csv"     
    LMBts <- read.csv(url(ts.u))
    tijts <- rename(select(LMBts,DATE,tij),WL_AVG = tij, DATE = DATE)
    #lmb_exp <- read.csv("LMB2018stan")
    tij.ts <- xts(tijts$WL_AVG, order.by = as.Date(tijts$DATE, "%Y-%m-%d"))
    dygraph(tij.ts, main = "2017 Toros Evolution WL PCT", ylab = "Win/Loss percentage") %>%
      #dyLegend(show = "follow") %>%
      dyOptions(colors = '#000000') %>%
      #dyLimit(lmb_exp$Pyth[15], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$lag <- renderDygraph({
    ts.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQCuLmj4gmK_L06zKI9nyc_EoiRNqVwl10H4CSDfGOsIjWHLok7OBCg_RGMA57C5sLjq1twOQMb488T/pub?gid=948498967&single=true&output=csv"     
    LMBts <- read.csv(url(ts.u))
    lagts <- rename(select(LMBts,DATE,vaq),WL_AVG = vaq, DATE = DATE)
    #lmb_exp <- read.csv("LMB2018stan")
    lag.ts <- xts(lagts$WL_AVG, order.by = as.Date(lagts$DATE, "%Y-%m-%d"))
    dygraph(lag.ts, main = "2018 Vaqueros Evolution WL PCT", ylab = "Win/Loss percentage") %>%
      #dyLegend(show = "follow") %>%
      dyOptions(colors = '#8B0000') %>%
     # dyLimit(lmb_exp$Pyth[16], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$lar <- renderDygraph({
    ts.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQCuLmj4gmK_L06zKI9nyc_EoiRNqVwl10H4CSDfGOsIjWHLok7OBCg_RGMA57C5sLjq1twOQMb488T/pub?gid=948498967&single=true&output=csv"     
    LMBts <- read.csv(url(ts.u))
    larts <- rename(select(LMBts,DATE,lar),WL_AVG = lar, DATE = DATE)
    #lmb_exp <- read.csv("LMB2018stan")
    lar.ts <- xts(larts$WL_AVG, order.by = as.Date(larts$DATE, "%Y-%m-%d"))
    dygraph(lar.ts, main = "2018 Tecolotes Evolution WL PCT", ylab = "Win/Loss percentage") %>%
      #dyLegend(show = "follow") %>%
      dyOptions(colors = '#FF0000') %>%
      #dyLimit(lmb_exp$Pyth[11], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$yuc <- renderDygraph({
    ts.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQCuLmj4gmK_L06zKI9nyc_EoiRNqVwl10H4CSDfGOsIjWHLok7OBCg_RGMA57C5sLjq1twOQMb488T/pub?gid=948498967&single=true&output=csv"     
    LMBts <- read.csv(url(ts.u))
    yucts <- rename(select(LMBts,DATE,yuc),WL_AVG = yuc, DATE = DATE)
    #lmb_exp <- read.csv("LMB2018stan")
    yuc.ts <- xts(yucts$WL_AVG, order.by = as.Date(yucts$DATE, "%Y-%m-%d"))
    dygraph(yuc.ts, main = "2018 Leones Evolution WL PCT", ylab = "Win/Loss percentage") %>%
      ##dyLegend(show = "follow") %>%
      dyOptions(colors = '#FF4500') %>%
      #dyLimit(lmb_exp$Pyth[6], color = 'red') %>%
      dyRangeSelector()
  })
  
  output$time <- renderDygraph({
    att_time.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vStbs_L96FBpcZKd3ZPm0OztsT8g9xYK95ZR67-UGsG0aYYxohtFmrWGhav4pqpn0bMZ8REBUMehiGi/pub?gid=1306250727&single=true&output=csv"
    LMBatt_time <- read.csv(url(att_time.u))
    att_time <- LMBatt_time %>%
      group_by(DATE) %>% summarise(
        ATT = mean(ATT),
        TIME = mean(TIME))
    time.ts <- xts(att_time[,3], order.by = as.Date(att_time$DATE, "%Y-%m-%d"))
    dygraph(time.ts, main = "2018 LMB GAMES TIME", ylab = "MINUTES") %>%
      dyLimit(mean(LMBatt_time$TIME), color = 'red') %>%
      dyRangeSelector()
  })
  
  output$att <- renderDygraph({
    att_time.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vStbs_L96FBpcZKd3ZPm0OztsT8g9xYK95ZR67-UGsG0aYYxohtFmrWGhav4pqpn0bMZ8REBUMehiGi/pub?gid=1306250727&single=true&output=csv"
    LMBatt_time <- read.csv(url(att_time.u))
    att_time <- LMBatt_time %>%
      group_by(DATE) %>% summarise(
        ATT = mean(ATT),
        TIME = mean(TIME))
    att.ts <- xts(att_time[,2], order.by = as.Date(att_time$DATE, "%Y-%m-%d"))
    dygraph(att.ts, main = "2018 LMB ATTENDANCE", ylab = "ATTENDANCE") %>%
      dyLimit(mean(LMBatt_time$ATT), color = 'red') %>%
      dyRangeSelector()
  })
  
  output$stan_nte <- renderDataTable({
    stan.nte.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSqxkxMkXPDti1hUWIyn7ikmuAkQG-o1RYhqqmrh3sGyGcRvl9TxN-3cNlKsWFsrIihsN6onrmsUFJu/pub?gid=2113179783&single=true&output=csv"
    stan.nte <- read.csv(url(stan.nte.u))
    #stan.nte = stan.nte[,-1]
    stan.nte <- arrange(stan.nte, desc(PCT), TEAM)
    datatable(stan.nte, options = list(paging = FALSE, searching = FALSE)) %>%
      formatRound(columns = c('PCT','RperG','RAperG','RunsRatio','PCTexp','WinRatio'), digits = 3)
  })
  
  output$stan_sur <- renderDataTable({
    stan.sur.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vT8Gt45XnEaKcm47RFwAzLeq0svwGMRgRKbuJu-wqYtPuRXSooZ4XrQJprFfMStY5KjCRyjpFbv5x2a/pub?gid=742234677&single=true&output=csv"
    stan.sur <- read.csv(url(stan.sur.u))
    #stan.sur = stan.sur[,-1]
    stan.sur <- arrange(stan.sur, desc(PCT), TEAM)
    datatable(stan.sur, options = list(paging = FALSE, searching = FALSE)) %>%
      formatRound(columns = c('PCT','RperG','RAperG','RunsRatio','PCTexp','WinRatio'), digits = 3)
  })
  
  output$bat <- renderDataTable({
    lmb.b.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vS5mSK8ZlY1qz1Sb7VSMiMSAthNY9JY5Mp05SyXbw5CsZAZhkOrgd75UU8NAEVH5zs122dDqmzam5NL/pub?gid=93773820&single=true&output=csv"
    lmb_b <- read.csv(url(lmb.b.u)) %>%
      arrange(desc(AVG))
    datatable(lmb_b, extensions = 'FixedColumns',
              options = list(scrollX = TRUE, fixedColumns = TRUE, pageLength = 6)
            ) %>%
      formatRound(columns = c('AVG','OBP','SLG','OPS'), digits = 3)
  })
  
  output$pit <- renderDataTable({
    lmb.p.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQZ0LlEBYTwbjyHvzhlsxD3WYBC0Dpzp79zyl2bNsXWUoyYN6Dk-4aE0zD7imwcfqstvf1Dvw1t36rl/pub?gid=203948910&single=true&output=csv"
    lmb_p <- read.csv(url(lmb.p.u)) %>%
      arrange(desc(ERA))
    datatable(lmb_p, extensions = 'FixedColumns',
              options = list(scrollX = TRUE, fixedColumns = TRUE, pageLength = 6)
    ) %>%
      formatRound(columns = c('ERA','WHIP'), digits = 2) %>%
      formatRound(columns = c('IP'), digits = 1)
  })
  
  output$bat2 <- renderDataTable({
    lmb.b2.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vS5mSK8ZlY1qz1Sb7VSMiMSAthNY9JY5Mp05SyXbw5CsZAZhkOrgd75UU8NAEVH5zs122dDqmzam5NL/pub?gid=93773820&single=true&output=csv"
    lmb_b <- read.csv(url(lmb.b2.u)) %>%
      arrange(desc(AVG))
    datatable(lmb_b, extensions = 'FixedColumns',
              options = list(scrollX = TRUE, fixedColumns = TRUE, pageLength = 6)
    ) %>%
      formatRound(columns = c('AVG','OBP','SLG','OPS'), digits = 3)
  })
  
  output$pit2 <- renderDataTable({
    lmb.p2.u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vT3nDmSldZQxj0yObqJGaqumTRUpMbJ-rN-atpxN0hIX_egbK0BQGmpiUL1Kxb-okQE-gTVKzBpjHkK/pub?gid=49577423&single=true&output=csv"
    lmb_p <- read.csv(url(lmb.p2.u)) %>%
      arrange(desc(ERA))
    datatable(lmb_p, extensions = 'FixedColumns',
              options = list(scrollX = TRUE, fixedColumns = TRUE, pageLength = 6)
    ) %>%
      formatRound(columns = c('ERA','WHIP','BB.9','K.9'), digits = 2) %>%
      formatRound(columns = c('IP'), digits = 1)
  })
  
  output$systime <- renderPrint({
    Sys.time()
  })
}

shinyApp(ui, server)

