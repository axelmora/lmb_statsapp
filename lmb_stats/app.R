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
    LMBts <- read.csv("LMBts.csv")
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
    LMBts <- read.csv("LMBts.csv")
    norte <- c(2,3,4,5,7,8,11,12,15)
    LMBts_nte <- LMBts[norte]
    lmb.tsN <- xts(LMBts_nte[,-1], order.by = as.Date(LMBts_nte$DATE, "%Y-%m-%d"))
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
    LMBts <- read.csv("LMBts.csv")
    sur <- c(2,6,9,10,13,14,16,17,18)
    LMBts_sur <- LMBts[sur]
    lmb.tsS <- xts(LMBts_sur[,-1], order.by = as.Date(LMBts_sur$DATE, "%Y-%m-%d"))
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
    LMBts <- read.csv("LMBts.csv")
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
    LMBts <- read.csv("LMBts.csv")
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
    LMBts <- read.csv("LMBts.csv")
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
    LMBts <- read.csv("LMBts.csv")
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
    LMBts <- read.csv("LMBts.csv")
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
    LMBts <- read.csv("LMBts.csv")
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
    LMBts <- read.csv("LMBts.csv")
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
    LMBts <- read.csv("LMBts.csv")
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
    LMBts <- read.csv("LMBts.csv")
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
    LMBts <- read.csv("LMBts.csv")
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
    LMBts <- read.csv("LMBts.csv")
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
    LMBts <- read.csv("LMBts.csv")
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
    LMBts <- read.csv("LMBts.csv")
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
    LMBts <- read.csv("LMBts.csv")
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
    LMBts <- read.csv("LMBts.csv")
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
    LMBts <- read.csv("LMBts.csv")
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
    LMBatt_time <- read.csv("LMB2018.csv")
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
    LMBatt_time <- read.csv("LMB2018.csv")
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
    stan.nte <- read.csv("LMB2018_stan_N.csv")
    stan.nte = stan.nte[,-1]
    stan.nte <- arrange(stan.nte, desc(PCT), TEAM)
    datatable(stan.nte, options = list(paging = FALSE, searching = FALSE)) %>%
      formatRound(columns = c('PCT','RperG','RAperG','RunsRatio','PCTexp','WinRatio'), digits = 3)
  })
  
  output$stan_sur <- renderDataTable({
    stan.sur <- read.csv("LMB2018_stan_S.csv")
    stan.sur = stan.sur[,-1]
    stan.sur <- arrange(stan.sur, desc(PCT), TEAM)
    datatable(stan.sur, options = list(paging = FALSE, searching = FALSE)) %>%
      formatRound(columns = c('PCT','RperG','RAperG','RunsRatio','PCTexp','WinRatio'), digits = 3)
  })
  
  output$bat <- renderDataTable({
    lmb_b <- read.csv("LMB2018_bat_bas.csv") %>%
      arrange(desc(AVG))
    datatable(lmb_b[,-1], extensions = 'FixedColumns',
              options = list(scrollX = TRUE, fixedColumns = TRUE, pageLength = 6)
            ) %>%
      formatRound(columns = c('AVG','OBP','SLG','OPS'), digits = 3)
  })
  
  output$pit <- renderDataTable({
    lmb_p <- read.csv("LMB2018_pit_bas.csv") %>%
      arrange(desc(ERA))
    datatable(lmb_p[,-1], extensions = 'FixedColumns',
              options = list(scrollX = TRUE, fixedColumns = TRUE, pageLength = 6)
    ) %>%
      formatRound(columns = c('ERA','WHIP'), digits = 2) %>%
      formatRound(columns = c('IP'), digits = 1)
  })
  
  output$bat2 <- renderDataTable({
    lmb_b <- read.csv("LMB2018_bat_bas.csv") %>%
      arrange(desc(AVG))
    datatable(lmb_b[,-1], extensions = 'FixedColumns',
              options = list(scrollX = TRUE, fixedColumns = TRUE, pageLength = 6)
    ) %>%
      formatRound(columns = c('AVG','OBP','SLG','OPS'), digits = 3)
  })
  
  output$pit2 <- renderDataTable({
    lmb_p <- read.csv("LMB2018_pit_sab.csv") %>%
      arrange(desc(ERA))
    datatable(lmb_p[,-1], extensions = 'FixedColumns',
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

