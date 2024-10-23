library(RCurl)
library(XML)
library(xml2)
library(chron)
library(xts)
library(tidyverse)
library(dygraphs)
library(jsonlite)
library(sqldf)

gdate <- "07/02/2024" #Select date
sportId <- "23" #AAA (LMB) SportId http://statsapi.mlb.com/api/v1/sports
teams = read.csv("/Users/axel.mora/teams.csv") #load LMB temas catalog with id
teams_ids <- teams[1]  #LMB teams ids

schedule_url <- paste0("http://statsapi.mlb.com/api/v1/schedule/games/?sportId=",sportId,"&date=",gdate) 
#url schedule from selected date

################################
schedule <- fromJSON(schedule_url) #get JSON schedule
gamesPk <- schedule[["dates"]][["games"]][[1]]$gamePk #games PK from schedule
hometeam <- schedule[["dates"]][["games"]][[1]]$teams$home$team$id #home teams
awayteam <- schedule[["dates"]][["games"]][[1]]$teams$away$team$id #away teams
gamesdf <- cbind(gamesPk,hometeam,awayteam) #df 
gamesdf <- data.frame(gamesdf) 
gameslmb <- sqldf("SELECT * FROM gamesdf WHERE hometeam in teams_ids") #select LMB games
gamesPklmb <- gameslmb$gamesPk #games pk from LMB games

###########

n <- length(gamesPklmb)
games_cm_urls <- list()
for(i in 1:n){
  games_cm_urls[i] <- paste0("http://statsapi.mlb.com/api/v1/game/",gamesPklmb[i],"/contextMetrics")
} #context metrics LMB games list

games_bs_urls <- list()
for(i in 1:n){
  games_bs_urls[i] <- paste0("http://statsapi.mlb.com/api/v1/game/",gamesPklmb[i],"/boxscore")
} #boxscore LMB games

#######

catch_league_aux = list()

for(i in teams_ids$team_id){
  catch_team_aux <- mlb_team_stats(team_id = i, stat_type = 'season',stat_group = 'catching', season = 2024)
  catch_league_aux[[i]] <- catch_team_aux
}
lmb_catch_24 = do.call(rbind, catch_league_aux)


######
for (i in gamesPklmb){
  for (j in 1:n){
    assign(paste0("game_bs_",i),fromJSON(games_bs_urls[[j]])) #get box score JSON for each LMB game
  }
}

for (i in gamesPklmb){
  for (j in 1:n){
    assign(paste0("game_cm_",i),fromJSON(games_cm_urls[[j]])) #get context metrics JSON for each LMB game
  }
}