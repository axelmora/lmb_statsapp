library(RCurl)
library(XML)
library(xml2)
library(chron)
library(xts)
library(tidyverse)
library(dygraphs)


##functions
gdate <- function(x){
  a <- xml_find_all(x, "/games/game/@id")
  val <- trimws(xml_text(a))
  val <- substring(val,1,10)
  val <- gsub("/","-",val)
  return(val)
}

awayteam <- function(x){
  a <- xml_find_all(x, "/games/game/@away_code")
  val <- trimws(xml_text(a))
  return(toupper(val))
}

hometeam <- function(x){
  a <- xml_find_all(x, "/games/game/@home_code")
  val <- trimws(xml_text(a))
  return(toupper(val))
}

home_wins <- function(x){
  a <- xml_find_all(x, "/games/game/@home_win")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}

home_losses <- function(x){
  a <- xml_find_all(x, "/games/game/@home_loss")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}

away_wins <- function(x){
  a <- xml_find_all(x, "/games/game/@away_win")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}

away_losses <- function(x){
  a <- xml_find_all(x, "/games/game/@away_loss")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}

##master function
wl <- function(x){
  date <- gdate(x)
  at <- awayteam(x)
  ht <- hometeam(x)
  aw <- away_wins(x)
  al <- away_losses(x)
  hw <- home_wins(x)
  hl <- home_losses(x)
  
  records <- data.frame(DATE = date,AWAY=at,HOME=ht,AW = aw,AL = al,HW = hw,HL=hl)
  return(records)
}

#bxmaster$HOME <- tolower(bxmaster$HOME) 
#bxmaster$AWAY <- tolower(bxmaster$AWAY)