library(RCurl)
library(XML)
library(xml2)
library(chron)
library(xts)
##functions
#names
names <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@name_display_first_last")
  val <- trimws(xml_text(a))
  return(toupper(val))
}
#AB
ab <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@ab")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#R
r <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@r")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#H
h <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@h")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#d
d <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@d")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#T
t <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@t")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#HR
hr <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@hr")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#RBI
rbi <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@rbi")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#BB
bb <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@bb")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#SO
so <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@so")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#HBP
hbp <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@hbp")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#SB
sb <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter")
  b <- xml_attr(a, "sb")
  
  for (i in 1:length(b)){
    if (is.na(b[i])){
      b[i] <- 0
    }else if (b[i] > 0){
      b[i]
    }
  }
  b <- as.integer(b)
  return(b)
}
#SF
sf <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@sf")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#SAC
sac <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@sac")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#TB
tb <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@tb")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
##master function
py_st_bt <- function(x){

  batter <- data.frame(Batter_Name = names(x),
                     AB = ab(x),
                     R = r(x),
                     H = h(x),
                     D = d(x),
                     Tr = t(x),
                     HR = hr(x),
                     RBI = rbi(x),
                     BB = bb(x),
                     SO = so(x),
                     HBP = hbp(x),
                     SB = sb(x),
                     SF = sf(x),
                     SH = sac(x)
                     #TB = tb(x)
                     )
  return(batter)
}