library(baseballr)
library(googlesheets4)
library(dplyr)
library(purrr)
library(tidyr)
library(jsonlite)
library(readr)
library(lubridate)

safe_numeric <- function(df, cols) {
  df %>%
    mutate(across(all_of(cols), ~ suppressWarnings(as.numeric(.x))))
}

# Load your ETL functions
source("stats_etl.R")

# Google Sheets IDs (replace with your actual Sheet IDs)
woba_fipc_gid = "1H8xuzPAjuNJxlWaBAk1fmo-lGH9Jfm0Zeyrcv0R5OPY"
park_factors_gid = "1VkbsNGuHrhZHYoMdxiWVjeUyc3kSMLafYpg0nLHHdcY"
hitting_gid = "1eVJ4dSg6KgATE8zmPxvriHyk6ZRZB94fuDWpdcloC1M"
pitching_gid = "1xlYBP_x1mfsuFDnMxk4gtPUIl7FTIJ0hfVRK3BPeeRQ"
fielding_gid = "1hVRO16lDvVHkYGva06sgG1715wYdEKHaIy2GwyC_K8U"
team_hitting_gid = "1gkOC_566Bp7XCuuyEsIkK7jT0V0TMn--Xbuv1i4Wdxc"
team_pitching_gid = "1uOckNtrCu811khLZ1CxSU7C2TLapZubH4g-Yh2XJTh4"
team_fielding_gid = "1GzUH0leAnoAM64ml-XQ2dR9IFL19u31YCY6Ta955xlQ"
game_logs_gid = "1NHm3ZAMeTpBag95kOpozo_8Ngkxh9VSXot4bbEQpkR8"
hitting_cp_gid = "1K-wOBfh9QW4ucEShjypBXQkti962Yajyf-Da5QEKbg0"
pitching_cp_gid = "1Bkbt91wCmAbAGSlMpMgx8dHcPhsyFjxVpi5eo9gy3do"
rosters_gid = "1zeor7gan9NlpzDx_Dh5h1soDpdSye-V-joHZwXH9F2Y"
trans_gid = "1_Wy1KW5AUH_NTbog_QJ9dFiq5Ujm719MBM0YZ0X7EZw"
lmb_att_gs = "1uer8QQuM-x8VyxCDJBlCtqXofPHE-Dlkzzk64xzLFBQ"
lmb_pace_gs = "1sJ_KjQgmKDUtLRh1MW0yHyQWTzllarKHXGzSGrLNVBk"
lmb_pace_venue_gs = "1_zF8o6iYKrcpE0Cwgky4dpm7gXMt4nU1At7Z4j4qFJI"
stan_nte_gid = "15yPmFjfBfHqfIlhyJVGopp57l-jmcOksu5igvF0h69I"
stan_sur_gid = "1sUlR2MkX1aYhiWBhQPobwahvVGX2GRowTZPAGl7I-gU"
stan_lmb_gid = "1IvGcj4AL-FHCjybAeZGT4ixIWyx2EGlzcwFiib71LAs"
hth_nte_matrix_gid = "1Q7PYR6QwXgFNWxydP8nWWeviEz95E1rNycVpiENlY5U"
hth_sur_matrix_gid = "1lhRN9K-q9uHgMYrY3Ey24IVOjCzo1lK-Pb42XM8zg84"

# Function to fetch the latest daily stats
fetch_latest_stats <- function() {
  today <- Sys.Date()  # Get today's date
  
  # Fetch hitting and pitching data from baseballR
  daily_hitting <- hitting_etl(2025)
  print("hitting extracted")
  daily_pitching <- pitching_etl(2025)
  print("pitching extracted")
  daily_fielding <- fielding_etl(2025)
  print("fielding extracted")
  daily_team_hitting <- team_hitting_etl(2025)
  print("team_hitting extracted")
  daily_team_pitching <- team_pitching_etl(2025)
  print("team_pitching extracted")
  daily_team_fielding <- team_fielding_etl(2025)
  print("team_fielding extracted")
  daily_game_logs <- lmb_game_logs(2025)
  print("game_logs extracted")
  daily_roster <- lmb_roster()
  print("rosters extracted")
  daily_att <- lmb_att(2025)
  print("att extracted")
  daily_pace_venue <- lmb_pace_venue(2025)
  print("pace_venue extracted")
  daily_pace <- lmb_pace(2025)
  print("pace extracted")
  daily_standings <- lmb_standings(2025)
  print("standings extracted")
  daily_standing_lmb <- lmb_standing_lmb(2025)
  print("standings extracted")
  daily_hth_nte <- hth_nte(2025)
  print("hth norte extracted")
  daily_hth_sur <- hth_sur(2025)
  print("hth sur extracted")
  
  # Add a date column for tracking
  daily_hitting <- daily_hitting
  daily_pitching <- daily_pitching
  daily_fielding <- daily_fielding
  daily_team_hitting <- daily_team_hitting 
  daily_team_pitching <- daily_team_pitching
  daily_team_fielding <- daily_team_fielding
  
  return(list(hitting = daily_hitting
              ,pitching = daily_pitching
              ,fielding = daily_fielding
              ,team_hitting = daily_team_hitting
              ,team_pitching = daily_team_pitching
              ,team_fielding = daily_team_fielding
              ,logs = daily_game_logs
              ,rosters = daily_roster
              ,att = daily_att
              ,pace_venue = daily_pace_venue
              ,pace = daily_pace
              ,standings = daily_standings
              ,standing_lmb = daily_standing_lmb
              ,hth_nte = daily_hth_nte
              ,hth_sur = daily_hth_sur
              ))
}

# Function to update Google Sheets
update_google_sheets <- function() {
  new_data <- fetch_latest_stats()  # Get today's stats
  
  # Read existing data
  existing_hitting <- read_sheet(ss = hitting_gid, sheet = "hitting_data")
  existing_hitting <- existing_hitting %>% filter(Year != 2025) %>%
    mutate(across(c(12,22,23,24,25,31,32,33), as.numeric)) %>%
    mutate(across(c(22,23,24,25), ~ round(.x, 3))) %>%
    mutate(across(c(31,32,33), ~ round(.x, 2))) %>%
    mutate(across(41, ~ round(.x, 1)))
  
  existing_pitching <- read_sheet(ss = pitching_gid, sheet = "pitching_data")
  existing_pitching <- existing_pitching %>% filter(Year != 2025) %>%
    mutate(across(c(7,8,21,32,33,34,35,36,39), as.numeric)) %>%
    mutate(across(c(8,21,32,33,34,35,36,39), ~ round(.x, 2)))
  
  existing_fielding <- read_sheet(ss = fielding_gid, sheet = "fielding_data")
  existing_fielding <- existing_fielding %>% filter(Year != 2025) %>%
    mutate(across(c(7,11,20,21), as.numeric)) %>%
    mutate(across(c(11,20,21), ~ round(.x, 2)))
  
  existing_team_pitching <- read_sheet(ss = team_pitching_gid, sheet = "team_pitching_data")
  existing_team_pitching <- existing_team_pitching %>% filter(Year != 2025)  %>%
    mutate(across(c(4,17,18,29,30,31,32,33,34,35,36), as.numeric)) %>%
    mutate(across(c(17,18,29,30,31,32,33,34,35,36), ~ round(.x, 2)))
  
  existing_team_hitting <- read_sheet(ss = team_hitting_gid, sheet = "team_hitting_data")
  existing_team_hitting <- existing_team_hitting %>% filter(Year != 2025)  %>%
    mutate(across(c(19,20,21,22,29,30,31), as.numeric)) %>%
    mutate(across(c(19,20,21,22), ~ round(.x, 3))) %>%
    mutate(across(c(29,30,31), ~ round(.x, 2)))
  
  existing_team_fielding <- read_sheet(ss = team_fielding_gid, sheet = "team_fielding_data")
  existing_team_fielding <- existing_team_fielding %>% filter(Year != 2025) %>%
    mutate(across(c(4,17,18), as.numeric)) %>%
    mutate(across(c(13,17,18), ~ round(.x, 2)))
  
  # Append new data
  updated_hitting <- bind_rows(existing_hitting, new_data$hitting)
  updated_pitching <- bind_rows(existing_pitching, new_data$pitching)
  updated_fielding <- bind_rows(existing_fielding, new_data$fielding)
  updated_team_hitting <- bind_rows(existing_team_hitting, new_data$team_hitting)
  updated_team_pitching <- bind_rows(existing_team_pitching, new_data$team_pitching)
  updated_team_fielding <- bind_rows(existing_team_fielding, new_data$team_fielding)
  
  is.na(updated_hitting) <- sapply(updated_hitting, is.infinite)
  is.na(updated_pitching) <- sapply(updated_pitching, is.infinite)
  is.na(updated_fielding) <- sapply(updated_fielding, is.infinite)
  is.na(updated_team_hitting) <- sapply(updated_team_hitting, is.infinite)
  is.na(updated_team_pitching) <- sapply(updated_team_pitching, is.infinite)
  is.na(updated_team_fielding) <- sapply(updated_team_fielding, is.infinite)
  
  
  hitters <- sort(unique(updated_hitting$Name))
  pitchers <- sort(unique(updated_pitching$Name))
  fielders <- sort(unique(updated_fielding$Name))
  write.csv(hitters,"hitters.csv")
  write.csv(pitchers,"pitchers.csv")
  write.csv(fielders,"fielders.csv")
  
  hitting_cp <- updated_hitting %>%
    select(Year, Name, mWAR, GP, PA, H, RBI, SB, HR, AVG, OBP, SLG, OPS, wOBA) %>%
    filter(Year == 2025)
  
  pitching_cp <- updated_pitching %>%
    select(Year, Name, mWAR,GP, IP, ERA, W, L, SV, HLD, WHIP, `K%`, FIP) %>%
    filter(Year == 2025) %>%
    mutate("W-L" = paste0(W,"-",L)) %>%
    select(Year, Name, mWAR, GP, IP, ERA, `W-L`, SV, HLD, WHIP, `K%`, FIP)
  
  # Write back to Google Sheets
  write_sheet(updated_hitting, hitting_gid, sheet = "hitting_data")
  saveRDS(updated_hitting, "hitting.rds")
  write_sheet(updated_pitching, pitching_gid, sheet = "pitching_data")
  saveRDS(updated_pitching, "pitching.rds")
  write_sheet(updated_fielding, fielding_gid, sheet = "fielding_data")
  saveRDS(updated_fielding, "fielding.rds")
  write_sheet(updated_team_hitting, team_hitting_gid, sheet = "team_hitting_data")
  saveRDS(updated_team_hitting, "team_hitting.rds")
  write_sheet(updated_team_pitching, team_pitching_gid, sheet = "team_pitching_data")
  saveRDS(updated_team_pitching, "team_pitching.rds")
  write_sheet(updated_team_fielding, team_fielding_gid, sheet = "team_fielding_data")
  saveRDS(updated_team_fielding, "team_fielding.rds")
  write_sheet(new_data$logs[-1], game_logs_gid, sheet = "Sheet1")
  saveRDS(new_data$logs[-1], "game_logs.rds")
  write_sheet(new_data$rosters, rosters_gid, sheet = "team_rosters_data")
  saveRDS(new_data$rosters, "rosters.rds")
  write_sheet(new_data$att, lmb_att_gs, sheet = "lmb_att_24")
  saveRDS(new_data$att, "lmb_att_24.rds")
  write_sheet(new_data$pace, lmb_pace_gs, sheet = "lmb_pace_24")
  saveRDS(new_data$pace, "lmb_pace_24.rds")
  write_sheet(new_data$pace_venue, lmb_pace_venue_gs, sheet = "lmb_pace_venue_24")
  saveRDS(new_data$pace_venue, "lmb_pace_venue_24.rds")
  write_sheet(hitting_cp, hitting_cp_gid, sheet = "hitting_cp")
  saveRDS(hitting_cp, "hitting_cp.rds")
  write_sheet(pitching_cp, pitching_cp_gid, sheet = "pitching_cp")
  saveRDS(pitching_cp, "pitching_cp.rds")
  write_sheet(new_data$standings[1:10,], stan_nte_gid, sheet = "stan_nte")
  saveRDS(new_data$standings[1:10,], "stan_nte.rds")
  write_sheet(new_data$standings[11:20,], stan_sur_gid, sheet = "stan_sur")
  saveRDS(new_data$standings[11:20,], "stan_sur.rds")
  write_sheet(new_data$standing_lmb, stan_lmb_gid, sheet = "stan_lmb")
  saveRDS(new_data$standing_lmb, "stan_lmb.rds")
  write_sheet(new_data$hth_nte, hth_nte_matrix_gid, sheet = "hth_nte_matrix")
  saveRDS(new_data$hth_nte, "hth_nte_matrix.rds")
  write_sheet(new_data$hth_sur, hth_sur_matrix_gid, sheet = "hth_sur_matrix")
  saveRDS(new_data$hth_sur, "hth_sur_matrix.rds")
  
  saveRDS(updated_hitting, "~/Documents/lmb_statsapp/cache/hitting.rds")
  saveRDS(updated_pitching, "~/Documents/lmb_statsapp/cache/pitching.rds")
  saveRDS(updated_fielding, "~/Documents/lmb_statsapp/cache/fielding.rds")
  saveRDS(updated_team_hitting, "~/Documents/lmb_statsapp/cache/team_hitting.rds")
  saveRDS(updated_team_pitching, "~/Documents/lmb_statsapp/cache/team_pitching.rds")
  saveRDS(updated_team_fielding, "~/Documents/lmb_statsapp/cache/team_fielding.rds")
  saveRDS(new_data$logs[-1], "~/Documents/lmb_statsapp/cache/game_logs.rds")
  saveRDS(new_data$rosters, "~/Documents/lmb_statsapp/cache/rosters.rds")
  saveRDS(new_data$att, "~/Documents/lmb_statsapp/cache/lmb_att_24.rds")
  saveRDS(new_data$pace, "~/Documents/lmb_statsapp/cache/lmb_pace_24.rds")
  saveRDS(new_data$pace_venue, "~/Documents/lmb_statsapp/cache/lmb_pace_venue_24.rds")
  saveRDS(hitting_cp, "~/Documents/lmb_statsapp/cache/hitting_cp.rds")
  saveRDS(pitching_cp, "~/Documents/lmb_statsapp/cache/pitching_cp.rds")
  saveRDS(new_data$standings[1:10,], "~/Documents/lmb_statsapp/cache/stan_nte.rds")
  saveRDS(new_data$standings[11:20,], "~/Documents/lmb_statsapp/cache/stan_sur.rds")
  saveRDS(new_data$standing_lmb, "~/Documents/lmb_statsapp/cache/stan_lmb.rds")
  saveRDS(new_data$hth_nte, "~/Documents/lmb_statsapp/cache/hth_nte_matrix.rds")
  saveRDS(new_data$hth_sur, "~/Documents/lmb_statsapp/cache/hth_sur_matrix.rds")
  
  print(paste("Google Sheets updated on", Sys.time()))
}

update_trans <- function(startDate,endDate){
  lmb_trans_data <- lmb_trans(startDate,endDate)
  
  write_sheet(lmb_trans_data, trans_gid, sheet = "lmb_trans")
  saveRDS(lmb_trans_data, "lmb_trans_data_2025.rds")
  saveRDS(lmb_trans_data, "~/Documents/lmb_statsapp/cache/lmb_trans_data_2025.rds")
  print("Transactions updated")
}

master_daily_etl <- function(startDate,endDate){
  update_google_sheets()
  update_trans(startDate,endDate)
}


master_daily_etl("2025-04-01",Sys.Date())
