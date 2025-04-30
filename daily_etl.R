library(baseballr)
library(googlesheets4)
library(dplyr)

# Google Sheets IDs (replace with your actual Sheet IDs)
woba_fipc_gid = "1H8xuzPAjuNJxlWaBAk1fmo-lGH9Jfm0Zeyrcv0R5OPY"
park_factors_gid = "1VkbsNGuHrhZHYoMdxiWVjeUyc3kSMLafYpg0nLHHdcY"

hitting = "1eVJ4dSg6KgATE8zmPxvriHyk6ZRZB94fuDWpdcloC1M"
pitching = "1xlYBP_x1mfsuFDnMxk4gtPUIl7FTIJ0hfVRK3BPeeRQ"
fielding = "1hVRO16lDvVHkYGva06sgG1715wYdEKHaIy2GwyC_K8U"
team_hitting = "1gkOC_566Bp7XCuuyEsIkK7jT0V0TMn--Xbuv1i4Wdxc"
team_pitching = "1uOckNtrCu811khLZ1CxSU7C2TLapZubH4g-Yh2XJTh4"
team_fielding = "1GzUH0leAnoAM64ml-XQ2dR9IFL19u31YCY6Ta955xlQ"
game_logs_gid = "1NHm3ZAMeTpBag95kOpozo_8Ngkxh9VSXot4bbEQpkR8"
hitting_cp = "1K-wOBfh9QW4ucEShjypBXQkti962Yajyf-Da5QEKbg0"
pitching_cp = "1Bkbt91wCmAbAGSlMpMgx8dHcPhsyFjxVpi5eo9gy3do"
rosters_gid = "1zeor7gan9NlpzDx_Dh5h1soDpdSye-V-joHZwXH9F2Y"
trans_gid = "1_Wy1KW5AUH_NTbog_QJ9dFiq5Ujm719MBM0YZ0X7EZw"
lmb_att_gs = "1uer8QQuM-x8VyxCDJBlCtqXofPHE-Dlkzzk64xzLFBQ"
lmb_pace_gs = "1sJ_KjQgmKDUtLRh1MW0yHyQWTzllarKHXGzSGrLNVBk"
lmb_pace_venue_gs = "1_zF8o6iYKrcpE0Cwgky4dpm7gXMt4nU1At7Z4j4qFJI"

cat("Starting job...\n", file = "job_progress.log", append = TRUE)


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
  
  # Add a date column for tracking
  daily_hitting <- daily_hitting %>% mutate(date = today)
  daily_pitching <- daily_pitching %>% mutate(date = today)
  daily_fielding <- daily_fielding %>% mutate(date = today)
  daily_team_hitting <- daily_team_hitting %>% mutate(date = today)
  daily_team_pitching <- daily_team_pitching %>% mutate(date = today)
  daily_team_fielding <- daily_team_fielding %>% mutate(date = today)
  
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
              ,pace = daily_pace))
}

# Function to update Google Sheets
update_google_sheets <- function() {
  new_data <- fetch_latest_stats()  # Get today's stats
  
  # Read existing data
  existing_hitting <- read_sheet(hitting)
  existing_hitting <- existing_hitting %>% filter(Year != 2025)
  
  existing_pitching <- read_sheet(pitching)
  existing_pitching <- existing_pitching %>% filter(Year != 2025)
  
  existing_fielding <- read_sheet(fielding)
  existing_fielding <- existing_fielding %>% filter(Year != 2025)
  
  existing_team_pitching <- read_sheet(team_pitching)
  existing_team_pitching <- existing_team_pitching %>% filter(Year != 2025)
  
  existing_team_hitting <- read_sheet(team_hitting)
  existing_team_hitting <- existing_team_hitting %>% filter(Year != 2025)
  
  existing_team_fielding <- read_sheet(team_fielding)
  existing_team_fielding <- existing_team_fielding %>% filter(Year != 2025)

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
  
  # Write back to Google Sheets
  write_sheet(updated_hitting, hitting, sheet = "hitting_data")
  write_sheet(updated_pitching, pitching, sheet = "pitching_data")
  write_sheet(updated_fielding, fielding, sheet = "fielding_data")
  write_sheet(updated_team_hitting, team_hitting, sheet = "team_hitting_data")
  write_sheet(updated_team_pitching, team_pitching, sheet = "team_pitching_data")
  write_sheet(updated_team_fielding, team_fielding, sheet = "team_fielding_data")
  write_sheet(new_data$logs[-1], game_logs_gid, sheet = "Sheet1")
  write_sheet(new_data$rosters, rosters_gid, sheet = "team_rosters_data")
  write_sheet(new_data$att, lmb_att_gs, sheet = "lmb_att_24")
  write_sheet(new_data$pace, lmb_pace_gs, sheet = "lmb_pace_24")
  write_sheet(new_data$pace_venue, lmb_pace_venue_gs, sheet = "lmb_pace_venue_24")
  
  
  print(paste("Google Sheets updated on", Sys.time()))
}

update_trans <- function(startDate,endDate){
  lmb_trans_data <- lmb_trans(startDate,endDate)
  
  write_sheet(lmb_trans_data, trans, sheet = "lmb_trans")
  print("Transactions updated")
}

master_daily_etl <- function(startDate,endDate){
  update_google_sheets()
  update_trans(startDate,endDate)
}

master_daily_etl("2025-04-01","2025-04-29")
cat("Finished job.\n", file = "job_progress.log", append = TRUE)
