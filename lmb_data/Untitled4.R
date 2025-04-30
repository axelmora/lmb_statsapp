library(baseballr)
library(dplyr)

lmb_pace <- function(year){
  lmb_pace_25 <- mlb_game_pace(season = year, sport_ids = 23)
  lmb_pace_25 <- lmb_pace_24 %>%
    select(season, hits_per9inn, runs_per9inn, pitches_per_pitcher, time_per_pitch, time_per_plate_appearance,
           pr_portal_calculated_fields_time_per9inn_game
           ) %>%
    rename("Year"=season,"Hits/9in"=hits_per9inn,"Runs/9in"=runs_per9inn,"Pitches/Pitcher"=pitches_per_pitcher,
           "Time/Pitch"=time_per_pitch,"Time/PA"=time_per_plate_appearance,"Time/9inGame"=pr_portal_calculated_fields_time_per9inn_game)
  lmb_pace_25
}
#write.csv(lmb_pace_24,"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/lmb_pace_24.csv")

lmb_pace_24 <- gs4_create("lmb_pace_24", sheets = lmb_pace_24)

lmb_pace_venue <- function(year){

  league_pace_venue = list()

  for(i in venues$venue_id){
    print(venues$venue_id)
    name <- venues %>% filter(venue_id == i) %>% 
              select(venue_name, team_full_name)
    pace_venue_aux <- mlb_game_pace(season = year, venue_ids = i, sport_ids = 23)
    pace_venue_select <- pace_venue_aux %>%
        select(hits_per9inn, runs_per9inn, pitches_per_pitcher, time_per_pitch, time_per_plate_appearance, 
               pr_portal_calculated_fields_time_per9inn_game)
    league_pace_venue[[i]] <- cbind(name,pace_venue_select)}
  
    lmb_pace_venue_25 = do.call(rbind, league_pace_venue)
  
    lmb_pace_venue_25 <- lmb_pace_venue_25 %>%
      rename("Venue"=venue_name,"Team"=team_full_name,
      "Hits/9in"=hits_per9inn,"Runs/9in"=runs_per9inn,"Pitches/Pitcher"=pitches_per_pitcher,
      "Time/Pitch"=time_per_pitch,"Time/PA"=time_per_plate_appearance,"Time/9inGame"=pr_portal_calculated_fields_time_per9inn_game)
    lmb_pace_venue_25
}
#lmb_pace_venue_24 <- gs4_create("lmb_pace_venue_24", sheets = lmb_pace_venue_24)

lmb_att <- function(year){
lmb_att_25 <- mlb_attendance(season = year, league_id = 125)
lmb_att_25 <- lmb_att_25 %>%
    select(team_name, openings_total_home, attendance_average_away, attendance_high, attendance_low, attendance_average_home, 
           attendance_total_home) %>%
    rename('Team' = team_name, 'Home Openings' = openings_total_home, 'AVG Away Attendance' = attendance_average_away, 
           'High Home Attendance' = attendance_high,
           'Low Home Attendance' = attendance_low,'Avg Home Attendance' = attendance_average_home,
           'Avg Away Attendance' = attendance_average_away,'Total Home Attendance' = attendance_total_home
           ) %>%
    inner_join(venues_cap, by = c("Team" = "team_full_name")) %>%
    select(!c(team_id:venue_name)) %>%
    rename('Capacity'=capacity) %>%
    mutate(`Avg Capacity%` = round(((`Avg Home Attendance`*100)/`Capacity`),1))
    lmb_att_25
}

#write.csv(lmb_att_24,"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/lmb_att_teams.csv")
lmb_att_24 <- gs4_create("lmb_att_24", sheets = lmb_att_24)


lmb_att_gs = "1uer8QQuM-x8VyxCDJBlCtqXofPHE-Dlkzzk64xzLFBQ"
lmb_pace_gs = "1sJ_KjQgmKDUtLRh1MW0yHyQWTzllarKHXGzSGrLNVBk"
lmb_pace_venue_gs = "1_zF8o6iYKrcpE0Cwgky4dpm7gXMt4nU1At7Z4j4qFJI"

write_sheet(lmb_att_25, lmb_att_gs, sheet = "lmb_att_24")
write_sheet(lmb_pace_25, lmb_pace_gs, sheet = "lmb_pace_24")
write_sheet(lmb_pace_venue_25, lmb_pace_venue_gs, sheet = "lmb_pace_venue_24")


lmb_att_avg <- sum(lmb_att_24$`Total Home Attendance`)/sum(lmb_att_24$`Home Openings`)
lmb_cap_pct <- (lmb_att_avg*100)/mean(lmb_att_24$Capacity)
lmb_max_att <- max(lmb_att_24$`High Home Attendance`)





write.csv(stats_api_live_empty_df,"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/table.csv")

woba_fipc <- read_csv("/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/woba_fipc.csv")
woba_fipc <- as.data.frame(woba_fipc[2:12])
woba_fipc <- gs4_create("woba_fipc", sheets = woba_fipc)
park_factors <- read_csv("/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/park_factors.csv")
park_factors <- as.data.frame(park_factors[2:8])
park_factors <- gs4_create("park_factors", sheets = park_factors)




gs_ids <- data.frame(woba_fipc, park_factors, fielding_std, fielding_adv, hitting_std, hitting_adv, pitching_std, pitching_adv, team_fielding_std, 
                     team_fielding_adv, team_hitting_std, team_hitting_adv, team_pitching_std, 
                     team_pitching_adv, lmb_att_24, lmb_pace_24, lmb_pace_venue_24)

write.csv(gs_ids,"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/gs_ids.csv")