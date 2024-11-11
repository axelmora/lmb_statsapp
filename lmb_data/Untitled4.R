library(baseballr)
library(dplyr)

lmb_pace_24 <- mlb_game_pace(season = 2024, sport_ids = 23)
lmb_pace_24 <- lmb_pace_24 %>%
  select(season, hits_per9inn, runs_per9inn, pitches_per_pitcher, time_per_pitch, time_per_plate_appearance,
         pr_portal_calculated_fields_time_per9inn_game
         ) %>%
  rename("Year"=season,"Hits/9in"=hits_per9inn,"Runs/9in"=runs_per9inn,"Pitches/Pitcher"=pitches_per_pitcher,
         "Time/Pitch"=time_per_pitch,"Time/PA"=time_per_plate_appearance,"Time/9inGame"=pr_portal_calculated_fields_time_per9inn_game
        )
write.csv(lmb_pace_24,"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/lmb_pace_24.csv")

venues <- read_csv("/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/venues.csv")
league_pace_venue = list()

for(i in venues$venue_id){
  print(venues$venue_id)
  name <- venues %>% filter(venue_id == i) %>% 
            select(venue_name, team_full_name)
  pace_venue_aux <- mlb_game_pace(season = 2024, venue_ids = i, sport_ids = 23)
  pace_venue_select <- pace_venue_aux %>%
      select(hits_per9inn, runs_per9inn, pitches_per_pitcher, time_per_pitch, time_per_plate_appearance, 
             pr_portal_calculated_fields_time_per9inn_game)
  league_pace_venue[[i]] <- cbind(name,pace_venue_select)
}
lmb_pace_venue_24 = do.call(rbind, league_pace_venue)

lmb_pace_venue_24 <- lmb_pace_venue_24 %>%
  rename("Venue"=venue_name,"Team"=team_full_name,
    "Hits/9in"=hits_per9inn,"Runs/9in"=runs_per9inn,"Pitches/Pitcher"=pitches_per_pitcher,
    "Time/Pitch"=time_per_pitch,"Time/PA"=time_per_plate_appearance,"Time/9inGame"=pr_portal_calculated_fields_time_per9inn_game
  )

write.csv(lmb_pace_venue_24,"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/lmb_pace_venue_24.csv")


lmb_att_24 <- mlb_attendance(season = 2024, league_id = 125)
lmb_att_24 <- lmb_att_24 %>%
    select(team_name, attendance_average_away, attendance_high, attendance_low, attendance_average_home, attendance_total_home) %>%
    rename('EQUIPO' = team_name, 'AVG ASISTENCIA GIRA' = attendance_average_away, 'MAXIMA ASISTENCIA CASA' = attendance_high,
           'MENOR ASISTENCIA CASA' = attendance_low,
           'AVG ASISTENCIA CASA' = attendance_average_home,
           'AVG ASISTENCIA GIRA' = attendance_average_away,
           'ASISTENCIA TOTAL CASA' = attendance_total_home
           )
write.csv(lmb_att_24,"/Users/axel.mora/lmb_att_24_0708.csv")