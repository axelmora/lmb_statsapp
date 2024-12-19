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
#write.csv(lmb_pace_24,"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/lmb_pace_24.csv")

lmb_pace_24 <- gs4_create("lmb_pace_24", sheets = lmb_pace_24)

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

#write.csv(lmb_pace_venue_24,"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/lmb_pace_venue_24.csv")
lmb_pace_venue_24 <- gs4_create("lmb_pace_venue_24", sheets = lmb_pace_venue_24)

lmb_att_24 <- mlb_attendance(season = 2024, league_id = 125)
lmb_att_24 <- lmb_att_24 %>%
    select(team_name, openings_total_home, attendance_average_away, attendance_high, attendance_low, attendance_average_home, 
           attendance_total_home) %>%
    rename('Team' = team_name, 'Home Openings' = openings_total_home, 'AVG Away Attendance' = attendance_average_away, 
           'High Home Attendance' = attendance_high,
           'Low Home Attendance' = attendance_low,'Avg Home Attendance' = attendance_average_home,
           'Avg Away Attendance' = attendance_average_away,'Total Home Attendance' = attendance_total_home
           ) %>%
    inner_join(teams, by = c("Team" = "team_full_name")) %>%
    select(!c(team_id:venue_name,league_id:sport_id)) %>%
    rename('Capacity'=capacity) %>%
    mutate(
      `Avg Capacity%` = round(((`Avg Home Attendance`*100)/`Capacity`),1)
    )

#write.csv(lmb_att_24,"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/lmb_att_teams.csv")
lmb_att_24 <- gs4_create("lmb_att_24", sheets = lmb_att_24)

lmb_att_avg <- sum(lmb_att_24$`Total Home Attendance`)/sum(lmb_att_24$`Home Openings`)
lmb_cap_pct <- (lmb_att_avg*100)/mean(lmb_att_24$Capacity)
lmb_max_att <- max(lmb_att_24$`High Home Attendance`)

write.csv(stats_api_live_empty_df,"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/table.csv")




dataframe2 <- data.frame(game_pk=character(),game_date=character(),index=character(),startTime=character(),
                         endTime=character(),
                         isPitch=character(),
                         type=character(),
                         playId=character(),
                         pitchNumber=character(),
                         details.description=character(),
                         details.event=character(),
                         details.awayScore=character(),
                         details.homeScore=character(),
                         details.isScoringPlay=character(),
                         details.hasReview=character(),
                         details.code=character(),
                         details.ballColor=character(),
                         details.isInPlay=character(),
                         details.isStrike=character(),
                         details.isBall=character(),
                         details.call.code=character(),
                         details.call.description=character(),
                         count.balls.x=character(),
                         count.strikes.x=character(),
                         count.outs.x=character(),
                         player.id=character(),
                         player.link=character(),
                         pitchData.strikeZoneTop=character(),
                         pitchData.strikeZoneBottom=character(),
                         details.fromCatcher=character(),
                         pitchData.coordinates.x=character(),
                         pitchData.coordinates.y=character(),
                         hitData.trajectory=character(),
                         hitData.hardness=character(),
                         hitData.location=character(),
                         hitData.coordinates.coordX=character(),
                         hitData.coordinates.coordY=character(),
                         actionPlayId=character(),
                         details.eventType=character(),
                         details.runnerGoing=character(),
                         position.code=character(),
                         position.name=character(),
                         position.type=character(),
                         position.abbreviation=character(),
                         battingOrder=character(),
                         atBatIndex=character(),
                         result.type=character(),
                         result.event=character(),
                         result.eventType=character(),
                         result.description=character(),
                         result.rbi=character(),
                         result.awayScore=character(),
                         result.homeScore=character(),
                         about.atBatIndex=character(),
                         about.halfInning=character(),
                         about.inning=character(),
                         about.startTime=character(),
                         about.endTime=character(),
                         about.isComplete=character(),
                         about.isScoringPlay=character(),
                         about.hasReview=character(),
                         about.hasOut=character(),
                         about.captivatingIndex=character(),
                         count.balls.y=character(),
                         count.strikes.y=character(),
                         count.outs.y=character(),
                         matchup.batter.id=character(),
                         matchup.batter.fullName=character(),
                         matchup.batter.link=character(),
                         matchup.batSide.code=character(),
                         matchup.batSide.description=character(),
                         matchup.pitcher.id=character(),
                         matchup.pitcher.fullName=character(),
                         matchup.pitcher.link=character(),
                         matchup.pitchHand.code=character(),
                         matchup.pitchHand.description=character(),
                         matchup.splits.batter=character(),
                         matchup.splits.pitcher=character(),
                         matchup.splits.menOnBase=character(),
                         batted.ball.result=character(),
                         home_team=character(),
                         home_level_id=character(),
                         home_level_name=character(),
                         home_parentOrg_id=character(),
                         home_parentOrg_name=character(),
                         home_league_id=character(),
                         home_league_name=character(),
                         away_team=character(),
                         away_level_id=character(),
                         away_level_name=character(),
                         away_parentOrg_id=character(),
                         away_parentOrg_name=character(),
                         away_league_id=character(),
                         away_league_name=character(),
                         batting_team=character(),
                         fielding_team=character(),
                         last.pitch.of.ab=character(),
                         pfxId=character(),
                         details.trailColor=character(),
                         details.type.code=character(),
                         details.type.description=character(),
                         pitchData.startSpeed=character(),
                         pitchData.endSpeed=character(),
                         pitchData.zone=character(),
                         pitchData.typeConfidence=character(),
                         pitchData.plateTime=character(),
                         pitchData.extension=character(),
                         pitchData.coordinates.aY=character(),
                         pitchData.coordinates.aZ=character(),
                         pitchData.coordinates.pfxX=character(),
                         pitchData.coordinates.pfxZ=character(),
                         pitchData.coordinates.pX=character(),
                         pitchData.coordinates.pZ=character(),
                         pitchData.coordinates.vX0=character(),
                         pitchData.coordinates.vY0=character(),
                         pitchData.coordinates.vZ0=character(),
                         pitchData.coordinates.x0=character(),
                         pitchData.coordinates.y0=character(),
                         pitchData.coordinates.z0=character(),
                         pitchData.coordinates.aX=character(),
                         pitchData.breaks.breakAngle=character(),
                         pitchData.breaks.breakLength=character(),
                         pitchData.breaks.breakY=character(),
                         pitchData.breaks.spinRate=character(),
                         pitchData.breaks.spinDirection=character(),
                         hitData.launchSpeed=character(),
                         hitData.launchAngle=character(),
                         hitData.totalDistance=character(),
                         injuryType=character(),
                         umpire.id=character(),
                         umpire.link=character()
                         )


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