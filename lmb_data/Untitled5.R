lmb_standings_24_raw <- mlb_standings(season = 2024, league_id = 125, standings_type = "byDivision")

lmb_standings_24 <- lmb_standings_24_raw %>%
                      select(team_records_division_rank, team_records_team_name, team_records_wins, team_records_losses, team_records_league_record_pct,
                             team_records_division_games_back, team_records_streak_streak_code)

lmb_standing_nte <- lmb_standings_24[1:10,] 
lmb_standing_sur <- lmb_standings_24[11:20,] 

lmb_standings_24_raw_0612 <- mlb_standings(season = 2024, league_id = 125, standings_type = "byDivision", date = "06/12/2024")

lmb_standings_24_0612 <- lmb_standings_24_raw_0612 %>%
  select(team_records_division_rank, team_records_team_name, team_records_wins, team_records_losses, team_records_league_record_pct,
         team_records_division_games_back, team_records_streak_streak_code)

lmb_standing_nte_0722 <- lmb_standings_24_0612[1:10,] 
write.csv(lmb_standing_nte_0722,"/Users/axel.mora/lmb_standing_nte_0722.csv")
lmb_standing_sur_0722 <- lmb_standings_24_0612[11:20,] 
write.csv(lmb_standing_sur_0722,"/Users/axel.mora/lmb_standing_sur_0722.csv")