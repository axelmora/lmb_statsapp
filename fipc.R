pitching <- mlb_stats(league_id = 125, stat_type = 'season', season = 2024, stat_group = 'pitching')



fipc <- 6.11 - (((13*2022)+(3*(822+5806))-(2*10261))/12836)

teams_23 <- mlb_teams(season = 2023, league_ids = 125) %>%
  select(team_id, team_full_name, team_code, team_abbreviation, franchise_name, club_name, venue_id, venue_name, league_id, 
         division_id, sport_id)

teams_16 <- mlb_teams(season = 2016, league_ids = 125) %>%
  select(team_id, team_full_name, team_code, team_abbreviation, franchise_name, club_name, venue_id, venue_name, league_id, 
         division_id, sport_id)

mlb_team_info(team_id = 4444)
lag <- 
mlb_team_history(team_ids = 447)


lmb <- mlb_seasons_all(sport_id = 23, league_id = 125)