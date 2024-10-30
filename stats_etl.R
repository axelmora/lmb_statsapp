####EXTRACTION
hitting <- (mlb_stats(stat_type = 'season', player_pool = 'all', stat_group = 'hitting', season = 2024, sport_ids = 23))
teams <- read_csv("/Users/axel.mora/Documents/lmb_statsapp/test_theme/teams.csv")

###TRANSFORMATION
hitting <- hitting %>%
  select(season,player_full_name,team_name,position_abbreviation,games_played,at_bats,plate_appearances,hits,rbi,
         stolen_bases,doubles,triples,home_runs,runs,total_bases,hit_by_pitch,sac_flies,strike_outs,base_on_balls,
         intentional_walks,avg,obp,slg,ops) %>%
  rename("Season"=season,"Name"=player_full_name,"Team"=team_name,"POS"=position_abbreviation,"GP"=games_played,
         "PA"=plate_appearances,"AB"=at_bats,"H"=hits,"RBI"=rbi,"SB"=stolen_bases,"2B"=doubles,"3B"=triples,"HR"=home_runs,"R"=runs,
         "TB"=total_bases,"HBP"=hit_by_pitch,"SF"=sac_flies,"SO"=strike_outs,"BB"=base_on_balls,"IBB"=intentional_walks,"AVG"=avg,
         "OBP"=obp,"SLG"=slg,"OPS"=ops) %>%
  inner_join(teams, by = c("Team" = "team_full_name")) %>%
  mutate(Team = team_abbreviation)

###LOAD
write.csv(hitting[1:24],"/Users/axel.mora/Documents/lmb_statsapp/test_theme/lmb_hitting_2024.csv")

####EXTRACTION
pitching<- (mlb_stats(stat_type = 'season', player_pool = 'all', stat_group = 'pitching', season = 2024, sport_ids = 23))
teams <- read_csv("/Users/axel.mora/Documents/lmb_statsapp/test_theme/teams.csv")

###TRANSFORMATION
pitching <- pitching %>%
  select(season,player_full_name,team_name,games_played,games_started,innings_pitched,era,wins,losses,
         saves,save_opportunities,holds,strike_outs,base_on_balls,intentional_walks,batters_faced,hits,
         home_runs,earned_runs,hit_by_pitch,ground_into_double_play,wild_pitches) %>%
  rename("Season"=season,"Name"=player_full_name,"Team"=team_name,"GP"=games_played,
         "GS"=games_started,"IP"=innings_pitched,"ERA"=era,"W"=wins,"L"=losses,"SV"=saves,"Svopp"=save_opportunities,"HLD"=holds,
         "K"=strike_outs,"BB"=base_on_balls,"IBB"=intentional_walks,"BF"=batters_faced,"H"=hits,
         "HR"=home_runs,"ER"=earned_runs,"HBP"=hit_by_pitch,"GIDP"=ground_into_double_play,"WP"=wild_pitches) %>%
  inner_join(teams, by = c("Team" = "team_full_name")) %>%
  mutate(Team = team_abbreviation)

###LOAD
write.csv(pitching,"/Users/axel.mora/Documents/lmb_statsapp/test_theme/lmb_pitching_2024.csv")


####EXTRACTION
fielding<- (mlb_stats(stat_type = 'season', player_pool = 'all', stat_group = 'fielding', season = 2024, sport_ids = 23))
teams <- read_csv("/Users/axel.mora/Documents/lmb_statsapp/test_theme/teams.csv")

###TRANSFORMATION
fielding <- fielding %>%
  select(season,player_full_name,team_name,games_played, games_started,assists,put_outs,errors,fielding,
         double_plays,triple_plays,caught_stealing,stolen_bases,stolen_base_percentage,passed_ball,pickoffs) %>%
  rename("Season"=season,"Name"=player_full_name,"Team"=team_name,"GP"=games_played,"GS"=games_started,
         "A"=assists,"PO"=put_outs,"E"=errors,"F%"=fielding,"DP"=double_plays,"TP"=triple_plays,
         "CS"=caught_stealing,"SB"=stolen_bases,"SB%"=stolen_base_percentage,"PB"=passed_ball) %>%
  inner_join(teams, by = c("Team" = "team_full_name")) %>%
  mutate(Team = team_abbreviation)

###LOAD
write.csv(fielding,"/Users/axel.mora/Documents/lmb_statsapp/test_theme/lmb_fielding_2024.csv")
