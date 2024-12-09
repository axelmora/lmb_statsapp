woba_fipc <- read_csv("lmb_stats/woba_fipc.csv")

####EXTRACTION
hitting <- (mlb_stats(stat_type = 'season', player_pool = 'all', stat_group = 'hitting', season = 2024, sport_ids = 23))
#write.csv(hitting[-c(55:56)],"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/hitting.csv")
teams <- read_csv("/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/teams.csv")

###TRANSFORMATION
hitting <- hitting %>%
  select(season,player_full_name,team_name,position_abbreviation,games_played,at_bats,plate_appearances,hits,rbi,
         stolen_bases,doubles,triples,home_runs,runs,total_bases,hit_by_pitch,strike_outs,base_on_balls,
         intentional_walks,avg,obp,slg,ops,
         left_on_base,sac_bunts,sac_flies,ground_outs,air_outs,ground_outs_to_airouts,babip,at_bats_per_home_run
        ) %>%
  rename("Year"=season,"Name"=player_full_name,"Team"=team_name,"POS"=position_abbreviation,"GP"=games_played,
         "PA"=plate_appearances,"AB"=at_bats,"H"=hits,"RBI"=rbi,"SB"=stolen_bases,"2B"=doubles,"3B"=triples,"HR"=home_runs,"R"=runs,
         "TB"=total_bases,"HBP"=hit_by_pitch,"K"=strike_outs,"BB"=base_on_balls,"IBB"=intentional_walks,"AVG"=avg,
         "OBP"=obp,"SLG"=slg,"OPS"=ops,
         "LOB"=left_on_base,"SH"=sac_bunts,"SF"=sac_flies,"GO"=ground_outs,"AO"=air_outs,"GO/AO"=ground_outs_to_airouts,
         "BABIP"=babip,"AB/HR"=at_bats_per_home_run,
         ) %>%
  inner_join(teams, by = c("Team" = "team_full_name")) %>%
  mutate(Team = team_abbreviation
         ,`K%` = round((K/PA)*100,1)
         ,`BB%` = round((BB/PA)*100,1)
         ,`BB/K` = round((BB/K)*100,1)
         ,`1B` = H - `2B` - `3B` - `HR`
         ,`wOBA` = round((woba_fipc$HBP * `HBP`+
                   woba_fipc$BB * `BB` +
                   woba_fipc$`1B` * `1B` +
                   woba_fipc$`2B` * `2B` +
                   woba_fipc$`3B` * `3B` +
                   woba_fipc$HR * `HR`) / (`AB` + `BB` + `SF` + `HBP`),3)
         ,wRAA = round(((`wOBA` - woba_fipc$wOBA) / woba_fipc$wOBAScale)* `PA`,3)
         ) %>%
  select(!c(team_id:sport_id))

###LOAD
write.csv(hitting[1:23],"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/lmb_hitting_standard.csv")
write.csv(hitting[-c(8:23,35)],"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/lmb_hitting_advanced.csv")


####EXTRACTION
pitching<- (mlb_stats(stat_type = 'season', player_pool = 'all', stat_group = 'pitching', season = 2024, sport_ids = 23))
#write.csv(pitching[-c(83:84)],"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/lmb_pitching_test.csv")

teams <- read_csv("/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/teams.csv")

###TRANSFORMATION
pitching <- pitching %>%
  select(season,player_full_name,team_name,position_abbreviation,games_played,games_started,innings_pitched,era,wins,losses,
         saves,save_opportunities,holds,strike_outs,base_on_balls,intentional_walks,batters_faced,hits,
         home_runs,earned_runs,hit_by_pitch,ground_into_double_play,wild_pitches,
         ground_outs,air_outs,number_of_pitches,outs,complete_games,shutouts,balks,
         ground_outs_to_airouts,pitches_per_inning,strikeout_walk_ratio,strikeouts_per9inn,walks_per9inn,hits_per9inn,
         runs_scored_per9,home_runs_per9
         ) %>%
  rename("Year"=season,"Name"=player_full_name,"Team"=team_name,"POS"=position_abbreviation,"GP"=games_played,
         "GS"=games_started,"IP"=innings_pitched,"ERA"=era,"W"=wins,"L"=losses,"SV"=saves,"SVO"=save_opportunities,"HLD"=holds,
         "K"=strike_outs,"BB"=base_on_balls,"IBB"=intentional_walks,"BF"=batters_faced,"H"=hits,
         "HR"=home_runs,"ER"=earned_runs,"HBP"=hit_by_pitch,"GIDP"=ground_into_double_play,"WP"=wild_pitches,
         "GO"=ground_outs,"AO"=air_outs,"NP"=number_of_pitches,"O"=outs,"CG"=complete_games,"SHO"=shutouts,
         "BK"=balks,"GO/AO"=ground_outs_to_airouts,"P/INN"=pitches_per_inning,"K/BB"=strikeout_walk_ratio,
         "K/9"=strikeouts_per9inn,"W/9"=walks_per9inn,"H/9"=hits_per9inn,"R/9"=runs_scored_per9,"HR/9"=home_runs_per9
         ) %>%
  inner_join(teams, by = c("Team" = "team_full_name")) %>%
  mutate(Team = team_abbreviation
         ,`K%` = round((K/BF)*100,1)
         ,`BB%` = round((BB/BF)*100,1)
         ,`K-BB%` = round((`K%`- `BB%`),1)
         ) %>%
  select(!c(team_id:sport_id))

###LOAD
write.csv(pitching[1:23],"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/lmb_pitching_standard.csv")
write.csv(pitching[-c(8:23)],"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/lmb_pitching_advanced.csv")



####EXTRACTION
fielding<- (mlb_stats(stat_type = 'season', player_pool = 'all', stat_group = 'fielding', season = 2024, sport_ids = 23))
#write.csv(fielding[-c(45,46)],"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/lmb_fielding_test.csv")
teams <- read_csv("/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/teams.csv")

###TRANSFORMATION
fielding <- fielding %>%
  select(season,player_full_name,team_name,position_abbreviation,games_played, games_started,innings,assists,put_outs,errors,fielding,
         double_plays,triple_plays,caught_stealing,stolen_bases,stolen_base_percentage,passed_ball,pickoffs,
         chances,range_factor_per_game,range_factor_per9inn,throwing_errors,catcher_era,catchers_interference
         ) %>%
  rename("Year"=season,"Name"=player_full_name,"Team"=team_name,"POS"=position_abbreviation,"GP"=games_played,"GS"=games_started,
         "INN"=innings,"A"=assists,"PO"=put_outs,"E"=errors,"F%"=fielding,"DP"=double_plays,"TP"=triple_plays,
         "CS"=caught_stealing,"SB"=stolen_bases,"CS%"=stolen_base_percentage,"PB"=passed_ball,"PK"=pickoffs,
         "TC"=chances,"RF/G"=range_factor_per_game,"RF/9"=range_factor_per9inn,"TE"=throwing_errors,"CERA"=catcher_era,
         "CI"=catchers_interference
         ) %>%
  inner_join(teams, by = c("Team" = "team_full_name")) %>%
  mutate(Team = team_abbreviation
         ,`CS%` = CS/(SB+CS)) %>%
  select(!c(team_id:sport_id))

###LOAD
write.csv(fielding[1:18],"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/lmb_fielding_standard.csv")
write.csv(fielding[-c(8:18)],"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/lmb_fielding_advanced.csv")


# ---------------------- STATS BY TEAMS --------------------------------------
team_hitting <- mlb_teams_stats(stat_type = 'season', stat_group = 'hitting', season = 2024, sport_ids = 23)

team_hitting  <- team_hitting  %>%
  select(season,team_name,games_played,at_bats,plate_appearances,hits,doubles,triples,home_runs,rbi,runs,
         stolen_bases,caught_stealing,total_bases,hit_by_pitch,strike_outs,base_on_balls,
         intentional_walks,avg,obp,slg,ops,
         ground_into_double_play,left_on_base,sac_bunts,sac_flies,ground_outs,air_outs,ground_outs_to_airouts,
         at_bats_per_home_run,babip
  ) %>%
  rename("Year"=season,"Team"=team_name,"GP"=games_played,"PA"=plate_appearances,"AB"=at_bats,"H"=hits,
         "2B"=doubles,"3B"=triples,"HR"=home_runs,"RBI"=rbi,"R"=runs,"SB"=stolen_bases,"CS"=caught_stealing,
         "TB"=total_bases,"HBP"=hit_by_pitch,"K"=strike_outs,"BB"=base_on_balls,"IBB"=intentional_walks,
         "AVG"=avg,"OBP"=obp,"SLG"=slg,"OPS"=ops,
         "GIDP"=ground_into_double_play,"LOB"=left_on_base,"SH"=sac_bunts,"SF"=sac_flies,"GO"=ground_outs,
         "AO"=air_outs,"GO/AO"=ground_outs_to_airouts,"AB/HR"=at_bats_per_home_run,"BABIP"=babip
  ) %>%
  mutate(`K%` = round((K/PA)*100,1)
         ,`BB%` = round((BB/PA)*100,1)
         ,`BB/K` = round((BB/K)*100,1)
  ) 

write.csv(team_hitting[1:22],"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/lmb_hitting_team_standard.csv")
write.csv(team_hitting[-c(6:22)],"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/lmb_hitting_team_advanced.csv")


team_pitching <- mlb_teams_stats(stat_type = 'season', stat_group = 'pitching', season = 2024, sport_ids = 23)

team_pitching <- team_pitching %>%
  select(season,team_name,games_played,innings_pitched,wins,losses,
         saves,save_opportunities,holds,strike_outs,base_on_balls,intentional_walks,batters_faced,hits,
         home_runs,earned_runs,era,whip,hit_by_pitch,ground_into_double_play,wild_pitches,
         ground_outs,air_outs,number_of_pitches,outs,complete_games,shutouts,balks,
         ground_outs_to_airouts,pitches_per_inning,strikeout_walk_ratio,strikeouts_per9inn,walks_per9inn,hits_per9inn,
         runs_scored_per9,home_runs_per9
  ) %>%
  rename("Year"=season,"Team"=team_name,"GP"=games_played,"IP"=innings_pitched,"ERA"=era,"W"=wins,"L"=losses,"SV"=saves,
         "SVO"=save_opportunities,"HLD"=holds,"K"=strike_outs,"BB"=base_on_balls,"IBB"=intentional_walks,"BF"=batters_faced,"H"=hits,
         "HR"=home_runs,"ER"=earned_runs,"ERA"=era,"WHIP"=whip,"HBP"=hit_by_pitch,"GIDP"=ground_into_double_play,"WP"=wild_pitches,
         "GO"=ground_outs,"AO"=air_outs,"NP"=number_of_pitches,"O"=outs,"CG"=complete_games,"SHO"=shutouts,
         "BK"=balks,"GO/AO"=ground_outs_to_airouts,"P/INN"=pitches_per_inning,"K/BB"=strikeout_walk_ratio,
         "K/9"=strikeouts_per9inn,"W/9"=walks_per9inn,"H/9"=hits_per9inn,"R/9"=runs_scored_per9,"HR/9"=home_runs_per9
  ) %>%
  mutate(
    `K%` = round((K/BF)*100,1)
    ,`BB%` = round((BB/BF)*100,1)
    ,`K-BB%` = round((`K%`- `BB%`),1)
  )

###LOAD
write.csv(team_pitching[1:21],"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/lmb_pitching_team_standard.csv")
write.csv(team_pitching[-c(5:21)],"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/lmb_pitching_team_advanced.csv")

team_fielding <- mlb_teams_stats(stat_type = 'season', stat_group = 'fielding', season = 2024, sport_ids = 23)

team_fielding <- team_fielding %>%
  select(season,team_name,games_played,innings,assists,put_outs,errors,fielding,
         double_plays,triple_plays,caught_stealing,stolen_bases,stolen_base_percentage,passed_ball,pickoffs,
         chances,range_factor_per_game,range_factor_per9inn,throwing_errors,catchers_interference
  ) %>%
  rename("Year"=season,"Team"=team_name,"GP"=games_played,
         "INN"=innings,"A"=assists,"PO"=put_outs,"E"=errors,"F%"=fielding,"DP"=double_plays,"TP"=triple_plays,
         "CS"=caught_stealing,"SB"=stolen_bases,"CS%"=stolen_base_percentage,"PB"=passed_ball,"PK"=pickoffs,
         "TC"=chances,"RF/G"=range_factor_per_game,"RF/9"=range_factor_per9inn,"TE"=throwing_errors,
         "CI"=catchers_interference
  ) %>%
  mutate(`CS%` = (CS/(SB+CS)*100)) 


write.csv(team_fielding[1:10],"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/lmb_fielding_team_standard.csv")
write.csv(team_fielding[-c(4:10)],"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/lmb_fielding_team_advanced.csv")

