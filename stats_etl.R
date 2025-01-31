woba_fipc <- read_csv("lmb_stats/woba_fipc.csv")
park_factors <- read_csv("lmb_stats/park_factors.csv")

pf_br <- park_factors %>%
  select(team_abbreviation, PF) %>%
  rename(Team = team_abbreviation)

hitting_etl <- function(year){
####EXTRACTION
sport_id <- if_else(year > 2020, 23, 11)
hitting <- (mlb_stats(stat_type = 'season', player_pool = 'all', stat_group = 'hitting', season = year, sport_ids = sport_id))
teams <- mlb_teams(season = year, league_ids = 125) %>%
  select(team_id, team_full_name, team_code, team_abbreviation, franchise_name, club_name, venue_id, venue_name, league_id, 
         division_id, sport_id)
woba_fipc <- woba_fipc %>% filter(Season == year)
###TRANSFORMATION
hitting <- hitting %>%
  filter(league_name == 'MEX') %>%
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
         "BABIP"=babip,"AB/HR"=at_bats_per_home_run
         ) %>%
  inner_join(teams, by = c("Team" = "team_full_name")) %>%
  mutate(Team = team_abbreviation
         ,`K%` = round((K/PA)*100,1)
         ,`BB%` = round((BB/PA)*100,1)
         ,`BB/K` = round((BB/K)*100,1)
         ,`1B` = H - `2B` - `3B` - `HR`
         ,`wOBA` = round((woba_fipc$wHBP * `HBP`+
                   woba_fipc$wBB * `BB` +
                   woba_fipc$`w1B` * `1B` +
                   woba_fipc$`w2B` * `2B` +
                   woba_fipc$`w3B` * `3B` +
                   woba_fipc$wHR * `HR`) / (`AB` + `BB` + `SF` + `HBP`),3)
         ,wRAA = round(((`wOBA` - woba_fipc$wOBA) / woba_fipc$wOBAScale)* `PA`,1)
         ,wRC = round((((`wOBA`- woba_fipc$wOBA)/woba_fipc$wOBAScale)+(woba_fipc$`R/PA`))*PA)
         ) %>%
  inner_join(pf_br, by = join_by(Team)) %>%
  select(!c(team_id:sport_id)) %>%
  mutate(`wRC+` = round((((wRAA/PA + woba_fipc$`R/PA`) + (woba_fipc$`R/PA` - ((PF/100) * woba_fipc$`R/PA`)))/(wRC/PA)) * 100)
        ) %>%
  select(!c(PF))

hitting
###LOAD

}

pitching_etl <- function(year){
  ####EXTRACTION
  sport_id <- if_else(year > 2020, 23, 11)
  pitching <- (mlb_stats(stat_type = 'season', player_pool = 'all', stat_group = 'pitching', season = year, sport_ids = sport_id))
  teams <- mlb_teams(season = year, league_ids = 125) %>%
    select(team_id, team_full_name, team_code, team_abbreviation, franchise_name, club_name, venue_id, venue_name, league_id, 
           division_id, sport_id)
  woba_fipc <- woba_fipc %>% filter(Season == year)
  
  ###TRANSFORMATION
  pitching <- pitching %>%
    filter(league_name == 'MEX') %>%
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
           ,FIP = round((((13*HR) + (3*(BB+HBP)) - (2*K)) / as.numeric(IP)) + 3.813 ,2)
    ) %>%
    select(!c(team_id:sport_id))
  
  pitching
  ###LOAD
  
}

fielding_etl <- function(year){
  ####EXTRACTION
  sport_id <- if_else(year > 2020, 23, 11)
  fielding <- (mlb_stats(stat_type = 'season', player_pool = 'all', stat_group = 'fielding', season = year, sport_ids = sport_id))
  teams <- mlb_teams(season = year, league_ids = 125) %>%
    select(team_id, team_full_name, team_code, team_abbreviation, franchise_name, club_name, venue_id, venue_name, league_id, 
           division_id, sport_id)
  
  ###TRANSFORMATION
  fielding <- fielding %>%
    filter(league_name == 'MEX') %>%
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
  
  fielding
  ###LOAD
  
}

team_hitting_etl <- function(year){
  sport_id <- if_else(year > 2020, 23, 11)
  team_hitting <- mlb_teams_stats(stat_type = 'season', stat_group = 'hitting', season = year, sport_ids = sport_id)
  teams <- mlb_teams(season = year, league_ids = 125) %>%
    select(team_id, team_full_name, team_code, team_abbreviation, franchise_name, club_name, venue_id, venue_name, league_id, 
           division_id, sport_id)
  woba_fipc <- woba_fipc %>% filter(Season == year)
  team_hitting  <- team_hitting  %>%
    filter(team_id %in% teams$team_id) %>%
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
           ,`1B` = H - `2B` - `3B` - `HR`
           ,`wOBA` = round((woba_fipc$wHBP * `HBP`+
                              woba_fipc$wBB * `BB` +
                              woba_fipc$`w1B` * `1B` +
                              woba_fipc$`w2B` * `2B` +
                              woba_fipc$`w3B` * `3B` +
                              woba_fipc$wHR * `HR`) / (`AB` + `BB` + `SF` + `HBP`),3)
           ,wRAA = round(((`wOBA` - woba_fipc$wOBA) / woba_fipc$wOBAScale)* `PA`,1)
           ,wRC = round((((`wOBA`- woba_fipc$wOBA)/woba_fipc$wOBAScale)+(woba_fipc$`R/PA`))*PA)
    ) %>%
    inner_join(park_factors, by = join_by(Team)) %>%
    mutate(`wRC+` = round((((wRAA/PA + woba_fipc$`R/PA`) + (woba_fipc$`R/PA` - ((PF/100) * woba_fipc$`R/PA`)))/(wRC/PA)) * 100)
    ) %>%
    select(!c(39:42))
  team_hitting
  
}

team_pitching_etl <- function(year){
  sport_id <- if_else(year > 2020, 23, 11)
  team_pitching <- mlb_teams_stats(stat_type = 'season', stat_group = 'pitching', season = year, sport_ids = sport_id)
  teams <- mlb_teams(season = year, league_ids = 125) %>%
    select(team_id, team_full_name, team_code, team_abbreviation, franchise_name, club_name, venue_id, venue_name, league_id, 
           division_id, sport_id)
  woba_fipc <- woba_fipc %>% filter(Season == year)
  team_pitching <- team_pitching %>%
    filter(team_id %in% teams$team_id) %>%
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
      ,FIP = round((((13*HR) + (3*(BB+HBP)) - (2*K)) / as.numeric(IP)) + 3.813 ,2)
    )
  team_pitching
  ###LOAD
  
}

team_fielding_etl <- function(year){
  sport_id <- if_else(year > 2020, 23, 11)
  team_fielding <- mlb_teams_stats(stat_type = 'season', stat_group = 'fielding', season = year, sport_ids = sport_id)
  teams <- mlb_teams(season = year, league_ids = 125) %>%
    select(team_id, team_full_name, team_code, team_abbreviation, franchise_name, club_name, venue_id, venue_name, league_id, 
           division_id, sport_id)
  team_fielding <- team_fielding %>%
    filter(team_id %in% teams$team_id) %>%
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
  team_fielding
  
}

lmb_stats_ETL <- function(x){
hitting_aux = list()
for(i in c(2019,2021:2024)){
  print("Player Hitting Stats")
  print(i)
  hitting_yr <- hitting_etl(i)
  hitting_aux[[i]] <- hitting_yr
}
hitting = do.call(rbind, hitting_aux)
is.na(hitting) <- sapply(hitting, is.infinite)
hitting_std <- gs4_create("htting_std", sheets = hitting[,c(1:23)])
hitting_adv <- gs4_create("hitting_adv", sheets = hitting[,-c(8:23,35)])
print("1/6")
pitching_aux = list()
for(i in c(2019,2021:2024)){
  print("Player Pitching Stats")
  print(i)
  pitching_yr <- pitching_etl(i)
  pitching_aux[[i]] <- pitching_yr
}
pitching = do.call(rbind, pitching_aux)
is.na(pitching) <- sapply(pitching, is.infinite)
pitching_std <- gs4_create("pitching_std", sheets = pitching[,c(1:23)])
pitching_adv <- gs4_create("pitching_adv", sheets = pitching[,-c(8:23)])
print("2/6")
fielding_aux = list()
for(i in c(2019,2021:2024)){
  print("Player Fielding Stats")
  print(i)
  fielding_yr <- fielding_etl(i)
  fielding_aux[[i]] <- fielding_yr
}
fielding = do.call(rbind, fielding_aux)
is.na(fielding) <- sapply(fielding, is.infinite)
fielding_std <- gs4_create("fielding_std", sheets = fielding[,c(1:18)])
fielding_adv <- gs4_create("fielding_adv", sheets = fielding[,-c(8:18)])
print("3/6")
team_hitting_aux = list()
for(i in c(2019,2021:2024)){
  print("Team Hitting Stats")
  print(i)
  team_hitting_yr <- team_hitting_etl(i)
  team_hitting_aux[[i]] <- team_hitting_yr
}
team_hitting = do.call(rbind, team_hitting_aux)
is.na(team_hitting) <- sapply(team_hitting, is.infinite)
team_hitting_std <- gs4_create("team_hitting_std", sheets = team_hitting[,c(1:22)])
team_hitting_adv <- gs4_create("team_hitting_adv", sheets = team_hitting[-c(6:22)])
print("4/6")
team_pitching_aux = list()
for(i in c(2019,2021:2024)){
  print("Team Pitching Stats")
  print(i)
  team_pitching_yr <- team_pitching_etl(i)
  team_pitching_aux[[i]] <- team_pitching_yr
}
team_pitching = do.call(rbind, team_pitching_aux)
is.na(team_pitching) <- sapply(team_pitching, is.infinite)
team_pitching_std <- gs4_create("team_pitching_std", sheets = team_pitching[,c(1:21)])
team_pitching_adv <- gs4_create("team_pitching_adv", sheets = team_pitching[-c(5:21)])
print("5/6")
team_fielding_aux = list()
for(i in c(2019,2021:2024)){
  print("Team Fielding Stats")
  print(i)
  team_fielding_yr <- team_fielding_etl(i)
  team_fielding_aux[[i]] <- team_fielding_yr
}
team_fielding = do.call(rbind, team_fielding_aux)
is.na(team_fielding) <- sapply(team_fielding, is.infinite)
team_fielding_std <- gs4_create("team_fielding_std", sheets = team_fielding[,c(1:10)])
team_fielding_adv <- gs4_create("team_fielding_adv", sheets = team_fielding[,-c(4:10)])
print("6/6")
print("COMPLETED")
}




# ---------------------- STATS BY TEAMS --------------------------------------



sheet <- read_sheet("https://docs.google.com/spreadsheets/d/1a4WWTuAB9d5Jr45egGW0yTO4Edl6ZerWAEF6WT0exo8/edit?gid=0#gid=0")

teams <- gs4_create("teams", sheets = teams)
read_sheet(teams)

  
hitting_war <- hitting %>%
      inner_join(pf_br, by = join_by(Team)) %>%
      mutate(BatRuns = hitting$wRAA + (woba_fipc$`R/PA` - ((hitting$PF/100) * woba_fipc$`R/PA`))*hitting$PA + 
               (woba_fipc$`R/PA`- (hitting$wRC/hitting$PA)) * hitting$PA)


teams_wl <- team_hitting %>%
              inner_join(team_pitching, by = join_by(Team)) %>%
              select(Year.x, Team, GP.y, W, L, R, ER) %>%
              rename(G = GP.y, RA = ER)

teams_wl <- teams_wl %>%
  mutate(RD = R - RA, Wpct = W / (W + L))

run_diff <- ggplot(teams_wl, aes(x = RD, y = Wpct)) + 
  geom_point() + 
  scale_x_continuous("Run differential") + 
  scale_y_continuous("Winning percentage")

linfit <- lm(Wpct ~ RD, data = teams_wl)
linfit

run_diff + 
  geom_smooth(method = "lm", se = FALSE)

teams_wl_aug <- augment(linfit, data = teams_wl)

base_plot <- ggplot(teams_wl_aug, aes(x = RD, y = .resid)) +
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept = 0, linetype = 3) + 
  xlab("Run differential") + ylab("Residual")

highlight_teams <- teams_wl_aug %>%
  arrange(desc(abs(.resid))) %>%
  slice_head(n = 6)

base_plot +
  geom_point(data = highlight_teams) + 
  geom_text_repel(
    data = highlight_teams, 
    aes(label = paste(Team, Year.x))
  )

comp <- team_hitting %>% filter(Team == "Tigres de Quintana Roo" | Team == "Diablos Rojos del Mexico") %>% 
  select(Year,Team,H,  `2B`,  `3B`,    HR,   RBI,     R,  AVG,   OBP,   SLG,   OPS, wOBA, wRAA, wRC, `wRC+` )





games_info <- list()
for (game_pk in lmb_24_games$game_pk){
  print(game_pk)
  game_info <- mlb_game_info(game_pk)
  games_info[[game_pk]] <- game_info
}
games_info <- Filter(Negate(is.null), games_info)
games_info_df = do.call(rbind, games_info)

games_info <- Filter(Negate(is.null), games_info)

lmb_24_games <- mlb_schedule(season = 2024, level_ids = 23)

lmb_24_games <- lmb_24_games %>%
  filter(series_description == 'Regular Season' & status_status_code == 'F') %>%
  select(date,game_pk,double_header,games_in_series,series_game_number,teams_away_team_name,teams_away_score,
         teams_away_league_record_wins,teams_away_league_record_losses,teams_home_team_name,teams_home_score,
         teams_home_league_record_wins,teams_home_league_record_losses,venue_name) %>%
  mutate(
       Away = paste0(teams_away_team_name," (",teams_away_league_record_wins,"-",teams_away_league_record_losses,")")
      ,Home = paste0(teams_home_team_name," (",teams_home_league_record_wins,"-",teams_home_league_record_losses,")")) %>%
  rename(
    "Score Away" = teams_away_score
    ,"Score Home" = teams_home_score
    ,"Venue" = venue_name
    ,"Date" = date
    ,"Series Game N" = series_game_number) %>%
  select(game_pk,"Date","Series Game N",Away,"Score Away",Home,"Score Home","Venue") 

lmb_24_games <- lmb_24_games %>%
  inner_join(games_info_df, by = c("game_pk" = "game_pk")) %>%
  select(!c(game_date:wind,game_id:gameday_sw)) %>%
  rename(
    "Attendance" = attendance
    ,"Start Time" = start_time
    ,"Game Time" = elapsed_time) %>%
  mutate(
    "Box Score" = paste0('<a href="https://www.milb.com/gameday/',game_pk,'/final/box">Box Score</a>')
    ,Attendance = as.numeric(gsub(",", "",Attendance))
  )



game_logs <- gs4_create("game_logs", sheets = lmb_24_games[-1])

sheet_write(lmb_24_games[-1], ss = game_logs, sheet = "Sheet1")
sheet = "1toeJeYcCvlauqXNPlG3WLv13uPH6ZQVBg2qV93raC-k"
sheet_write(hitting_adv, ss = sheet, sheet = "Sheet1")




hitting_p <- left_join(hitting_std,hitting_adv, by =c("Name"="Name","Year"="Year"), relationship = "many-to-many")


hitting_p <- hitting_p %>%
  select(Year, Name, mWAR, GP.x, PA.x, H, RBI, SB, HR, AVG, OBP, SLG, OPS, wOBA) %>%
  filter(Year == 2024) %>%
  rename("G" = GP.x, "PA" = PA.x)

hitting_cp <- gs4_create("hitting_cp", sheets = hitting_cp)



hitting_tst <- (mlb_stats(stat_type = 'season', player_pool = 'all', stat_group = 'hitting', season = 2024, sport_ids = 23))