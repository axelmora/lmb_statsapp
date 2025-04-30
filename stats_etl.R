woba_fipc <- read_csv("lmb_stats/woba_fipc.csv")
park_factors <- read_csv("lmb_stats/park_factors.csv")

#pf_br <- park_factors %>%
#  select(team_abbreviation, PF) %>%
#  rename(Team = team_abbreviation)

hitting_etl <- function(year){
####EXTRACTION
sport_id <- if_else(year > 2020, 23, 11)
hitting <- (mlb_stats(stat_type = 'season', player_pool = 'all', stat_group = 'hitting', season = year, sport_ids = sport_id))
teams <- mlb_teams(season = year, league_ids = 125) %>%
  select(team_id, team_full_name, team_code, team_abbreviation, franchise_name, club_name, venue_id, venue_name, league_id, 
         division_id, sport_id)
year <- (year-1)
woba_fipc <- woba_fipc %>% filter(Season == year)
pf_br <- park_factors %>% select(Team,ends_with(as.character(year))) %>% filter(!is.na(.[[2]]))
p_adj <- pos_adj %>% select(POS,ends_with(as.character(year)))
rpw <- RpW_Master %>% filter(Year == year)
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
  mutate(`wRC+` = round((((wRAA/PA + woba_fipc$`R/PA`) + (woba_fipc$`R/PA` - ((.[[39]]/100) * woba_fipc$`R/PA`)))/(wRC/PA)) * 100)
        ) %>%
  inner_join(p_adj, by = c("POS" = "POS")) %>%
  mutate(mWAR = round(((wRAA+as.numeric(.[[41]]))/rpw$rpw),1)) %>%
  select(!c(35,39,41))

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
  year <- (year-1)
  woba_fipc <- woba_fipc %>% filter(Season == year)
  pad <- pAdj_Master %>% filter(Year == year)
  pf_br <- park_factors %>% select(Team,ends_with(as.character(year))) %>% filter(!is.na(.[[2]]))
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
           ,FIP = round((((13*HR) + (3*(BB+HBP)) - (2*K)) / as.numeric(IP)) + woba_fipc$cFIP ,2)
    ) %>%
    select(!c(team_id:sport_id)) %>%
    mutate(FIPR9 = FIP+pad$pAdj) %>%      
    inner_join(pf_br, by = join_by(Team)) %>%                      
    mutate(pFIPR9 = FIPR9/(.[[44]]/100))
  
  lgFIP <- 8.51
  lgFIPR9 <- lgFIP+pad$pAdj
  
  pitching <- pitching %>%
    mutate(RAAP9 = lgFIPR9 - pFIPR9
           ,dRPW = (((
             ((18-((O/3)/GP))*(lgFIPR9)) + (((O/3)/GP)*pFIPR9))/18)+2)*1.5
           ,WPGAA = RAAP9/dRPW
           ,rpL = 0.03*(1-(GS/GP))+0.12*(GS/GP)
           ,WPGAR = WPGAA+rpL
           ,mWAR = round(WPGAR*((O/3)/9),1) 
          ) %>%
    select(!c(43:50))
    
  
  
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
  year <- (year-1)
  woba_fipc <- woba_fipc %>% filter(Season == year)
  pf_br <- park_factors %>% select('Team Full Name',ends_with(as.character(year))) %>% filter(!is.na(.[[2]]))
  p_adj <- pos_adj %>% select(POS,ends_with(as.character(year)))
  rpw <- RpW_Master %>% filter(Year == year)
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
    inner_join(pf_br, by = c("Team" = "Team Full Name")) %>%
    mutate(`wRC+` = round((((wRAA/PA + woba_fipc$`R/PA`) + (woba_fipc$`R/PA` - ((.[[39]]/100) * woba_fipc$`R/PA`)))/(wRC/PA)) * 100)
    ) %>%
    mutate(mWAR = 0) %>%
             #round(((wRAA+.[[41]])/rpw$rpw),1)) %>%
    select(!c(35,39))
  team_hitting
  
}

team_pitching_etl <- function(year){
  sport_id <- if_else(year > 2020, 23, 11)
  team_pitching <- mlb_teams_stats(stat_type = 'season', stat_group = 'pitching', season = year, sport_ids = sport_id)
  teams <- mlb_teams(season = year, league_ids = 125) %>%
    select(team_id, team_full_name, team_code, team_abbreviation, franchise_name, club_name, venue_id, venue_name, league_id, 
           division_id, sport_id)
  year <- (year-1)
  woba_fipc <- woba_fipc %>% filter(Season == year)
  pad <- pAdj_Master %>% filter(Year == year)
  pf_br <- park_factors %>% select('Team Full Name',ends_with(as.character(year))) %>% filter(!is.na(.[[2]]))
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
      ,FIP = round((((13*HR) + (3*(BB+HBP)) - (2*K)) / as.numeric(IP)) + woba_fipc$cFIP ,2)
    ) %>%
    mutate(FIPR9 = FIP+pad$pAdj) %>%      
    inner_join(pf_br, by = c("Team" = "Team Full Name")) %>%                      
    mutate(pFIPR9 = FIPR9/(.[[42]]/100)) %>%
    select(!c(42))
  
  lgFIP <- 8.51
  lgFIPR9 <- lgFIP+pad$pAdj
  
  team_pitching <- team_pitching %>%
    mutate(RAAP9 = 0 #lgFIPR9 - pFIPR9
          ,dRPW = 0 #(((((18-((O/3)/GP))*(lgFIPR9)) + (((O/3)/GP)*pFIPR9))/18)+2)*1.5
          ,WPGAA = 0 #RAAP9/dRPW
          ,rpL = 0 #0.03*(1-(GS/GP))+0.12*(GS/GP)
          ,WPGAR = 0 #WPGAA+rpL
          ,mWAR = 0 #round(WPGAR*((O/3)/9),1) 
    )%>%
    select(!c(41:47))
  
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

lmb_stats_ETL_HIST  <- function(x){
hitting_aux = list()
for(i in c(2019,2021:2024)){
  print("Player Hitting Stats")
  print(i)
  hitting_yr <- hitting_etl(i)
  hitting_aux[[i]] <- hitting_yr
}
hitting_data = do.call(rbind, hitting_aux)
is.na(hitting_data) <- sapply(hitting_data, is.infinite)
write_sheet(hitting_data, ss = "1eVJ4dSg6KgATE8zmPxvriHyk6ZRZB94fuDWpdcloC1M", sheet = "hitting_data")
#hitting <- gs4_create("htting", sheets = hitting_data) #1eVJ4dSg6KgATE8zmPxvriHyk6ZRZB94fuDWpdcloC1M
#hitting_adv <- gs4_create("hitting_adv", sheets = hitting[,-c(8:23,35)])
print("1/6")
pitching_aux = list()
for(i in c(2019,2021:2024)){
  print("Player Pitching Stats")
  print(i)
  pitching_yr <- pitching_etl(i)
  pitching_aux[[i]] <- pitching_yr
}
pitching_data = do.call(rbind, pitching_aux)
is.na(pitching_data) <- sapply(pitching_data, is.infinite)
write_sheet(pitching_data, ss="1eVJ4dSg6KgATE8zmPxvriHyk6ZRZB94fuDWpdcloC1M", sheet = "pitching_data")
#pitching <- gs4_create("pitching", sheets = pitching_data) #1xlYBP_x1mfsuFDnMxk4gtPUIl7FTIJ0hfVRK3BPeeRQ
#pitching_adv <- gs4_create("pitching_adv", sheets = pitching[,-c(8:23)])
print("2/6")
fielding_aux = list()
for(i in c(2019,2021:2024)){
  print("Player Fielding Stats")
  print(i)
  fielding_yr <- fielding_etl(i)
  fielding_aux[[i]] <- fielding_yr
}
fielding_data = do.call(rbind, fielding_aux)
is.na(fielding_data) <- sapply(fielding_data, is.infinite)
fielding <- gs4_create("fielding", sheets = fielding_data) #1hVRO16lDvVHkYGva06sgG1715wYdEKHaIy2GwyC_K8U
#fielding_adv <- gs4_create("fielding_adv", sheets = fielding[,-c(8:18)])
print("3/6")
team_hitting_aux = list()
for(i in c(2019,2021:2024)){
  print("Team Hitting Stats")
  print(i)
  team_hitting_yr <- team_hitting_etl(i)
  team_hitting_aux[[i]] <- team_hitting_yr
}
team_hitting_data = do.call(rbind, team_hitting_aux)
is.na(team_hitting_data) <- sapply(team_hitting_data, is.infinite)
#team_hitting <- gs4_create("team_hitting", sheets = team_hitting_data)
write_sheet(team_hitting_data, ss = "1gkOC_566Bp7XCuuyEsIkK7jT0V0TMn--Xbuv1i4Wdxc", sheet = "team_hitting_data")
#team_hitting_adv <- gs4_create("team_hitting_adv", sheets = team_hitting[-c(6:22)])
print("4/6")
team_pitching_aux = list()
for(i in c(2019,2021:2024)){
  print("Team Pitching Stats")
  print(i)
  team_pitching_yr <- team_pitching_etl(i)
  team_pitching_aux[[i]] <- team_pitching_yr
}
team_pitching_data = do.call(rbind, team_pitching_aux)
is.na(team_pitching_data) <- sapply(team_pitching_data, is.infinite)
#team_pitching <- gs4_create("team_pitching", sheets = team_pitching_data)
write_sheet(team_pitching, ss = "1uOckNtrCu811khLZ1CxSU7C2TLapZubH4g-Yh2XJTh4", sheet = "team_pitching_data")
#team_pitching_adv <- gs4_create("team_pitching_adv", sheets = team_pitching[-c(5:21)])
print("5/6")
team_fielding_aux = list()
for(i in c(2019,2021:2024)){
  print("Team Fielding Stats")
  print(i)
  team_fielding_yr <- team_fielding_etl(i)
  team_fielding_aux[[i]] <- team_fielding_yr
}
team_fielding_data = do.call(rbind, team_fielding_aux)
is.na(team_fielding_data) <- sapply(team_fielding_data, is.infinite)
team_fielding <- gs4_create("team_fielding", sheets = team_fielding_data)
#team_fielding_adv <- gs4_create("team_fielding_adv", sheets = team_fielding)
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

#######GAME LOGS##########
lmb_game_logs <- function(year){

  lmb_25_games <- mlb_schedule(season = year, level_ids = 23)
  lmb_25_games <- lmb_25_games %>%
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

    games_info <- list()
    for (game_pk in lmb_25_games$game_pk){
      #print(game_pk)
      game_info <- mlb_game_info(game_pk)
      games_info[[game_pk]] <- game_info
    }
    games_info <- Filter(Negate(is.null), games_info)
    games_info_df = do.call(rbind, games_info)
    games_info <- Filter(Negate(is.null), games_info)

    lmb_25_games <- lmb_25_games %>%
      inner_join(games_info_df, by = c("game_pk" = "game_pk")) %>%
      select(!c(game_date:wind,game_id:gameday_sw)) %>%
      rename(
        "Attendance" = attendance
        ,"Start Time" = start_time
        ,"Game Time" = elapsed_time) %>%
      mutate(
        "Box Score" = paste0('<a href="https://www.milb.com/gameday/',game_pk,'/final/box">Box Score</a>')
        ,Attendance = as.numeric(gsub(",", "",Attendance)))
lmb_25_games
}
#game_logs <- gs4_create("game_logs", sheets = lmb_24_games[-1])
game_logs = "1NHm3ZAMeTpBag95kOpozo_8Ngkxh9VSXot4bbEQpkR8"
sheet_write(lmb_25_games[-1], "1NHm3ZAMeTpBag95kOpozo_8Ngkxh9VSXot4bbEQpkR8", sheet = "Sheet1")

########
team_rosters <- function(teamId){
  rosters <- mlb_rosters(team_id = teamId, season = 2025, roster_type = 'active')
  player_info <- mlb_people(rosters$person_id)
  teams <- mlb_teams(season = 2025, league_ids = 125) %>%
    select(team_id, team_full_name, team_code, team_abbreviation, franchise_name, club_name, venue_id, venue_name, league_id, 
           division_id, sport_id)
  teams <- teams %>%
    filter(team_id == teamId)
  
  rosters <- rosters %>%
    inner_join(player_info, by = c("person_id" = "id")) %>%
    select(team_id, jersey_number, person_full_name, position_abbreviation,birth_date,birth_country,bat_side_code,pitch_hand_code) %>%
    inner_join(teams, by = c("team_id" = "team_id")) %>% 
    rename("Number" = jersey_number
           ,"Name" = person_full_name
           ,"Position" = position_abbreviation
           ,"Team" = team_full_name
           ,"Birth date" = birth_date
           ,"Birth Country" = birth_country
           ,"Bats" = bat_side_code
           ,"Throws" = pitch_hand_code) %>%
    select(!c(1,10:18)) %>%
    mutate(
      position_group = case_when(
        Position %in% c("C") ~ "Catchers",
        Position %in% c("1B", "2B", "3B", "SS", "IF") ~ "Infield",
        Position %in% c("LF", "CF", "RF", "OF") ~ "Outfield",
        Position %in% c("P") ~ "Pitchers",
        TRUE ~ "Other")) 
  
  rosters
}

lmb_roster <- function(){
  team_roster_aux = list()
  for(i in teams$team_id){
    print("roster")
    print(i)
    team_roster <- team_rosters(i)
    team_roster_aux[[i]] <- team_roster
  }
  team_rosters_data = do.call(rbind, team_roster_aux)
  is.na(team_rosters_data) <- sapply(team_rosters_data, is.infinite)
  team_rosters_data
}

#rosters <- gs4_create("rosters", sheets = team_rosters_data)

write_sheet(team_rosters_data, rosters, sheet = "team_rosters_data")

#############

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

#league_pace_venue[[i]] <- cbind(name,pace_venue_select)}


#############
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

###############
lmb_pace <- function(year){
  lmb_pace_25 <- mlb_game_pace(season = year, sport_ids = 23)
  lmb_pace_25 <- lmb_pace_25 %>%
    select(season, hits_per9inn, runs_per9inn, pitches_per_pitcher, time_per_pitch, time_per_plate_appearance,
           pr_portal_calculated_fields_time_per9inn_game
    ) %>%
    rename("Year"=season,"Hits/9in"=hits_per9inn,"Runs/9in"=runs_per9inn,"Pitches/Pitcher"=pitches_per_pitcher,
           "Time/Pitch"=time_per_pitch,"Time/PA"=time_per_plate_appearance,"Time/9inGame"=pr_portal_calculated_fields_time_per9inn_game)
  lmb_pace_25
}

#########
lmb_trans <- function(startDate,endDate){
  # Define the URL
  url <- paste0("https://statsapi.mlb.com/api/v1/transactions?startDate=",startDate,"&endDate=",endDate)
  
  # Fetch and parse the JSON response
  response <- fromJSON(url)
  
  lmb_trans <- response$transactions
  
  lmb_trans <- lmb_trans %>%
    unnest_wider(person, names_sep = "_") %>%
    unnest_wider(toTeam, names_sep = "_") %>%
    unnest_wider(fromTeam, names_sep = "_") %>%
    filter(toTeam_id %in% teams$team_id | fromTeam_id %in% teams$team_id) %>%
    select(effectiveDate, typeDesc, toTeam_name, person_fullName, description) %>%
    mutate(effectiveDate = as.Date(effectiveDate)) %>%
    rename("Date" = effectiveDate,
           "Type" = typeDesc,
           "Team" = toTeam_name,
           "Player Name" = person_fullName,
           "Description" = description) %>%
    arrange(desc(Date))
  lmb_trans
}
############

sheet = "1toeJeYcCvlauqXNPlG3WLv13uPH6ZQVBg2qV93raC-k"
sheet_write(hitting_adv, ss = sheet, sheet = "Sheet1")




hitting_p <- left_join(hitting_std,hitting_adv, by =c("Name"="Name","Year"="Year"), relationship = "many-to-many")


hitting_p <- hitting_p %>%
  select(Year, Name, mWAR, GP.x, PA.x, H, RBI, SB, HR, AVG, OBP, SLG, OPS, wOBA) %>%
  filter(Year == 2024) %>%
  rename("G" = GP.x, "PA" = PA.x)

hitting_cp <- gs4_create("hitting_cp", sheets = hitting_cp)

pitching_cp <- pitching_data %>%
  select(Year, Name, mWAR, GP, IP, W, L, SV, HLD, , K, 'K%', BB, 'BB%') %>%
  filter(Year == 2024) %>%
  mutate("W-L" = paste0(W,"-",L)) %>%
  select(Year, Name, mWAR, GP, IP, 'W-L', SV, HLD, , K, 'K%', BB, 'BB%')

pitching_cp <- gs4_create("pitching_cp", sheets = pitching_cp)




hitting_tst <- (mlb_stats(stat_type = 'season', player_pool = 'all', stat_group = 'hitting', season = 2024, sport_ids = 23))