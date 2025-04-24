lg_totals <- function(year){
  sport_id <- if_else(year > 2020, 23, 11)
  team_hitting <- mlb_teams_stats(stat_type = 'season', stat_group = 'hitting', season = year, sport_ids = sport_id)
  team_pitching <- mlb_teams_stats(stat_type = 'season', stat_group = 'pitching', season = year, sport_ids = sport_id)
  teams <- mlb_teams(season = year, league_ids = 125) %>%
    select(team_id, team_full_name, team_code, team_abbreviation, franchise_name, club_name, venue_id, venue_name, league_id, 
           division_id, sport_id)
  team_pitching <- team_pitching %>%
    select(season,team_name,innings_pitched,strike_outs,base_on_balls,intentional_walks,hit_by_pitch,
           home_runs,earned_runs,outs
    ) %>%
    rename("Year"=season,"Team"=team_name,"IP"=innings_pitched,
           "K"=strike_outs,"BB"=base_on_balls,"IBB"=intentional_walks,"HBP"=hit_by_pitch,
           "HR"=home_runs,"ER"=earned_runs,"O"=outs
    )
  team_hitting  <- team_hitting  %>%
    select(season,team_name,games_played,plate_appearances,runs,stolen_bases,caught_stealing,
           hits,doubles,triples,home_runs
    ) %>%
    rename("Year"=season,"Team"=team_name,"GP"=games_played,"PA"=plate_appearances,"R"=runs,"SB"=stolen_bases,"CS"=caught_stealing) %>%
    mutate("1B" = hits - doubles - triples - home_runs) %>%
    left_join(team_pitching, join_by(Year,Team)) %>%
    filter(Team %in% teams$team_full_name) %>%
    group_by(Year) %>%
    summarise("Teams" = nrow(.), "GP" = sum(GP), "PA" = sum(PA), "R" = sum(R), "IP" = sum(O), "ER" = sum(ER), "K" = sum(K),
              "1B" = sum(`1B`), "BB" = sum(BB), "HBP" = sum(HBP), "IBB" = sum(IBB), "HR" = sum(HR), "SB" = sum(SB), "CS" = sum(CS)) %>%
    mutate(IP = floor(IP / 3) + (IP %% 3) / 10)
  team_hitting
}

totals_aux = list()
for (i in c(2005:2017,"2018.1","2018.2",2019,2021:2024)) {
  print(i)
  Season <- i
  totals_yr <- lg_totals(Season)
  print(totals_yr)
  totals_aux[[i]] <- totals_yr
}
totals = do.call(rbind, totals_aux)