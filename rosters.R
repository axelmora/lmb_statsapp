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
