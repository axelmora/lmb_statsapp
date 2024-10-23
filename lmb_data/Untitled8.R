teams <- read.csv("~/teams.csv")
lmb_pks_mty_24 <- mlb_schedule(season = 2024, level_ids = 23) %>%
  filter(status_coded_game_state == 'F'& (teams_home_team_name == 'Sultanes de Monterrey' | teams_away_team_name == 'Sultanes de Monterrey'))

result_events = list()
games_info = list()

for(i in lmb_pks_mty_24$game_pk){
  pbp_aux <- mlb_pbp(game_pk = i)

  results_pbp <- pbp_aux %>%
    select(batting_team, type, atBatIndex, pitchNumber, result.event) %>%
    filter(batting_team == 'Sultanes de Monterrey',type == 'pitch') %>%
    group_by(as.numeric(atBatIndex), result.event) %>%
    summarise(pitchCount = max(pitchNumber), .groups = "keep")
  result_events[[i]] <- results_pbp
  print(result_events[[i]])
}

result_events_mty_24 = do.call(rbind, result_events)
result_events_mty_24 <- result_events_mty_24[,2:3]
result_events_mty_pct_24 <- result_events_mty_24[,1] %>%
  #count(result.event)
  summarise(percent = 100 * n()/nrow(result_events_mty_24))
result_events_mty_pct_24 <- cbind('Sultanes de Monterrey',result_events_mty_pct_24)
write.csv(result_events_mty_pct_24,"/Users/axel.mora/result_events_mty_pct_24.csv")



saraperos_home <- mlb_schedule(season = 2024, level_ids = 23) %>%
  filter(status_coded_game_state == 'F' & (teams_home_team_name == 'Saraperos de Saltillo'))


pbp_769739 <- mlb_pbp(game_pk = 769739)

results_763739 <- pbp_769739 %>%
  select(fielding_team, type, atBatIndex, pitchNumber, result.event) %>%
  filter(fielding_team == 'Toros de Tijuana',type == 'pitch') %>%
  group_by(as.numeric(atBatIndex), result.event) %>%
  summarise(pitchCount = max(pitchNumber), .groups = "keep")

results_763739_pct <- results_763739[,2] %>%
  summarise( percent = 100 * n()/nrow(results_763739))

pbp_769874 <- mlb_pbp(game_pk = 769874)