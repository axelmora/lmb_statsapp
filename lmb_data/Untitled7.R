lmb_pks_24 <- mlb_schedule(season = 2024, level_ids = 23) %>%
    filter(status_coded_game_state == 'F')


pbp_769739 <- mlb_pbp(game_pk = 769739)

results_763739 <- pbp_769739 %>%
                    select(fielding_team, type, atBatIndex, pitchNumber, result.event) %>%
                    filter(fielding_team == 'Toros de Tijuana',type == 'pitch') %>%
                    group_by(as.numeric(atBatIndex), result.event) %>%
                    summarise(pitchCount = max(pitchNumber), .groups = "keep")

results_763739_pct <- results_763739[,2] %>%
                    summarise( percent = 100 * n()/nrow(results_763739))

pbp_769874 <- mlb_pbp(game_pk = 769874)


063426
lmb_gm_status_19 <- mlb_schedule(season = 2019, level_ids = 11) %>%
  filter(double_header == 'Y', teams_home_team_id %in% teams$team_id) %>%
  select(calendar_event_id, game_number, status_detailed_state)
