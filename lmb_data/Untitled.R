for (x in 1:length(lmb_standings_24$team_records_team_name)) {
  df <- (lmb_standings_24[[44]][[x]] %>% filter(type == "oneRun"))
}


