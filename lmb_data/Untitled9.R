library(baseballr)
library(dplyr)


teams <- read.csv("~/teams.csv")
lmb_schedule <- mlb_schedule(season = 2024, level_ids = 23)
lmb_dates <- unique(lmb_schedule$date)

si




test <- mlb_standings(season = 2024, date = i, standings_type = 'byDivision', league_id = 125)

test %>% 
  left_join(teams, by = team_id) %>% 
  mutate(team_records_team_id = ifelse(!is.na(team_abbreviation)))