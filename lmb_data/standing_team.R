library(baseballr)
library(dplyr)


teams <- read.csv("~/teams.csv")
lmb_schedule <- mlb_schedule(season = 2024, level_ids = 23)
lmb_dates <- unique(lmb_schedule$date)

ranks_team = list()

for(i in lmb_dates){
  print(i)
  rank_aux <- mlb_standings(season = 2024, date = i, standings_type = 'byDivision', league_id = 125)
  
  rank_hist <- rank_aux %>%
    select(team_records_team_name, team_records_division_rank, team_records_league_record_pct) %>%
    filter(team_records_team_name == 'Leones de Yucatan')
  
  ranks_team[[i]] <- cbind(i,rank_hist)
  print(ranks_team[[i]])
  
  ranks_team_yuc = do.call(rbind, ranks_team)
}