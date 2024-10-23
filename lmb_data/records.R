library(baseballr)
library(dplyr)


teams <- read.csv("~/teams.csv")
lmb_schedule <- mlb_schedule(season = 2024, level_ids = 23)
lmb_dates <- unique(lmb_schedule$date)

ranks_team = list()

for(i in lmb_dates[1:107]){
  print(i)
  rank_aux <- mlb_standings(season = 2024, date = i, standings_type = 'byDivision', league_id = 125)
  
  rank_hist <- rank_aux %>%
    select(team_records_team_name, team_records_division_rank, team_records_league_record_pct) %>%
    filter(team_records_team_name == 'Piratas de Campeche')

  ranks_team[[i]] <- cbind(i,rank_hist)
  print(ranks_team[[i]])
  
  ranks_team_cam = do.call(rbind, ranks_team)
  
  names(ranks_team_cam)[names(ranks_team_cam) == 'team_records_league_record_pct'] <- 'cam Pct'
  names(ranks_team_cam)[names(ranks_team_cam) == 'team_records_division_rank'] <- 'cam Rank'
}


names(ranks_team_tab)[names(ranks_team_tab) == 'team_records_league_record_pct'] <- 'TAB Pct'
names(ranks_team_tab)[names(ranks_team_tab) == 'team_records_division_rank'] <- 'TAB Rank'


assign(paste0("game_bs_",i),fromJSON(games_bs_urls[[j]]))

p <- ggplot(ranks_team_tij, aes(x=as.Date(i), y=as.double(team_records_league_record_pct))) +
  geom_line( color="red") + 
  geom_point() +
  xlab("") +
  #theme_ipsum() +
  #theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(limit=c(as.Date("2024-04-11"),as.Date("2024-08-01"))) 

p

ranks 

write.csv(ranks_team_lar,"/Users/axel.mora/ranks_team_lar.csv")
write.csv(ranks_team_lag,"/Users/axel.mora/ranks_team_lag.csv")
write.csv(ranks_team_mty,"/Users/axel.mora/ranks_team_mty.csv")

lar.ts <- xts(ranks_team_lar[,c(1,3)], order.by = as.Date(ranks_team_lar$i, "%Y-%m-%d"))
leo.ts <- xts(ranks_team_leo[,c(1,3)], order.by = as.Date(ranks_team_leo$i, "%Y-%m-%d"))
mty.ts <- xts(ranks_team_mty[,c(1,3)], order.by = as.Date(ranks_team_mty$i, "%Y-%m-%d"))
tab.ts <- xts(ranks_team_tab[,c(1,3)], order.by = as.Date(ranks_team_tab$i, "%Y-%m-%d"))
pue.ts <- xts(ranks_team_pue[,c(1,3)], order.by = as.Date(ranks_team_pue$i, "%Y-%m-%d"))
oax.ts <- xts(ranks_team_oax[,c(1,3)], order.by = as.Date(ranks_team_oax$i, "%Y-%m-%d"))
ver.ts <- xts(ranks_team_ver[,c(1,3)], order.by = as.Date(ranks_team_ver$i, "%Y-%m-%d"))
yuc.ts <- xts(ranks_team_yuc[,c(1,3)], order.by = as.Date(ranks_team_yuc$i, "%Y-%m-%d"))
mex.ts <- xts(ranks_team_mex[,c(1,3)], order.by = as.Date(ranks_team_mex$i, "%Y-%m-%d"))
qro.ts <- xts(ranks_team_qro[,c(1,3)], order.by = as.Date(ranks_team_qro$i, "%Y-%m-%d"))
cam.ts <- xts(ranks_team_cam[,c(1,3)], order.by = as.Date(ranks_team_cam$i, "%Y-%m-%d"))
tig.ts <- xts(ranks_team_tig[,c(1,3)], order.by = as.Date(ranks_team_tig$i, "%Y-%m-%d"))
cam.ts <- xts(ranks_team_cam[,c(1,3)], order.by = as.Date(ranks_team_cam$i, "%Y-%m-%d"))

nte_leader <- cbind(lar.ts, lag.ts, mty.ts, tab.ts)
sur_leader <- cbind(oax.ts, pue.ts, tig.ts, qro.ts, mex.ts, ver.ts, cam.ts, tab.ts, yuc.ts, leo.ts)

dygraph(sur_leader, main = "2024 LMB Sur") %>%
  dyAxis("y", label = "Rank", valueRange = c(10, 1)) %>%
  dyOptions(digitsAfterDecimal=3) %>%
  dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2,hideOnMouseOut = FALSE) %>%
  dyRangeSelector()

