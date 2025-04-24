games_19 <- games_19 %>%
  filter(teams_away_team_id %in% teams19$team_id & teams_home_team_id %in% teams19$team_id)
teams19 <- baseballmexR::teams(season = 2019, league = "lmb")
games_19 <- mlb_schedule(season = 2019, level_ids = 11)

dw19$play <- dw19$play %>%
  filter(game_id %in% games_19$game_pk)

plays_sb_cs <- dw19$play

plays_sb_cs <- plays_sb_cs %>%
  mutate_if(is.character, ~replace_na(., ""))
plays_sb_cs <- plays_sb_cs %>%
  mutate_if(is.integer, ~replace_na(., 0))

plays_sb_cs <- plays_sb_cs %>%
  filter(is_stolen_base == TRUE | is_caught_stealing == TRUE)

plays_sb_cs <- as.data.frame(plays_sb_cs)

plays_sb_cs %>% 
  
  mutate(BASES = 
           paste0(ifelse(pre_runner_1b_id == 0, 0, 1),
                  ifelse(pre_runner_2b_id == 0, 0, 1),
                  ifelse(pre_runner_3b_id == 0, 0, 1)
           ),
         STATE = paste(BASES, pre_outs)) -> plays_sb_cs

plays_sb_cs %>%
  mutate(NEW.BASES = 
           paste0(ifelse(post_runner_1b_id == 0, 0, 1),
                  ifelse(post_runner_2b_id == 0, 0, 1),
                  ifelse(post_runner_3b_id == 0, 0, 1)
           ),
         NEW.STATE = paste(NEW.BASES, post_outs)) -> plays_sb_cs

plays_sb_cs <- plays_sb_cs %>% 
  select(game_id, event_index, is_stolen_base, is_caught_stealing,  STATE, NEW.STATE, runs_on_play)


plays_sb_cs <- plays_sb_cs %>% 
  left_join(RUNS_1, by = "STATE") %>%
  rename("RE Before" = "Mean") %>%
  select(!c("Outs"))

  
plays_sb_cs <- plays_sb_cs %>% 
    left_join(RUNS_1, by = c("NEW.STATE" = "STATE")) %>%
    rename("RE After" = "Mean") %>%
  select(!c("Outs"))

plays_sb_cs <- plays_sb_cs %>%
  mutate_if(is.numeric, ~replace_na(., 0))

plays_sb_cs <- plays_sb_cs %>%
  mutate(run_vale = (`RE After` + runs_on_play) - `RE Before`)

runSB <- plays_sb_cs %>%
  filter(is_stolen_base == TRUE) %>% summarise(mean(run_vale))

runCS <- plays_sb_cs %>%
  filter(is_caught_stealing == TRUE) %>% summarise(mean(run_vale))
  
lgwSB=((1135*runSB)+(617*runCS))/(13696+5567+997)

runSB19 <- runSB
runCS19 <- runCS
lgwSB19 <- lgwSB
Year <- c(2024,2023,2022,2021,2019)
runSB <- c(runSB24$`mean(run_vale)`, runSB23$`mean(run_vale)`, runSB22$`mean(run_vale)`, runSB21$`mean(run_vale)`, runSB19$`mean(run_vale)`)
runCS <- c(runCS24$`mean(run_vale)`, runCS23$`mean(run_vale)`, runCS22$`mean(run_vale)`, runCS21$`mean(run_vale)`, runCS19$`mean(run_vale)`)
lgwSB <- c(lgwSB24$`mean(run_vale)`, lgwSB23$`mean(run_vale)`, lgwSB22$`mean(run_vale)`, lgwSB21$`mean(run_vale)`, lgwSB19$`mean(run_vale)`)

pAdj_Master
RpW_Master

RlR <- (570 * (905/905)) * (RpW_Master$rpw[which(RpW_Master$Year == 2024)]/70365)

lgFIP = c(round((((13*HR) + (3*(BB+HBP)) - (2*K)) / as.numeric(IP)) + woba_fipc$cFIP ,2)
          ,round((((13*HR) + (3*(BB+HBP)) - (2*K)) / as.numeric(IP)) + woba_fipc$cFIP ,2)
          ,round((((13*HR) + (3*(BB+HBP)) - (2*K)) / as.numeric(IP)) + woba_fipc$cFIP ,2)
          ,round((((13*HR) + (3*(BB+HBP)) - (2*K)) / as.numeric(IP)) + woba_fipc$cFIP ,2)
          ,round((((13*HR) + (3*(BB+HBP)) - (2*K)) / as.numeric(IP)) + woba_fipc$cFIP ,2))


guts <- cbind(woba_fipc, runSB, runCS, lgwSB, pAdj_Master[,2:4])