for(i in c("2018.1","2018.2")){
  print(i)
  start_date <- lmb_seasons$regular_season_start_date[which(lmb_seasons$season_id == i)]
  end_date <- lmb_seasons$regular_season_end_date[which(lmb_seasons$season_id == i)]
  print(start_date)
  print(end_date)
  
  assign(paste0('dw',i),download_statsapi(start_date, end_date, level = "aaa"))
}


REM <- function(gamedata){
  dw <- gamedata
  re24_plays <- data.frame()
  RUNS_1 <- data.frame()
  re24_plays <- dw$event
  
  re24_plays <-
    re24_plays %>% 
    group_by(game_id) %>% 
    mutate(RUNS = lag(cumsum(runs_on_event)))
  
  re24_plays <-
    re24_plays %>% 
    mutate(
      HALF.INNING = paste(game_id,inning,half_inning),
      OUTS.ON.PLAY = as.integer(post_outs) - as.integer(pre_outs),
      RUNS.SCORED = runs_on_event)
  
  re24_plays %>%
    group_by(HALF.INNING) %>%
    summarize(Outs.Inning = sum(OUTS.ON.PLAY), 
              Runs.Inning = sum(RUNS.SCORED),
              Runs.Start = first(RUNS),
              MAX.RUNS = Runs.Inning + Runs.Start) -> half_innings
  
  re24_plays %>%
    inner_join(half_innings, by = "HALF.INNING") %>%
    mutate(RUNS.ROI = abs(MAX.RUNS - RUNS)) -> re24_plays
  
  re24_plays[,c(15:17,19:21)] <- sapply((re24_plays[,c(15:17,19:21)]), as.character)
  #re24_plays[is.na(re24_plays[,c(15:17,19:21)])] <- ""
  
  re24_plays <- re24_plays %>%
    mutate_if(is.character, ~replace_na(., ""))
  re24_plays <- re24_plays %>%
    mutate_if(is.integer, ~replace_na(., 0))
  
  re24_plays <- as.data.frame(re24_plays)
  
  re24_plays %>% 
    
    mutate(BASES = 
             paste0(ifelse(pre_runner_1b_id == "", 0, 1),
                    ifelse(pre_runner_2b_id == "", 0, 1),
                    ifelse(pre_runner_3b_id == "", 0, 1)
             ),
           STATE = paste(BASES, pre_outs)) -> re24_plays
  
  re24_plays %>%
    mutate(NEW.BASES = 
             paste0(ifelse(post_runner_1b_id == "", 0, 1),
                    ifelse(post_runner_2b_id == "", 0, 1),
                    ifelse(post_runner_3b_id == "", 0, 1)
             ),
           NOUTS = as.integer(pre_outs) + as.integer(OUTS.ON.PLAY),
           NEW.STATE = paste(NEW.BASES, NOUTS)) -> re24_plays
  
  
  re24_plays %>% 
    filter((STATE != NEW.STATE) | (as.integer(RUNS.SCORED) > 0)) -> re24_plays
  
  
  re24_plays %>%
    filter(as.integer(Outs.Inning) == 3) -> re24_plays_final
  
  re24_plays_final %>% 
    group_by(STATE) %>%
    summarize(Mean = mean(as.integer(RUNS.ROI))) %>%
    mutate(Outs = substr(STATE, 5, 5)) %>%
    arrange(Outs) -> RUNS_1 
  
  re24n <- matrix(round(RUNS_1$Mean, 2), 8, 3)
  
  dimnames(re24n)[[2]] <- c("0 outs", "1 out", "2 outs")
  dimnames(re24n)[[1]] <- c("___", "__3", "_2_", "_23", 
                            "1__", "1_3", "12_", "123")
  
  re_24_1 <- t(re24n)
  col.order <- c("___", "1__", "_2_", "__3", "12_", "1_3", "_23", "123")
  run_expectancy_matrix <- (t(re_24_1[, col.order]))

  return(run_expectancy_matrix)
}

factors_aux = list()
for (i in c(2005:2017,"2018.1","2018.2",19,21,22,23,24)) {
  print(i)
  dw <- get(paste0("dw",i))
  Season <- i
  factors_yr <- func_weights(dw)
  factors_aux[[i]] <- cbind(Season,factors_yr)
  
  #assign(paste0("weights",i),func_weights(dw))
}
weights_data = do.call(rbind, factors_aux)


func_weights <- function(gamedata){
  dw <- gamedata
  re24_plays <- data.frame()
  RUNS_1 <- data.frame()
  re24_plays <- dw$event
  
  re24_plays <-
    re24_plays %>% 
    group_by(game_id) %>% 
    mutate(RUNS = lag(cumsum(runs_on_event)))
  
  re24_plays <-
    re24_plays %>% 
    mutate(
      HALF.INNING = paste(game_id,inning,half_inning),
      OUTS.ON.PLAY = as.integer(post_outs) - as.integer(pre_outs),
      RUNS.SCORED = runs_on_event)
  
  
  re24_plays %>%
    group_by(HALF.INNING) %>%
    summarize(Outs.Inning = sum(OUTS.ON.PLAY), 
              Runs.Inning = sum(RUNS.SCORED),
              Runs.Start = first(RUNS),
              MAX.RUNS = Runs.Inning + Runs.Start) -> half_innings
  
  re24_plays %>%
    inner_join(half_innings, by = "HALF.INNING") %>%
    mutate(RUNS.ROI = abs(MAX.RUNS - RUNS)) -> re24_plays
  
  re24_plays[,c(15:17,19:21)] <- sapply((re24_plays[,c(15:17,19:21)]), as.character)
  #re24_plays[is.na(re24_plays[,c(15:17,19:21)])] <- ""
  
  re24_plays <- re24_plays %>%
    mutate_if(is.character, ~replace_na(., ""))
  re24_plays <- re24_plays %>%
    mutate_if(is.integer, ~replace_na(., 0))
  
  re24_plays <- as.data.frame(re24_plays)
  
  re24_plays %>% 
    
    mutate(BASES = 
             paste0(ifelse(pre_runner_1b_id == "", 0, 1),
                    ifelse(pre_runner_2b_id == "", 0, 1),
                    ifelse(pre_runner_3b_id == "", 0, 1)
             ),
           STATE = paste(BASES, pre_outs)) -> re24_plays
  
  re24_plays %>%
    mutate(NEW.BASES = 
             paste0(ifelse(post_runner_1b_id == "", 0, 1),
                    ifelse(post_runner_2b_id == "", 0, 1),
                    ifelse(post_runner_3b_id == "", 0, 1)
             ),
           NOUTS = as.integer(pre_outs) + as.integer(OUTS.ON.PLAY),
           NEW.STATE = paste(NEW.BASES, NOUTS)) -> re24_plays
  
  
  re24_plays %>% 
    filter((STATE != NEW.STATE) | (as.integer(RUNS.SCORED) > 0)) -> re24_plays
  
  
  re24_plays %>%
    filter(as.integer(Outs.Inning) == 3) -> re24_plays_final
  
  re24_plays_final %>% 
    group_by(STATE) %>%
    summarize(Mean = mean(as.integer(RUNS.ROI))) %>%
    mutate(Outs = substr(STATE, 5, 5)) %>%
    arrange(Outs) -> RUNS_1 
  
  
  re24n <- matrix(round(RUNS_1$Mean, 2), 8, 3)
  
  
  
  dimnames(re24n)[[2]] <- c("0 outs", "1 out", "2 outs")
  dimnames(re24n)[[1]] <- c("___", "__3", "_2_", "_23", 
                            "1__", "1_3", "12_", "123")
  
  re_24_1 <- t(re24n)
  col.order <- c("___", "1__", "_2_", "__3", "12_", "1_3", "_23", "123")
  run_expectancy_matrix <- (t(re_24_1[, col.order]))
  
  run_expectancy_matrix_24 <- run_expectancy_matrix

  # Preparation work for finding run values
  re24_plays %>%
    
    # Concentrate run values by state
    left_join(select(RUNS_1, -Outs), by = "STATE") %>%
    rename(Runs.State = Mean) %>%
    left_join(select(RUNS_1, -Outs), 
              by = c("NEW.STATE" = "STATE")) %>%
    rename(Runs.New.State = Mean) %>%
    replace_na(list(Runs.New.State = 0)) %>%
    
    # Find run value of each play
    mutate(run_value = Runs.New.State - Runs.State +
             RUNS.SCORED) -> re24_plays
  
  # Create necessary variable
  outs <- rep(0, nrow(re24_plays))
  
  # Find plays that resulted in outs
  re24_plays %>% 
    filter(OUTS.ON.PLAY > 0) -> outs
  
  # Find run value of outs
  outs %>%
    summarize(mean_run_value = mean(run_value)) -> mean_outs
  
  print(mean_outs)
  
  ############
  # Create necessary variable
  hbp <- rep(0, nrow(re24_plays))
  
  # Find all instances of a single
  for (i in 1:nrow(re24_plays)){
    if (isTRUE(grepl("Hit By Pitch", re24_plays$event[i]))) {
      hbp[i] <- 1
    }
  }
  
  # Select plays with a single
  re24_plays %>% 
    mutate(hbps = hbp) %>% 
    filter(hbps == 1) -> hbps
  
  # Find run value of singles by finding the mean run values of all plays with
  # a single
  hbps %>%
    summarize(mean_run_value = mean(run_value)) -> mean_hbps
  
  # Find the run value of a single to that of an out
  HBP <- mean_hbps - mean_outs
  
  print(HBP)
  ##############
  # Create necessary variable
  walk <- rep(0, nrow(re24_plays))
  
  # Find all instances of a single
  for (i in 1:nrow(re24_plays)){
    if (isTRUE(grepl("Walk", re24_plays$event[i]))) {
      walk[i] <- 1
    }
  }
  
  # Select plays with a single
  re24_plays %>% 
    mutate(walks = walk) %>% 
    filter(walks == 1) -> walks
  
  # Find run value of singles by finding the mean run values of all plays with
  # a single
  walks %>%
    summarize(mean_run_value = mean(run_value)) -> mean_walks
  
  # Find the run value of a single to that of an out
  BB <- mean_walks - mean_outs
  
  print(BB)
  
  # Create necessary variable
  home_run <- rep(0, nrow(re24_plays))
  
  # Find all instances of a single
  for (i in 1:nrow(re24_plays)){
    if (isTRUE(grepl("Home Run", re24_plays$event[i]))) {
      home_run[i] <- 1
    }
  }
  
  # Select plays with a single
  re24_plays %>% 
    mutate(home_runs = home_run) %>% 
    filter(home_runs == 1) -> home_runs
  
  # Find run value of singles by finding the mean run values of all plays with
  # a single
  home_runs %>%
    summarize(mean_run_value = mean(run_value)) -> mean_home_runs
  
  # Find the run value of a single to that of an out
  HOME_RUN <- mean_home_runs - mean_outs
  
  print(HOME_RUN)
  
  # Create necessary variable
  triple <- rep(0, nrow(re24_plays))
  
  # Find all instances of a single
  for (i in 1:nrow(re24_plays)){
    if (isTRUE(grepl("Triple", re24_plays$event[i]))) {
      triple[i] <- 1
    }
  }
  
  # Select plays with a single
  re24_plays %>% 
    mutate(triples = triple) %>% 
    filter(triples == 1) -> triples
  
  # Find run value of singles by finding the mean run values of all plays with
  # a single
  triples %>%
    summarize(mean_run_value = mean(run_value)) -> mean_triples
  
  # Find the run value of a single to that of an out
  TRIPLE <- mean_triples - mean_outs
  
  print(TRIPLE)
  
  # Create necessary variable
  double <- rep(0, nrow(re24_plays))
  
  # Find all instances of a single
  for (i in 1:nrow(re24_plays)){
    if (isTRUE(grepl("Double", re24_plays$event[i]))) {
      double[i] <- 1
    }
  }
  
  # Select plays with a single
  re24_plays %>% 
    mutate(doubles = double) %>% 
    filter(doubles == 1) -> doubles
  
  # Find run value of singles by finding the mean run values of all plays with
  # a single
  doubles %>%
    summarize(mean_run_value = mean(run_value)) -> mean_doubles
  
  # Find the run value of a single to that of an out
  DOUBLE <- mean_doubles - mean_outs
  
  print(DOUBLE)
  
  # Create necessary variable
  single <- rep(0, nrow(re24_plays))
  
  # Find all instances of a single
  for (i in 1:nrow(re24_plays)){
    if (isTRUE(grepl("Single", re24_plays$event[i]))) {
      single[i] <- 1
    }
  }
  
  # Select plays with a single
  re24_plays %>% 
    mutate(singles = single) %>% 
    filter(singles == 1) -> singles
  
  # Find run value of singles by finding the mean run values of all plays with
  # a single
  singles %>%
    summarize(mean_run_value = mean(run_value)) -> mean_singles
  
  # Find the run value of a single to that of an out
  SINGLE <- mean_singles - mean_outs
  
  print(SINGLE)
  
  # Create necessary variable
  sf <- rep(0, nrow(re24_plays))
  ibb <- rep(0, nrow(re24_plays))
  
  # Find all instances of a sacrifice fly
  for (i in 1:nrow(re24_plays)){
    if (isTRUE(grepl("Sac Fly", re24_plays$result.description[i]))) {
      sf[i] <- 1
    }
    if (isTRUE(grepl("Intent Walk", re24_plays$result.description[i]))) {
      ibb[i] <- 1
    }
  }
  
  # Select plays with a sacrifice fly
  re24_plays %>% 
    mutate(sf = sf, ibb = ibb) -> re24_plays
  
  # Create "wOBA Multiplier" by calculating the league wOBA figure
  woba_multiplier <- 
    (HBP*nrow(hbps) + BB*nrow(walks) + SINGLE*nrow(singles) + DOUBLE*nrow(doubles) 
     + TRIPLE*nrow(triples) + HOME_RUN*nrow(home_runs))/(nrow(re24_plays) - sum(ibb) 
                                                         - sum(sf))
  # Calculate the league OBP figure
  league_obp <- 
    (nrow(hbps) + nrow(walks) + nrow(singles) + nrow(doubles) + nrow(triples) 
     + nrow(home_runs))/(nrow(re24_plays) - sum(ibb))
  
  woba_scale <- league_obp / woba_multiplier
  
  # Multiply the play run values by the wOBA scale to obtain the weights used in
  # the wOBA calculation
  woba_weights <- c(woba_scale*HBP, woba_scale*BB, woba_scale*SINGLE, 
                    woba_scale*DOUBLE, woba_scale*TRIPLE, woba_scale*HOME_RUN)
  
  # Create an easily readable table containing the wOBA weights
  linear_weights_table <- as.data.frame(woba_weights)
  
  # Ensure the table is named correctly
  colnames(linear_weights_table) <- c("HBP", "BB", "1B", "2B", "3B", "HR")
  #rownames(linear_weights_table) <- c("Weights")
  
  # View the final product
  print(linear_weights_table)
  
  plays_sb_cs <- dw$play
  
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
  
  lgwSB=0
  
  weights <- linear_weights_table %>%
    mutate(
           wOBA = woba_multiplier$mean_run_value,
           "wOBAScale" = woba_scale$mean_run_value,
           `R/PA` = 0,
           cFIP = 0) %>%
    relocate(wOBA, "wOBAScale") %>%
    rename(
      ,`wHBP` = `HBP`
      ,`wBB` = `BB`
      ,`w1B` = `1B`
      ,`w2B` = `2B`
      ,`w3B` = `3B`
      ,`wHR` = `HR`
    )
    
    weights <- cbind(weights,runSB$`mean(run_vale)`,runCS$`mean(run_vale)`,lgwSB)
    
    
    return(weights)
}

weights_data$Season <- c(2005:2017,"2018.1","2018.2",2019,2021:2024)

weights_data$`R/PA` <- (totals$R/totals$PA)[which(totals$Year == weights_data$Season)]
weights_data$lgERA <- ((totals$ER/totals$IP)*9)[which(totals$Year == weights_data$Season)]
weights_data$lgwSB <- (((totals$SB*weights_data$runSB)+
                         (totals$CS*weights_data$runCS))/
                         (totals$`1B`+totals$BB+totals$HBP-totals$IBB))[which(totals$Year == weights_data$Season)]
weights_data$cFIP <- weights_data$lgERA - ((((13*totals$HR)+(3*(totals$BB+totals$HBP))-
                                               (2*totals$K))/totals$IP))[which(totals$Year == weights_data$Season)]
weights_data$lgRA9 <- ((totals$R/totals$IP)*9)[which(totals$Year == weights_data$Season)]
weights_data$pAdj <- weights_data$lgRA9-weights_data$lgERA
weights_data$RpW <- ((9*(totals$R/totals$IP)*1.5)+3)[which(totals$Year == weights_data$Season)]
weights_data$RlR <- ((570 * ((totals$GP/2)/(totals$GP/2))) * (weights_data$RpW/totals$PA))[which(totals$Year == weights_data$Season)]
weights_data$posFactor <- (((totals$GP/totals$Teams)*9)/(162*9))[which(totals$Year == weights_data$Season)]
weights_data$lgFIP <- round((((((13*totals$HR)+(3*(totals$BB+totals$HBP))-
                                               (2*totals$K))/totals$IP))+weights_data$cFIP),2)[which(totals$Year == weights_data$Season)]

POS <- c("C","SS","2B","CF","3B","RF","LF","1B","DH","X")
ADJ <- c(12.5,7.5,3.5,2.5,2.5,-7.5,-7.5,-12.5,-17.5,0)

weights_data$C <- 12.5*weights_data$posFactor
weights_data$SS <- 7.5*weights_data$posFactor
weights_data$`2B` <- 3.5*weights_data$posFactor
weights_data$CF <- 2.5*weights_data$posFactor
weights_data$`3B` <- 2.5*weights_data$posFactor
weights_data$RF <- -7.5*weights_data$posFactor
weights_data$LF <- -7.5*weights_data$posFactor
weights_data$`1B` <- -12.5*weights_data$posFactor
weights_data$DH <- -17.5*weights_data$posFactor
weights_data$X <- 0*weights_data$posFactor