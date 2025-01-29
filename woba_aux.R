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


e24 <- dw$event %>%
  filter(game_id %in% games$game_pk)


games <- mlb_schedule(season = 2019, level_ids = 11) %>%
  filter(teams_away_team_id %in% teams_19$team_id & teams_home_team_id %in% teams_19$team_id)



erm <- re24_plays_final |> 
  group_by(BASES, pre_outs) |>
  summarize(mean_run_value = mean(as.integer(RUNS.ROI)))

erm |>
  pivot_wider(
    names_from = pre_outs, 
    values_from = mean_run_value, 
    names_prefix = "Outs="
  )


woba <- (
  linear_weights_table$HBP*4 + 
    linear_weights_table$BB*26 + 
    linear_weights_table$`1B`*101 + 
    linear_weights_table$`2B`*26 + 
    linear_weights_table$`3B`*0 + 
    linear_weights_table$HR*14)/
  (327+26+4+3)


`R/PA` <- sum(lmb_hitting_team_standard$R)/sum(lmb_hitting_team_standard$PA)