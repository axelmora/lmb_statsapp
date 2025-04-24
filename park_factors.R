
runs_home <- dw19$game %>%
  group_by(team_name_home) %>%
  summarise(HomeRS = sum(score_home), HomeRA = sum(score_away), HomeG = n()) %>%
  rename("Team" = team_name_home)

runs_away <- dw19$game %>%
  group_by(team_name_away) %>%
  summarise(AwayRS = sum(score_away), AwayRA = sum(score_home), AwayG = n()) %>%
  rename("Team" = team_name_away)

runs_total <- runs_home %>%
  inner_join(runs_away, by = join_by(Team))

runs_total <- runs_total %>%
  mutate(PF = round(100*((HomeRS+HomeRA)/HomeG) /
                    ((AwayRS+AwayRA)/AwayG)
                  ))


park_factors_19 <- runs_total %>%
  select(Team, PF)

teams <- teams_19 %>%
            rename(Team = team_full_name)

park_factors_19 <- park_factors_19 %>%
                  inner_join(teams, by = join_by(Team))

park_factors_19 <- park_factors_19 %>%
                  select(Team, team_abbreviation, Venue = venue_name, PF)

park_factors_2124 <- park_factors

park_factors_24 <- read_csv("Documents/lmb_statsapp/lmb_stats/park_factors.csv")
park_factors_24 <- park_factors_24[-1]

park_factors3 <- merge(park_factors_24[-c(2:3)],park_factors_23[-c(2:3)],
                       by = 'Team', all = TRUE)

park_factors2 <- merge(park_factors_21[-c(2:3)],park_factors_22[-c(2:3)],
                      by = 'Team', all = TRUE)

park_factors <- merge(park_factors_2124, park_factors_19[-c(2:3)],by = 'Team', all = TRUE)

park_factors <- park_factors %>%
  rename(
    "PF 2024" = "PF.x.x", 
    "PF 2023" = "PF.y.x", 
    "PF 2022" = "PF.x.y", 
    "PF 2021" = "PF.y.y", 
    "PF 2019" = "PF"  )

park_factors <- park_factors %>%
  mutate("Avg 5 Yrs" = round(rowMeans(park_factors[,2:6], na.rm=TRUE)))
  

teams

write.csv(teams,"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/teams.csv")


teams_19 <- mlb_teams(season = 2019, league_ids = 125) %>%
              select(team_id, team_full_name, team_code, team_abbreviation, franchise_name, club_name, venue_id, venue_name, league_id, 
                     division_id, sport_id)