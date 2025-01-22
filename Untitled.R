soto_2021 <- statcast_search(
  start_date = "2021-01-01",
  end_date = "2021-11-01",
  playerid = "665742",
  player_type = "batter"
) %>% 
  filter(!is.na(hc_x)) %>%
  filter(game_type == "R")

soto_2023 <- statcast_search(
  start_date = "2023-01-01",
  end_date = "2023-11-01",
  playerid = "665742",
  player_type = "batter"
) %>% 
  filter(!is.na(hc_x)) %>%
  filter(game_type == "R")

df <- rbind(soto_2021, soto_2023)

buttons = 
  list('print', list(
    extend = 'collection',
    buttons = c('csv', 'excel', 'pdf'),
    text = 'Download'
  ))