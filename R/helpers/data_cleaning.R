# ======================================================
# Data Cleaning Helpers
# ======================================================

# Clean player stats dataset
clean_player_stats <- function(df) {
  df %>%
    dplyr::mutate(
      # standardize names
      player = as.character(player),
      season = as.integer(season),
      game_date = as.Date(game_date),
      
      # round metrics for cleaner UI
      wOBA = round(wOBA, 3),
      OPS = round(OPS, 3),
      FIP = round(FIP, 2)
    ) %>%
    dplyr::arrange(player, game_date)
}
