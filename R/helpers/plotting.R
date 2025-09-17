# ======================================================
# Plotting Helpers
# ======================================================

plot_woba_trend <- function(df, player_name) {
  ggplot(df, aes(x = game_date, y = wOBA)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(color = "darkred", size = 2) +
    labs(
      title = paste("wOBA Trend:", player_name),
      x = "Game Date",
      y = "wOBA"
    ) +
    theme_minimal(base_size = 13)
}
