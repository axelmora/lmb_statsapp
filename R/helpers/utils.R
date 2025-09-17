# ======================================================
# Utility Functions
# ======================================================

# Example: Safe readRDS with fallback to demo data
safe_read_player_stats <- function(path = "data/processed/player_stats.rds") {
  if (file.exists(path)) {
    readRDS(path)
  } else {
    # fallback demo dataset
    tibble::tibble(
      player = rep(c("Juan Perez", "Luis Gomez"), each = 10),
      season = rep(2024, 20),
      game_date = rep(seq.Date(Sys.Date() - 9, Sys.Date(), by = "days"), 2),
      wOBA = runif(20, .250, .450),
      OPS = runif(20, .600, 1.100),
      FIP = runif(20, 2.5, 5.0)
    )
  }
}
