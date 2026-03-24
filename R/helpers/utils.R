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

reactable_lmb_theme <- function() {
  reactable::reactableTheme(
    borderColor = "#dee2e6",
    stripedColor = "rgba(0, 0, 0, 0.03)",
    highlightColor = "rgba(0, 123, 255, 0.08)",
    headerStyle = list(
      background = "#f8f9fa",
      fontFamily = "'IBM Plex Sans Condensed', 'Source Sans Pro', sans-serif",
      fontWeight = 600
    )
  )
}

reactable_lmb_coldef <- function(...) {
  reactable::colDef(
    ...,
    style = function(value) {
      if (is.numeric(value)) {
        list(
          fontFamily = "'JetBrains Mono', ui-monospace, monospace",
          fontVariantNumeric = "tabular-nums"
        )
      }
    }
  )
}

style_dt_numeric_columns <- function(dt, df) {
  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  if (length(numeric_cols) == 0) return(dt)

  DT::formatStyle(
    dt,
    columns = numeric_cols,
    `font-family` = "'JetBrains Mono', ui-monospace, monospace",
    `font-variant-numeric` = "tabular-nums"
  )
}
