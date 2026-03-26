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

reactable_numeric_columns <- function(df, digits = 3) {
  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  if (!length(numeric_cols)) return(list())

  col_defs <- lapply(numeric_cols, function(col_name) {
    values <- df[[col_name]]
    has_fractional <- any(!is.na(values) & (abs(values - round(values)) > .Machine$double.eps^0.5))

    formatter <- if (has_fractional) {
      reactable::colFormat(digits = digits, separators = TRUE)
    } else {
      reactable::colFormat(separators = TRUE)
    }

    reactable::colDef(
      align = "right",
      format = formatter,
      minWidth = 80,
      maxWidth = 120
    )
  })

  stats::setNames(col_defs, numeric_cols)
}
