# LMB Stats Lab

A fully modernized Shiny application for exploring Mexican Baseball League (LMB) data with a clean Bootstrap 5 UI, responsive layouts, and robust data loading.

## What's new

- **Modern UI stack** using `bslib::page_navbar()` and cards/value boxes.
- **Robust data loading** with fallback paths (`data/cache` and `cache`).
- **Interactive explorers** for players and teams with dynamic metric detection.
- **League-level insights** for standings, park factors, and sabermetric constants.
- **Operational reliability** with safer defaults and graceful handling when certain columns are missing.

## App sections

1. **Overview**
   - KPI cards (players, teams, games, seasons)
   - Team performance snapshot
   - Attendance and pace signal chart
2. **Player Explorer**
   - Switch between hitting/pitching/fielding
   - Dynamic season and metric widgets
   - Top-N leaderboard and full table
3. **Team Explorer**
   - Team/season filters and metric selector
   - Trend chart + detailed table
4. **Game Center**
   - Game logs, rosters, and transactions
5. **League Insights**
   - Standings, park factors, and guts constants

## Data expectations

The app reads `.rds` files from either:

- `data/cache/*.rds` (preferred)
- `cache/*.rds` (fallback)

## Run locally

```r
shiny::runApp()
```

## Notes

- This project keeps historical modular files under `R/modules/` for reference, while the new `app.R` provides a complete modern rebuild.
