# LMB Shiny App

A modular Shiny application for exploring **Mexican Baseball League (LMB)** statistics (2019–2025).  
This app provides interactive dashboards for player stats, team stats, and advanced sabermetric analysis, with daily-updated data pipelines.

---

## 🚀 Features

- **Player Statistics Module**: 
  - Interactive selection of players
  - Time series trends (wOBA, OPS, FIP, etc.)
  - Table and visualization outputs

- **Team Statistics Module** (coming soon):
  - Compare teams by season
  - Explore pitching and batting strengths

- **Data Pipeline Integration**:
  - Data pulled from Google Sheets (daily ETL with GitHub Actions)
  - Caching for faster load times

- **Modular Structure**:
  - Each feature is isolated as a module (`player_stats`, `team_stats`, etc.)
  - Helper functions for data cleaning, plotting, and utilities

---

## 📂 Project Structure

