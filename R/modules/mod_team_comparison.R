# =============================================================================
#  mod_team_comparison.R
#  Module: Team Stats Comparison — X (Pitching) vs Y (Hitting), colored by wPct
#  Logos are drawn via ggimage from www/img/logos/
# =============================================================================

# ---------------------------------------------------------------------------
# Column lists (excludes row indices, keys, count/rank columns)
# ---------------------------------------------------------------------------
hitting_vars <- c(
  "R", 
  "BB", "HBP", "K",
  "GO", "AO", "GO/AO", "AB/HR",
  "AVG", "OBP", "SLG", "OPS", "BABIP", "ISO",
  "K%", "BB%", "wOBA", "wRAA", "wRC", "wRC+"
)

pitching_vars <- c(
  "R", "BB", "HBP",
  "K", 
  "GO", "AO", "GO/AO",
  "ERA", "WHIP", "FIP", "pFIP",
  "P/INN", "K/BB", "K/9", "W/9", "H/9", "R/9", "HR/9",
  "K%", "BB%", "K-BB%", "ERA-", "FIP-", "pFIP-"
)

# Stats where LOWER is better (used to flip axis label hints and quadrant labels)
pitching_lower_is_better <- c(
  "ERA", "WHIP", "FIP", "pFIP", "ERA-", "FIP-", "pFIP-",
  "H", "HR", "ER", "R", "BB", "HBP", "WP", "BK",
  "H/9", "R/9", "HR/9", "BB%", "W/9"
)

hitting_higher_is_better <- c(
  "PA", "AB", "H", "1B", "2B", "3B", "HR", "R", "RBI",
  "BB", "SB", "TB", "GO",          # counting stats — more is generally better
  "AVG", "OBP", "SLG", "OPS", "BABIP", "ISO",
  "K%", "BB%", "wOBA", "wRAA", "wRC", "wRC+",
  "BaseRuns", "BaseRuns_diff", "SB%", "K/BB"
)

# =============================================================================
#  UI
# =============================================================================
teamComparisonUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # --- controls row --------------------------------------------------------
    fluidRow(
      column(
        width = 4,
        selectInput(
          ns("hit_var"),
          label   = "Hitting Variable (Y axis)",
          choices = hitting_vars,
          selected = "OPS"
        )
      ),
      column(
        width = 4,
        selectInput(
          ns("pit_var"),
          label   = "Pitching Variable (X axis)",
          choices = pitching_vars,
          selected = "ERA"
        )
      ),
      column(
        width = 4,
        sliderInput(
          ns("logo_size"),
          label = "Logo size",
          min = 0.02, max = 0.12, value = 0.06, step = 0.01
        )
      )
    ),
    
    # --- plot ----------------------------------------------------------------
    fluidRow(
      column(
        width = 12,
        plotOutput(ns("scatter_plot"), height = "600px")
      )
    )
  )
}

# =============================================================================
#  Server
# =============================================================================
teamComparisonServer <- function(id, team_hitting, team_pitching, teams_ref, logos, lmb_std, current_season) {
  # current_season: a reactive returning the season integer to filter on
  
  moduleServer(id, function(input, output, session) {
    
    # -------------------------------------------------------------------------
    # Build the analysis data frame for the selected season
    # -------------------------------------------------------------------------
    team_analyze <- reactive({
      req(current_season())
      season <- current_season()
      
      hit_col <- input$hit_var
      pit_col <- input$pit_var
      req(hit_col, pit_col)
      
      # Filter to season
      hit <- team_hitting  |> filter(Season == season)
      pit <- team_pitching |> filter(Season == season)
      ref <- teams_ref     |> filter(Season == season)
      
      # Select only the columns we need (handle backtick-needed names safely)
      hit_sel <- hit |> select(Team, all_of(hit_col))
      pit_sel <- pit |> select(Team, all_of(pit_col))
      
      # Rename to generic names so downstream code stays simple
      names(hit_sel)[2] <- "hit_stat"
      names(pit_sel)[2] <- "pit_stat"
      
      df <- hit_sel |>
        inner_join(pit_sel, by = "Team") |>
        left_join(ref |> select(Team_full, Team_short, Abbr, TeamId),
                  by = c("Team" = "Team_full")) |>
        left_join(stan_lmb |> select(Team, wPct),
                  by = c("Team_short" = "Team")) |>
        mutate(
          wPct    = as.numeric(wPct),
          hit_stat = as.numeric(hit_stat),
          pit_stat = as.numeric(pit_stat)
        ) |>
        # Attach logo path via logos dataset (logos$team_id == ref$TeamId)
        left_join(logos |> select(team_id, logo),
                  by = c("TeamId" = "team_id")) |>
        rename(Team_label = Abbr) |>
        filter(!is.na(hit_stat), !is.na(pit_stat))
      
      df
    })
    
    # -------------------------------------------------------------------------
    # Quadrant label helpers
    # -------------------------------------------------------------------------
    quadrant_labels <- reactive({
      pit_col <- input$pit_var
      hit_col <- input$hit_var
      
      pit_low  <- pit_col %in% pitching_lower_is_better   # lower x = better pitching
      hit_high <- hit_col %in% hitting_higher_is_better   # higher y = better hitting
      
      if (pit_low && hit_high) {
        list(
          top_left     = list(label = "Strong Teams",             color = "darkgreen", face = "bold"),
          top_right    = list(label = "Great Offense,\nPoor Pitching", color = "darkorange", face = "italic"),
          bot_left     = list(label = "Great Pitching,\nWeak Offense", color = "darkorange", face = "italic"),
          bot_right    = list(label = "Weak Teams",               color = "red",       face = "bold")
        )
      } else if (!pit_low && hit_high) {
        list(
          top_left     = list(label = "Good Offense,\nLow Pitching stat", color = "darkorange", face = "italic"),
          top_right    = list(label = "Strong Teams",             color = "darkgreen", face = "bold"),
          bot_left     = list(label = "Weak Teams",               color = "red",       face = "bold"),
          bot_right    = list(label = "High Pitching stat,\nWeak Offense", color = "darkorange", face = "italic")
        )
      } else if (pit_low && !hit_high) {
        list(
          top_left     = list(label = "Great Pitching,\nHigh hit stat",   color = "darkorange", face = "italic"),
          top_right    = list(label = "Weak Teams",               color = "red",       face = "bold"),
          bot_left     = list(label = "Strong Teams",             color = "darkgreen", face = "bold"),
          bot_right    = list(label = "Great Offense,\nPoor Pitching",     color = "darkorange", face = "italic")
        )
      } else {
        list(
          top_left     = list(label = "Weak Teams",               color = "red",       face = "bold"),
          top_right    = list(label = "High stat,\nWeak Pitching",color = "darkorange", face = "italic"),
          bot_left     = list(label = "Good Pitching,\nLow hit stat", color = "darkorange", face = "italic"),
          bot_right    = list(label = "Strong Teams",             color = "darkgreen", face = "bold")
        )
      }
    })
    
    # -------------------------------------------------------------------------
    # Axis direction labels
    # -------------------------------------------------------------------------
    axis_labels <- reactive({
      pit_col <- input$pit_var
      hit_col <- input$hit_var
      
      x_hint <- if (pit_col %in% pitching_lower_is_better) " (Lower = Better)" else " (Higher = Better)"
      y_hint <- if (hit_col %in% hitting_higher_is_better) " (Higher = Better)" else " (Lower = Better)"
      
      list(
        x = paste0("Team ", pit_col, x_hint),
        y = paste0("Team ", hit_col, y_hint)
      )
    })
    
    # -------------------------------------------------------------------------
    # Render the plot
    # -------------------------------------------------------------------------
    output$scatter_plot <- renderPlot({
      df   <- team_analyze()
      req(nrow(df) > 0)
      
      ql   <- quadrant_labels()
      al   <- axis_labels()
      
      mean_x <- mean(df$pit_stat, na.rm = TRUE)
      mean_y <- mean(df$hit_stat, na.rm = TRUE)
      
      x_min <- min(df$pit_stat, na.rm = TRUE)
      x_max <- max(df$pit_stat, na.rm = TRUE)
      y_min <- min(df$hit_stat, na.rm = TRUE)
      y_max <- max(df$hit_stat, na.rm = TRUE)
      
      logo_sz <- input$logo_size
      
      # y-range used to convert logo_size (proportion of width) to data-unit offset
      y_range   <- diff(range(df$hit_stat, na.rm = TRUE))
      x_range   <- diff(range(df$pit_stat, na.rm = TRUE))
      # logo height in data units (approximate: assume square logo, width ~ logo_sz * x_range)
      logo_offset <- logo_sz * x_range * 0.05   # shifts logo centre above the text baseline
      
      p <- ggplot(df, aes(x = pit_stat, y = hit_stat, label = Team_label)) +
        
        # Colored quadrants
        geom_rect(aes(xmin = -Inf, xmax = mean_x, ymin = mean_y, ymax = Inf),
                  fill = "#c7f9cc", alpha = 0.2, inherit.aes = FALSE) +
        geom_rect(aes(xmin = mean_x, xmax = Inf,  ymin = mean_y, ymax = Inf),
                  fill = "#fff3cd", alpha = 0.2, inherit.aes = FALSE) +
        geom_rect(aes(xmin = -Inf, xmax = mean_x, ymin = -Inf,   ymax = mean_y),
                  fill = "#fff3cd", alpha = 0.2, inherit.aes = FALSE) +
        geom_rect(aes(xmin = mean_x, xmax = Inf,  ymin = -Inf,   ymax = mean_y),
                  fill = "#ffe5ec", alpha = 0.2, inherit.aes = FALSE) +
        
        # Team abbreviation colored by winning % (drawn first, at the data point)
        geom_text(aes(color = wPct),
                  size = 4.2, fontface = "bold",
                  nudge_y = -(y_range * 0.025),   # push label slightly below logo
                  check_overlap = FALSE) +
        
        # Team logos in original colors, centred above the abbreviation
        ggimage::geom_image(aes(y = hit_stat + logo_offset, image = logo),
                            size = logo_sz, by = "width", inherit.aes = TRUE) +
        
        # Reference lines
        geom_vline(xintercept = mean_x, linetype = "dashed", color = "gray50") +
        geom_hline(yintercept = mean_y, linetype = "dashed", color = "gray50") +
        
        # Quadrant annotations
        annotate("text",
                 x = x_min, y = y_max,
                 label    = ql$top_left$label,
                 hjust = 0, vjust = 1.5,
                 fontface = ql$top_left$face,
                 color    = ql$top_left$color,
                 size = 3.5) +
        annotate("text",
                 x = x_max, y = y_max,
                 label    = ql$top_right$label,
                 hjust = 1, vjust = 1.5,
                 fontface = ql$top_right$face,
                 color    = ql$top_right$color,
                 size = 3.5) +
        annotate("text",
                 x = x_min, y = y_min,
                 label    = ql$bot_left$label,
                 hjust = 0, vjust = -0.5,
                 fontface = ql$bot_left$face,
                 color    = ql$bot_left$color,
                 size = 3.5) +
        annotate("text",
                 x = x_max, y = y_min,
                 label    = ql$bot_right$label,
                 hjust = 1, vjust = -0.5,
                 fontface = ql$bot_right$face,
                 color    = ql$bot_right$color,
                 size = 3.5) +
        
        # Color scale by winning %
        scale_color_gradient(low = "red", high = "darkgreen", na.value = "gray70") +
        
        # Labels
        labs(
          title    = paste0("LMB Teams: ", input$pit_var, " vs ", input$hit_var,
                            " (Colored by Winning %)"),
          subtitle = "Colored quadrants based on team profile  |  Dashed lines = league avg",
          x        = al$x,
          y        = al$y,
          color    = "Win %"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.title    = element_text(face = "bold"),
          plot.subtitle = element_text(color = "gray40"),
          legend.position = "right"
        )
      
      p
    })
  })
}