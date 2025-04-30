ggplot(team_analyze, aes(x = FIP, y = wOBA, label = Team, color = wPct)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dotted") +
  
  geom_text(aes(label = Team), nudge_y = 0.002, check_overlap = TRUE) +

  
  # Mean lines
  geom_vline(xintercept = mean(team_analyze$FIP), linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = mean(team_analyze$wOBA), linetype = "dashed", color = "gray50") +
  
  # Quadrant labels
  annotate("text", x = min(team_analyze$FIP), y = max(team_analyze$wOBA), 
           label = "Strong Teams", hjust = 0, vjust = 1.5, fontface = "bold", color = "darkgreen") +
  annotate("text", x = max(team_analyze$FIP), y = max(team_analyze$wOBA), 
           label = "Great Offense,\nPoor Pitching", hjust = 1, vjust = 1.5, fontface = "italic", color = "orange") +
  annotate("text", x = min(team_analyze$FIP), y = min(team_analyze$wOBA), 
           label = "Great Pitching,\nWeak Offense", hjust = 0, vjust = -0.5, fontface = "italic", color = "orange") +
  annotate("text", x = max(team_analyze$FIP), y = min(team_analyze$wOBA), 
           label = "Weak Teams", hjust = 1, vjust = -0.5, fontface = "bold", color = "red") +
  
  # Color and themes
  scale_color_gradient(low = "red", high = "green") +
  labs(
    title = "LMB Teams: FIP vs wOBA (Colored by Winning %)",
    subtitle = "Dashed lines = league average; dotted line = regression",
    x = "Team FIP (Lower = Better Pitching)",
    y = "Team wOBA (Higher = Better Offense)",
    color = "Winning %"
  ) +
  theme_minimal()

