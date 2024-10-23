league_att_hist = list()

for(i in 2021:202{
  print(i)
  league_att_hist_aux <- mlb_attendance(season = i, league_id = 125)
  league_att_hist_select <- league_att_hist_aux  
  
  league_att_hist[[i]] <- league_att_hist_select   %>%
    select(team_name, attendance_average_home, attendance_total_home) 
  #%>%
  #  rename('EQUIPO' = team_name,'AVG ASISTENCIA CASA' = attendance_average_home,'ASISTENCIA TOTAL CASA' = attendance_total_home)
  assign(paste0("lmb_att_",i),league_att_hist[[i]])
  write.csv(league_att_hist[[i]],paste0("/Users/axel.mora/lmb_pace_venue_",i,".csv"))
}
lmb_att_hist = do.call(cbind.fill, league_att_hist)

write.csv(lmb_pace_venue_24,"/Users/axel.mora/lmb_pace_venue_24.csv")

lmb_att_24 <- mlb_attendance(season = 2024, league_id = 125)
lmb_att_24 <- lmb_att_24 %>% arrange(team_name)
lmb_att_24 <- cbind(lmb_att_24, teams$capacity)
lmb_att_24 <- lmb_att_24 %>%
  select(team_name, attendance_average_away, attendance_high, attendance_low, openings_total_home, attendance_average_home, attendance_total_home, V2) %>%
  mutate(pct_capacity = round((attendance_average_home/V2)*100)) %>%
  rename('EQUIPO' = team_name, 'AVG ASISTENCIA GIRA' = attendance_average_away, 'MAXIMA ASISTENCIA CASA' = attendance_high,
         'MENOR ASISTENCIA CASA' = attendance_low,
         'AVG ASISTENCIA CASA' = attendance_average_home,
         'AVG ASISTENCIA GIRA' = attendance_average_away,
         'APERTURAS EN CASA' = openings_total_home,
         'CAPACIDAD' = V2,
         'ASISTENCIA TOTAL CASA' = attendance_total_home,
         'PCT CAPACIDAD' = pct_capacity)
write.csv(lmb_att_24,"/Users/axel.mora/lmb_att_24_0801.csv")