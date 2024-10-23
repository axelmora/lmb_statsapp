vsdiv_standing = list()

for(i in teams$team_id){
    name_record <- lmb_standings_24 %>% filter(team_records_team_id == i) %>% 
                        select(team_records_team_name, team_records_wins, team_records_losses, team_records_winning_percentage)
    
    vsdiv_standing_aux <- lmb_standings_24 %>% filter(team_records_team_id == i) %>% 
                          select(team_records_records_division_records)
    
    vsdiv_standing_aux <- vsdiv_standing_aux[[1]][[1]]
    
    
    vs_Nte <- vsdiv_standing_aux %>% 
                    filter(division.name == 'Mexican League Norte') %>%
                    rename(W_vs_Nte = wins, L_vs_Nte = losses, PCT_vs_Nte = pct) %>%
                    select(W_vs_Nte, L_vs_Nte, PCT_vs_Nte) 
    
    vs_Sur <- vsdiv_standing_aux %>% 
                    filter(division.name == 'Mexican League Sur') %>%
                    rename(W_vs_Sur = wins, L_vs_Sur = losses, PCT_vs_Sur = pct) %>%
                    select(W_vs_Sur, L_vs_Sur, PCT_vs_Sur) 
    
    vsdiv_standing[[i]] <- cbind(name_record, vs_Sur, vs_Nte)
    
    print(vsdiv_standing[[i]])
 
    
}
lmb_vs_zona_24 = do.call(rbind, vsdiv_standing)

write.csv(lmb_vs_zona_24,"/Users/axel.mora/lmb_field_24.csv")