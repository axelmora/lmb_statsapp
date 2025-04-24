rpw <- c((9*(9903/15714)*1.5)+3
,(9*(8174/13885)*1.5)+3
,(9*(9598/12836)*1.5)+3
,(9*(6344/9999)*1.5)+3
,(9*(11561/16461)*1.5)+3)
Year <- c(2024,2023,2022,2021,2019)

RpW_Master <- cbind(Year,rpw)
RpW_Master <- as.data.frame(RpW_Master)

pos_factor_24 <- (93*9)/(162*9)
pos_factor_23 <- (90*9)/(162*9)
pos_factor_22 <- (90*9)/(162*9)
pos_factor_21 <- (66*9)/(162*9)
pos_factor_19 <- (120*9)/(162*9)


POS <- c("C","SS","2B","CF","3B","RF","LF","1B","DH","X")
ADJ <- c(12.5,7.5,3.5,2.5,2.5,-7.5,-7.5,-12.5,-17.5,0)

pos_adj <- as.data.frame(POS)

pos_adj <- pos_adj %>% 
  mutate(ADJ_2024 = ADJ*pos_factor_24
         ,ADJ_2023 = ADJ*pos_factor_23
         ,ADJ_2022 = ADJ*pos_factor_22
         ,ADJ_2021 = ADJ*pos_factor_21
         ,ADJ_2019 = ADJ*pos_factor_19)

lgRA9 <- c(
   (9903/15714)*9
  ,(8174/13885)*9
  ,(9598/12836)*9
  ,(6344/9999)*9
  ,(11561/16461)*9)
lgERA <- c(
  5.16
  ,4.78
  ,6.11
  ,5.71
  ,5.79)
pAdj <- lgRA9-lgERA
Year <- c(2024,2023,2022,2021,2019)

pAdj_Master <- cbind(Year,lgRA9,lgERA,pAdj)
pAdj_Master <- as.data.frame(pAdj_Master)

pos_adj_Master <- gs4_create("pos_adj", sheets = pos_adj) #1lhmqR8F9-2S1JHYjGovMa0z7uMHplhHb0pIrj8HAjWA
RpW_Master <- gs4_create("RpW_Master", sheets = RpW_Master) #1eB7ZAdQvgjKfo8ru8CRj33uT5Qb0urtUY4H3QOEj_Q0


RlR <- (570 * (905/905)) * (RpW_Master$rpw[which(RpW_Master$Year == 2024)]/70365)

library(ggplot2)

# Data for WAR categories
war_data <- data.frame(
  Category = c("MVP-Level", "All-Star", "Above-Average Starter", "Average Starter", "Replacement-Level"),
  WAR_Range = c("4.0–4.8", "3.0–3.9", "2.0–2.9", "1.0–1.9", "<0.0"),
  Min_WAR = c(4.0, 3.0, 2.0, 1.0, 0.0),
  Max_WAR = c(4.8, 3.9, 2.9, 1.9, 0.9)
)

# Plot WAR chart
ggplot(war_data, aes(x = Category, ymin = Min_WAR, ymax = Max_WAR)) +
  geom_linerange(size = 4, color = "blue") +
  geom_point(aes(y = Max_WAR), size = 3, color = "darkred") +
  labs(
    title = "2024 Mexican League WAR Scale",
    x = "Player Impact",
    y = "WAR Range"
  ) +
  theme_minimal()



library(ggplot2)


  ggplot(hitting_adv %>% filter(Year == 2024), aes(x = Name, y = wOBA, fill = Team)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "Player wOBA (2024)", x = "Player", y = "wOBA") +
    theme_minimal()
