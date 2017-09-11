##### filtros norte y sur del df principal
norte <- c(1,2,4,6,7,8,12,14,15)
sur <- c(1,3,5,9,10,11,13,16,17)

lmbts_nte <- lmbts[norte]
lmbts_sur <- lmbts[sur]

##### attendance and time games
LMB_att_time <- read.csv("lmb_stats/LMB_att_time.csv")

att_time <- LMB_att_time %>%
  group_by(DATE) %>% summarise(
  ATT = mean(ATT),
  TIME = mean(TIME))

#time series att and time
time.ts <- xts(att_time[,3], order.by = as.Date(att_time$DATE, "%Y-%m-%d"))
dygraph(time.ts, main = "2017 LMB GAMES TIME", ylab = "MINUTES") %>%
  dyLimit(mean(att_time$TIME), color = 'red') %>%
  dyRangeSelector()

att.ts <- xts(att_time[,2], order.by = as.Date(att_time$DATE, "%Y-%m-%d"))
dygraph(att.ts, main = "2017 LMB ATTENDANCE", ylab = "ATTENDANCE") %>%
  dyLimit(mean(att_time$ATT), color = 'red') %>%
  dyRangeSelector()


