
library(dplyr)

#master
bx <- read_xml("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_03/day_31/master_scoreboard.xml")

#games
j1 <- read_xml("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_05/day_04/gid_2017_05_04_camaaa_leoaaa_1/rawboxscore.xml")
j2 <- read_xml("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_05/day_04/gid_2017_05_04_mtyaaa_vaqaaa_1/rawboxscore.xml")
j3 <- read_xml("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_05/day_04/gid_2017_05_04_mvaaaa_duraaa_1/rawboxscore.xml")
j4 <- read_xml("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_05/day_04/gid_2017_05_04_mxoaaa_sltaaa_1/rawboxscore.xml")
j5 <- read_xml("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_05/day_04/gid_2017_05_04_oaxaaa_yucaaa_1/rawboxscore.xml")
j6 <- read_xml("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_05/day_04/gid_2017_05_04_pueaaa_tabaaa_1/rawboxscore.xml")
j7 <- read_xml("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_05/day_04/gid_2017_05_04_tijaaa_aguaaa_1/rawboxscore.xml")
j8 <- read_xml("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_05/day_04/gid_2017_05_04_vraaaa_quiaaa_1/rawboxscore.xml")

#COLLECT BATTING STATS
g1.b <- py_st_bt(j1)
g2.b <- py_st_bt(j2)
g3.b <- py_st_bt(j3)
g4.b <- py_st_bt(j4)
g5.b <- py_st_bt(j5)
g6.b <- py_st_bt(j6)
g7.b <- py_st_bt(j7)
g8.b <- py_st_bt(j8)
#COLLECT PITCHING STATS
g1.p <- py_st_pt(j1)
g2.p <- py_st_pt(j2)
g3.p <- py_st_pt(j3)
g4.p <- py_st_pt(j4)
g5.p <- py_st_pt(j5)
g6.p <- py_st_pt(j6)
g7.p <- py_st_pt(j7)
g8.p <- py_st_pt(j8)
#COLLECT SCORE
g1.s <- bs(j1)
g2.s <- bs(j2)
g3.s <- bs(j3)
g4.s <- bs(j4)
g5.s <- bs(j5)
g6.s <- bs(j6)
g7.s <- bs(j7)
g8.s <- bs(j8)
#WL series
wls <- wl(bx)


#FIRST
LMB_2018_bat <- rbind(g1.b,g2.b,g3.b,g4.b,g5.b,g6.b,g7.b,g8.b)
LMB_2018_pit <- rbind(g1.p,g2.p,g3.p,g4.p,g5.p,g6.p,g7.p,g8.p)

#UPDATE
LMB_2018_bat_UP <- rbind(g1.b,g2.b,g3.b,g4.b,g5.b,g6.b,g7.b,g8.b)
LMB_2018_pit_UP <- rbind(g1.p,g2.p,g3.p,g4.p,g5.p,g6.p,g7.p,g8.p)

LMB_2018_bat <- LMB_2018_bat %>%
  rbind(., LMB_2018_bat_UP) %>%
  group_by(Batter_Name) %>%
  summarise(AB = sum(AB), 
            R = sum(R), 
            H = sum(H),
            D = sum(D),
            Tr = sum(Tr),
            HR = sum(HR),
            RBI = sum(RBI),
            BB = sum(BB),
            SO = sum(SO),
            HBP = sum(HBP),
            SB = sum(SB),
            SF = sum(SF),
            SH = sum(SH))

LMB_2018_pit <- LMB_2018_pit %>%
  rbind(.,LMB_2018_pit_UP) %>%
  group_by(Pitcher_Name) %>%
  summarise(OUT = sum(OUT),
            H = sum(H),
            HR = sum(HR),
            ER = sum(HR),
            SO = sum(SO),
            BB = sum(BB),
            BK = sum(BK),
            W = sum(W),
            L = sum(L),
            HLD = sum(HLD),
            SV = sum(SV))

#BATTING STATS
bat_bas <- mutate(LMB_2018_bat,
                  PA = AB+BB+SF+SH)

bat_bas <- bat_bas %>% 
  filter(PA > 0)

bat_bas <- mutate(bat_bas,
                  AVG = round((H/AB),3),
                  OBP = round(((H+BB+HBP)/(AB+BB+HBP+SF)),3),
                  SLG = round((((1*H)+(2*D)+(3*Tr)+(4*HR))/AB),3),
                  OPS = round((OBP+SLG),3))

#bat_sab <- 

#PITCHING STATS
pit_bas <- select(LMB_2018_pit, Pitcher_Name, H:SV)

pit_bas <- mutate(pit_bas,
                  IP = round(as.numeric(paste0(trunc(LMB_2018_pit$OUT/3),".",LMB_2018_pit$OUT%%3)),2),
                  ERA = round(9*(ER/IP),2),
                  WHIP = round((BB+H)/IP,2))

pit_sab <- select(pit_bas, Pitcher_Name, SO:BB,IP:WHIP)

pit_sab <- mutate(pit_sab,
                  'BB/9' = round(9*(BB/IP),2),
                  'K/9' = round(9*(SO/IP),2))

#MASTER BOXSCORE
wls1 <- wl(bx)
wls <- rbind(wls,wls1)

#SCORES
LMB_2018_SC <- rbind(g1.s,g2.s,g3.s,g4.s,g5.s,g6.s,g7.s,g8.s)
LMB_2018_SC_UP <- rbind(g1.s,g2.s,g3,s,g4.s,g5.s,g6.s,g7.s,g8.s)

LMB_2018_SC <- rbind(LMB_2018_SC,LMB_2018_SC_UP)

sc <- LMB_2018_SC

###
sc #Game scores, attendance, times

wls #W-L series

pit_bas #pitching stats basic
pit_sab #pitching stats sabr

bat_bas #batting stats basic
bat_sab #batting stats sabr
