library(dplyr)   # data manipulation
library(ggplot2) 

LMB2018$HOME <- tolower(LMB2018$HOME) 
LMB2018$AWAY <- tolower(LMB2018$AWAY)

teams <- c("cam","pue","agu","mxo","mva", "mty","oax","qui","tab","leo","vaq","dur","lar","yuc","slt","tij")

for(i in teams){
  
  assign(paste0(i,'x'),rbind(assign(paste0(i,'h'),rename(
    select(
      filter(LMB2018, HOME == i),
      DATE, HOME,HW,HL),
    TEAM = HOME,
    W = HW,
    L = HL)
  ),
  assign(paste0(i,'a'),rename(
    select(
      filter(LMB2018, AWAY == i),
      DATE, AWAY,AW,AL),
    TEAM = AWAY,
    W = AW,
    L = AL)
  )
  )
  )
}

vaqx <- vaqx[order(vaqx$DATE),]
tijx <- tijx[order(tijx$DATE),]
larx <- larx[order(larx$DATE),]
mxox <- mxox[order(mxox$DATE),]
mtyx <- mtyx[order(mtyx$DATE),]
mvax <- mvax[order(mvax$DATE),]
yucx <- yucx[order(yucx$DATE),]
quix <- quix[order(quix$DATE),]
agux <- agux[order(agux$DATE),]
durx <- durx[order(durx$DATE),]
leox <- leox[order(leox$DATE),]
oaxx <- oaxx[order(oaxx$DATE),]
sltx <- sltx[order(sltx$DATE),]
puex <- puex[order(puex$DATE),]
tabx <- tabx[order(tabx$DATE),]
camx <- camx[order(camx$DATE),]

vaqx$pct <- ((vaqx$W/(vaqx$W+vaqx$L)))
tijx$pct <- ((tijx$W/(tijx$W+tijx$L)))
larx$pct <- ((larx$W/(larx$W+larx$L)))
mxox$pct <- ((mxox$W/(mxox$W+mxox$L)))
mtyx$pct <- ((mtyx$W/(mtyx$W+mtyx$L)))
mvax$pct <- ((mvax$W/(mvax$W+mvax$L)))
yucx$pct <- ((yucx$W/(yucx$W+yucx$L)))
quix$pct <- ((quix$W/(quix$W+quix$L)))
agux$pct <- ((agux$W/(agux$W+agux$L)))
durx$pct <- ((durx$W/(durx$W+durx$L)))
leox$pct <- ((leox$W/(leox$W+leox$L)))
oaxx$pct <- ((oaxx$W/(oaxx$W+oaxx$L)))
sltx$pct <- ((sltx$W/(sltx$W+sltx$L)))
puex$pct <- ((puex$W/(puex$W+puex$L)))
tabx$pct <- ((tabx$W/(tabx$W+tabx$L)))
camx$pct <- ((camx$W/(camx$W+camx$L)))

lmb.bump <- rbind(vaqx,tijx,larx,mxox,mtyx,mvax,yucx,
               quix,agux,durx,leox,oaxx,sltx,puex,tabx,camx)


lmb.rankings <- lmb.bump %>% 
  group_by(DATE) %>% 
  arrange(DATE, desc(W), TEAM) %>% 
  mutate(ranking = row_number(),
         day = as.numeric(as.Date(DATE)) - 17571) %>% 
  as.data.frame()


ggplot(data = lmb.rankings, aes(x = day, y = ranking, group = TEAM)) +
  geom_line(aes(color = TEAM, alpha = 1), size = 2) +
  geom_point(aes(color = TEAM, alpha = 1), size = 4) +
  scale_y_reverse(breaks = 1:nrow(lmb.rankings))