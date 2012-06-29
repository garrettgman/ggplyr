# Exploring afghanistan data
library(devtools)
load_all(".")
load("data/afg.RData")


# timeline data
afg$glon <-  round(afg$lon * 2) / 2
afg$glat <- round(afg$lat * 2) / 2
afg$gid <- id(afg[c("glon", "glat")], drop = TRUE)

afgtimeline <- ddply(afg, c("glon", "glat", "gid", "date"), summarise, 
  friendly.kia = sum(friendly.kia, na.rm = TRUE), 
  friendly.wia = sum(friendly.wia, na.rm = TRUE),
  friendly.cas = sum(friendly.cas, na.rm = TRUE),
  host.kia = sum(host.kia, na.rm = TRUE), 
  host.wia = sum(host.wia, na.rm = TRUE),
  host.cas = sum(host.cas, na.rm = TRUE),  
  civilian.kia = sum(civilian.kia, na.rm = TRUE), 
  civilian.wia = sum(civilian.wia, na.rm = TRUE),
  civilian.cas = sum(civilian.cas, na.rm = TRUE),
  enemy.kia = sum(enemy.kia, na.rm = TRUE), 
  enemy.wia = sum(enemy.wia, na.rm = TRUE),
  enemy.cas = sum(enemy.cas, na.rm = TRUE),
  total.kia = sum(total.kia, na.rm = TRUE), 
  total.wia = sum(total.wia, na.rm = TRUE),
  total.cas = sum(total.cas, na.rm = TRUE), .progress = "text")
save(afgtimeline, file = "data/afgtimeline.RData", compress = "bzip2")                     
                                     
timeline <- afgtimeline[afgtimeline$total.cas != 0, ]                     
polygon + geom_subplot(aes(glon[1], glat[1], group = gid, 
  subplot = geom_star(aes(angle = date, r = total.cas))), data = timeline,
  ref = ref_box())



ggplot(afgtimeline) + geom_freqpoly(aes(date, color = glat, group = gid))
ggplot(afgtimeline) + geom_freqpoly(aes(total.cas, color = glat, group = gid))