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
  subplot = geom_histogram(aes(date), fill = "navy")), data = afg)

polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_histogram(aes(date), fill = "navy")), 
  bins = c(15,15), data = afg, ref = NULL)

enemies <- afg[afg$type == "Enemy Action",]
ggplot(enemies) + geom_freqpoly(aes(date, fill = length(date)))
ieds <- afg[afg$type == "Explosive Hazard",]
ggplot(ieds) + geom_freqpoly(aes(date, fill = length(date)))

# we seem to lose more than they do
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_smooth(aes(friendly.cas, enemy.cas), method = "lm", se = FALSE)), 
  bins = c(15,15), data = afg, ref = ref_box(fill = "NA", 
  color = "grey80"))
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_smooth(aes(friendly.kia, enemy.kia), method = "lm", se = FALSE)), 
  bins = c(15,15), data = afg, ref = ref_box(fill = "NA", 
  color = "grey80"))

my_cumsum <- function(vec) {
  vec[is.na(vec)] <- 0
  cumsum(vec)
}

# we seem to be doing worse around kabul
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_line(aes(date, my_cumsum(enemy.cas) - my_cumsum(friendly.cas)))), 
  bins = c(15,15), data = afg, ref = ref_box(fill = "NA", color = "grey80"),
  y_scale = free, x_scale = free) 

# we are quite vulnerable to ieds
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_line(aes(date, my_cumsum(enemy.cas) - my_cumsum(friendly.cas)))), 
  bins = c(15,15), data = ieds, ref = ref_box(fill = "NA", color = "grey80"),
  y_scale = free, x_scale = free)

# are they getting better at using ieds? Are we getting better at avoiding them?
library(stringr)
categories <- unique(afg$category)
IED <- str_detect(categories, "IED")
ied <- str_detect(categories, "ied")
ieds <- subset(afg, category %in% c("IED Explosion", "IED Found/Cleared", 
  "IED Ambush", "IED Threat"))
ied.susp <- subset(afg, category %in% c("IED Suspected", "IED Suspected"))
ied.hoax <- subset(afg, category %in% c("IED HOAX", "IED Hoax"))

library(reshape2)
ied.cas <- ieds[, c("date", "year", "month", "day", "lon", "lat", 
  "friendly.wia", "friendly.kia", "friendly.cas", "host.wia", "host.kia", 
  "host.cas", "civilian.wia", "civilian.kia", "civilian.cas", "enemy.wia", 
  "enemy.kia", "enemy.cas", "total.kia", "total.wia", 
  "total.cas","fatal", "harmful", "complex.attack")]
# individual explosions are too granulated. I want summary information. 
# Cumulative sums and averages
ied.cas <- arrange(ied.cas, date)
ied.cum <- mutate(ied.cas, 
  friendly.wia = my_cumsum(friendly.wia),
  friendly.kia = my_cumsum(friendly.kia),               
  friendly.cas = my_cumsum(friendly.cas),
  host.wia = my_cumsum(host.wia),
  host.kia = my_cumsum(host.kia),               
  host.cas = my_cumsum(host.cas),
  civilian.wia = my_cumsum(civilian.wia),
  civilian.kia = my_cumsum(civilian.kia),               
  civilian.cas = my_cumsum(civilian.cas),
  enemy.wia = my_cumsum(enemy.wia),
  enemy.kia = my_cumsum(enemy.kia),  
  enemy.cas = my_cumsum(enemy.cas),
  total.wia = my_cumsum(total.wia),
  total.kia = my_cumsum(total.kia),               
  total.cas = my_cumsum(total.cas),
  n = 1:nrow(ied.cas),  
  .progress == "text")

ied.cum <- melt(ied.cum, id = c("date", "year", "month", "day", "lon", "lat", 
  "fatal", "harmful", "complex.attack", "n"))
ied.cum <- cbind(ied.cum, colsplit(ied.cum$variable, "\\.", c("victim", "type")))
ied.cum$variable <- NULL
ied.cum <- dcast(ied.cum, formula = ... ~ type)
ied.cum$avg.wia <- ied.cum$wia / ied.cum$n
ied.cum$avg.kia <- ied.cum$kia / ied.cum$n
ied.cum$avg.cas <- ied.cum$cas / ied.cum$n
save(ied.cum, file = "data/ied_cum.csv", compress = "bzip2")

ied.cas2 <- melt(ied.cas, id = c("date", "year", "month", "day", "lon", "lat", 
  "fatal", "harmful", "complex.attack"))
ied.cas2 <- cbind(ied.cas2, colsplit(ied.cas2$variable, "\\.", c("victim", "type")))
ied.cas2$variable <- NULL
ied.cas2 <- dcast(ied.cas2, formula = ... ~ type, fun.aggregate = sum)
                  
ggplot(ied.cas2) + ply_aes(geom_line(aes(date, my_cumsum(cas), 
  group = victim, color = victim)))
# line graph
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = ply_aes(geom_line(aes(date, my_cumsum(cas), 
    group = victim, color = victim)))),
  data = ied.cas2[ied.cas2$victim != "total",], bins = c(15,15),
  ref = ref_box(fill = NA, color = "grey90"))
# bar graph
road + geom_subplot2d(aes(lon, lat, 
  subplot = ply_aes(geom_bar(aes(victim, sum(kia), group = victim, 
    fill = victim)))),
  data = ied.cas2[ied.cas2$victim != "total",], bins = c(15,15),
  ref = NULL)

# bar graph of all casualties
casualties <- afg[, c("date", "year", "month", "day", "lon", "lat", 
  "friendly.wia", "friendly.kia", "friendly.cas", "host.wia", "host.kia", 
  "host.cas", "civilian.wia", "civilian.kia", "civilian.cas", "enemy.wia", 
  "enemy.kia", "enemy.cas", "total.kia", "total.wia", "total.cas","fatal", 
  "harmful", "type", "category")]
casualties <- melt(casualties, id = c("date", "year", "month", "day", "lon", "lat", 
  "fatal", "harmful", "type", "category"))
casualties <- cbind(casualties, colsplit(casualties$variable, "\\.", c("victim", "casualty")))
casualties$variable <- NULL
casualties <- dcast(casualties, formula = ... ~ casualty, fun.aggregate = sum)
save(casualties, file = "data/casualties.RData", compress = "bzip2")

theme_fullframe <- function (base_size = 12){
  structure(list(
    axis.line = theme_blank(), 
    axis.text.x = theme_blank(), 
    axis.text.y = theme_blank(),
    axis.ticks = theme_blank(), 
    axis.title.x = theme_blank(), 
    axis.title.y = theme_blank(), 
    axis.ticks.length = unit(0, "lines"), 
    axis.ticks.margin = unit(0, "lines"), 
    legend.position = "none", 
    panel.background = theme_blank(), 
    panel.border = theme_blank(), 
    panel.grid.major = theme_blank(), 
    panel.grid.minor = theme_blank(), 
    panel.margin = unit(0, "lines"), 
    plot.background = theme_blank(), 
    plot.margin = unit(0*c(-1.5, -1.5, -1.5, -1.5), "lines")
  ), class = "options")
}

casualties$victim <- factor(casualties$victim, 
  levels = c("enemy", "civilian", "host", "friendly"))

##############################################
####        Overplotting demo plot        ####
##############################################
p <- polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_bar(aes(victim, sum(cas, na.rm = TRUE), 
    group = victim, fill = victim), 
     color = rev(brewer.pal(5,"Blues"))[1], size = 1/4
    )),
  data = casualties[casualties$victim != "total",], bins = c(15,19),
  ref = NULL, width = rel(0.8)) + 
  theme_bw() + 
  opts(legend.position = "bottom") +
  coord_fixed(xlim = c(60, 74)) +
  scale_fill_manual(values = rev(brewer.pal(5,"Blues"))[1:4]) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
ggsave("afg_casualties_bar.png", width = 8, height = 6, dpi = 500)

road + geom_point(aes(lon, lat, color = victim),
  data = casualties[casualties$victim != "total",], size = 1/5) + 
    opts(legend.position = "bottom") +
    coord_map()



polygon + geom_subplot2d(aes(lon, lat, 
subplot = geom_line(aes(date, my_cumsum(enemy.cas) - my_cumsum(friendly.cas)))), 
bins = c(15,15), data = ieds, ref = ref_box(fill = "NA", color = "grey80"),
y_scale = free, x_scale = free)

# odd ied distribution
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_point(aes(0,0, size = length(date), 
  color = length(date)))), bins = c(20,20), data = ieds, ref = NULL)

# creepy! You can see the roads
terrain + geom_point(aes(lon, lat, alpha = friendly.kia), data = ieds) +
  scale_alpha_continuous(limits = c(0, 1))

ggplot(afgtimeline) + geom_freqpoly(aes(date, color = glat, group = gid))
ggplot(afgtimeline) + geom_freqpoly(aes(total.cas, color = glat, group = gid))