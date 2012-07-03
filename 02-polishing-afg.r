# Exploring afghanistan data
library(devtools)
load_all(".")
load("data/afg.RData")
load("data/ied_cas.csv")

# how effective are ieds at targeting troops?
# how about tracking civilians per ied vs. friendlies per ied?
library(lubridate)
ied.cas$ mdate <- round_date(ied.cas$date, "month")
save(ied.cas, file = "data/ied_cas.csv", compress = "bzip2")

# how about:
# civilian per ied by month,
# friendly per ied by month,
# host per ied by month,
# enemy per ied by month,
# total per ied by month
one_month <- function(df) {
  n <- nrow(df)
  civilian.cas <- sum(df$cas[df$victim == "civilian"], na.rm = TRUE) / n
  civilian.kia <- sum(df$kia[df$victim == "civilian"], na.rm = TRUE) / n
  civilian.wia <- civilian.cas - civilian.kia
  friendly.cas <- sum(df$cas[df$victim == "friendly"], na.rm = TRUE) / n
  friendly.kia <- sum(df$kia[df$victim == "friendly"], na.rm = TRUE) / n
  friendly.wia <- friendly.cas - friendly.kia
  host.cas <- sum(df$cas[df$victim == "host"], na.rm = TRUE) / n
  host.kia <- sum(df$kia[df$victim == "host"], na.rm = TRUE) / n
  host.wia <- host.cas - host.kia  
  enemy.kia <- sum(df$kia[df$victim == "enemy"], na.rm = TRUE) / n
  enemy.wia <- sum(df$wia[df$victim == "enemy"], na.rm = TRUE) / n
  enemy.cas <- enemy.kia + enemy.wia
  total.cas <- sum(df$cas[df$victim == "total"], na.rm = TRUE) / n
  total.kia <- sum(df$kia[df$victim == "total"], na.rm = TRUE) / n
  total.wia <- total.cas - total.kia
  
  data.frame(victim = c("civilian", "friendly", "host", "enemy", "total"),
    wia = c(civilian.wia, friendly.wia, host.wia, enemy.wia, total.wia),
    kia = c(civilian.kia, friendly.kia, host.kia, enemy.kia, total.kia),
    cas = c(civilian.cas, friendly.cas, host.cas, enemy.cas, total.cas),
    n = c(n, n, n, n, n))
}

geogrid <- function(df, bins) {
  grid_vec <- function(vec, nbins) {
    span <- range(vec)
    dif <- diff(span)/nbins
    breaks <- seq(span[1], span[2], by = dif)
    labels <- (breaks - dif/2)[-1]
    as.numeric(as.character(cut(vec, breaks = breaks, labels = labels)))
  }
  
  df$lon <- grid_vec(df$lon, bins[1])
  df$lat <- grid_vec(df$lat, bins[2])
  
  df
}

g.ied.cas <- geogrid(ied.cas, c(15,19))
per.ied <- ddply(g.ied.cas, c("mdate", "lat", "lon"), one_month, 
  .progress = "text")
per.ied$g <- interaction(per.ied$lon, per.ied$lat)
counts <- table(per.ied$g, per.ied$victim)
two_plus <- row.names(counts)[as.logical(pmax(0, counts[,1] - 1))]
per.ied <- per.ied[per.ied$g %in% two_plus, ]

save(ied.cas, per.ied, file = "data/ied_cas.csv", compress = "bzip2")

# visualizing rates over space and time
per.ieds <- per.ied[per.ied$victim == "total",]
polygon + geom_subplot(aes(lon, lat, group = g,
  subplot = geom_smooth(aes(mdate, wia))),
  data = per.ieds, ref = ref_box())

qplot(date, data = afg, fill = as.factor(year))
polygon + geom_subplot2d(aes(lon, lat, subplot = geom_histogram(aes(date, 
 fill = as.factor(year)))),
 data = afg[afg$type == "Air Mission",], ref = ref_box())



# how about casualties over time in different parts of the country?
my_deaggregate <- function(df, variable) {
  df <- df[, c("date", "lat", "lon", "year", variable)]
  df <- df[!is.na(df[[variable]]),]
  rows <- 1:nrow(df)
  rows <- rep(rows, times = as.numeric(df[[variable]]))
  df[variable] <- variable
  names(df)[names(df) == variable] <- "event"
  df[rows,]
}
    
civilian.cas <- my_deaggregate(afg, "civilian.cas")
polygon + geom_subplot2d(aes(lon, lat, 
 subplot = geom_freqpoly(aes(date), binwidth = 2630312 * 5)),
 data = civilian.cas, ref = NULL)

civilian.kia <- my_deaggregate(afg, "civilian.kia")
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_freqpoly(aes(date), binwidth = 2630312 * 5)),
  data = civilian.kia, ref = NULL)

friendly.cas <- my_deaggregate(afg, "friendly.cas")
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_freqpoly(aes(date), binwidth = 2630312 * 5)),
  data = friendly.cas, ref = NULL)

friendly.kia <- my_deaggregate(afg, "friendly.kia")
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_freqpoly(aes(date), binwidth = 2630312 * 5)),
  data = friendly.kia, ref = NULL)

host.cas <- my_deaggregate(afg, "host.cas")
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_freqpoly(aes(date), binwidth = 2630312 * 5)),
  data = host.cas, ref = NULL)

host.kia <- my_deaggregate(afg, "host.kia")
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_freqpoly(aes(date), binwidth = 2630312 * 5)),
  data = host.kia, ref = NULL)

enemy.cas <- my_deaggregate(afg, "enemy.cas")
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_freqpoly(aes(date), binwidth = 2630312 * 5)),
  data = enemy.cas, ref = NULL)

enemy.kia <- my_deaggregate(afg, "enemy.kia")
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_freqpoly(aes(date), binwidth = 2630312 * 5)),
  data = enemy.kia, height = rel(2), ref = NULL)

mixed.cas <- rbind(enemy.cas, friendly.cas)
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_freqpoly(aes(date, color = event), binwidth = 2630312 * 5)),
  data = mixed.cas, ref = NULL)

mixed.kia <- rbind(enemy.kia, friendly.kia)
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_freqpoly(aes(date, color = event), binwidth = 2630312 * 5)),
  data = mixed.kia, height = rel(2), ref = NULL)
# dang! very little contest

mixed.cas2 <- rbind(civilian.cas, friendly.cas)
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_freqpoly(aes(date, color = event), binwidth = 2630312 * 5)),
  data = mixed.cas2, ref = NULL)

# invocative, but maybe we should compare civilian deaths to enemy deaths
mixed.kia2 <- rbind(civilian.kia, friendly.kia)
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_freqpoly(aes(date, color = event), binwidth = 2630312 * 5)),
  data = mixed.kia2, height = rel(2), ref = NULL)

mixed.cas3 <- rbind(civilian.cas, enemy.cas)
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_freqpoly(aes(date, color = event), binwidth = 2630312 * 5)),
  data = mixed.cas3, ref = NULL)

# not very interesting
mixed.kia3 <- rbind(civilian.kia, enemy.kia)
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_freqpoly(aes(date, color = event), binwidth = 2630312 * 5)),
  data = mixed.kia3, height = rel(2), ref = NULL)

# all casualties - meh
all.cas <- rbind(civilian.cas, friendly.cas, host.cas, enemy.cas)

#######################################
####        Example plot 1    ?    ####
#######################################
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_freqpoly(aes(date), binwidth = 2630312 * 5)),
  bins = c(18,14), data = all.cas, ref = ref_box(aes(fill = length(date), 
  alpha = length(date)), color = "grey90")) +
  scale_fill_gradient("count", low = "white") +
  scale_alpha("count") +
  coord_map() +
  opts(legend.position = "bottom")
ggsave("casualties_line.png", width = 8, height = 6, dpi = 500)
# good as just an indicator of casualties

# meh
all.kia <- rbind(civilian.kia, friendly.kia, host.kia, enemy.kia)
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_freqpoly(aes(date), binwidth = 2630312 * 5)),
  bins = c(14,21), data = all.kia, height = rel(2), ref = ref_box(fill = NA, 
  color = "grey90"))
# good as just an indicator of ied deaths

# interesting. Let's try it for ieds. 
library(stringr)
ieds <- subset(afg, category %in% c("IED Explosion", "IED Found/Cleared", 
  "IED Ambush", "IED Threat"))

# it is pretty shocking when you don't look at host forces. 
# Civilians vastly outnumber friendly troops.
civilian.ied <- my_deaggregate(ieds, "civilian.cas")
friendly.ied <- my_deaggregate(ieds, "friendly.cas")
host.ied <- my_deaggregate(ieds, "host.cas")
enemy.ied <- my_deaggregate(ieds, "enemy.cas")
friendly.ied$event <- "coalition.cas"
host.ied$event <- "coalition.cas"
mixed.ied.cas <- rbind(civilian.ied, friendly.ied)
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_freqpoly(aes(date, color = event), binwidth = 2630312 * 5)),
  data = mixed.ied.cas, ref = NULL)

polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_freqpoly(aes(date, color = event), binwidth = 2630312 * 5)),
  data = enemy.ied, ref = NULL)

civilian.kied <- my_deaggregate(ieds, "civilian.kia")
friendly.kied <- my_deaggregate(ieds, "friendly.kia")
host.kied <- my_deaggregate(ieds, "host.kia")
friendly.kied$event <- "coalition.kia"
host.kied$event <- "coalition.kia"
mixed.ied.kia <- rbind(civilian.kied, friendly.kied)
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_freqpoly(aes(date, color = event), binwidth = 2630312 * 5)),
  bins = c(15, 19), data = mixed.ied.kia, ref = ref_box(fill = NA, 
  color = "grey90"))

#######################################
####        Example plot 1    ?    ####
#######################################
all.ied.cas <- rbind(civilian.ied, friendly.ied, host.ied, enemy.ied)
polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_freqpoly(aes(date), binwidth = 2630312 * 5)),
  bins = c(18,14), data = all.ied.cas, ref = ref_box(aes(fill = length(date), 
  alpha = length(date)), color = "grey90")) +
  scale_fill_gradient("count", low = "white") +
  scale_alpha("count") +
  coord_map() +
  opts(legend.position = "bottom")
ggsave("ied_casualties_line.png", width = 8, height = 6, dpi = 500)



# glyphs
ggplot(all.cas) + geom_coxcomb(aes(angle = event, fill = event))

polygon + geom_subplot2d(aes(lon, lat, 
  subplot = geom_coxcomb(aes(angle = event, fill = event))),
  bins = c(19, 15), data = all.ied.cas, ref = NULL, x_scale = free,
  y_scale = free) 

# bad idea. will take forever
# ggplot(all.ied.cas) + geom_subplot(aes(date, date, 
  group = interaction(lon, lat), 
  subplot = geom_coxcomb(aes(angle = event, fill = event))))

gafg <- geogrid(afg, c(18,14))
polygon + ply_aes(geom_point(aes(mean(lon), mean(lat), color = region, 
  group = region), data = afg))
regional <- afg[afg$region != "NONE SELECTED" & afg$region != "UNKNOWN",]
region_stats <- function(df) {
  n <- nrow(df)
  civilian.cas <- sum(df$civilian.cas, na.rm = TRUE) / n
  civilian.kia <- sum(df$civilian.kia, na.rm = TRUE) / n
  civilian.wia <- civilian.cas - civilian.kia
  friendly.cas <- sum(df$friendly.cas, na.rm = TRUE) / n
  friendly.kia <- sum(df$friendly.kia, na.rm = TRUE) / n
  friendly.wia <- friendly.cas - friendly.kia
  host.cas <- sum(df$host.cas, na.rm = TRUE) / n
  host.kia <- sum(df$host.kia, na.rm = TRUE) / n
  host.wia <- host.cas - host.kia  
  enemy.kia <- sum(df$enemy.kia, na.rm = TRUE) / n
  enemy.wia <- sum(df$enemy.wia, na.rm = TRUE) / n
  enemy.cas <- enemy.kia + enemy.wia
  
  data.frame(victim = c("civilian", "friendly", "host", "enemy"),
             wia = c(civilian.wia, friendly.wia, host.wia, enemy.wia),
             kia = c(civilian.kia, friendly.kia, host.kia, enemy.kia),
             cas = c(civilian.cas, friendly.cas, host.cas, enemy.cas),
             n = n, lon = mean(df$lon), lat = mean(df$lat), 
             enemy.actions = sum(df$type == "Enemy Action"),
             friendly.actions = sum(df$type == "Friendly Action"))
}

regional.cas <- ddply(regional, c("region"), region_stats, .progress = "text")
polygon + geom_subplot(aes(lon[1], lat[1], group = region,
  subplot = geom_coxcomb(aes(angle = victim, r = cas, group = victim, 
  fill = victim))), data = regional.cas)
ggplot(regional.cas) +
  geom_bar(aes(victim, cas, group = victim, fill = victim)) + facet_wrap(~region)
ggplot(regional.cas) +
  geom_coxcomb(aes(angle = victim, r = cas, group = victim, fill = victim)) + 
  facet_wrap(~region) + coord_fixed(xlim = c(-1,1))

# okay, let's look at casualties vs number of enemy and friendly actions
# everything is on top of each other
ggplot(regional.cas) + geom_subplot(aes(log(friendly.actions), log(enemy.actions), group = region,
  subplot = geom_coxcomb(aes(angle = victim, r = cas, group = victim, fill = victim))))
ggplot(regional.cas) + geom_subplot(aes(log(enemy.actions), log(friendly.actions), group = region,
  subplot = geom_bar(aes(victim, cas, group = victim, fill = victim))))
# what if we plot the data and then bin it? Like in the diamonds data set?
# not so good, we want to see how one relationship between two variables changes 
# relative to another relationship between two variables.
# Let's group by day and look at how the ratio of civilian ot enemy deaths 
# changes over time
IED <- str_detect(afg$category, "ied") + str_detect(afg$category, "IED")
afg$ied.cas <- 0
afg$ied.cas[IED] <- afg$total.cas[IED]

ratios <- function(df) {
  civ <- sum(df$civilian.cas, na.rm = T)
  friend <- sum(df$friendly.cas, na.rm = T)
  enemy <- sum(df$enemy.cas, na.rm = T)
  total <- sum(df$total.cas, na.rm = T)
  ied <- sum(df$ied.cas, na.rm = T)
  
  enemy.act <- sum(df$type == "Enemy Action", na.rm = T)
  friend.act <- sum(df$type == "Friendly Action", na.rm = T)
  friend.init <- sum(df$attack.on == "FRIEND", na.rm = T)
  enemy.init <- sum(df$attack.on == "ENEMY", na.rm = T)
  
  per <- function(num, den) {
    if (!(num + den)) return(NA)
    if (!den) den <- 0.5
    num/den
  }
  
  data.frame(
    civ.per.friend = per(civ, friend),
    civ.per.enemy = per(civ, enemy),
    civ.per.total = per(civ, total),
    civ.per.ied = per(civ, ied),
    friend.per.enemy = per(friend, enemy),
    friend.per.total = per(friend, total),
    friend.per.ied = per(friend, total),
    enemy.per.total = per(enemy, total),
    enemy.actions = enemy.act,
    friendly.actions = friend.act,
    enemy.init = enemy.init,
    friend.init = friend.init,
    total.cas = total,
    by.friend = per(friend.init, enemy.init + friend.init),
    avg.lon = mean(df$lon, na.rm = T),
    avg.lat = mean(df$lat, na.rm = T)
  )
}
    
day.ratios <- ddply(afg, "date", ratios, .progress = "text")

# then let's look at the percentage of civilian.cas in enemy vs friendly






# illustrative progression of plots
regional.combat <- regional[regional$harmful, ]
polygon + geom_point(aes(lon, lat, color = region), data = regional.combat)
polygon + geom_subplot(aes(median(lon), median(lat), group = region,
 subplot = geom_freqpoly(aes(date), color = "white")), 
 data = regional.combat, ref = ref_box(aes(fill = region)),
 width = rel(1.2))+ #y_scale = free) +
 coord_map() +
 opts(legend.position = "bottom")