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
    span <- range(vec, na.rm = T)
    dif <- diff(span)/nbins
    breaks <- seq(span[1], span[2], by = dif)
    labels <- (breaks - dif/2)[-1]
    as.numeric(as.character(cut(vec, breaks = breaks, labels = labels)))
  }
  
  df$glon <- grid_vec(df$lon, bins[1])
  df$glat <- grid_vec(df$lat, bins[2])
  
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
afg$ied.civ <- 0
afg$ied.civ[IED] <- afg$civilian.cas[IED]
afg$ied.friend <- 0
afg$ied.friend[IED] <- afg$friendly.cas[IED]
afg$ied.host <- 0
afg$ied.host[IED] <- afg$host.cas[IED]
afg$ied <- IED

ratios <- function(df) {
  civ <- sum(df$civilian.cas, na.rm = T)
  friend <- sum(df$friendly.cas, na.rm = T)
  enemy <- sum(df$enemy.cas, na.rm = T)
  total <- sum(df$total.cas, na.rm = T)
  ied <- sum(df$ied.cas, na.rm = T)
  ied.civ <- sum(df$ied.civ, na.rm = T)
  ied.friend <- sum(df$ied.friend, na.rm = T)
  
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

hafg <- afg[afg$harmful, ]
day.ratios <- ddply(hafg, "date", ratios, .progress = "text")
save(day.ratios, file = "data/afg.ratios.RData", compress = "bzip2")

qplot(date, civ.per.enemy, geom = "smooth", data = day.ratios) 
qplot(date, civ.per.friend, geom = "smooth", data = day.ratios)
# interesting lately more of us have been dying than of them
qplot(date, friend.per.enemy, geom = "smooth", data = day.ratios, se = F) +
  geom_smooth(aes(date, civ.per.enemy), color = "red", se = F) +
  geom_smooth(aes(date, enemy.per.total), color = "green", se = F)
qplot(date, friend.per.total, geom = "smooth", data = day.ratios)
# we did alot in the beginning, now we're just sitting around as targets
qplot(date, friendly.actions, geom = "smooth", data = day.ratios)    
qplot(date, enemy.actions, geom = "smooth", data = day.ratios)
qplot(date, by.friend, geom = "smooth", data = day.ratios)
qplot(date, avg.lon, geom = "smooth", data = day.ratios)
qplot(date, avg.lat, geom = "smooth", data = day.ratios)
qplot(avg.lon, avg.lat, geom = "point", data = day.ratios, alpha = 0.25)
qplot(friendly.actions, enemy.actions, geom = "jitter", size = civ.per.total, 
  color = civ.per.total, data = day.ratios)
qplot(date, enemy.per.total, geom = "smooth", 
  data = day.ratios[day.ratios$enemy.per.total < 50,]) +
  geom_smooth(aes(date, enemy.per.total))
qplot(date, friend.per.enemy, geom = "point", data = day.ratios) +
  geom_smooth(aes(date, friend.per.enemy))
# there's an inverse relationship between enemy casualties and the number of 
# friends hurt to make them
qplot(log(enemy.per.total * total.cas), log(friend.per.enemy), data =day.ratios)

ggplot(day.ratios) + 
  geom_subplot2d(aes(friendly.actions, enemy.actions, 
    subplot = geom_point(aes(date, friend.per.enemy), se = F)),
    bins = c(8, 12))


day.ratios <- ddply(afg, "date", ratios, .progress = "text")
save(day.ratios, file = "data/afg.ratios.RData", compress = "bzip2")
# then let's look at the percentage of civilian.cas in enemy vs friendly

# friends and enemies
afg$friend.per.enemy <- 0
afg$denom <- afg$enemy.cas
afg$denom[!afg$denom & !afg$friendly.cas] <- NA
afg$denom[!afg$denom & afg$friendly.cas] <- 0.05
afg$friend.per.enemy <- afg$friendly.cas / afg$denom
qplot(log(enemy.cas + 1), log(friendly.cas + 1), data = afg, color = type, geom = "jitter")
, geom = "smooth",
  method = "lm", se = F, color = type)
ggplot(afg[afg$type %in% c("Enemy Action", "Friendly Action"), ]) + geom_subplot2d(aes(log(enemy.cas + 1), log(friendly.cas + 1),
  subplot = geom_bar(aes(type, fill = type)), bins = c(10,15)), y_scale = free)

# why do day ratios take FOREVER?
# we're going to need place ratios if we wish to make an interactive chart
# let's just subdivide by months
library(lubridate)
hafg$mdate <- hafg$date
day(hafg$mdate[!is.na(hafg$mdate)]) <- 1
hafg <- geogrid(hafg, c(19,15))
ratios2 <- function(df) {
  civ <- sum(df$civilian.cas, na.rm = T)
  friend <- sum(df$friendly.cas, na.rm = T)
  enemy <- sum(df$enemy.cas, na.rm = T)
  host <- sum(df$host.cas, na.rm = T)
  total <- sum(df$total.cas, na.rm = T)
  ied <- sum(df$ied.cas, na.rm = T)
  ied.civ <- sum(df$ied.civ, na.rm = T)
  ied.friend <- sum(df$ied.friend, na.rm = T)
  ied.host <- sum(df$ied.host, na.rm = T)
  friend.init <- sum(df$attack.on == "FRIEND", na.rm = T)
  enemy.init <- sum(df$attack.on == "ENEMY", na.rm = T)
  ied.n <- sum(df$ied, na.rm = TRUE)
  
  per <- function(num, den) {
    if (!(num + den)) return(NA)
    if (!den) den <- 0.5
    num/den
  }
  
  data.frame(
    civ.per.friend = per(civ, friend),
    civ.per.host = per(civ, host),
    civ.per.coalition = per(civ, host + friend),
    civ.per.enemy = per(civ, enemy),
    civ.per.total = per(civ, total),
    civ.per.ied = per(ied.civ, ied),
    civ.ied = ied.civ,
    friend.per.enemy = per(friend, enemy),
    friend.per.total = per(friend, total),
    friend.per.ied = per(ied.friend, ied),
    friend.ied = ied.friend,
    host.per.enemy = per(host, enemy),
    host.per.total = per(host, total),
    host.per.ied = per(ied.host, ied),
    host.ied = ied.host, 
    coalition.per.enemy = per(host + friend, enemy),
    coalition.ied = sum(ied.host, ied.friend, na.rm = T),
    enemy.per.total = per(enemy, total),
    enemy.init = enemy.init,
    friend.init = friend.init,
    civ.cas = civ,
    host.cas = host,
    enemy.cas = enemy,
    friend.cas = friend,
    coalition.cas = host + friend,
    total.cas = total,
    by.friend = per(friend.init, enemy.init + friend.init)
  )
}


place.ratios <- ddply(hafg, c("mdate", "glon", "glat"), ratios2, 
  .progress = "text")
save(place.ratios, file = "data/afg.ratios.RData", compress = "bzip2")

qplot(mdate, civ.per.enemy, geom = "smooth", data = place.ratios) 
qplot(mdate, civ.per.friend, geom = "smooth", data = place.ratios)
# interesting lately more of us have been dying than of them
qplot(mdate, friend.per.enemy, geom = "point", data = place.ratios, se = F) +
  geom_smooth(aes(mdate, friend.per.enemy), color = "red", se = F)
ggplot(place.ratios) + 
  geom_subplot2d(aes(friend.init, enemy.init,
    subplot = geom_smooth(aes(mdate, friend.per.enemy), , se = F)), 
    ref = ref_box(aes(fill = mean(friend.cas)), color = "grey90"), 
    bins = c(8,8))

polygon + 
  geom_subplot(aes(glon[1], glat[1], group = interaction(glon, glat),
    subplot = geom_line(aes(mdate, y = 1))), data = place.ratios) +
  geom_subplot(aes(glon[1], glat[1], group = interaction(glon, glat),
    subplot = geom_smooth(aes(mdate, friend.per.enemy), method = "lm", se = F)), 
    data = place.ratios, ref = ref_box(fill = NA, color = "grey80"))

month.places <- ddply(hafg, "mdate", summarise, avg.lon = mean(lon), 
  avg.lat = mean(lat), .progress = "text")
polygon + geom_point(aes(avg.lon, avg.lat, color = mdate), data = month.places)

year.places <- ddply(hafg, "year", summarise, avg.lon = mean(lon), 
                      avg.lat = mean(lat), .progress = "text")
polygon + geom_point(aes(avg.lon, avg.lat, color = year), data = year.places)


qplot(date, friend.per.total, geom = "smooth", data = day.ratios)
# we did alot in the beginning, now we're just sitting around as targets
qplot(date, friendly.actions, geom = "smooth", data = day.ratios)    
qplot(date, enemy.actions, geom = "smooth", data = day.ratios)
qplot(date, by.friend, geom = "smooth", data = day.ratios)
qplot(date, avg.lon, geom = "smooth", data = day.ratios)
qplot(date, avg.lat, geom = "smooth", data = day.ratios)
qplot(avg.lon, avg.lat, geom = "point", data = day.ratios, alpha = 0.25)
qplot(friendly.actions, enemy.actions, geom = "jitter", size = civ.per.total, 
      color = civ.per.total, data = day.ratios)
qplot(date, enemy.per.total, geom = "smooth", 
      data = day.ratios[day.ratios$enemy.per.total < 50,]) +
        geom_smooth(aes(date, enemy.per.total))
qplot(date, friend.per.enemy, geom = "point", data = day.ratios) +
  geom_smooth(aes(date, friend.per.enemy))
# there's an inverse relationship between enemy casualties and the number of 
# friends hurt to make them
qplot(log(enemy.per.total * total.cas), log(friend.per.enemy), data =day.ratios)






# illustrative progression of plots
regional.combat <- regional[regional$harmful, ]
polygon + geom_point(aes(lon, lat, color = region), data = regional.combat)
polygon + geom_subplot(aes(median(lon), median(lat), group = region,
 subplot = geom_freqpoly(aes(date), color = "white")), 
 data = regional.combat, ref = ref_box(aes(fill = region)),
 width = rel(1.2))+ #y_scale = free) +
 coord_map() +
 opts(legend.position = "bottom")