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
per.ied <- ddply(g.ied.cas, c("mdate", "lat", "lon"), one_month, .progress = "text")
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

library(lubridate)
afg$year <- year(afg$date)
qplot(date, data = afg, fill = as.factor(year))
polygon + geom_subplot2d(aes(lon, lat, subplot = geom_histogram(aes(date, fill = as.factor(year)))),
 data = afg[afg$type == "Air Mission",], ref = ref_box())

deaggregate <- function(df, variable) {
  df <- df[c("date", "lat", "lon", variable)]
  df$.row <- 1:nrow(df)
  expand <- function(df) {
    
  df <- ddply(df, ".row", expand)

