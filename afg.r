# Exploring wikileaks Afghanistan data
all <- read.csv("data/afg.csv", stringsAsFactors = FALSE, header = FALSE)
names(all) <- tolower(c("Report Key", "Date", "Type", "Category", "Tracking Number", 
  "Title", "Summary", "Region", "Attack On", "Complex Attack", "Reporting Unit",
  "Unit Name", "Type of Unit", "Friendly WIA", "Friendly KIA", 
  "Host nation WIA", "Host nation KIA", "Civilian WIA", "Civilian KIA", 
  "Enemy WIA", "Enemy KIA", "Enemy Detained", "MGRS", "Latitude", "Longitude", 
  "Originator Group", "Updated by Group", "Ccir", "Sigact", "Affiliation", 
   "D Color", "Classification"))
interesting <- tolower(c("Date", "Type", "Category", "Region", "Attack On", 
  "Complex Attack", "Friendly WIA", "Friendly KIA", "Host nation WIA", 
  "Host nation KIA", "Civilian WIA", "Civilian KIA", "Enemy WIA", "Enemy KIA", 
  "Enemy Detained", "Latitude", "Longitude", "Reporting Unit", "Unit Name", 
  "Type of Unit", "Affiliation"))
afg <- all[interesting]
names(afg) <- c("date", "type", "category", "region", "attack.on", 
  "complex.attack", "friendly.wia", "friendly.kia", "host.wia", "host.kia", 
  "civilian.wia", "civilian.kia", "enemy.wia", "enemy.kia", "enemy.captured", 
  "lat", "lon", "reporting.unit", "unit.name", "unit.type", "affiliation")

library(lubridate)
afg$date <- ymd_hms(afg$date)
afg$year <- year(afg$date)
afg$month <- month(afg$date, label = TRUE, abbr = FALSE)
afg$day <- wday(afg$date, label = TRUE, abbr = FALSE)
save(afg, file = "data/afg.RData", compress = "bzip2")
load("data/afg.RData")

# building maps
library(ggmap)
roadmap <- get_map(location = c(59,29,76,39))
terrainmap <- get_map(location = c(59,29,76,39), maptype = "terrain")
                
library(maps)
afghanistan <- map_data("world", region = "Afghanistan")

polygon <- ggplot() + geom_polygon(aes(long, lat, group = group), fill = "white", data = afghanistan)
road <- ggmap(roadmap, extent = "device")
terrain <- ggmap(terrainmap, extent = "device")

# Many lon, lat values outside of Afghanistan. Cleaning this.
polygon + geom_point(aes(lon, lat), data = afg)
afg <- afg[!is.na(afg$lon),]
afg <- afg[!is.na(afg$lat),]
afg <- afg[afg$lon > 60 & afg$lat > 29, ]
afg <- afg[afg$lon < 75 & afg$lat < 39, ]
terrain + geom_point(aes(lon, lat), data = afg)


# investigating incidence of casualties
afg$total.kia <- afg$friendly.kia + afg$host.kia + afg$civilian.kia + afg$enemy.kia
afg$total.wia <- afg$friendly.wia + afg$host.wia + afg$civilian.wia + afg$enemy.wia
afg$total.cas <- afg$total.kia + afg$total.wia
qplot(total.cas, data = afg, binwidth = 1) # mostly 0
table(afg$total.cas) # 62556 events with no casualties

afg$fatal <- afg$total.kia != 0
ggplot(afg) + geom_bar(aes(type, fill = type)) + facet_grid(fatal ~.)


# what events occur most often?
qplot(type, data = afg, fill = type)
ggplot(afg) + geom_subplot2d(aes(lon, lat, subplot = geom_bar(aes(type, fill = type))),
  bins = c(20, 20))
ggplot(afg) + geom_subplot2d(aes(lon, lat, subplot = geom_bar(aes(type, fill = type))),
  bins = c(20, 20), y_scale = free)
# Enemy and friendly actions

actions <- subset(afg, type == "Enemy Action" | type == "Friendly Action")
terrain + geom_point(aes(lon, lat, color = type), data = actions) + facet_wrap(~year)
# less deaths in winter
ggplot(actions) + geom_point(aes(year, total.cas, color = type))
# no day of the week effect. But nearly 2x as many enemy actions

ggplot(actions) + geom_point(aes(civilian.
ggplot(actions) + geom_bar(aes(day, fill = type), position = "dodge")