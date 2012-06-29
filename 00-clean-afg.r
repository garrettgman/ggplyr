# Cleaning wikileaks Afghanistan data

# load data
all <- read.csv("data/afg.csv", stringsAsFactors = FALSE, header = FALSE)

# provide column names
names(all) <- tolower(c("Report Key", "Date", "Type", "Category", "Tracking Number", 
  "Title", "Summary", "Region", "Attack On", "Complex Attack", "Reporting Unit",
  "Unit Name", "Type of Unit", "Friendly WIA", "Friendly KIA", 
  "Host nation WIA", "Host nation KIA", "Civilian WIA", "Civilian KIA", 
  "Enemy WIA", "Enemy KIA", "Enemy Detained", "MGRS", "Latitude", "Longitude", 
  "Originator Group", "Updated by Group", "Ccir", "Sigact", "Affiliation", 
   "D Color", "Classification"))

# subset data
interesting <- tolower(c("Date", "Type", "Category", "Region", "Attack On", 
  "Complex Attack", "Friendly WIA", "Friendly KIA", "Host nation WIA", 
  "Host nation KIA", "Civilian WIA", "Civilian KIA", "Enemy WIA", "Enemy KIA", 
  "Enemy Detained", "Latitude", "Longitude", "Reporting Unit", "Unit Name", 
  "Type of Unit", "Affiliation"))
afg <- all[interesting]

# rename data
names(afg) <- c("date", "type", "category", "region", "attack.on", 
  "complex.attack", "friendly.wia", "friendly.kia", "host.wia", "host.kia", 
  "civilian.wia", "civilian.kia", "enemy.wia", "enemy.kia", "enemy.captured", 
  "lat", "lon", "reporting.unit", "unit.name", "unit.type", "affiliation")

# parse dates
library(lubridate)
afg$date <- ymd_hms(afg$date)
afg$year <- year(afg$date)
afg$month <- month(afg$date, label = TRUE, abbr = FALSE)
afg$day <- wday(afg$date, label = TRUE, abbr = FALSE)

# Many lon, lat values outside of Afghanistan. Cleaning this.
afg <- afg[!is.na(afg$lon),]
afg <- afg[!is.na(afg$lat),]
afg <- afg[afg$lon > 60 & afg$lat > 29, ]
afg <- afg[afg$lon < 75 & afg$lat < 39, ]


# building maps
library(ggmap)
roadmap <- get_map(location = c(59,29,76,39), maptype = "roadmap")
terrainmap <- get_map(location = c(59,29,76,39))


# new variables
afg$total.kia <- afg$friendly.kia + afg$host.kia + afg$civilian.kia + afg$enemy.kia
afg$total.wia <- afg$friendly.wia + afg$host.wia + afg$civilian.wia + afg$enemy.wia
afg$friendly.cas <- afg$friendly.kia + afg$friendly.wia
afg$host.cas <- afg$host.kia + afg$host.wia
afg$civilian.cas <- afg$civilian.kia + afg$civilian.wia
afg$enemy.cas <- afg$enemy.kia + afg$enemy.wia
afg$total.cas <- afg$total.kia + afg$total.wia
afg$fatal <- afg$total.kia != 0
afg$harmful <- afg$total.cas != 0


# building maps
library(ggmap)
roadmap <- get_map(location = c(59,29,76,39), maptype = "roadmap")
terrainmap <- get_map(location = c(59,29,76,39))

library(maps)
afghanistan <- map_data("world", region = "Afghanistan")

polygon <- ggplot() + geom_polygon(aes(long, lat, group = group), fill = "white", data = afghanistan)
road <- ggmap(roadmap, extent = "device")
terrain <- ggmap(terrainmap, extent = "device")

save(afg, polygon, road, terrain, file = "data/afg.RData", compress = "bzip2")
                                 
                                 
                                 