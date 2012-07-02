###################################################
###             geom_scatterplots               ###
###################################################
library(devtools)
install_github("ggplyr", "garrettgman", "ggsubplot")
load_all("../ggplyr")
load("data/nasa.RData")
load("data/testdata.RData")
load("data/seasons.RData")
load("data/years.RData")
load("inst/extdata/map_layers.RData")
###########################################
###              working                ###
###########################################
# imac 10.274 seconds vs. 21.736 seconds
# laptop 24.499 seconds (48.287 w/ ref_box) vs. 37.797 seconds
system.time(print(ggplot(nasa) + geom_subplot(aes(long[1], lat[1], 
  group = id, subplot = geom_point(aes(x = surftemp, 
  y = temperature), size = 1/5)), width = 3, height = 3)))
# vs. 
system.time(print(qplot(surftemp, temperature, data = nasa, size = I(1/5)) + 
  facet_grid(lat~long)))

# embed points (continuous - no stats)
ggplot(test.data) + geom_subplot(aes(mean(Fertility), mean(Education), 
  group = interaction(long, lat), subplot = geom_point(aes(Fertility, 
  Agriculture, color = rank(Catholic)), size = 3)))

# embed bars (categorical with stats)
ggplot(mpg) + geom_subplot(aes(mean(displ), mean(cty), group = year,
  subplot = geom_bar(aes(trans, fill = year))), y_scale = free, width = 1/4, 
  height = 1/4)
  
bars <- geom_bar(aes(trans, fill = year))
bars3 <- bars
test <- function(bars2) {
ggplot(mpg) + geom_subplot(aes(mean(displ), mean(cty), group = year,
  subplot = bars2), y_scale = free, width = 1/4, height = 1/4)
}
test(bars3)


# reference boxes
# boxes
ggplot(test.data) + geom_subplot(aes(mean(Fertility), mean(Education), 
  group = interaction(long, lat), subplot = geom_point(aes(Fertility, 
  Agriculture, color = rank(Catholic)), size = 3)), 
  ref = ref_box(aes(fill = mean(Catholic)), alpha = 0.2))


# hlines
ggplot(test.data) + geom_subplot(aes(mean(Fertility), mean(Education), 
  group = interaction(long, lat), subplot = geom_point(aes(Fertility, 
  Agriculture, color = rank(Catholic)), size = 3)), 
  ref = ref_hline(aes(fill = mean(Catholic))))

# vlines
ggplot(test.data) + geom_subplot(aes(mean(Fertility), mean(Education), 
  group = interaction(long, lat), subplot = geom_point(aes(Fertility, 
  Agriculture, color = rank(Catholic)), size = 3)), 
  ref = ref_vline(aes(fill = mean(Catholic))))

ggplot(mpg) + geom_subplot(aes(mean(displ), mean(cty), group = year, 
  subplot = geom_bar(aes(x = trans, fill = year))), width = 1/3, height = 1/3, 
  reference = ref_box(aes(fill = mean(hwy)), alpha = 0.1))

# merging overlaps
ggplot(mpg) + geom_subplot(aes(mean(displ), mean(cty), group = year, 
  subplot = geom_bar(aes(trans, fill = year, group = year), 
  position = "dodge")), y_scale = free, position = "merge", ref = ref_box()) 

# vs.

ggplot(mpg) + geom_subplot(aes(mean(displ), mean(cty), group = year, 
  subplot = geom_bar(aes(trans, fill = year, group = year),
  position = "dodge")), y_scale = free, reference = ref_box()) 

ggplot(test.data) + geom_subplot(aes(mean(Fertility), mean(Education), 
  group = interaction(long, lat), subplot = geom_point(aes(Fertility, 
  Agriculture, color = rank(Catholic)), size = 3)), ref = ref_box(aes(fill = 
  mean(Catholic))), position = "merge", width = rel(1), height = rel(1))
  
# vs.

ggplot(test.data) + geom_subplot(aes(mean(Fertility), mean(Education), 
  group = interaction(lat, long), subplot =  geom_point(aes(Fertility, 
  Agriculture, color = rank(Catholic)), size = 3)), width = rel(1), 
  height = rel(1), ref = ref_box(aes(fill = mean(Catholic))))
  
ggplot(test.data) + geom_subplot(aes(mean(Fertility), mean(Education), 
  group = interaction(long, lat), subplot = geom_point(aes(Fertility, 
  Agriculture, color = rank(Catholic)), size = 3)), ref = ref_hline(aes(fill = 
  mean(Catholic))), position = "merge", width = rel(1), height = rel(1))  

# categorical axiis
titanic <- data.frame(Titanic)
ggplot(titanic) + geom_subplot(aes(Sex, Age, group = interaction(Age, Sex), 
  subplot = geom_bar(aes(x = Survived, y = Freq, fill = Class), 
  position = "dodge")), ref =ref_box())

# groupwise aesthetics
ggplot(mpg) + geom_point(aes(hwy, cty, color = rank(displ))) + 
  facet_wrap(~cyl)
#vs
ggplot(mpg) + ply_aes(geom_point(aes(hwy, cty, color = rank(displ))), 
  c("cyl")) + facet_wrap(~cyl)

# new tactic for overplotting  
ggplot() + geom_point(aes(x = temperature, y = ozone, color = lat), data = nasa)	
# vs.
ggplot() + ply_aes(geom_point(aes(x = mean(temperature), y = mean(ozone), 
  color = lat[1]), data = nasa), c("lat", "long"))
#vs.	
ggplot() + geom_point(aes(x = mean(temperature), y = mean(ozone), 
  color = lat[1]), data = nasa)  

# geom_scatterplots - are redundant
# laptop: 44.722 seconds
system.time(print(ggplot(nasa) + geom_subplot(aes(long[1], lat[1], 
  group = interaction(lat, long), subplot = geom_point(aes(surftemp, 
  temperature), size = 1/5)), ref = ref_box())))

# laptop: 41.349 seconds
system.time(print(ggplot(nasa) + geom_subplot2d(aes(long, lat, 
  subplot = geom_point(aes(surftemp, temperature), size = 1/5)), bins = 23)))

# trying out geom_star
# without glyphing
ggplot(mpg) + GeomStar$new(mapping = aes(r = hwy, angle = cty, 
  group = cyl)) + facet_wrap(~cyl)

ggplot(mpg) + GeomStar$new(mapping = aes(angle = trans, r = cty, 
  group = cyl)) + facet_wrap(~cyl)

ggplot(test.data) + geom_star(mapping = aes( 
  r = Catholic, angle = Fertility, group = lat)) + facet_wrap(~lat)

ggplot(nasa) + ply_aes(geom_star(aes(r = ozone, angle = date, x = 0, y = 0, 
  fill = mean(temperature))), c("lat")) + facet_wrap(~ lat)

ggplot(nasa) + ply_aes(geom_star(aes(r = ozone, angle = date, 
  fill = mean(temperature))), c("lat")) + facet_wrap(~ lat, scales = "free")

# laptop: 55.2 seconds :P
system.time(print(ggplot(nasa) + map_nasa +
  geom_subplot(aes(long[1], lat[1], group = id, 
  subplot = geom_star(aes(r = ozone, angle = date, x = 0, y = 0, 
  fill = mean(temperature)))))))

ggplot(mpg) + geom_subplot(aes(cyl[1], y = 1, group = cyl, 
  subplot = geom_star(mapping = aes(r = hwy, angle = cty, fill = mean(hwy)))))

ggplot(mpg) + ply_aes(geom_star(mapping = aes(x = cyl[1], y = 1, 
  r = hwy, angle = cty, fill = mean(hwy), group = cyl)))

ggplot(mpg) + geom_star(mapping = aes(x = cyl[1], y = 1, 
  r = hwy, angle = cty, fill = mean(hwy)))

ggplot(nasa) + geom_star(aes(r = ozone, angle = date, x = 0, y = 0))

# new gridding
ggplot(test.data) + geom_point(aes(Fertility, Education))

ggplot(test.data) + 
  geom_subplot2d(aes(Fertility, Education, 
    subplot = geom_point(aes(Fertility, Education))), bins = 10,
    ref= ref_box(aes(fill = mean(Catholic))))

cheap.diamonds <- subset(diamonds, price <= 5000 & price >= 600)
ggplot(cheap.diamonds) +
  geom_subplot2d(aes(carat, price, subplot = geom_bar(aes(color, fill = color), 
    position = "dodge")), bins = c(10, 14), y_scale = free, height.adjust = 0.5, 
    width.adjust = 0.5, ref = ref_box(aes(color = length(color))))


# trying out geom_coxcomb
ggplot(mpg) + 
  geom_bar(aes(x = trans, fill = as.factor(cyl)), position = "stack") + 
  coord_polar() +
  facet_wrap(~year)

mpg$lat <- sample(1:4, nrow(mpg), replace = TRUE)
ggplot(mpg) + GeomCoxcomb$new(mapping = aes(angle = trans, fill = lat, 
  group = lat)) + facet_wrap(~cyl)

ggplot(mpg[mpg$cy != 5, ]) + geom_subplot(aes(cyl, y = 1, group = cyl, 
  subplot = geom_coxcomb(mapping = aes(angle = trans, fill = lat, 
  group = lat))))

ggplot(mpg) + geom_coxcomb(aes(angle = trans, fill = lat))

ggplot(cheap.diamonds) +
  geom_subplot2d(aes(carat, price, subplot = geom_coxcomb(aes(angle = color, 
    fill = color))), bins = c(10, 14), height.adjust = 0.5, 
    width.adjust = 0.5, ref = NULL)

# check area and spacing
cox <- data.frame(a = c(rep(1:3, each = 100), rep(4, 50)), b = c("a", "b"))
ggplot(cox) + geom_coxcomb(aes(angle = factor(a), fill = b, group = b), binwidth = .9)


# laptop: 20.287
system.time(print(ggplot(cheap.diamonds) +
  geom_subplot2d(aes(carat, price, subplot = geom_coxcomb(aes(angle = color, 
    fill = color))), bins = c(10, 14), x_scale = free, y_scale = free,
    height.adjust = 0.5, width.adjust = 0.5, ref = NULL)))

# laptop: 87.049 seconds :P
system.time(print(ggplot(seasons) + 
  geom_subplot(aes(lon[1], lat[1], group = stn, 
    subplot = geom_line(aes(x = time, y = pred))), height = 1, width = 2,
    ref = ref_box(aes(fill = avg)), position = "merge")))

# geom_freqstar
test <- data.frame(a = rep(1:100, 100))
ggplot(test) + geom_freqstar(aes(angle = a), binwidth = 1)
qplot(a, data = test, geom = "freqstar")