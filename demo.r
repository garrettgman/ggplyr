###################################################
###             geom_scatterplots               ###
###################################################
library(devtools)
load_all("../ggplyr")
load("data/nasa.RData")
load("data/testdata.RData")
load("data/seasons.RData")
load("data/years.RData")
load("inst/extdata/map_layers.RData")
###########################################
###              working                ###
###########################################
# imac 10.274 seconds
# system.time(print(qplot(surftemp, temperature, data = nasa) + 
# facet_grid(long~lat)))
system.time(print(ggplot(nasa) + glyph(geom_point(aes(x = surftemp, 
  y = temperature), size = 1/5), glyph.by = c("lat", "long"), width = 3, 
  height = 3, major = aes(x = long[1], y = lat[1]))))
# compare to 21.736 seconds
# system.time(print(qplot(surftemp, temperature, data = nasa, size = I(1/5)) + 
#  facet_grid(long~lat)))

# embed points (continuous - no stats)
ggplot(test.data) + glyph(geom_point(aes(Fertility, Agriculture, 
  color = rank(Catholic)), size = 3), glyph.by = c("lat", "long"), 
  major = aes(mean(Fertility), mean(Education)))

# embed bars (categorical with stats)
ggplot(mpg) + glyph(geom_bar(aes(x = trans, fill = year)), 
  aes(x = mean(displ), y = mean(cty)), c("year"), y_scale = free, 
  width = 1/4, height = 1/4)
  
# reference boxes
# boxes
ggplot(test.data) + glyph(geom_point(aes(Fertility, Agriculture, 
  color = rank(Catholic)), size = 3), glyph.by = c("lat", "long"), 
  width = 10, height = 10,
  major = aes(mean(Fertility), mean(Education)), ref = ref_box(aes(fill = 
  mean(Catholic)), alpha = 0.2))

# hlines
ggplot(test.data) + glyph(geom_point(aes(Fertility, Agriculture, 
  color = rank(Catholic)), size = 3), glyph.by = c("lat", "long"), 
  major = aes(mean(Fertility), mean(Education)), ref = ref_hline(aes(fill = 
  mean(Catholic))))

# vlines
ggplot(test.data) + glyph(geom_point(aes(Fertility, Agriculture, 
  color = rank(Catholic)), size = 3), glyph.by = c("lat", "long"), 
  major = aes(mean(Fertility), mean(Education)), ref = ref_vline(aes(fill = 
  mean(Catholic))))
  
# points
ggplot(mpg) + glyph(geom_bar(aes(x = trans, fill = year)), 
  aes(x = mean(displ), y = mean(cty)), c("year"), y_scale = free, 
  width = 1/3, height = 1/3, reference = ref_points(aes(fill = mean(hwy)), 
  size = 3, alpha = 0.1)) 

ggplot(mpg) + glyph(geom_bar(aes(x = trans, fill = year)), 
                    aes(x = mean(displ), y = mean(cty)), c("year"), 
                    width = 1/3, height = 1/3, reference = ref_box(aes(fill = mean(hwy)), 
                                                                    alpha = 0.1))

# merging overlaps
ggplot(mpg) + glyph(geom_bar(aes(x = trans, fill = year, group = year), 
  position = "dodge"), aes(x = mean(displ), y = mean(cty)), c("year"), 
  y_scale = free, merge = TRUE, reference = ref_box()) 

# vs.

ggplot(mpg) + glyph(geom_bar(aes(x = trans, fill = year, group = year),
  position = "dodge"), aes(x = mean(displ), y = mean(cty)), c("year"), 
  y_scale = free, reference = ref_box()) 

ggplot(test.data) + glyph(geom_point(aes(Fertility, Agriculture, 
  color = rank(Catholic)), size = 3), glyph.by = c("lat", "long"), 
  major = aes(mean(Fertility), mean(Education)), ref = ref_box(aes(fill = 
  mean(Catholic))), merge = TRUE, width = rel(1), height = rel(1))
  
# vs.

ggplot(test.data) + glyph(geom_point(aes(Fertility, Agriculture, 
  color = rank(Catholic)), size = 3), glyph.by = c("lat", "long"), 
  major = aes(mean(Fertility), mean(Education)), ref = ref_box(aes(fill = 
  mean(Catholic))), width = rel(1), height = rel(1))
  
ggplot(test.data) + glyph(geom_point(aes(Fertility, Agriculture, 
  color = rank(Catholic)), size = 3), glyph.by = c("lat", "long"), 
  major = aes(mean(Fertility), mean(Education)), ref = ref_hline(aes(fill = 
  mean(Catholic))), merge = TRUE, width = rel(1), height = rel(1))  

# categorical axiis
titanic <- data.frame(Titanic)
ggplot(titanic) + glyph(geom_bar(aes(x = Survived, y = Freq, fill = Class), 
  position = "dodge"), glyph.by = c("Age", "Sex"), 
  major = aes(x = Sex, y = Age), ref =ref_box(fill = "grey80"))

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
ggplot(nasa) + glyph(geom_point(aes(surftemp, temperature), size = 1/5), 
  aes(long[1], lat[1]), c("lat", "long"), reference = ref_box())

# ggplot(nasa) + geom_scatterplots(mapping = aes(x = long[1], y = lat[1], 
#  minor.x = surftemp, minor.y = temperature), glyph.by = c("long", "lat"), 
#  size = 1/5, reference = ref_box())

ggplot(nasa) + glyph(geom_point(aes(x = surftemp, y = temperature), 
  size = 1/5), glyph.by = c("lat", "long"), major = aes(x = long[1], 
  y = lat[1]), ref= ref_box())

# trying out geom_star
# without glyphing
ggplot(mpg) + GeomStar$new(mapping = aes(r = hwy, angle = cty, x = 0, y = 0, 
  group = cyl)) + facet_wrap(~cyl)

ggplot(test.data) + geom_star(mapping = aes(x = 0, y = 0, 
  r = Catholic, angle = Fertility, group = lat)) + facet_wrap(~lat)

ggplot(nasa) + ply_aes(geom_star(aes(r = ozone, angle = date, x = 0, y = 0, 
  fill = mean(temperature))), c("lat")) + facet_wrap(~ lat)

ggplot(nasa) + map_nasa +
  glyph(geom_star(aes(r = ozone, angle = date, x = 0, y = 0, 
  fill = mean(temperature))), aes(long[1], lat[1]), c("long", "lat"))

ggplot(mpg) + ply_aes(geom_star(mapping = aes(x = cyl[1], y = 1, 
  r = hwy, angle = cty, fill = mean(hwy), group = cyl)))

ggplot(nasa) + geom_star(aes(r = ozone, angle = date, x = 0, y = 0))

# new gridding
ggplot(test.data) + geom_point(aes(Fertility, Education))
ggplot(test.data) + 
  grid(geom_point(aes(Fertility, Education)), x.nbin = 10, y.nbin = 10, 
       ref= ref_box(aes(fill = mean(Catholic))))

cheap.diamonds <- subset(diamonds, price <= 5000 & price >= 600)
ggplot(cheap.diamonds) +
  grid(geom_bar(aes(x = color, fill = color), position = "dodge"),
    grid.aes = aes(x = carat, y = price), x.nbin = 10, y.nbin = 14,
    y_scale = free, height.adjust = 0.5, width.adjust = 0.5,
    ref = ref_box(aes(color = length(color))))


# trying out geom_coxcomb
p <- ggplot(mpg) + 
  geom_bar(aes(x = trans, fill = as.factor(cyl)), position = "stack") + 
  coord_polar() +
  facet_wrap(~year)

mpg$lat <- sample(1:4, nrow(mpg), replace = TRUE)
ggplot(mpg) + GeomCoxcomb$new(mapping = aes(angle = trans, fill = lat, 
  group = lat)) + facet_wrap(~cyl)
ggplot(mpg) + geom_coxcomb(aes(angle = trans, fill = lat))
        , group = lat))
###########################################
###          not yet working            ###
###########################################
load_all("../ggplyr")

ggplot(seasons) + 
  glyph(
    geom_line(aes(x = time, y = pred)), 
    major = aes(lon[1], lat[1]), glyph.by = "stn", 
    height = 1, width = 2,
    ref = ref_box(aes(fill = avg)), merge = TRUE)



###########################################
###             obsolete                ###
###########################################
load_all("../ggplyr")
# splitting by grids - obsolete
ggplot(test.data) + geom_point(aes(Fertility, Agriculture, color = Catholic))
ggplot(test.data) + geom_point(aes(grid(Fertility, 3), grid(Agriculture, 3), 
  color = Catholic))
ggplot(test.data) + geom_point(aes(grid(Fertility, 3), grid(Agriculture, 3), 
  color = Catholic), position = position_jitter(width = 1, height = 1))
ggplot(test.data) + glyph(geom_point(aes(Fertility, Agriculture, 
  color = Catholic)), aes(I(grid(Fertility, 3)), I(grid(Agriculture, 3))), 
  grid_by(Fertility, 3, Agriculture, 3))