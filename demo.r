###################################################
###             geom_scatterplots               ###
###################################################


###########################################
###              working                ###
###########################################
ggplot(nasa) + glyph(geom_point(aes(x = surftemp, y = temperature), 
  size = 1/5), glyph.by = c("lat", "long"), width = 3, height = 3, 
  major = aes(x = long[1], y = lat[1]))

# embed points (continuous - no stats)
ggplot(test.data) + glyph(geom_point(aes(Fertility, Agriculture, 
  color = rank(Catholic)), size = 3), glyph.by = c("lat", "long"), 
  major = aes(Fertility, Education))

# embed bars (categorical with stats)
ggplot(mpg) + glyph(geom_bar(aes(x = trans, fill = year)), 
  aes(x = mean(displ), y = mean(cty)), c("year"), y_scale = free, 
  width = 1/4, height = 1/4)
  
# reference boxes
# boxes
ggplot(test.data) + glyph(geom_point(aes(Fertility, Agriculture, 
  color = rank(Catholic)), size = 3), glyph.by = c("lat", "long"), 
  major = aes(Fertility, Education), ref = ref_box(aes(fill = 
  mean(Catholic))))

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
  size = 1)) 

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
ggplot() + ply_aes(geom_point(aes(x = mean(temperature), y = mean(ozone), color = lat[1]), data = nasa), c("lat", "long"))
#vs.	
ggplot() + geom_point(aes(x = mean(temperature), y = mean(ozone), color = lat[1]), data = nasa)  

###########################################
###          not yet working            ###
###########################################
ggplot(nasa) + geom_scatterplots(aes(x = long, y = lat, minor.x = surftemp, 
  minor.y = temperature), size = 1/5, width = 3, height = 3, ref = "box")
