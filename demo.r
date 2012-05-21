# ggplyr features demo - sift for tests

# setwd("/Users/GarrettsiMac/Dropbox/research")
# setwd("/Users/garrettgrolemund/Dropbox/research")
library(devtools)
load_all("ggplyr")

#######################################
###       transform a graph         ###
#######################################

# make a graph
head(test.data)
(p <- qplot(Fertility, Agriculture, data = test.data, color = Education))

# change mappings
ggtransform(p, mapping = aes(size = Catholic))

# nullify mappings, change axis
ggtransform(p, mapping = aes(y = Education, color = NULL, size = Catholic))

# change parameter
ggtransform(p, mapping = aes(y = Education, color = NULL, size = Catholic), shape = 15)




#########################################################
###       use plyr to make groupwise mappings         ###
#########################################################

# Our data is (arbitrarily) grouped into four locations
qplot(Fertility, Agriculture, data = test.data, color = interaction(lat, long))

# use plyr to create a graph
# the aesthetics can be calculated groupwise
dgply(test.data,
	.split = c("lat", "long"),
	.apply = fun(geom_point, "data", mapping = aes(x = Fertility, y = Agriculture), 		size = 3),
	.combine = fun(in_place, "layers", mapping = aes(color = rank(Education))))


data <- group_by(test.data, c("lat", "long"))
lapply(data, fun(qplot, "data", x = Fertility, y = Agriculture, color = rank(Education), size = I(3), xlim = range(test.data$Fertility), ylim = range(test.data$Agriculture)))


dgply(mpg,
	.split = c("year"),
	.apply = fun(geom_bar, "data", mapping = aes(x = trans)),
	.combine = fun(in_place, "layers", mapping = aes(fill = mean(hwy)), 
		position = "dodge"))

# compare with	
ggplot() + geom_bar(aes(x = trans, fill = mean(hwy), group = year), data = mpg, position = "dodge")



################################################
###       use plyr to embed subplots         ###
################################################
qplot(Fertility, Education, data = test.data, size = I(3))

# relocate data and add within group aes
# MISSING LEGEND
dgply(test.data,
	.split = c("lat", "long"),
	.apply = fun(geom_point, "data", mapping = aes(x = Fertility, y = Education, color = rank(Catholic)), size = 3),
	.combine = fun(nest, "layers", mapping = aes(x = mean(Fertility), 
		y = mean(Education)), width = 5, height = 5))

# NOT WORKING - smooth won't work on majors that only give back one value per group
# relocate data and add across group aes
dgply(test.data,
	.split = c("lat", "long"),
	.apply = fun(geom_smooth, "data", mapping = aes(x = Fertility, y = Education), method = "lm", se = F),
	.combine = fun(nest, "layers", mapping = aes(x = mean(Fertility), 
		y = mean(Education), color = interaction(long, lat)[1])))


# big data		
qplot(surftemp, temperature, data = nasa)

dgply(nasa,
	.split = c("lat", "long"),
	.apply = fun(geom_point, "data", mapping = aes(x = surftemp, y = temperature), 
		size = 1/5),
	.combine = fun(nest, "layers", mapping = aes(x = long, 
		y = lat), width = 3, height = 3),
	.progress = "text")

qplot(surftemp, temperature, data = nasa, geom = "smooth")

# NOT WORKING
dgply(nasa,
	.split = c("lat", "long"),
	.apply = fun(geom_smooth, "data", mapping = aes(x = surftemp, y = temperature), 
		se = F),
	.combine = fun(nest, "layers", mapping = aes(x = long, 
		y = lat), se = F, x_scale = rescale01, y_scale = rescale01, 
		width = 1, height = 1),
	.progress = "text")

		
dgply(nasa,
	.split = c("lat", "long"),
	.apply = fun(geom_point, "data", mapping = aes(x = 1, y = 1)),
	.combine = fun(nest, "layers", mapping = aes(x = mean(pressure), 
		y = mean(ozone), color = interaction(long[1], lat[1])), width = 3, height = 3),
	.progress = "text")
		
		
		
# nest on just one axiis - NOT WORKING
dgply(test.data,
	.split = c("lat", "long"),
	.apply = fun(geom_point, "data", mapping = aes(x = Fertility, y = Education), 
		size = 3),
	.combine = fun(nest, "layers", mapping = aes(x = mean(Fertility), color = rank(Catholic))))	
# needs fixed




# now with bars
qplot(trans, data = mpg, geom = "bar", fill = year, group = year, position = "dodge")

# NOT WORKING
dgply(mpg,
	.split = c("year"),
	.apply = fun(geom_bar, "data", mapping = aes(x = trans, fill = year)),
	.combine = fun(nest, "layers", mapping = aes(x = mean(hwy), y = mean(cty)), position = "dodge"))
		
		
##################################################
###         recreating model glyphs            ###
##################################################

lgply(m1,
	.apply = fun(geom_point, "data", mapping = aes(x = long[1], y = lat[1])),
	.combine = fun(nest, "layers", mapping = aes(x = mean(temperature), 
		y = mean(surftemp), 
		color = max(surftemp - temperature) == max(abs(surftemp - temperature)))
	)
)



##################################################
###               testing nest                 ###
##################################################
# goal: return a layer that has a subplots attribute which 
# contains subplot x and y positions by .gid

splits <- group_by(test.data, c("lat", "long"))
layers <- llply(splits, fun(geom_histogram, "data", mapping = aes(x = Fertility, fill = long)))
p <- nest(layers, mapping = aes(x = mean(Fertility), y = mean(Education)))