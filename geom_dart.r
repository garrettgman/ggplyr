#' @include plyr_aes.r
NULL

geom_dart <- function (mapping = NULL, data = NULL, glyphs = NULL, stat = "identity", position = "identity", ...) { 
	GeomDart$new(mapping = mapping, data = data, glyphs = glyphs, 
		stat = stat, position = position, ...)
}


GeomDart <- proto(ggplot2:::Geom, {
	objname <- "dart"
	
	compute_aesthetics <- plyr_aesthetics_summary
	
	reparameterise <- function(., df, params) {

		df$size <- df$size %||% params$size %||% 
			(resolution(df$y, FALSE) * 0.2)
		df$width <- df$width %||% params$width %||% 
			(resolution(df$x, FALSE) * 2)
		df$angle <- df$angle %||% params$angle %||% 0			
		df				
	}
	
	# finalize minor axes from non_position aes
	compute_glyphs <- function(df, layer) {
	
		l <- sqrt((df$width + 1)^2 * df$size^2) / 2
		lambda <- atan(1 / df$width)
		
		df$ax <- l * cos(df$angle + pi - lambda)
		df$ay <- l * sin(df$angle + pi - lambda)
		
		df$bx <- l * cos(df$angle + pi + lambda)
		df$by <- l * sin(df$angle + pi + lambda)
		
		df$cx <- df$width * df$size * cos(df$angle) / 2
		df$cy <- df$width * df$size * sin(df$angle) / 2
		
		df
		
	}
	
	# map minor axes into major axes. 
	# scale minor x and y to 0 - 1
	# use width and height to compute actual x, y
	# center at major x and y
	# Will need width, height, majors
	# majors already stored in data frame as x and y
	map_glyphs <- function(df, width, height) {
		if (any(table$df > 1)) {
			message("Multiple subplots mapped to same glyph. do you need to adjust your glyph's group criteria?")
		}
		
		data <- ddply(df, "GLYPH", function(df) {
			df <- df[c(1,1,1), ]
			df$x <- c(ax, bx, cx)
			df$y <- c(ay, by, cy)
			df[setdiff(names(df), c("ax", "bx", "cx", "ay", "by", 
				"cy"))]
		})
	}
	
	
	draw <- function(., data, scales, coordinates, ...) {
		ggname(.$my_name(), gTree(children=gList(
			with(coord_munch(coordinates, data, scales), 
				polygonGrob(x, y, default.units="native", id = GLYPH, 					gp=gpar(col=colour, fill = alpha(fill, alpha), 
					lwd = size * .pt, lty = linetype)
				)
			)
		)))
	}	

	default_stat <- function(.) StatIdentity
	default_aes <- function(.) aes(weight=1, colour="NA",
		fill="grey20", alpha = NA, linetype = "solid", angle = 0, 
		width = NULL, size = NULL)
	required_aes <- c("x", "y")
	guide_geom <- function(.) "polygon"

	draw_legend <- function(., data, ...)  {
		data <- aesdefaults(data, .$default_aes(), list(...))
		with(data, grobTree(
			rectGrob(gp = gpar(col = colour, fill = alpha(fill, alpha), 
				lty = linetype)),
			linesGrob(gp = gpar(col = colour, lwd = size * .pt, 
				lineend="butt", lty = linetype))
		))
	}
	
	new <- function(., mapping = NULL, data = NULL, glyphs = NULL, stat = NULL, 
		position = NULL, ...){
			
		lyr <- do.call("layer", list(mapping = mapping, data = data, 
			stat = stat, geom = ., position = position, ...))
		lyr$glyphs <- glyphs
		glayer(lyr)
		
	}

})


scale_angle <- function(..., range = c(0, 2 * pi)) {
	continuous_scale("angle", "angle_c", rescale_pal(range), ...)
}

scale_angle_continuous <- scale_angle

scale_angle_discrete <- function(...,range = c(0, 2 * pi)) {
	discrete_scale("angle", "angle_d",
		function(n) seq(range[1], range[2], length = n), ...)
}


scale_width <- function(..., range = c(0.1, 2)) {
	continuous_scale("width", "width_c", rescale_pal(range), ...)
}

scale_width_continuous <- scale_width

scale_width_discrete <- function(..., range = c(0.1, 2)) {
	discrete_scale("width", "width_d",
		function(n) seq(range[1], range[2], length = n), ...)
}