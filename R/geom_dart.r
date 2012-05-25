dartsGrob <- function(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), angle = 0, length = 2, size = 1, id = NULL, name = NULL, default.units = "native", gp = gpar(fill = "grey20"), vp = NULL) {

	if (length(x) != length(y)) {
		xy <- data.frame(x, y)
		x <- xy$x
		y <- xy$y
	}
	
	size <- 0.0075 * size
		
	
	l <- sqrt((length + 1)^2 * size^2) / 2
	lambda <- atan(1 / length)
		
	ax <- x + l * cos(angle + pi - lambda)
	ay <- y + l * sin(angle + pi - lambda)
		
	bx <- x + l * cos(angle + pi + lambda)
	by <- y + l * sin(angle + pi + lambda)
		
	cx <- x + length * size * cos(angle) / 2
	cy <- y + length * size * sin(angle) / 2		
		
	group <- seq_along(x)
	

	# might want different units here	
	polygonGrob(x = c(ax, bx, cx, ax), y = c(ay, by, cy, ay), id = c(group, group, group, group),
		default.units = default.units, gp = gp, vp = vp)
}




geom_dart <- function (mapping = NULL, data = NULL, stat = "identity",
position = "identity", ...) { 
	GeomDart$new(mapping = mapping, data = data, stat = stat, 
		position = position, ...)
}


GeomDart <- proto(ggplot2:::Geom, {
	objname <- "dart"
	
	reparameterise <- function(., df, params) {

		df$size <- df$size %||% params$size %||% (resolution(df$y, FALSE) * 0.2)
		df$width <- df$width %||% params$width %||% (resolution(df$x, FALSE) * 2)
		df$angle <- df$angle %||% params$angle %||% 0			
		df				
	}
	
	draw <- function(., data, scales, coordinates, ...) {
		ggname(.$my_name(), gTree(children=gList(
			with(coord_munch(coordinates, data, scales), 
				dartsGrob(x, y, default.units="native", angle = angle, 
				length = width, size = size, gp=gpar(col=colour,
				fill=alpha(fill, alpha), lwd=size * .pt, lty=linetype))
			)
		)))
	}	

	default_stat <- function(.) StatIdentity
	default_aes <- function(.) aes(weight=1, colour="NA", fill="grey20", 
		alpha = NA, linetype = "solid", angle = 0, width = NULL, size = NULL)
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