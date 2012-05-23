#' nest combines subplot x and y data with plot level x and y coordinates to embed the subplots into a set of major axiis. nest calculates the plot level coordinates and returns a list object whcih is used by embed_layers to perform the actual comining. This list contains a function to transform the subplot data and a mapping to be used to correctly label the x and y axes.
nest <- function(data, major_aes = NULL, x_scale = identity, y_scale = identity, width = 1, height = 1) {
	
	xy.in <- c("x", "y") %in% names(major_aes)
	if (!all(xy.in)) {
		stop(paste("Missing required major_aes for nest:", 
			paste(c("x", "y")[!xy.in], collapse = ", ")), call. = FALSE)
	}
	
	minor_aes <- !(names(major_aes) %in% c("x", "y"))
	if (any(minor_aes)) {
		stop(paste("major_aes should only contain x and y aesthetics.", 
			paste(names(major_aes)[minor_aes], collapse = ", "), 
			"should be calculated within individual supblots."), call. = FALSE)
	}
	
	majors <- globalize(ddply(data, ".gid", apply_major, major_aes))	
	
	combine_fun <- function(data) {

		data <- join(data, majors, by = ".gid")
		
		xvar <- get_xs(data)
		yvar <- get_ys(data)
		
		# scale if necessary
		if (!identical(x_scale, identity) || 
			!identical(y_scale, identity)) {
				data <- ddply(data, ".gid", 
					function(df) {
						df[xvar] <- x_scale(df[xvar])
						df[yvar] <- y_scale(df[yvar])
						df
					}
				)
		}
		
		if (is.rel(width)) {
			width <- diff(range(data$X)) / max(data$.gid) * unclass(width)
		}

		if (is.rel(height)) {
			height <- diff(range(data$Y)) / max(data$.gid) * unclass(height)
		}
	
		# update x and y related variables
		# don't scale individually or xmin and xmax's will end up on top of one another
		data[xvar] <- vet(data$X) + rescale_11(data[xvar]) * width
		data[yvar] <- vet(data$Y) + rescale_11(data[yvar]) * height
		
		data$X <- NULL
		data$Y <- NULL
		data
	}
	
	list(fun = combine_fun, major_aes = major_aes)
}


vet <- function(x) {
	if (is.character(x)) {
			x <- as.numeric(factor(x))
	}	
	if (is.factor(x)) {
		x <- as.numeric(x)
	}
	x
}