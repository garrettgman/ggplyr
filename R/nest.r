#' nest combines subplot x and y data with plot level x and y coordinates to embed the subplots into a set of major axiis. nest calculates the plot level coordinates and returns a list object whcih is used by embed_layers to perform the actual comining. This list contains a function to transform the subplot data and a mapping to be used to correctly label the x and y axes.
nest <- function(data, major_aes = NULL, x_scale = identity, y_scale = identity, width = 1, height = 1, reference = NULL, merge.overlaps = FALSE) {
	
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
	
	#subplot level data
	idata <- individual_data(data)	
	
	# major level data
	if (is.null(reference)) {
		global_aes <- major_aes
	} else {
		ref_aes <- just_refs(reference$mapping, major_aes)
		global_aes <- combine_aes(major_aes, ref_aes)
	}
	
	
	globals <- ddply(idata, ".gid", apply_major, global_aes)	
	
	# parse width and height
	if (is.rel(width)) {
		width <- diff(range(globals$x)) / max(globals$.gid) * unclass(width)
	}
	if (is.rel(height)) {
		height <- diff(range(globals$y)) / max(globals$.gid) * unclass(height)
	}
	
	# combine overlapping subplots
	if (merge.overlaps) {
		merges <- merge_overlaps(globals, width, height)
		idata <- update_gid(idata, merges)
		globals <- ddply(idata, ".gid", apply_major, global_aes)
	} 
	
	majors <- globalize(globals[c(".gid", names(major_aes))])
	
	# relocates subplots within major axes at build
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
	
		# update x and y related variables
		# don't scale individually or xmin and xmax's will end up on top of one another
		data[xvar] <- vet(data$X) + rescale_11(data[xvar]) * width
		data[yvar] <- vet(data$Y) + rescale_11(data[yvar]) * height
		
		data$X <- NULL
		data$Y <- NULL
		data
	}
	
	prototype <- layer_clone(data[[1]])
	prototype$data <- idata
	prototype$embed <- list(fun = combine_fun, major_aes = major_aes)
	
	if (is.null(reference)) {
		gglayer(prototype)
	} else {
		prototype$embed$ref_aes <- ref_aes
		refs <- globals[c(".gid", "x", "y", names(ref_aes))]
		ref_layer <- reference$make_layer(refs, width, height)
		list(ref_layer, gglayer(prototype))
	}
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

update_gid <- function(df, updates) {
	if (!(".gid" %in% names(df))) {
		stop("Cannot update gids: data.frame does not have .gid variable", 
			call. = FALSE)
	}
	
	obsolete <- df$.gid %in% names(updates)
	df$.gid[obsolete] <- updates[as.character(df$.gid[obsolete])]
	df
}