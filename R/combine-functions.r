#' Combine subplots without adjusting locations
#'
#' @param layers a list of ggplot2 layer objects. These should be \code{\link{proto}} objects that work to create a graph when added to ggplot().
#' @param mapping an uneval object created with \code{\link{aes}}
#' @param x_scale
#' @param y_scale
#' @param width
#' @param height
#' @param reference
#' @param position
#' @param ... 
in_place <- function(layers, mapping = NULL, position = NULL, ...) {

	# build prototype of layer to return
	prototype <- layer_clone(layers[[1]])
	prototype$data <- individual_data(layers)
	prototype$plyr <- TRUE
	prototype$mapping <- update_aes(mapping, prototype$mapping)
	prototype$mapping$.gid <- as.name(".gid")
	
	old_par <- prototype$geom_params
	new_par <- list(...) 
	params <- c(new_par, old_par[setdiff(names(old_par), names(new_par))])
	prototype$geom_params <- params
	
	if (!is.null(position)) {
		prototype$position <- get(paste("position", position, sep = "_"))()
	}
	
	prototype$compute_aesthetics <- compute_plyr_aesthetics
	
	prototype
}


nest <- function(layers, mapping, x_scale = identity, y_scale = identity, width = rel(1), height = rel(1), reference = NULL, position = "identity", ...) {
	
	# build out subplots
	data <- individual_data(layers)
	prototype <- layer_clone(layers[[1]])
	prototype$mapping$.gid <- as.name(".gid")	
	prototype$data <- data
	prototype$plyr <- TRUE
	prototype$compute_aesthetics <- compute_plyr_aesthetics
	prototype$position <- get(paste("position", position, sep = "_"))()
	sdata <- suppressMessages(build_subplots(prototype)) #what happens to .gid?
	
	# build a .combine fun
	.combine <- nest_subplots(x_scale = x_scale, y_scale = y_scale, 
		width = width, height = height, sdata = sdata)
		
	# build prototype of layer to return
	prototype$mapping <- super_aes(mapping, prototype$mapping)
	prototype$subplots <- list(data = sdata, fun = .combine)
	old_par <- prototype$geom_params
	new_par <- list(...) 
	params <- c(new_par, old_par[setdiff(names(old_par), names(new_par))])
	prototype$geom_params <- params
	
	if (!is.null(position)) {
		prototype$position <- get(paste("position", position, sep = "_"))()
	}
	
	if (is.null(reference)) {
		gglayer(prototype)
	} else {
		ref.layer <- reference(prototype)
		list(ref.layer, gglayer(prototype))
	}
	
}

subplot_info <- function(layers, mapping, ...) {
	
	# transfer all non-xy mappings to the subplots
	non.xy <- !(names(mapping) %in% c(.x_aes, .y_aes))
	gmaps <- mapping[non.xy]
	maps <- layers[[1]]$mapping
	maps[names(gmaps)] <- gmaps
	maps$.gid <- as.name(".gid")
	
	# update parameters
	old_par <- layers[[1]]$geom_params
	new_par <- list(...) 
	params <- c(new_par, old_par[setdiff(names(old_par), names(new_par))])
	
	# prepare build
	prototype <- layer_clone(layers[[1]])
	prototype$data <- individual_data(layers)
	prototype$mapping <- maps
	prototype$plyr <- TRUE
	prototype$geom_params <- params
	prototype$compute_aesthetics <- compute_plyr_aesthetics
	
	build <- suppressMessages(ggplot_build(ggplot() + prototype + facet_wrap(~ .gid)))
	
	scales <- non_xy_scales(build$plot$scales$scales)
	
	list(data = build$data[[1]], scales = scales)
}

	
non_xy_scales <- function(scales) {
	scales <- name_scales(scales)
	non.xy <- setdiff(names(scales), c("x", "y"))
	scales <- scales[non.xy]
	
	names(scales) <- NULL
	if (length(non.xy) == 0) {
		return(NULL)
	}
	
	scales
}	

name_scales <- function(scales) {
	get_name <- function(scale) {
		scale$aesthetics[[1]]
	}
	scale.names <- unlist(lapply(scales, get_name))
	names(scales) <- scale.names
	scales
}
	












#' update_aes builds a mapping to correctly guide a layer that has combined individual subplots into a single metaplot.
#'
#' @keywords internal
#' @param imap The mapping for each individual subplot
#' @param gmap The mapping for the single, combined plot
#' @export
super_aes <- function(gmap, imap) {
	
	# remove xy mappings from imap
	imap <- imap[setdiff(names(imap), c(.x_aes, .y_aes))]
	
	# combine global and individual subplot mappings
	map <- combine_aes(gmap, imap)
		
	# change group aes
	map$group <- add_gid(map$group)
	
	map
}

#' update_aes builds a mapping to correctly guide a layer that has combined individual subplots into a single metaplot.
#'
#' @keywords internal
#' @param imap The mapping for each individual subplot
#' @param gmap The mapping for the single, combined plot
#' @export
update_aes <- function(imap, gmap) {
	
	# combine global and individual subplot mappings
	map <- combine_aes(gmap, imap)
		
	# change group aes
	map$group <- add_gid(map$group)
	
	map
}

nest_subplots <- function(x_scale, y_scale, width, height, sdata) {
	function(data) {

		gdata <- globalize(.gid_coords(data))
		sdata <- join(sdata, gdata, by = ".gid")
		
		# locate x and y related variables in subplot data
		xvar <- get_xs(sdata)
		yvar <- get_ys(sdata)
		
		# scale if necessary
		if (!identical(x_scale, identity) || 
			!identical(y_scale, identity)) {
				sdata <- ddply(sdata, ".gid", 
					function(df) {
						df[xvar] <- lapply(df[xvar], x_scale)
						df[yvar] <- lapply(df[yvar], y_scale)
						df
					}
				)
		}
		
		if (is.rel(width)) {
			width <- diff(range(sdata$X)) / max(sdata$.gid) * unclass(width)
		}

		if (is.rel(height)) {
			height <- diff(range(sdata$Y)) / max(sdata$.gid) * unclass(height)
		}
	
		# update x and y related variables
		fun.x <- function(x) sdata$X + rescale11(x) * width
		fun.y <- function(y) sdata$Y + rescale11(y) * height
		sdata[xvar] <- lapply(sdata[xvar], fun.x)
		sdata[yvar] <- lapply(sdata[yvar], fun.y)
		
		sdata <- sdata[setdiff(names(sdata), c("X", "Y"))]
		data[names(sdata)] <- sdata # are the orders the same?
		data
	}
}

.gid_coords <- function(data) {
	data <- data[c(".gid", "x", "y")]
	n <- length(unique(data$.gid))
	data <- unique(data)
	
	if (nrow(data) != n) {
		stop("Each subplot must be mapped to a single x and y variable.")
	}
	
	data
}
		


build_subplots <- function(layer) {
	
	plot <- ggplot() + layer
	if (length(plot$layers) == 0) stop("No layers in subplot", 
		call.=FALSE)
 
	layers <- plot$layers
	layer_data <- list(layers[[1]]$data)
	
	scales <- plot$scales
	# Apply function to layer and matching data
	dlapply <- function(f) {
  		list(f(d = data[[1]], p = layers[[1]]))
	}
	
	# Initialise panels, add extra data for margins & missing facetting
	# variables, and add on a PANEL variable to data
	panel <- ggplot2:::new_panel()
	panel <- ggplot2:::train_layout(panel, plot$facet, layer_data, 
		plot$data)
		
	# data is a list of data sets. All are manipulated in one pass
	# figures out data for layer, assigns rows to panels   
	data <- ggplot2:::map_layout(panel, plot$facet, layer_data, 
		plot$data)
		
	# Compute aesthetics to produce data with generalised variable names
	# replaces data set with just the explicitly stated aesthetics
	data <- dlapply(function(d, p) p$compute_aesthetics(d, plot))

	# adds a group column based on group/fill/color aesthetic
	data <- lapply(data, ggplot2:::add_group)
	
	# Transform all scales
	# transforms data based on the scale settings?
	data <- lapply(data, ggplot2:::scales_transform_df, scales = scales)
	
	# Map and train positions so that statistics have access to ranges
	# and all positions are numeric
	scale_x <- function() scales$get_scales("x")
	scale_y <- function() scales$get_scales("y")
	
	# panel gets scales here
	panel <- ggplot2:::train_position(panel, data, scale_x(), scale_y())
	
	# characters and factors are converted to numbers (in x and y axes?)
	data <- ggplot2:::map_position(panel, data, scale_x(), scale_y())
	
	# Apply and map statistics
	# fills out the columns with aesthetics generated from the stat
	# combines rows according to stat
	# .gid disappears - fix is to use ddply
	calc_stat_by_gid <- function(data) {
		ggplot2:::calculate_stats(panel, list(data), layers)[[1]]
	}
	data <- list(ddply(data[[1]], ".gid", calc_stat_by_gid))
	
	# calculates missing aesthetics from stat (like y)
	data <- dlapply(function(d, p) p$map_statistic(d, plot)) 
	
	# seems like groups were already in order...
	data <- lapply(data, ggplot2:::order_groups)
	
	# Reparameterise geoms from (e.g.) y and width to ymin and ymax
	# builds new aesthetic columns from stat aesthetic columns
	# predicts from the stat which aes grid will need to draw
	data <- dlapply(function(d, p) p$reparameterise(d))
	
	# Apply position adjustments
	# changes x, reorders rows
	data <- dlapply(function(d, p) p$adjust_position(d))  
	
	xy(data[[1]])
}


xy <- function(df) {
	xvars <- get_xs(df)
	yvars <- get_ys(df)
	df[c(".gid", "PANEL", xvars, yvars)]
}