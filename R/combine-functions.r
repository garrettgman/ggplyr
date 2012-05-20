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


nest <- function(layers, mapping, x_scale = identity, y_scale = identity, width = rel(1), height = rel(1), reference = NULL, position = NULL, ...) {
	# build prototype of layer to return
	prototype <- layer_clone(layers[[1]])
	prototype$mapping <- update_aes(globalize(mapping), prototype$mapping)
	prototype$mapping$.gid <- as.name(".gid")	
	prototype$data <- individual_data(layers)
	prototype$plyr <- TRUE
	prototype$subplots <- list(x_scale = x_scale, y_scale = y_scale, 
		width = width, height = height)
	
	old_par <- prototype$geom_params
	new_par <- list(...) 
	params <- c(new_par, old_par[setdiff(names(old_par), names(new_par))])
	prototype$geom_params <- params
	
	if (!is.null(position)) {
		prototype$position <- get(paste("position", position, sep = "_"))()
	}
	
	prototype$compute_aesthetics <- compute_plyr_aesthetics
	prototype$adjust_position <- adjust_position_nest
	
	labels <- list()
	if (!is.null(mapping$x)) labels$x <- mapping$x
	if (!is.null(mapping$y)) labels$y <- mapping$y
	class(labels) <- "labels"
	
	if (is.null(reference)) {
		list(gglayer(prototype), labels)
	} else {
		ref.layer <- reference(prototype)
		list(ref.layer, gglayer(prototype), labels)
	}
	
}