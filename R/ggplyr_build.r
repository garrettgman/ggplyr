#' Build a ggplyr object for rendering
#'
#' This function takes the plot object, and performs all steps necessary to produce an object that can be rendered.
ggplyr_build <- function(plot){
	
	if (length(plot$layers) == 0) stop("No layers in plot", call.=FALSE)
	if (!identical(plot$facet, facet_null())) {
		stop("ggplyr does not support facetting", call. = FALSE)
	}
	
	plot <- ggplot2:::plot_clone(plot)
	layers <- plot$layers
	
	# separate into gglayers and normal layers
	ggs <- unlist(lapply(layers, is.gglayer))
	if (all(!ggs)) return(ggplot2:::ggplot_build(plot))
	gglayers <- layers[ggs]
	plot$layers <- layers[!ggs]
	
	# build normal layers
	plot$layers <- layers[!ggs]
	normal <- ggplot2:::ggplot_build(plot)
	
	# build embeded layers
	embedded <- list()
	for (i in seq_along(gglayers)) {
		embedded[[i]] <- gglayer_build(gglayer)
	}
	
	# combine the builds
	build <- embedded[[1]]
	
	# data
	lapply(build, function
	
	# calculate correct panel
	# collect all unique scales
	# collect all data
	
	




is.gglayer <- function(x) {
	is(x, "gglayer")
}