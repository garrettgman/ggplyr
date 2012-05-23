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
	if (all(!ggs)) return(ggplot_build(plot))
	if (all(ggs) && sum(ggs) == 1) return(gglayer_build(layers[[ggs]]))
	gglayers <- layers[ggs]
	plot$layers <- layers[!ggs]
	
	# build normal layers
	normal <- NULL
	if (length(plot$layers) > 0) {
		normal <- ggplot_build(plot)
	}
	
	# build embeded layers
	embedded <- list()
	for (i in seq_along(gglayers)) {
		embedded[[i]] <- gglayer_build(gglayers[[i]])
	}
	
	# combine the builds
	build <- embedded[[1]]
	
	# data
	edata <- lapply(embedded, function(bd) bd$data[[1]])
	data <- c(normal$data, edata)
	
	# panel
	xspan <- range(unlist(lapply(data, function(df) df[names(df) %in% .x_aes])))
	yspan <- range(unlist(lapply(data, function(df) df[names(df) %in% .y_aes])))
	panel <- ggplot_build(qplot(xspan, yspan))$panel
	
	# scales - collect all unique scales
	scales <- build$plot$scales$scales
	scales[[which_x(scales)]] <- panel$x_scales[[1]]
	scales[[which_y(scales)]] <- panel$y_scales[[1]]
	scale.names <- names_scales(scales)
	for (i in seq_along(embedded[-1])) {
		escales <- embedded[[i + 1]]$plot$scales$scales
		unique <- !(names_scales(escales) %in% scale.names)
		scales <- c(scales, escales[unique])
		scale.names <- names_scales(scales)
	}
	nscales <- normal$plot$scales$scales
	unique <- !(names_scales(nscales) %in% scale.names)
	scales <- c(scales, nscales[unique])
	
	# layers
	layers <- build$plot$layers
	for (i in seq_along(embedded[-1])) {
		layers <- c(embedded[[i]]$plot$layers, layers)
	}
	layers <- c(normal$plot$layers, layers)
	
	build$data <- data
	build$panel <- panel
	build$plot$scales$scales <- scales
	build$plot$layers <- layers
	
	build
}




is.gglayer <- function(x) {
	"embed" %in% ls(x)
}