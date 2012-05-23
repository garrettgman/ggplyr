#' gglayer_build prepares an embedded layer (class gglayer) for plotting
gglayer_build <- function(layer) {

	if (!("embed" %in% ls(layer))) {
		stop("layer does not have embedded subplots")
	}
	 
	layer <- layer_clone(layer)
	
	minor <- ggplot_build(ggplot() + layer + facet_wrap(".gid")) 

	# data
	data <- unpanel(minor$data[[1]])
	data <- layer$embed$fun(data)
	data$PANEL <- 1L
	
	# panel
	xspan <- range(unlist(data[names(data) %in% .x_aes]))
	yspan <- range(unlist(data[names(data) %in% .y_aes]))
	panel <- ggplot_build(qplot(xspan, yspan))$panel

	# scales
	scales <- minor$plot$scales$scales
	scales[[which_x(scales)]] <- panel$x_scales[[1]]
	scales[[which_y(scales)]] <- panel$y_scales[[1]]
	
	# axis labels
	if (!is.null(layer$embed$major_aes)) {
		labels <- labs(layer$embed$major_aes)
		minor$plot$options$labels[c("x", "y")] <- labels[c("x", "y")]
	}
	if (!is.null(layer$embed$ref_aes)) {
		lab.names <- names(minor$plot$options$labels)
		new <- setdiff(names(layer$embed$ref_aes), lab.names)
		minor$plot$options$labels[new] <- layer$embed$ref_aes[new]
	}
		
	
	minor$data <- list(data)
	minor$panel <- panel
	minor$plot$facet <- facet_null()
	minor$plot$scales$scales <- scales
	
	minor
}


unpanel <- function(df) {
	df$.gid <- as.numeric(as.character(df$PANEL))
	df$PANEL <- NULL
	df
}


which_x <- function(scales) {
	vars <-  names_scales(scales)
	which(vars == "x")
}


which_y <- function(scales) {
	vars <- names_scales(scales)
	which(vars == "y")
}
	
	