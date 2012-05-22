

	panel <- major$panel
	
	scales <- minor$plot$scales$scales
	SCALES <- major$plot$scales$scales
	scales[which_xy(scales)] <- SCALES[which_xy(SCALES)]
	minor$plot$scales$scales <- scales # necessary?
	 
	minor$plot$facet <- facet_null()
	minor$plot$options$labels[c("x", "y")] <- major$plot$options$labels[c("x", "y")]
	
	ggplot2:::reset_scales(panel)
	scale_x <- function() minor$plot$scales$get_scales("x")
    scale_y <- function() minor$plot$scales$get_scales("y")
    panel <- ggplot2:::train_position(panel, list(data), scale_x(), scale_y())
    data <- ggplot2:::map_position(panel, list(data), scale_x(), scale_y())
	panel <- ggplot2:::train_ranges(panel, minor$plot$coordinates)
	
	minor$data <- data
	minor$panel <- panel
	
	minor
}
