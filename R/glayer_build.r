#' glayer_build prepares an glyph layer (class glayer) for plotting
glayer_build <- function(layer) {
  if (!("embed" %in% ls(layer))) {
    stop("layer does not have embedded subplots")
  }
	 
  layer <- layer_clone(layer)
  layer$data <- layer$assign_glyphs(layer$data)
  minor <- ggplot_build(ggplot() + layer + facet_wrap("GLYPH")) 
	
	
  ### combine subplots (minor) into single plot
  # data
  data <- unpanel(minor$data[[1]])
  data <- layer$combine_glyphs(data)
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
  if (!is.null(layer$embed$major.aes)) {
    labels <- labs(layer$embed$major.aes)
    minor$plot$options$labels[c("x", "y")] <- labels[c("x", "y")]
  }
		
  # make build
  minor$data <- list(data)
  minor$panel <- panel
  minor$plot$facet <- facet_null()
  minor$plot$scales$scales <- scales
	
  minor
}


unpanel <- function(df) {
  df$GLYPH <- as.numeric(as.character(df$PANEL))
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

#' Returns the first aes of a scale, to use as a name
names_scales <- function(scales) {
  unlist(lapply(scales, function(s) s$aesthetics[[1]]))
}	
	