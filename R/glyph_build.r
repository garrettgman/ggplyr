#' Build a glyphs object for rendering
#' 
#' glyph_build takes a glyph plot object (class glyphs), and performs all steps 
#' necessary to produce an object that can be rendered. This function outputs 
#' two pieces: a list of data frames (one for each layer), and a panel object, 
#' which contain all information about axis limits, breaks, etc.
#' 
#' @keywords internal
#' @param layer an object of class glayer
#' @seealso \code{\link{print.glyphs}} for functions that contain the complete 
#' set of steps for generating a glyphs plot
#' @export
glyph_build <- function(plot){
  if (length(plot$layers) == 0) stop("No layers in plot", call.=FALSE)
  if (!identical(plot$facet, ggplot2::facet_null())) {
  	stop("glyphs do not support facetting", call. = FALSE)
  }
	
  plot <- ggplot2:::plot_clone(plot)
  layers <- plot$layers
  layers <- propogate_data(layers, plot$data)
	
  # separate into glayers and normal layers
  gls <- unlist(lapply(layers, is.glayer))
  if (all(!gls)) return(ggplot2::ggplot_build(plot))
  if (all(gls) && sum(gls) == 1) return(glayer_build(layers[[gls]]))
  glayers <- layers[gls]
  plot$layers <- layers[!gls]
  gl.order <- seq_along(layers)[gls]
  nl.order <- seq_along(layers)[!gls]
	
  # build normal layers
  normal <- NULL
  if (length(plot$layers) > 0) {
	normal <- ggplot2::ggplot_build(plot)
  }
	
  # build glyph layers (embedded plots)
  embedded <- list()
  for (i in seq_along(glayers)) {
    embedded[[i]] <- glayer_build(glayers[[i]])
  }
	
	
  ### combine the builds
	
  # plot
  build <- embedded[[1]]
	
  # data
  # take care to order
  edata <- lapply(embedded, function(bd) bd$data[[1]])
  data <- list()
  data[gl.order] <- edata
  data[nl.order] <- normal$data
	
  # panel
  xspan <- range(unlist(lapply(data, function(df) df[names(df) %in% .x_aes])))
  yspan <- range(unlist(lapply(data, function(df) df[names(df) %in% .y_aes])))
  panel <- ggplot2::ggplot_build(ggplot2::qplot(xspan, yspan))$panel
	
  # scales 
  # collect all unique scales
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
  # take care to order
  gl.layers <- build$plot$layers
  for (i in seq_along(embedded[-1])) {
    gl.layers <- c(gl.layers, embedded[[i + 1]]$plot$layers)
  }
  layers[gl.order] <- gl.layers
  layers[nl.order] <- normal$plot$layers
  
  # labels 
  # collect all unique labels
  labels <- build$plot$option$labels
  for (i in seq_along(embedded[-1])) {
  	new.labels <- embedded[[i+1]]$plot$options$labels
  	unique <- !(names(new.labels) %in% names(labels))
  	labels <- c(labels, new.labels[unique])
  }
  norm.labels <- normal$plot$options$labels
  unique <- !(names(norm.labels) %in% names(labels))
  labels <- c(labels, norm.labels[unique])
	
  # make build
  build$data <- data
  build$panel <- panel
  build$plot$scales$scales <- scales
  build$plot$layers <- layers
  build$plot$options$labels <- labels
	
  build
}

#' Ensure each layer contains a data set
#' 
#' propogate_data checks each layer for a data set. If none is found it assigns 
#' a copy of the plot level data set to the layer. propogate_data avoids the 
#' side effects of ggplot2:::map_layout, which performs a similar function.
#' @param layers ggplot2 layer objects
#' @param plot_data the global data set for a ggplot2 plot
propogate_data <- function(layers, plot_data) {
	ensure_data <- function(layer){
		if (inherits(layer$data, "waiver")) {
			layer$data <- plot_data
		}
		layer
	}
	lapply(layers, ensure_data)
}