#' Build a glayer for rendering
#' 
#' glayer_build takes a glyph layer (class glayer), and performs all steps 
#' necessary to produce an object that can be rendered. This function outputs 
#' two pieces: a list of data frames (one for each layer), and a panel object, 
#' which contain all information about axis limits, breaks, etc.
#' 
#' If the glayer is accompanied by regular layers, glayer_build will be used in 
#' conjunction with \code{\link{glyph_build}} to build the plot for rendering.
#' 
#' @keywords internal
#' @param layer an object of class glayer
#' @seealso \code{\link{print.glyphs}} and \code{\link{glyph_build}} for 
#' functions that contain the complete set of steps for generating a glyphs plot
#' @export
glayer_build <- function(layer) {
  if (!("embed" %in% ls(layer))) {
    stop("layer does not have embedded subplots")
  }
	 
  layer <- layer_clone(layer)
  layer$data <- layer$assign_glyphs(layer$data)
  minor <- ggplot2::ggplot_build(ggplot2::ggplot() + layer + 
    ggplot2::facet_wrap("GLYPH")) 

  ### combine subplots (minor) into single plot
  # data
  data <- unpanel(minor$data[[1]])
  data <- layer$combine_glyphs(data)
  data$PANEL <- 1L
	
  # panel
  xspan <- range(unlist(data[names(data) %in% .x_aes]))
  yspan <- range(unlist(data[names(data) %in% .y_aes]))
  panel <- ggplot2::ggplot_build(ggplot2::qplot(xspan, yspan))$panel

  # scales
  scales <- minor$plot$scales$scales
  scales[[which_x(scales)]] <- panel$x_scales[[1]]
  scales[[which_y(scales)]] <- panel$y_scales[[1]]
	
  # axis labels
  if (!is.null(layer$embed$major.aes)) {
    labels <- ggplot2::labs(layer$embed$major.aes)
    minor$plot$options$labels[c("x", "y")] <- labels[c("x", "y")]
  }
		
  # make build
  minor$data <- list(data)
  minor$panel <- panel
  minor$plot$facet <- ggplot2::facet_null()
  minor$plot$scales$scales <- scales
	
  minor
}

#' Format data from a facet plot to use in a glyph plot
#' 
#' unpanel replaces the PANEL variable of a data frame with a GLYPH variable. It 
#' adjusts the data frame's group variable to retain the grouping information 
#' provided by the PANEL variable.
#' 
#' @param df A data frame. Should be the output of a facetted plot built with 
#' \code{\link[ggplot2]{ggplot_build}}
#' @return A modified data frame. See Details.
unpanel <- function(df) {
  if (!is.null(df$group)) {
    df$group <- interaction(df$group, df$PANEL)
  } 
  df$GLYPH <- as.numeric(as.character(df$PANEL))
  df$PANEL <- NULL
  df
}

#' find x scale
#' 
#' which_x picks out the scale that controls x from a list of scales
#' @param scales A list of ggplot2 scales
which_x <- function(scales) {
  vars <-  names_scales(scales)
  which(vars == "x")
}

#' find y scale
#' 
#' which_y picks out the scale that controls y from a list of scales
#' @param scales A list of ggplot2 scales
which_y <- function(scales) {
  vars <- names_scales(scales)
  which(vars == "y")
}

#' Returns the first aes of a scale, to use as an identifier for the scale
#' @param scales a list of ggplot2 scales
names_scales <- function(scales) {
  unlist(lapply(scales, function(s) s$aesthetics[[1]]))
}	
	