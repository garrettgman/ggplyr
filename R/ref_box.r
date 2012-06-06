#' Reference box glyph
#' 
#' ref_box creates a layer of reference boxes to be plotted behind a layer of 
#' glyphs. Each box spans the full width and height of the glyph. Reference 
#' boxes make it easier to determine the location of an object within a glyph 
#' and to compare  objects across glyphs. Reference boxes can also convey 
#' information on their own through fill, colour, alpha, linetype, and (line) 
#' size mappings. By default the fill and colour parameters of a reference box
#' match the grey and white color scheme of ggplot2 panels in 
#' \code{\link[ggplot2]{theme_grey}}.
#' 
#' ref_box is a second order function. It returns a function that can be used to 
#' create a layer of reference boxes with the specified mapping and parameters. 
#' The output of ref_box is intended to be passed as the reference argument for 
#' \code{\link{grid}} or \code{\link{glyph}}.
#' 
#' @param mapping An aesthetic mapping, usually constructed with 
#' \code{\link[ggplot2]{aes}}.
#' @param fill The color, as a character string, to be used as the fill if fill 
#' is not specified in the mapping
#' @param color The color, as a character string, to be used as the color if 
#' color is not specified in the mapping
#' @param ... other arguments to be used as parameters in the reference box 
#' layer
#' @seealso \code{\link{ref_hline}}, \code{\link{ref_vline}} and 
#' \code{\link{ref_points}}
#' @export
ref_box <- function(mapping = NULL, fill = "grey90", color = "white", ...) {	
  function(layer, type, major.aes, glyph.by = NULL, width = rel(1), 
  height = rel(1), merge.overlaps = FALSE, x.nbin = 10, y.nbin = 10) {
  	
  	def_aes <- ggplot2::aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1)
  	mapping <- c(mapping, def_aes[setdiff(names(def_aes), names(mapping))])
  	class(mapping) <- "uneval"
  	
    rlayer <- ply_aes(ggplot2::geom_rect(mapping = mapping, ...))
  	if (is.null(mapping$fill)) rlayer$geom_params$fill <- fill
  	if (is.null(mapping$colour)) rlayer$geom_params$colour <- color
  	if (!inherits(layer$data, "waiver")) rlayer$data <- layer$data
  	
    switch(type,
      glyph = glyph(rlayer, major.aes = major.aes, glyph.by = glyph.by, 
        width = width, height = height, merge.overlaps = merge.overlaps, 
        reference = NULL, .ref = TRUE), 
      grid = grid(rlayer, grid.aes = major.aes, x.nbin = x.nbin, 
        y.nbin = y.nbin, width.adjust = 1, height.adjust = 1, 
        reference = NULL, .ref = TRUE)
    )
  }
} 


#' Horizontal reference line glyph
#' 
#' ref_hline creates a layer of horizontal reference lines to be plotted behind 
#' a layer of glyphs. Each line spans the full width of the glyph. The thickness 
#' of the line can be adjusted with the thickness argument. Reference lines make it 
#' easier to determine the location of an object within a glyph and to compare 
#' objects across glyphs. Reference lines can also convey information on their 
#' own through fill, colour, alpha, linetype, and (line) size mappings. By 
#' default the fill parameter of a reference line is set to white.
#' 
#' ref_hline is a second order function. It returns a function that can be used 
#' to create a layer of reference lines with the specified mapping and 
#' parameters. The output of ref_hline is intended to be passed as the reference 
#' argument for \code{\link{grid}} or \code{\link{glyph}}.
#' 
#' @param mapping An aesthetic mapping, usually constructed with 
#' \code{\link[ggplot2]{aes}}.
#' @param thickness the thickness of the line as a proportion of the overall 
#' glyph height. Defaults to 0.2.
#' @param fill The color, as a character string, to be used as the fill if fill 
#' is not specified in the mapping
#' @param ... other arguments to be used as parameters in the reference box 
#' layer
#' @seealso \code{\link{ref_box}}, \code{\link{ref_vline}} and \code{\link{ref_points}}
#' @export
ref_hline <- function(mapping = NULL, thickness = 0.2, fill = "white", ...) {	
  function(layer, type, major.aes, glyph.by = NULL, width = rel(1), 
  height = rel(1), merge.overlaps = FALSE, x.nbin = 10, y.nbin = 10) {

  	def_aes <- list(xmin = -1, xmax = 1, ymin = -thickness/2, ymax = thickness/2)
  	mapping <- c(mapping, def_aes[setdiff(names(def_aes), names(mapping))])
  	class(mapping) <- "uneval"
  	
  	rlayer <- ply_aes(ggplot2::geom_rect(mapping = mapping, ...))
  	if (is.null(mapping$fill)) rlayer$geom_params$fill <- fill
  	if (!inherits(layer$data, "waiver")) rlayer$data <- layer$data
  	
  	switch(type,
  	  glyph = glyph(rlayer, major.aes = major.aes, glyph.by = glyph.by, 
  	    width = width, height = height, merge.overlaps = merge.overlaps, 
        reference = NULL, .ref = TRUE), 
  	  grid = grid(rlayer, grid.aes = major.aes, x.nbin = x.nbin, 
  	    y.nbin = y.nbin, width.adjust = 1, height.adjust = 1, 
  	    reference = NULL, .ref = TRUE)
  	)
  }
} 

#' Vertical reference line glyph
#' 
#' ref_vline creates a layer of vertical reference lines to be plotted behind a 
#' layer of glyphs. Each line spans the full height of the glyph. The thickness 
#' of the line can be adjusted with the thickness argument. Reference lines make it 
#' easier to determine the location of an object within a glyph and to compare 
#' objects across glyphs. Reference lines can also convey information on their 
#' own through fill, colour, alpha, linetype, and (line) size mappings. By 
#' default the fill parameter of a reference line is set to white.
#' 
#' ref_vline is a second order function. It returns a function that can be used 
#' to create a layer of reference lines with the specified mapping and 
#' parameters. The output of ref_vline is intended to be passed as the reference 
#' argument for \code{\link{grid}} or \code{\link{glyph}}.
#' 
#' @param mapping An aesthetic mapping, usually constructed with 
#' \code{\link[ggplot2]{aes}}.
#' @param thickness the thickness of the line as a proportion of the overall 
#' glyph width. Defaults to 0.2.
#' @param fill The color, as a character string, to be used as the fill if fill 
#' is not specified in the mapping
#' @param ... other arguments to be used as parameters in the reference box 
#' layer
#' @seealso \code{\link{ref_box}}, \code{\link{ref_hline}} and 
#' \code{\link{ref_points}}
#' @export
ref_vline <- function(mapping = NULL, thickness = 0.2, fill = "white", ...) {	
  function(layer, type, major.aes, glyph.by = NULL, width = rel(1), 
  height = rel(1), merge.overlaps = FALSE, x.nbin = 10, y.nbin = 10) {
  	
  	def_aes <- list(xmin = -thickness/2, xmax = thickness/2, ymin = -1, ymax = 1)
  	mapping <- c(mapping, def_aes[setdiff(names(def_aes), names(mapping))])
  	class(mapping) <- "uneval"
  	
  	rlayer <- ply_aes(ggplot2::geom_rect(mapping = mapping, ...))
  	if (is.null(mapping$fill)) rlayer$geom_params$fill <- fill
  	if (!inherits(layer$data, "waiver")) rlayer$data <- layer$data
  	
  	switch(type,
  	  glyph = glyph(rlayer, major.aes = major.aes, glyph.by = glyph.by, 
  	    width = width, height = height, merge.overlaps = merge.overlaps, 
        reference = NULL, .ref = TRUE), 
  	  grid = grid(rlayer, grid.aes = major.aes, x.nbin = x.nbin, 
  	    y.nbin = y.nbin, width.adjust = 1, height.adjust = 1,
        reference = NULL, .ref = TRUE)
  	)
  }
} 

#' Corner points reference glyph
#' 
#' ref_points creates a layer of reference points to be plotted behind a layer of 
#' glyphs. Each glyph is given four reference points, which are plotted one in 
#' each corner of the glyph's two dimensional range. Reference points make it 
#' easier to determine the location of an object within a glyph and to compare 
#' objects across glyphs. Reference lines can also convey information on their 
#' own through colour, alpha, and size mappings. By default the colour parameter 
#' of a reference line is set to white.
#' 
#' ref_points is a second order function. It returns a function that can be used 
#' to create a layer of reference points with the specified mapping and 
#' parameters. The output of ref_points is intended to be passed as the reference 
#' argument for \code{\link{grid}} or \code{\link{glyph}}.
#' 
#' @param mapping An aesthetic mapping, usually constructed with 
#' \code{\link[ggplot2]{aes}}.
#' @param colour The color, as a character string, to be used as the color if 
#' color is not specified in the mapping
#' @param size The size of the points, to be used as the color if size is not 
#' specified in the mapping
#' @param ... other arguments to be used as parameters in the reference box 
#' layer
#' @seealso \code{\link{ref_box}}, \code{\link{ref_hline}} and 
#' \code{\link{ref_vline}}
#' @export
ref_points <- function(mapping = NULL, colour = "white", size = 1/2, ...) {	
  function(layer, type, major.aes, glyph.by = NULL, width = rel(1), 
  height = rel(1), merge.overlaps = FALSE, x.nbin = 10, y.nbin = 10) {
  	
  	corner <- function(def_aes) {
  	  mapping <- c(mapping, def_aes[setdiff(names(def_aes), names(mapping))])
  	  class(mapping) <- "uneval"
  	
  	  rlayer <- ply_aes(ggplot2::geom_point(mapping = mapping, ...))
  	  if (is.null(mapping$fill)) rlayer$geom_params$colour <- colour
  	  if (is.null(mapping$size)) rlayer$geom_params$size <- size
  	  if (!inherits(layer$data, "waiver")) rlayer$data <- layer$data
  	
  	  switch(type,
  	    glyph = glyph(rlayer, major.aes = major.aes, glyph.by = glyph.by, 
  	      width = width, height = height, merge.overlaps = merge.overlaps, 
          reference = NULL, .ref = TRUE), 
  	    grid = grid(rlayer, grid.aes = major.aes, x.nbin = x.nbin, 
  	      y.nbin = y.nbin, width.adjust = 1, height.adjust = 1, 
  	      reference = NULL, .ref = TRUE)
  	  )
  	}
  	list(corner(ggplot2::aes(x = -1, y = -1)),
  	  corner(ggplot2::aes(x = -1, y = 1)),
  	  corner(ggplot2::aes(x = 1, y = -1)),
  	  corner(ggplot2::aes(x = 1, y = 1)))
  }
}  
