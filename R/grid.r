#' Turn a layer into a grid of embedded subplots.
#' 
#' grid turns an ordinary layer into an array of subplots laid out in a grid. 
#' Each subplot inherits the mappings, stat, and parameters of the initial 
#' layer. The mappings and stat for each subplot are mapped just to the data 
#' that occurs in the subplot's region on the grid. Any variables in the layer's 
#' data set may be used as x and y axes to create a grid on. These 'major' x and 
#' y axes need not correspond to the 'minor' x and y axes within each subplot. 
#' Major x and y axes are set with the grid.aes argument. Minor x and y axes are 
#' inherited from the layer's original mapping. To allow interpretation, the 
#' major axes of a gridded layer should correspond to the x and y aesthetics for 
#' any other (non - gridded) layers in the plot.
#' 
#' The layout of the grid is specified by separately choosing the number of bins 
#' to use along the x and y axes with the x.nbin and y.nbin arguments.
#' 
#' Gridded layers follow usual data inheritence rules for ggplot2 layer objects. 
#' If a layer contains no data, grid will use the global data set for the plot. 
#' This is the data set specified in \code{\link{ggplot}}.
#' 
#' @param layer a ggplot2 layer object. See \code{\link[ggplot2]{layer}}.
#' @param grid.aes An aesthetic mapping, usually constructed with 
#' \code{\link[ggplot2]{aes}}. This mapping determines which variables will be 
#' used to create the grid. grid will use the x grid aesthetic as the major x 
#' axis and the y grid aesthetic as the major y axis. All other aesthetics will 
#' be ignored - consider placing them in the layer's aes mapping instead.
#' @param x.nbin numeric. The number of bins to divide the major x axis into 
#' when creating the grid. Defaults to 10.
#' @param y.nbin numeric. The number of bins to divide the major y axis into 
#' when creating the grid. Defaults to 10.
#' @param x_scale function. The scaling to use for the x axis within each glyph. 
#' If x_scale equals \code{\link{identity}}(default), the x limits within each 
#' glyph will correspond to the range of x across all glyphs. This aids 
#' comparison because each glyph will use the same scale. If x_scale equals 
#' \code{\link{free}}, each glyph will use its own x scale. The limits of this 
#' scale will be set to the range of x values in that glyph.
#' @param y_scale function. y_scale behaves the same as x_scale but controls the 
#' scales for the y axis within each glyph.
#' @param width.adjust numeric. The proportion of horizontal space within a 
#' grid box that each subplot should occupy. Used to control overlapping and 
#' appearance. Each subplot is anchored to the bottom left corner of the grid 
#' box and then spans the proportion of the box specified by width adjust.
#' @param height.adjust numeric. The proportion of vertical space within a grid 
#' box that each subplot should occupy. Behaves the same as width.adjust.
#' @param reference function. Function used to create reference objects for 
#' the embedded plots. If NULL, no reference objects are used. Reference objects 
#' are plotted on a layer beneath the subplots. They provide a consistent frame 
#' of reference to aid comparisons across subplots. Functions that create 
#' reference objects include \code{\link{ref_box}}, \code{\link{ref_hline}}, 
#' \code{\link{ref_vline}}, and \code{\link{ref_points}}. By default, reference 
#' is set to ref_box, which creates the familiar mesh pattern associated with 
#' grids.
#' @param ply.aes logical. If TRUE (default) aesthetics are calculated 
#' separately for each subplot, as with \code{\link{ply_aes}}. If FALSE 
#' aesthetics are calculated based on entire data set for the layer.
#' @param .ref internal argument used for plotting reference objects.
#' @return an object of class glayer
#' @export 
grid <- function(layer, grid.aes = ggplot2::aes(), x.nbin = 10, y.nbin = 10, 
  x_scale = identity, y_scale = identity, width.adjust = 0.95, 
  height.adjust = 0.95, reference = ref_box(), ply.aes = TRUE, .ref = FALSE) {
  
  if (is.glayer(layer) || is.list(layer)) {
    stop("Cannot grid glayer", call. = TRUE)
  }
 # if ("plyr" %in% ls(layer)) {
  #  stop("grid and ply_aes used in wrong order.", call. = FALSE)
  #}
  
  # aes
  if(is.null(grid.aes$x)) {
    if (is.null(layer$mapping$x)) {
      stop("Grid requires an x aesthetic to be specified")
    }
    grid.aes$x <- layer$mapping$x
  }
  if(is.null(grid.aes$y)) {
    if (is.null(layer$mapping$y)) {
      stop("Grid requires a y aesthetic to be specified")
    }
    grid.aes$y <- layer$mapping$y
  }
    
  
  layer <- layer_clone(layer)
  layer$embed <- list(x_scale = x_scale, y_scale = y_scale,
    major.aes = grid.aes[c("x", "y")], grid_env = parent.frame(),
    x.nbin = x.nbin, y.nbin = y.nbin, w.adjust = width.adjust, 
    h.adjust = height.adjust, ref = .ref)
  layer$assign_glyphs <- assign_grid
  layer$combine_glyphs <- combine_glyphs
  if (.ref) layer$combine_glyphs <- combine_refs
  #layer$compute_aesthetics <- plyr_aesthetics
  
  if (is.null(reference)) {
    if (ply.aes) {
      ply_aes(glayer(layer))
    } else {
      glayer(layer)
    }
  } else {
    ref.layer <- reference(layer, "grid", grid.aes, x.nbin = x.nbin, 
      y.nbin = y.nbin)
    if (ply.aes) {
      list(ref.layer, ply_aes(glayer(layer)))
    } else {
      list(ref.layer, glayer(layer))
    }
  }
}


# Divides a layer's data into gridded subplots at build time. Also computes the 
# location of each subplot as well as the width and height of the subplots. 
# Assign_grid stores this information in the layer's embed variable to be 
# retrieved by combine_glyphs. Assign_grid is intended to be used in a glayer's 
# assign_glyphs slot.
assign_grid <- function(., data) {
  # major x and y
  x <- eval(embed$major.aes$x, envir = data, enclos = embed$grid_env)
  x.range <- range(x)
  x.breaks <- seq(x.range[1], x.range[2], length = embed$x.nbin + 1)
  width <- diff(x.range) / embed$x.nbin * embed$w.adjust
  # positions in bottom left corner, not on ref box line
  x.val <- (x.breaks + (1 + 0.05 * !embed$ref) * width/2)[-(embed$x.nbin + 1)]
  x.glyph <- cut(x, x.breaks, labels = x.val, include.lowest = TRUE)
  
  y <- eval(embed$major.aes$y, envir = data, enclos = embed$grid_env)
  y.range <- range(y)
  y.breaks <- seq(y.range[1], y.range[2], length = embed$y.nbin + 1)
  height <- diff(y.range) / embed$y.nbin * embed$h.adjust
  y.val <- (y.breaks + (1 + 0.05 * !embed$ref) * height/2)[-(embed$y.nbin + 1)]
  y.glyph <- cut(y, y.breaks, labels = y.val, include.lowest = TRUE)
  
  data$GLYPH <- as.numeric(interaction(x.glyph, y.glyph, drop = TRUE))
  globals <- unique(data.frame(GLYPH = data$GLYPH, 
    x = as.numeric(as.character(x.glyph)),
    y = as.numeric(as.character(y.glyph))))
                        
  .$embed$width <- width 
  .$embed$height <- height 
  .$embed$globals <- globals
  
  data
}