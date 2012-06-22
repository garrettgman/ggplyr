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
  
  if (is.sp_layer(layer) || is.list(layer)) {
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
      ply_aes(sp_layer(layer))
    } else {
      sp_layer(layer)
    }
  } else {
    ref.layer <- reference(layer, "grid", grid.aes, x.nbin = x.nbin, 
      y.nbin = y.nbin)
    if (ply.aes) {
      list(ref.layer, ply_aes(sp_layer(layer)))
    } else {
      list(ref.layer, sp_layer(layer))
    }
  }
}


#' Bin data and visualize with a grid of subplots.
#' 
#' geom_subplot bins data into a two dimensional grid and then visualizes the 
#' data within each bin with an embedded subplot. Mappings, stat, and parameters
#' are applied consistently across subplots, but each subplot only uses the data
#' that falls withn its 2d bin. 
#' 
#' Any variables in the data set may be used as x and y axes to bin on. These 
#' 'major' x and y axes need not correspond to the 'minor' x and y axes within 
#' each subplot. Minor x and y axes are defined in the subplot aesthetic of 
#' geom_subplot2d. To allow interpretation, the major axes of a gridded layer 
#' should correspond to the x and y aesthetics for any other (non - gridded) 
#' layers in the plot.
#' 
#' @param mapping An aesthetic mapping, usually constructed with 
#' \code{\link[ggplot2]{aes}}. This mapping should contain x, y, and subplot 
#' aesthetics. All other aesthetics will be ignored - consider placing them in 
#' the subplot aesthetics's mapping instead. The subplot aesthetic should be a 
#' layer or plot specification. For example, the subplot aesthetic could be the 
#' output of \code{\link[ggplot2]{qplot} or \code{\link[ggplot2]{geom_point}. 
#' Any data argument in the subplot aesthetic will be ignored, the subplot 
#' mapping will be applied to the data inherited by the geom_subplot2d layer.
#' @param bins numeric. The number of bins to divide each major axis into. If 
#' bins is of length 2, the first number will be applied to the x axis and the 
#' second to the y. Defaults to 10.
#' @param binwidth numeric. The binwidth to use when dividing the major x and y 
#' axes into bins. If set, binwidth will override the bins argument. If binwidth 
#' is of length 2, the first number will be applied to the x axis and the second 
#' to the y. Defaults to NULL.
#' @param breaks a vector of breaks or a function from the densityvis package 
#' (unpublished), such as interval_breaks. breaks determines which breaks are 
#' used to bin the x and y axes. If set, breaks will override the bins and 
#' binwidth arguments. If breaks is a list, the first element will be applied to 
#' the x axis and the second to the y. Defaults to NULL.
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
#' @param position character. "identity"
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

geom_subplot2d <- function(mapping, bins = 10,  binwidth = NULL, breaks = NULL, 
  x_scale = identity, y_scale = identity, width.adjust = 0.95, 
  height.adjust = 0.95, position = "identity", reference = ref_box(), 
  ply.aes = TRUE, .ref = FALSE) {
  
  missing <- c(is.null(mapping$x), is.null(mapping$y), is.null(mapping$subplot))
  if (any(missing)) {
    stop(paste("Missing required aesthetics in geom_subplot2d:", 
      paste(c("x", "y", "subplot")[missing], collapse = ", ")))
  }
  
  if (position != "identity") {
    stop("geom_subplot2d only supports position = 'identity'.", call. = FALSE)
  }
  
  breaks <- distill_breaks(bins, binwidth, breaks)
  
  layer <- extract_layer(mapping$subplot, parent.frame())
  mapping$subplot <- NULL
  layer$embed <- list(x_scale = x_scale, y_scale = y_scale,
    major.aes = mapping, breaks = breaks, w.adjust = width.adjust, 
    h.adjust = height.adjust, ref = .ref)
  layer$assign_subplots <- assign_grid
  layer$combine_subplots <- combine_subplots
  if (.ref) layer$combine_subplots <- combine_refs
  
  if (is.null(reference)) {
    if (ply.aes) {
      ply_aes(sp_layer(layer))
    } else {
      sp_layer(layer)
    }
  } else {
    ref.layer <- reference(layer, "subplot2d", mapping, breaks = breaks)
    if (ply.aes) {
      list(ref.layer, ply_aes(sp_layer(layer)))
    } else {
      list(ref.layer, sp_layer(layer))
    }
  }
}


# Divides a layer's data into gridded subplots at build time. Also computes the 
# location of each subplot as well as the width and height of the subplots. 
# Assign_grid stores this information in the layer's embed variable to be 
# retrieved by combine_glyphs. Assign_grid is intended to be used in a glayer's 
# assign_glyphs slot.
assign_grid <- function(., data, env) {
  # major x and y
  x <- eval(embed$major.aes$x, envir = data, enclos = env)
  x.breaks <- embed$breaks$x.breaks
  if (is.function(x.breaks)) {
    if (embed$ref) {
      x.breaks <- suppressMessages(x.breaks(x))
    } else {
      x.breaks <- x.breaks(x)
    }
  }
  xn <- length(x.breaks) - 1
  width <- diff(range(x.breaks)) / xn  * embed$w.adjust
  # positions in bottom left corner, not on ref box line
  x.val <- (x.breaks + (1 + 0.08 * !embed$ref) * width/2)[-(xn + 1)]
  x.subplot <- cut(x, x.breaks, labels = x.val, include.lowest = TRUE)
  
  y <- eval(embed$major.aes$y, envir = data, enclos = env)
  y.breaks <- embed$breaks$y.breaks
  if (is.function(y.breaks)) {
    if (embed$ref) {
      y.breaks <- suppressMessages(y.breaks(y))
    } else {
      y.breaks <- y.breaks(y)
    }
  }
  yn <- length(y.breaks) - 1
  height <- diff(range(y.breaks)) / yn * embed$h.adjust
  y.val <- (y.breaks + (1 + 0.08 * !embed$ref) * height/2)[-(yn + 1)]
  y.subplot <- cut(y, y.breaks, labels = y.val, include.lowest = TRUE)
  
  data$SUBPLOT <- as.numeric(interaction(x.subplot, y.subplot, drop = TRUE))
  globals <- unique(data.frame(SUBPLOT = data$SUBPLOT, 
    x = as.numeric(as.character(x.subplot)),
    y = as.numeric(as.character(y.subplot))))
  
  .$embed$width <- width 
  .$embed$height <- height 
  .$embed$globals <- globals
  
  data
}

# from densityvis
#' Pick breaks for interval (1d) bins.
#'
#' Specify either \code{bins} or \code{binwidth}.
#'
#' @param bins Desired number of bins
#' @param binwidth Desired bin width
#' @param origin Desired origin of first bin
#' @param range Range of values to use, if different to range of data.
#' @return A function that takes a single parameter, a numeric x specifying
#'   the data for which breaks are needed, and returns a vector of breaks.
#' @export
interval_breaks <- function(bins = 10, binwidth = NULL, origin = NULL, range = NULL) {
  
  function(x) {
    
    if (is.integer(x)) x <- as.numeric(x)
    
    if (is.null(range)) {
      range <- range(x, na.rm = TRUE, finite = TRUE)
    }
    # If x is a point mass, make a single bin
    if (diff(range) < 1e-07) return(range)
    
    if (is.null(binwidth)) {
      binwidth <- diff(range) / bins
      message("Using binwidth ", format(binwidth, digits = 3))
    }
    
    if (is.null(origin)) {
      breaks <- fullseq(range, binwidth)
    } else {
      breaks <- seq(origin, max(range) + binwidth, binwidth)
    }
    
    breaks
  } 
}


distill_breaks <- function(bins, binwidth, breaks) {
  if (!is.null(breaks)) {
    if (is.list(breaks) & length(breaks) == 2) {
      names(breaks) <- c("x.breaks", "y.breaks")
      return(breaks)
    } else {
      return(list(x.breaks = breaks, y.breaks = breaks))
    }
  }
    
  if (!is.null(binwidth)) {
    if (length(binwidth) == 1) {
      return(list(x.breaks = interval_breaks(binwidth = binwidth),
        y.breaks = interval_breaks(binwidth = binwidth)))
    } else {
      return(list(x.breaks = interval_breaks(binwidth = binwidth[1]),
        y.breaks = interval_breaks(binwidth = binwidth[2])))
    }
  }
      
  if (!is.null(bins)) {
    if (length(bins) == 1) {
      return(list(x.breaks = interval_breaks(bins = bins),
        y.breaks = interval_breaks(bins = bins)))
    } else { 
      return(list(x.breaks = interval_breaks(bins = bins[1]),
        y.breaks = interval_breaks(bins = bins[2])))
    }
  }
      
  stop("geom_subplot2d requires one of bins, binwidth, or breaks.", 
    call. = FALSE)
}