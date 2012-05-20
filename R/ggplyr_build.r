#' Build a ggplyr object for rendering
#'
#' This function takes the plot object, and performs all steps necessary to produce an object that can be rendered. This function outputs two pieces: a list of data.frames (one for each layer), and a panel object, which contain all information about axis limits, breaks, etc.
#'
#' ggplyr_build follows \code{\link{ggplot_build}} but implements extra steps to calcuate groupwise aesthetics and positioning.
#'
#' @param plot ggplyr object
#' @export
ggplyr_build <- function(plot){
  if (length(plot$layers) == 0) stop("No layers in plot", call.=FALSE)
  
  plot <- ggplot2:::plot_clone(plot)
  layers <- plot$layers
  layer_data <- lapply(layers, function(y) y$data)
  
  scales <- plot$scales
  # Apply function to layer and matching data
  dlapply <- function(f) {
    out <- vector("list", length(data))
    for(i in seq_along(data)) {
      out[[i]] <- f(d = data[[i]], p = layers[[i]])
    }
    out
  }

  # Initialise panels, add extra data for margins & missing facetting
  # variables, and add on a PANEL variable to data
  
  panel <- ggplot2:::new_panel()
  panel <- ggplot2:::train_layout(panel, plot$facet, layer_data, plot$data)
  
  # data is a list of data sets. All are manipulated in one pass
  # figures out data for layer, assigns rows to panels (usually panel 1)
  data <- ggplot2:::map_layout(panel, plot$facet, layer_data, plot$data)

  
  ######################################################
  ### note: groupwise aesthetics get calculated here ###
  ######################################################
  
  # Compute aesthetics to produce data with generalised variable names
  # replaces data set with just the explicitly stated aesthetics
  data <- dlapply(function(d, p) p$compute_aesthetics(d, plot))
  
  #######################################################
  
  
  
  # adds a group column based on group/fill/color aesthetic
  data <- lapply(data, ggplot2:::add_group)
  
  # Transform all scales
  # transforms data based on the scale settings?
  data <- lapply(data, ggplot2:::scales_transform_df, scales = scales)
  
  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")

  # panel gets scales here
  panel <- ggplot2:::train_position(panel, data, scale_x(), scale_y())
  
  # characters and factors are converted to numbers (in x and y axes?)
  data <- ggplot2:::map_position(panel, data, scale_x(), scale_y())
  
  # Apply and map statistics
  # fills out the columns with aesthetics generated from the stat
  # combines rows according to stat
  data <- ggplot2:::calculate_stats(panel, data, layers)
  
  # calculates missing aesthetics from stat (like y)
  data <- dlapply(function(d, p) p$map_statistic(d, plot)) 
  
  # seems like groups were already in order...
  data <- lapply(data, ggplot2:::order_groups)
  
  # Reparameterise geoms from (e.g.) y and width to ymin and ymax
  # builds new aesthetic columns from stat aesthetic columns
  # predicts from the stat which aes grid will need to draw
  data <- dlapply(function(d, p) p$reparameterise(d))


  #####################################################
  ###      subplot adjustment occurs here           ### 
  ###      (along with p$adjust_position)           ###
  #####################################################
  # Apply position adjustments
  # changes x, reorders rows
  ## Note: most of the departures here from ggplot_build are to 
  #  realign the x and y scales with the new axis
  ## Note: these departures and adjust_position will have to work with 
  #  non gglayer layers
  new <- ggplot()
  data <- dlapply(function(d, p) p$adjust_position(d, plot, new))
  scales <- plot$scales <- new$scales
  panel$x_scales <- NULL
  panel$y_scales <- NULL
  panel <- ggplot2:::train_position(panel, data, scale_x(), scale_y())
  #####################################################
  
  
  
   
  # Reset position scales, then re-train and map.  This ensures that facets
  # have control over the range of a plot: is it generated from what's 
  # displayed, or does it include the range of underlying data?
  ggplot2:::reset_scales(panel)
  panel <- ggplot2:::train_position(panel, data, scale_x(), scale_y())
  data <- ggplot2:::map_position(panel, data, scale_x(), scale_y())
  
  # Train and map non-position scales
  npscales <- scales$non_position_scales()  
  if (npscales$n() > 0) {
    lapply(data, ggplot2:::scales_train_df, scales = npscales)
    
    # replaces numbers in things like fill to color values, etc.
    data <- lapply(data, ggplot2:::scales_map_df, scales = npscales)
  }
  
  # Train coordinate system
  # builds all of the ranges for the x and y axiis
  panel <- ggplot2:::train_ranges(panel, plot$coordinates)
  
  list(data = data, panel = panel, plot = plot)
}
