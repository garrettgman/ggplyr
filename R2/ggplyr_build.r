#' Build a ggplyr object for rendering
#'
#' This function takes the plot object, and performs all steps necessary to produce an object that can be rendered.
ggplyr_build <- function(plot){

  if (length(plot$layers) == 0) stop("No layers in plot", call.=FALSE)
  
  plot <- ggplot2:::plot_clone(plot)
  layers <- plot$layers
  layer_data <- lapply(layers, function(y) y$data)
  scales <- plot$scales
  
  dlapply <- function(f) {
    out <- vector("list", length(data))
    for(i in seq_along(data)) {
      out[[i]] <- f(d = data[[i]], p = layers[[i]])
    }
    out
  }
  
  panel <- ggplot2:::new_panel()
  panel <- ggplot2:::train_layout(panel, plot$facet, layer_data, plot$data)
  data <- ggplot2:::map_layout(panel, plot$facet, layer_data, plot$data)

  
  ######################################################
  ### note: groupwise aesthetics get calculated here ###
  ######################################################
  
  # Compute aesthetics to produce data with generalised variable names
  # replaces data set with just the explicitly stated aesthetics
  data <- dlapply(function(d, p) p$compute_aesthetics(d, plot))
  
  #######################################################
  
  
  data <- lapply(data, ggplot2:::add_group)
  data <- lapply(data, ggplot2:::scales_transform_df, scales = scales)
  
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")
  panel <- ggplot2:::train_position(panel, data, scale_x(), scale_y())
  data <- ggplot2:::map_position(panel, data, scale_x(), scale_y())
  
  
  ######################################################
  ### note: subplot data get uploaded here           ###
  ######################################################
  
  # reorganizes data to plot subplots for applicable layers
  data <- dlapply(make_subplots)
  #plot$layers <- update_mappings(layers)
  update_scales(scales, layers)
  ######################################################
   
   
  # Reset position scales, then re-train and map.  This ensures that facets
  # have control over the range of a plot
  ggplot2:::reset_scales(panel)
  panel <- ggplot2:::train_position(panel, data, scale_x(), scale_y())
  data <- ggplot2:::map_position(panel, data, scale_x(), scale_y())
    
  panel <- ggplot2:::train_ranges(panel, plot$coordinates)
  list(data = data, panel = panel, plot = plot)
}




make_subplots <- function(d, p) {
	if (!("combine" %in% ls(p))) {
		return(d)
	}
	p$combine(d)
}

update_scales <- function(scales, layers) {
	scales$scales <- name_scales(scales$scales)
	new.scales <- list()
	for (i in seq_along(layers)) {
		if ("subplots" %in% ls(layers[[i]])) {
			new.scales <- name_scales(layers[[1]]$subplots$scales)
			new.scales <- new.scales[setdiff(names(new.scales), names(scales$scales))]
			scales$scales <- c(scales$scales, new.scales)
		}
	}
	names(scales$scales) <- NULL
}

update_mappings <- function(layer.list) {
	layers <- lapply(layer.list, layer_clone)	
	update_mapping <- function(layer) {
		if ("subplots" %in% ls(layer)) {
			layer$mapping <- final_map(layer)
			layer
		}
	}
	lapply(layers, update_mapping)
}

final_map <- function(layer) {
	gmap <- layer$mapping
	smap <- layer$subplots$mapping
	smap <- smap[setdiff(names(smap), names(gmap))]
	layer$mapping <- structure(c(gmap, smap), class = "uneval")
	layer
}