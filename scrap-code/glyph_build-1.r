#' this might work for normal layers too? yes. confirmed!
glyph_build <- function (plot) {
    if (length(plot$layers) == 0) 
        stop("No layers in plot", call. = FALSE)
    plot <- ggplot2:::plot_clone(plot)
    layers <- plot$layers
    layer_data <- lapply(layers, function(y) y$data)
    scales <- plot$scales
    dlapply <- function(f) {
        out <- vector("list", length(data))
        for (i in seq_along(data)) {
            out[[i]] <- f(d = data[[i]], p = layers[[i]])
        }
        out
    
        }
    panel <- ggplot2:::new_panel()
    panel <- ggplot2:::train_layout(panel, plot$facet, layer_data, plot$data)
    data <- ggplot2:::map_layout(panel, plot$facet, layer_data, plot$data)
    data <- dlapply(function(d, p) add_glyph(d, p))
    
    data <- dlapply(function(d, p) p$compute_aesthetics(d, plot))
    
    data <- lapply(data, ggplot2:::add_group)
    data <- lapply(data, ggplot2:::scales_transform_df, 
    	scales = scales)
    scale_x <- function() scales$get_scales("x")
    scale_y <- function() scales$get_scales("y")
    panel <- ggplot2:::train_position(panel, data, scale_x(), scale_y())
    data <- ggplot2:::map_position(panel, data, scale_x(), scale_y())
    
    data <- dlapply(function(d, p) name_majors(d, p))
    data <- plyr_stats(panel, data, layers) #slow. can we skip?
    data <- dlapply(function(d, p) p$map_statistic(d, plot))
    data <- dlapply(function(d, p) name_minors(d, p))
    data <- lapply(data, ggplot2:::order_groups)
    
    npscales <- scales$non_position_scales()
    if (npscales$n() > 0) {
        lapply(data, ggplot2:::scales_train_df, scales = npscales)
        data <- lapply(data, ggplot2:::scales_map_df, 
        	scales = npscales)
    }
    
    data <- dlapply(function(d, p) p$reparameterise(d))
    data <- dlapply(function(d, p) p$adjust_position(d))
    data <- dlapply(function(d, p) map_glyph(d, p))
    
    ggplot2:::reset_scales(panel)
    panel <- ggplot2:::train_position(panel, data, scale_x(), scale_y())
    data <- ggplot2:::map_position(panel, data, scale_x(), scale_y())
    panel <- ggplot2:::train_ranges(panel, plot$coordinates)
    list(data = data, panel = panel, plot = plot)
}

#' the names x and y receive special attention during the ggplot2 build process. 
#' These functions flip the naming scheme to control whether the major or minor axiis 
#' receive this attention at various parts of the build
name_majors <- function(data, layer) {
  if ("glyph.by" %in% ls(layer)) {
    dnames <- names(data)
    dnames[dnames == "x"] <- "major.x"
    dnames[dnames == "y"] <- "major.y"
    dnames[dnames == "minor.x"] <- "x"
    dnames[dnames == "minor.y"] <- "y"
    names(data) <- dnames
  }
  data
}

name_minors <- function(data, layer) {
  if ("glyph.by" %in% ls(layer)) {
    dnames <- names(data)
    dnames[dnames == "x"] <- "minor.x"
    dnames[dnames == "y"] <- "minor.y"
    dnames[dnames == "major.x"] <- "x"
    dnames[dnames == "major.y"] <- "y"
    names(data) <- dnames
  }
  data
}