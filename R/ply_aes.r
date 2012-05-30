#' ply_aes causes the aesthetics of a layer to be computed groupwise.
ply_aes <- function(layer, .vars) {
  if (is.glayer(layer)) return(layer)	
  layer <- layer_clone(layer)
  layer$ply.by <- .vars
  layer$compute_aesthetics <- function (., data, plot) {
    data$.gid <- id(data[ply.by], drop = TRUE)	
    aesthetics <- .$layer_mapping(plot$mapping)
    if (!is.null(.$subset)) {
      include <- data.frame(eval.quoted(.$subset, data, plot$env))
      data <- data[rowSums(include, na.rm = TRUE) == ncol(include), ]
    }
    if (!is.null(.$geom_params$group)) {
      aesthetics["group"] <- .$geom_params$group
    }
    scales_add_defaults(plot$scales, data, aesthetics, plot$plot_env)
    compact(eval.plyr(aesthetics, data, c(".gid", "PANEL"), plot$plot_env))
  }
  layer
} 