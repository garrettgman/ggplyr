#' what replaces compute_aesthetics
plyr_aesthetics <- function (., data, plot) {
  aesthetics <- .$layer_mapping(plot$mapping)
  if (!is.null(.$subset)) {
    include <- data.frame(eval.quoted(.$subset, data, plot$env))
    data <- data[rowSums(include, na.rm = TRUE) == ncol(include), ]
  }
  if (!is.null(.$geom_params$group)) {
    aesthetics["group"] <- .$geom_params$group
  }
  scales_add_defaults(plot$scales, data, aesthetics, plot$plot_env)

  if (!is.null(aesthetics$group)) {
    data$group <- unlist(eval.quoted(aesthetics$group, envir = data, 
      enclos = plot$plot_env))
    aesthetics$group <- quote(group)
  }
  if ("GLYPH" %in% names(data)) {
    aesthetics$GLYPH <- quote(GLYPH)
  }
  aesthetics$PANEL <- quote(PANEL)
  
  criteria <- c("group", "GLYPH", "PANEL", .$plyr$ply.by)
  criteria <- criteria[criteria %in% names(data)]  
  data$ply.by <- id(data[criteria], drop = TRUE)
  
  data <- aesply(data, "ply.by", aesthetics)
  data$ply.by <- NULL
  data
}
