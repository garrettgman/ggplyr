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
  
  no.ply <- unlist(lapply(aesthetics, first_name)) == "I"
  if (any(no.ply)) {
    uber.aes <- remove_I(aesthetics[[no.ply]])
    evaled <- compact(eval.quoted(uber.aes, data, plot$plot_env))
    lengths <- vapply(evaled, length, integer(1))
    n <- if (length(lengths) > 0) 
      max(lengths)
    else 0
    wrong <- lengths != 1 & lengths != n
    if (any(wrong)) {
      stop("Aesthetics must either be length one, or the same length as the data", 
           "Problems:", paste(aesthetics[wrong], collapse = ", "), 
           call. = FALSE)
    }
    if (empty(data) && n > 0) {
      evaled$PANEL <- 1
    }
    else {
      evaled$PANEL <- data$PANEL
    }
    uber.data <- data.frame(evaled) 
    aesthetics[no.ply] <- lapply(names(aesthetics)[no.ply], as.name)
    data <- compact(eval.plyr(aesthetics, data, c("GLYPH", "PANEL"), 
                              plot$plot_env))
    join(data, uber.data, by = c("GLYPH", "PANEL"), type = "full")
  } else {
    compact(eval.plyr(aesthetics, data, c("GLYPH", "PANEL"), plot$plot_env))
  }
}

#' ply_aes causes the aesthetics of a layer to be computed groupwise.
ply_aes <- function(layer, .vars) {
  
  if (!is.function(.vars)) {
    .vars <- group_by(.vars)
  }
  
  if (is.glayer(layer)) return(layer)  
  layer <- layer_clone(layer)
  layer$plyr <- list(ply.by = .vars)
  layer$compute_aesthetics <- function (., data, plot) {
    data$.gid <- plyr$ply.by(data)	
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