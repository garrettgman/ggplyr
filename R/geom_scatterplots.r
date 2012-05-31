geom_scatterplots <- function(mapping = NULL, glyph.by = NULL, data = NULL,
  width = rel(0.95), height = rel(0.95), x_scale = identity, y_scale = identity,
  merge.overlaps = FALSE, reference = NULL, ...) {
  
  required <- c("x", "y", "minor.x", "minor.y")
  missing <- !(required %in% names(mapping))
  if (any(missing)) {
    stop(paste("geom_scatterplots requires the following missing aesthetics:",
     paste(required[missing], collapse = ", ")), call. = FALSE)
  }
  major <- mapping[c("x", "y")]
  mapping$x <- NULL
  mapping$y <- NULL
  names(mapping)[names(mapping) == "minor.x"] <- "x"
  names(mapping)[names(mapping) == "minor.y"] <- "y"
  
  glyph(geom_point(mapping = mapping, data = data, ...), major, glyph.by, width, 
    height, x_scale, y_scale, merge.overlaps, reference)
}
  