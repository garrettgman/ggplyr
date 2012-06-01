merge_overlaps <- function(globals, width, height) {
  x.overlaps <- abs(outer(globals$x, globals$x, "-")) < width
  y.overlaps <- abs(outer(globals$y, globals$y, "-")) < height
  overlaps <- data.frame(x.overlaps & y.overlaps)
  names(overlaps) <- as.character(globals$GLYPH)

  for (i in seq_along(overlaps)) {
    names(overlaps)[overlaps[[i]]] <- names(overlaps)[i]
  }
  vec <- as.numeric(factor(names(overlaps)))
  names(vec) <- globals$GLYPH
  vec
}
  