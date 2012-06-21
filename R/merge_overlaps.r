#' Identify and merge overlapping glyph assignments
#' 
#' merge_overlaps checks glyph positions against glyph width and heights to 
#' identify groups of overlapping glyphs. It then computes an alternative GLYPH 
#' variable that assigns all glyphs in an overlapping group to the same name.
#' 
#' @keywords internal
#' @param globals a data frame of glyph names and positions
#' @param width glyph width in the same units as the global x positions
#' @param height glyph height in the same units as global y positions
#' @return A named vector The names of the vector correspond to old glyph 
#' assignments, the values correspond to new assignments that merge overlapping 
#' glyphs.
#' @export
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
  