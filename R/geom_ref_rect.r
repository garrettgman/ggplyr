#' geom_ref_rect calls a rect geom but ensures that only one box is drawn per 
#' glyph
geom_ref_rect <- function (mapping = NULL, data = NULL, stat = "identity", 
  position = "identity", ...) {
  
  layer <- geom_rect(mapping = mapping, data = data, stat = stat, 
    position = position, ...)
  layer$geom$reparameterise <- function(., data, params) {
    unique(data)
  }
  layer
}