#' geom_ref_point calls a point geom but ensures that only one point is drawn per 
#' glyph
geom_ref_point <- function (mapping = NULL, data = NULL, stat = "identity", 
                            position = "identity", ...) {
  
  layer <- geom_point(mapping = mapping, data = data, stat = stat, 
                      position = position, ...)
  layer$geom$reparameterise <- function(., data, params) {
    unique(data)
  }
  layer
}