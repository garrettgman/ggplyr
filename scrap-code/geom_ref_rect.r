#' geom_ref_rect calls a rect geom but ensures that only one box is drawn per 
#' glyph. geom_ref-rect is not needed with new aesply and condense functions.
#'
#' @keywords internal
#' @param mapping The aesthetic mapping, usually constructed with 
#' \code{\link[ggplot2]{aes}}. Only needs to be set at the layer level if you 
#' are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override 
#' the plot defaults
#' @param stat The statistical transformation to use for this layer.
#' @param position The position adjustment to use for overlapping points in this 
#' layer
#' @param na.rm If FALSE (the default), removes missing values with a warning. 
#' If TRUE, silently removes missing variables.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This 
#' can include aesthetics whose values you want to set, not map. See 
#' \code{\link[ggplot2]{layer}} for more details.
#' @export
geom_ref_rect <- function (mapping = NULL, data = NULL, stat = "identity", 
  position = "identity", ...) {
  
  layer <- geom_rect(mapping = mapping, data = data, stat = stat, 
    position = position, ...)
  layer$geom$reparameterise <- function(., data, params) {
    unique(data)
  }
  layer
}