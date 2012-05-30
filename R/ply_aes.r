#' ply_aes causes the aesthetics of a layer to be computed groupwise.
ply_aes <- function(layer, .vars = NULL) {
  if (is.glayer(layer)) return(layer)
  layer <- layer_clone(layer)
  if (!is.null(.vars)) {
    if (!is.function(.vars)) {
      .vars <- group_by(.vars)
    }
    layer$plyr <- list(ply.by = .vars)
  }
  layer$compute_aesthetics <- plyr_aesthetics
  layer
}