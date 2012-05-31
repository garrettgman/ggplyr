#' ply_aes causes the aesthetics of a layer to be computed groupwise.
ply_aes <- function(layer, .vars = NULL) {
  UseMethod("ply_aes")
}

ply_aes.list <- function(layer, .vars = NULL) {
  lapply(layer, ply_aes, .vars)
}

ply_aes.glayer <- function(layer, .vars = NULL) {
  layer$compute_aesthetics <- plyr_aesthetics
  layer
}
  
ply_aes.proto <- function(layer, .vars = NULL) {
  if (!is.null(.vars)) {
    if (!is.function(.vars)) {
      .vars <- group_by(.vars)
    }
    layer$plyr <- list(ply.by = .vars)
  }
  layer$compute_aesthetics <- plyr_aesthetics
  layer
}