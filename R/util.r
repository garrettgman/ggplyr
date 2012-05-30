.x_aes <- c("x", "xend", "xmax", "xmin")
.y_aes <- c("y", "yend", "ymax", "ymin")

#' retrieves the first name used in an expression. To be used with screening 
#' mappings for plyr or regular computation.
first_name <- function(expr) {
  names <- all.names(expr)
  names <- names[names != "["]
  firsts <- names[1]
  firsts[is.na(firsts)] <- ""
  firsts
}


#' get_xs retrieves all mappings that need to be altered for plotting on a new x axis
#'
#' @keywords internal
#' @param data a data.frame
#' @export
get_xs <- function(data) {
  names(data)[names(data) %in% .x_aes]
}

#' get_ys retrieves all mappings that need to be altered for plotting on a new y axis
#'
#' @keywords internal
#' @param data a data.frame
#' @export
get_ys <- function(data) {
  names(data)[names(data) %in% .y_aes]
}


layer_clone <- function(layer) {
  ggplot2:::plot_clone(ggplot() + layer)$layers[[1]]
}

#' null.omit removes the NULL elements from a list and returns the remaining objects as a more concise list.
#' 
#' @keywords internal
#' @param lst a list
#' @export
null_omit <- function(lst) {
  if (is.null(lst)) {
    return(NULL)
  }
  lst[!(unlist(lapply(lst, is.null)))]
}