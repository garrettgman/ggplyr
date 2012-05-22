#' Combine subplots without adjusting locations
#'
#' @param layers a list of ggplot2 layer objects. These should be \code{\link{proto}} objects that work to create a graph when added to ggplot().
in_place <- function(layers) {
	
	list(fun = function(data) data, major_aes = NULL)

}
