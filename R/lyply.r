
#' split list, apply function, and return results as a ggplot2 layer object
#' 
#' For each element of a list, apply a function and then combine results into a ggplot2 layer object. This layer can be used to build a ggplot2 plot. Results will be combined with the function provided as the .combine.fun argument. This function should be compatible with the output of the function provided as the .apply.fun argument.
#'
#' @param .data list to be processed
#' @param .apply.fun function to apply to each piece of the list
#' @param .combine.fun function to combine output of .apply.fun into a graph. .combine.fun should return a ggplot2 layer object, or a list of ggplot2 layer objects.
#' @param .progress name of the progress bar to use, see \code{\link{create_progress_bar}}
#' @param .parallel if TRUE, apply function in parallel using the parallel backend provided by foreach
#' @return A ggplot2 layer object
#' @export
lyply <- function(.data, .apply.fun = NULL, .combine.fun = fun(as_is, "layers"), .progress = "none", .parallel = FALSE) {
	
	data <- llply(.data, .fun = .apply.fun, .progress = .progress, 
		.parallel = .parallel)
	
	.combine.fun(data)
}