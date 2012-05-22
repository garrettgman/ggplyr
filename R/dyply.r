#' split data.frame, apply function, and return results as a ggplot2 layer object
#' 
#' For each subset of a data.frame, apply a function and then combine results into a ggplot2 layer object. This layer can be used to build a ggplot2 plot. Results will be combined with the function provided as the .combine.fun argument. This function should be compatible with the output of the function provided as the .apply.fun argument.
#'
#' @param .data data.frame to be processed
#' @param .split.fun function used to split the data set, or the names of the variables to split on as character strings.
#' @param .apply.fun function to apply to each subset of the data.frame
#' @param .combine.fun function to combine output of .apply.fun into a graph. .combine.fun should return a ggplot2 layer object, or a list of ggplot2 layer objects.
#' @param .progress name of the progress bar to use, see \code{\link{create_progress_bar}}
#' @param .parallel if TRUE, apply function in parallel using the parallel backend provided by foreach
#' @return A ggplot2 layer object
#' @export
dyply <- function(.data, .split.fun = NULL, .apply.fun = NULL, .combine.fun = fun(as_is, "layers"), .progress = "none", .parallel = FALSE) {

	if (!is.call(.split.fun)) {
		vars <- .split.fun
		.split.fun <- function(x) group_by(x, vars = vars)
	}
	
	data <- .split.fun(.data)
	
	lyply(data, .apply.fun, .combine.fun, .progress, .parallel)
	
}