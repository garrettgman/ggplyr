#dgply(test.data, 
#	.split = c("lat", "long"),
#	.apply = fun(geom_point, "data", mapping = aes(x = Education, y = Catholic)),
#	.combine = fun(nest, "layers", mapping = aes(x = mean(Education), 
#		y = mean(Catholic)), .input = "layers"))


#' split list, apply function, and return results as a ggplot2 graph
#' 
#' For each element of a list, apply a function and then combine results into a ggplot2 plot. Results will be combined with the function provided as the .combine.fun argument. This function should be compatible with the output of the function provided as the .apply.fun argument.
#'
#' @param .data list to be processed
#' @param .apply.fun function to apply to each piece of the list
#' @param .combine.fun function to combine output of .apply.fun into a graph. .combine.fun should return a ggplot2 layer object, or a list of ggplot2 layer objects.
#' @param .progress name of the progress bar to use, see \code{\link{create_progress_bar}}
#' @param .parallel if TRUE, apply function in parallel using the parallel backend provided by foreach
#' @return A ggplot2 object of class ggplot
#' @export
lgply <- function(.data, .apply.fun = NULL, .combine.fun = fun(as_is, "layers"), .progress = "none", .parallel = FALSE) {
	
	ggplot() + lyply(.data, .apply.fun, .combine.fun, .progress, .parallel)

}


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


#' split data.frame, apply function, and return results as a ggplot2 graph
#' 
#' For each subset of a data.frame, apply a function and then combine results into a ggplot2 graph. Results will be combined with the function provided as the .combine.fun argument. This function should be compatible with the output of the function provided as the .apply.fun argument.
#'
#' @param .data data.frame to be processed
#' @param .split.fun function used to split the data set, or the names of the variables to split on as character strings.
#' @param .apply.fun function to apply to each subset of the data.frame
#' @param .combine.fun function to combine output of .apply.fun into a graph. .combine.fun should return a ggplot2 layer object, or a list of ggplot2 layer objects.
#' @param .progress name of the progress bar to use, see \code{\link{create_progress_bar}}
#' @param .parallel if TRUE, apply function in parallel using the parallel backend provided by foreach
#' @return A ggplot2 object of class ggplot
#' @export
dgply <- function(.data, .split.fun = NULL, .apply.fun = NULL, .combine.fun = fun(as_is, "layers"), .progress = "none", .parallel = FALSE) {
	
	ggplot() + dyply(.data, .split.fun, .apply.fun, .combine.fun, .progress, .parallel)
	
}





#' split ggplot2 layer, apply function, and return results as a ggplot2 layer object
#' 
#' For each subset of data in a ggplot2 layer, apply a function and then combine results into a ggplot2 layer object. This layer can be used to build a ggplot2 plot. Results will be combined with the function provided as the .combine.fun argument. This function should be compatible with the output of the function provided as the .apply.fun argument. Note, yyply can only manipulate data that is stored in the ggplot2 layer. If the layer inherits data from a global \code{\layer{ggplot}} call, yyplot will not have access to the data.
#'
#' @param .data ggplot2 layer to be processed
#' @param .split.fun function used to split the data set that underlies the ggplot2 layer, or the names of the variables to split on as character strings.
#' @param .apply.fun function to apply to each subset of the ggplot2 layer
#' @param .combine.fun function to combine output of .apply.fun into a graph. .combine.fun should return a ggplot2 layer object, or a list of ggplot2 layer objects.
#' @param .progress name of the progress bar to use, see \code{\link{create_progress_bar}}
#' @param .parallel if TRUE, apply function in parallel using the parallel backend provided by foreach
#' @return A ggplot2 layer object
#' @export
yyply <- function(.data, .split.fun = NULL, .apply.fun = NULL, .combine.fun = fun(as_is, "layers"), .progress = "none", .parallel = FALSE) {
	
	if (!is.call(.split.fun)) {
		.split.fun <- fun(group_by, "data", vars = .split.fun)
	}
	
	data <- .split.fun(.data$data)
	data <- lapply(data, recreate_geom(.data))
	
	data <- llply(data, .fun = .apply.fun, .progress = .progress, 
		.parallel = .parallel)
	
	.combine.fun(data)
}


#' split ggplot2 layer, apply function, and return results as a ggplot2 graph
#' 
#' For each subset of data in a ggplot2 layer, apply a function and then combine results into a ggplot2 graph. Results will be combined with the function provided as the .combine.fun argument. This function should be compatible with the output of the function provided as the .apply.fun argument. Note, ygply can only manipulate data that is stored in the ggplot2 layer. If the layer inherits data from a global \code{\layer{ggplot}} call, yyplot will not have access to the data.
#'
#' @param .data ggplot2 layer to be processed
#' @param .split.fun function used to split the data set that underlies the ggplot2 layer, or the names of the variables to split on as character strings.
#' @param .apply.fun function to apply to each subset of the ggplot2 layer
#' @param .combine.fun function to combine output of .apply.fun into a graph. .combine.fun should return a ggplot2 layer object, or a list of ggplot2 layer objects.
#' @param .progress name of the progress bar to use, see \code{\link{create_progress_bar}}
#' @param .parallel if TRUE, apply function in parallel using the parallel backend provided by foreach
#' @return A ggplot2 object of class ggplot
#' @export
ygply <- function(.data, .split.fun = NULL, .apply.fun = NULL, .combine.fun = fun(as_is, "layers"), .progress = "none", .parallel = FALSE) {
	
	ggplot() + yyply(.data, .split.fun, .apply.fun, .combine.fun, .progress, .parallel)
	
}


#' split ggplot2 graph, apply function, and return results as a ggplot2 layer object
#' 
#' For each subset of data in a ggplot2 graph, apply a function and then combine results into a list of ggplot2 layers. This list can be used to build a ggplot2 plot. Results will be combined with the function provided as the .combine.fun argument. This function should be compatible with the output of the function provided as the .apply.fun argument. Note, ggply will attempt to modify each layer in a graph, which means ggply is unlikely to work if multiple data sets exist between different layers. ggply is compatible with multiple data sets in a graph, so long as each data set can be manipulated with the same \code{\link{dlply}} call.
#'
#' @param .data ggplot2 graph to be processed
#' @param .split.fun function used to split the data sets that underlie the layers of the ggplot2 graph, or the names of the variables to split on as character strings.
#' @param .apply.fun function to apply to each subset of the above data sets.
#' @param .combine.fun function to combine output of .apply.fun into a graph. .combine.fun should return a ggplot2 layer object, or a list of ggplot2 layer objects.
#' @param .progress name of the progress bar to use, see \code{\link{create_progress_bar}}
#' @param .parallel if TRUE, apply function in parallel using the parallel backend provided by foreach
#' @return A list of ggplot2 layer objects
#' @export

gyply <- function(.data, .split.fun = NULL, .apply.fun = NULL, .combine.fun = fun(as_is, "layers"), .progress = "none", .parallel = FALSE) {
	
	ggplot <- ggplot2:::plot_clone(ggplot)
	ggplot <- ggplot_standardise(ggplot)
	
	lapply(ggplot$layers, 
		fun(yyply, ".data",
			.split.fun = .split.fun, 
			.apply.fun = .apply.fun, 
			.combine.fun = .combine.fun, 
			.progress = .progress, .parallel = .parallel
		)
	)
}


#' split ggplot2 graph, apply function, and return results as a ggplot2 graph
#' 
#' For each subset of data in a ggplot2 graph, apply a function and then combine results into a ggplot2 graph. Results will be combined with the function provided as the .combine.fun argument. This function should be compatible with the output of the function provided as the .apply.fun argument. Note, ggply will attempt to modify each layer in a graph, which means ggply is unlikely to work if multiple data sets exist between different layers. ggply is compatible with multiple data sets in a graph, so long as each data set can be manipulated with the same \code{\link{dlply}} call.
#'
#' @param .data ggplot2 graph to be processed
#' @param .split.fun function used to split the data sets that underlie the layers of the ggplot2 graph, or the names of the variables to split on as character strings.
#' @param .apply.fun function to apply to each subset of the above data sets.
#' @param .combine.fun function to combine output of .apply.fun into a graph. .combine.fun should return a ggplot2 layer object, or a list of ggplot2 layer objects.
#' @param .progress name of the progress bar to use, see \code{\link{create_progress_bar}}
#' @param .parallel if TRUE, apply function in parallel using the parallel backend provided by foreach
#' @return A ggplot2 object of class ggplot
#' @export
ggply <- function(.data, .split.fun = NULL, .apply.fun = NULL, .combine.fun = fun(as_is, "layers"), .progress = "none", .parallel = FALSE) {
	
	ggplot() + gyply(.data, .split.fun, .apply.fun, .combine.fun, .progress, .parallel)
	
}