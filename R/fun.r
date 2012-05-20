#' Reorganize a function to take a single argument
#'
#' fun reorganizes a function to take a single argument. It sets other specified arguments to known values. fun can be used to change the order of arguments within a function. This is helpful for passing functions to \code{\link{plyr}} style functions which automatically use the first argument of a function's definition.
#'
#' @param name name of the function to reorganize as an object or character string
#' @param .input name of the argument that new function will take
#' @param ... values to set other arguments of function to. If an argument is not named it will be set to its default value.
#' @export
fun <- function(name, .input, ...) {

	if (!is.function(name)) {
		FUN <- get(name)
	} else {
		FUN <- name
	}
	
	args <- c(FUN, .input = as.name("x"), match.call(expand.dots = FALSE)$...)
	names(args)[names(args) == ".input"] <- .input
	function(x) eval(as.call(args))
}
# ggplot() + fun(geom_point, "data", mapping = aes(x = hwy, y = cty), size = 5)(mpg)
# fun(median, "x")