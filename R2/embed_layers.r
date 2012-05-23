#' embed_layers takes a list of layers and arranges them into an embedded layer that will be plotted as an embedded plot. Each layer must have the same parameters, geom, stat, and aesthetics. Only the data differs.
#'
#' note: .fun should return a list(fun = combine function, major_aes = aes(x = , y = )
embed_layers <- function(layers, .fun) {
	prototype <- layer_clone(layers[[1]])
	prototype$data <- individual_data(layers)
	prototype$embed <- .fun(prototype$data)
	gglayer(prototype)
}


