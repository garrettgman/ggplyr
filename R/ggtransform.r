ggtransform <- function(ggobject, mapping = NULL, ...) {
	UseMethod("ggtransform")
}

ggtransform.proto <- function(ggobject, mapping = NULL, ...) {

	ggobject <- layer_clone(ggobject)
	ggobject$mapping <- combine_aes(mapping, ggobject$mapping)
	ggobject$geom_params <- c(ggobject$geom_params, list(...))
	ggobject
}

ggtransform.ggplot <- function(ggobject, mapping = NULL, ...) {

	ggobject <- ggplot2:::plot_clone(ggobject)
	ggobject$mapping <- combine_aes(mapping, ggobject$mapping)
	ggobject$layers <- lapply(ggobject$layers, ggtransform, 
		mapping = mapping, ...)
	labels <- ggobject$options$labels
	ggobject$options$labels <- combine_labels(lapply(mapping, deparse), 
		labels) 
	ggobject
	
}
	
