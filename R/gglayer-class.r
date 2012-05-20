#' @include ggplyr-class.r
NULL

check_gglayer <- function(object) {
	errors <- character()
	if (!is.proto(object@layer)) {
		msg <- "gglayer must be a proto object."
		errors <- c(errors, msg)
	}
	if (length(errors) == 0) 
		TRUE
	else
		errors
}

setOldClass(c("proto", "environment"))

#' gglayer class
#'
#' gglayers are layers made with ggplyr methods. They are equivalent to the layers made by ggplot2 functions in all ways except that they contain an extra grouping variable (to denote subplot membership) and a plyr_function slot, which correctly locates subplots within the graph when plotting.
#'
#' @name gglayer-class
#' @rdname gglayer-class
#' @exportClass gglayer
#' @aliases show,gglayer-method
#' @aliases c,gglayer-method
#' @aliases rep,gglayer-method
#' @aliases [,gglayer-method
#' @aliases [<-,gglayer-method
#' @aliases $,gglayer-method
#' @aliases $<-,gglayer-method
#' @aliases +,ggplot,gglayer-method
#' @aliases +,ggplyr,gglayer-method
#' @aliases ggtransform,gglayer-method
setClass("gglayer", representation(layer = "proto"), validity = check_gglayer)

#' @export
setMethod("show", signature(object = "gglayer"), function(object) {
	print(object@layer) 
})

#' @export
setMethod("c", signature(x = "gglayer"), function(x, ...){
	# c(get_layer(x), unlist(lapply(list(...), get_layer)))
	stop("object of type 'gglayer' is not subsettable")
})

#' @export
setMethod("rep", signature(x = "gglayer"), function(x, ...){
	stop("object of type 'gglayer' is not subsettable")
})

#' @export
setMethod("[", signature(x = "gglayer"), 
	function(x, i, j, ..., drop = TRUE) {
    	new("gglayer", layer = x@layer[i])
	}
)

#' @export
setMethod("[<-", signature(x = "gglayer"), function(x, i, j, ..., value) {
  	x@layer[i] <- value
	x
})


#' @export
setMethod("$", signature(x = "gglayer"), function(x, name) {
	"$"(x@layer, name)
})

#' @export
setMethod("$<-", signature(x = "gglayer"), function(x, name, value) {
	x@layer <- "$<-"(x@layer, name, value)
	x
})

#' @export
setMethod("+", signature(e1 = "ggplot", e2 = "gglayer"), function(e1, e2) {
	ggplyr(e1 + e2@layer)
})

#' @export
setMethod("+", signature(e1 = "ggplyr", e2 = "gglayer"), function(e1, e2) {
	ggplyr(e1@.Data + e2@layer)
})
	
	
#' @export
setMethod("ggtransform", signature(ggobject = "gglayer"), 
	function(ggobject, mapping, ...){
		layer <- ggtransform(ggobject@layer, mapping, ...)
		new("gglayer", layer = layer)
	}
)
	
#' Create a gglayer object
#' 
#' gglayer gives a ggplot2 layer object the S4 class gglayer, see \code{\link{gglayer-class}}. ggplot layer objects are usually non-specific \code{\link{proto}} class objects.
#'
#' @export gglayer
#' @param layer a proto object that can be used as a layer by the \code{\link{ggplot2}} package (i.e, ggplot() + layer should return a graph).
gglayer <- function(layer) {
	new("gglayer", layer = layer)
}