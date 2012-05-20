#' @include apply-functions.r
NULL

check_ggplyr <- function(object) {
	errors <- character()
	if (!is(object@.Data, "ggplot")) {
		msg <- "ggplyr must be a ggplot object."
		errors <- c(errors, msg)
	}
	if (length(errors) == 0) 
		TRUE
	else
		errors
}

setOldClass(c("ggplot", "list"))

#' ggplyr class
#'
#' a ggplyr object is a ggplot object that has been extended to include methods for applying plyr when plotting. 
#'
#' @name ggplyr-class
#' @rdname ggplyr-class
#' @exportClass ggplyr
#' @aliases show,ggplyr-method
#' @aliases ggtransform,ggplyr-method
setClass("ggplyr", contains = c("ggplot"), validity = check_ggplyr)

#' @export
setMethod("show", signature(object = "ggplyr"), function(object){
	print(object)
})

#' @S3method print ggplyr
print.ggplyr <- function(x, newpage = is.null(vp), vp = NULL, ...) {
    ggplot2:::set_last_plot(x)
    if (newpage) 
        grid.newpage()
    data <- ggplyr_build(x)
    gtable <- ggplot_gtable(data)
    if (is.null(vp)) {
        grid.draw(gtable)
    }
    else {
        if (is.character(vp)) 
            seekViewport(vp)
        else pushViewport(vp)
        grid.draw(gtable)
        upViewport()
    }
    invisible(data)
}

#' @export
setGeneric("ggtransform")

#' @export
setMethod("ggtransform", signature(ggobject = "ggplyr"), 
	function(ggobject, mapping, ...){
		ggplot <- ggtransform(ggobject@.Data, mapping, ...)
		new("ggplyr", ggplot)
	}
)




#' Create a ggplyr object
#' 
#' ggplyr gives a ggplot object the S4 class ggplyr, see \code{\link{ggplyr-class}}. ggplyr denotes ggplot objects that contain extra information to be used to apply plyr functions when plotting.
#' 
#' @export ggplyr
#' @param ggplot a ggplot object
ggplyr <- function(ggplot) {
	new("ggplyr", ggplot)
}

