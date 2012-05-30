#' @include ggtransform.r
NULL

check_glyphs <- function(object) {
	errors <- character()
	if (!is(object@.Data, "ggplot")) {
		msg <- "glyphs must be a ggplot object."
		errors <- c(errors, msg)
	}
	if (length(errors) == 0) 
		TRUE
	else
		errors
}

setOldClass(c("ggplot", "list"))

#' glyphs class
#'
#' a glyphs object is a ggplot object that has been extended to include methods for applying plyr when plotting. 
#'
#' @name glyphs-class
#' @rdname glyphs-class
#' @exportClass glyphs
#' @aliases show,glyphs-method
#' @aliases ggtransform,glyphs-method
setClass("glyphs", contains = c("ggplot"), validity = check_glyphs)

#' @export
setMethod("show", signature(object = "glyphs"), function(object){
	print(object)
})

#' @S3method print ggplyr
print.glyphs <- function(x, newpage = is.null(vp), vp = NULL, ...) {
    ggplot2:::set_last_plot(x)
    if (newpage) 
        grid.newpage()
    data <- glyph_build(x)
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
setMethod("ggtransform", signature(ggobject = "glyphs"), 
	function(ggobject, mapping, ...){
		ggplot <- ggtransform(ggobject@.Data, mapping, ...)
		new("glyphs", ggplot)
	}
)




#' Create a glyphs object
#' 
#' glyph_plot gives a ggplot object the S4 class ggplyr, see \code{\link{glyphs-class}}. ggplyr denotes ggplot objects that contain extra information to be used to apply plyr functions when plotting.
#' 
#' @export glyph_plot
#' @param ggplot a ggplot object
glyph_plot <- function(ggplot) {
	new("glyphs", ggplot)
}

