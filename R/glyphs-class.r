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

#' @exportClass list
#' @exportClass ggplot
setOldClass(c("ggplot", "list"))

#' glyphs class
#'
#' a glyphs object is a ggplot object that has been extended to include methods 
#' for embedding subplots when plotting. 
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


#' Create a glyphs object
#' 
#' glyph_plot gives a ggplot object the S4 class `glyphs', see 
#' \code{\link{glyphs-class}}. glyphs denotes ggplot objects that contain extra 
#' information to be used to embed subplots when plotting. glyphs objects have 
#' similar, but different print and build methods than ggplot2 objects.
#' 
#' @param ggplot a ggplot object
#' #' @export glyph_plot
glyph_plot <- function(ggplot) {
	new("glyphs", ggplot)
}

