add_glyph <- function(data, layer) {
	if (empty(data)) 
        return(data)
	if ("assign_glyphs" %in% ls(layer)) {
		data <- layer$assign_glyphs(data)
	}
	if (is.null(data$GLYPH)) {
		data$GLYPH <- 1L
	}
	data
}