ref_box <- function(mapping = NULL, data = NULL, ...) {	
	make_layer <- function(globals, width, height) {

		ref_data <- transform(globals, xmin = x - width, xmax = x + width, 
			ymin = y - height, ymax = y + height)
		ref_data$x <- NULL
		ref_data$y <- NULL
	
		ref_map <- lapply(names(ref_data), as.name)
		names(ref_map) <- names(ref_data)
		ref_map$.gid <- NULL
		class(ref_map) <- "uneval"
		
		ref_layer <- geom_rect(ref_map, data = ref_data, ...)
		ref_layer
	}
	
	
	list(mapping = mapping, make_layer = make_layer)
	
}



just_refs <- function(ref.aes, major.aes) {
	non_ref <- c(.x_aes, .y_aes, names(major.aes))
	ref.aes[setdiff(names(ref.aes), non_ref)]
}