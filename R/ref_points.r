ref_points <- function(mapping = NULL, ...) {
	make_layer <- function(globals, width, height) {

		ref_data1 <- transform(globals, x = x + width, y = y - height)
		ref_data2 <- transform(globals, x = x - width, y = y - height)
		ref_data3 <- transform(globals, x = x + width, y = y + height)
		ref_data4 <- transform(globals, x = x - width, y = y + height)
		ref_data <- rbind(ref_data1, ref_data2, ref_data3, ref_data4)
	
		ref_map <- lapply(names(ref_data), as.name)
		names(ref_map) <- names(ref_data)
		ref_map$.gid <- NULL
		class(ref_map) <- "uneval"
		
		ref_layer <- geom_point(ref_map, data = ref_data, ...)
		ref_layer
	}
	
	
	list(mapping = mapping, make_layer = make_layer)
}