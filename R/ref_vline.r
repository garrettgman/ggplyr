ref_vline <- function(mapping = NULL, ...) {
	make_layer <- function(globals, width, height) {

		ref_data1 <- transform(globals, y = y - height, group = .gid)
		ref_data2 <- transform(globals, y = y + height, group = .gid)
		ref_data <- rbind(ref_data1, ref_data2)
	
		ref_map <- lapply(names(ref_data), as.name)
		names(ref_map) <- names(ref_data)
		ref_map$.gid <- NULL
		class(ref_map) <- "uneval"
		
		ref_layer <- geom_line(ref_map, data = ref_data, ...)
		ref_layer
	}
	
	
	list(mapping = mapping, make_layer = make_layer)

}