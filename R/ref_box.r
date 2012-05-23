ref_box <- function(mapping = NULL, ...) {	
	geom_rect(mapping, ...)
}
	
ref_hline <- function(layers, mapping = NULL, ...) {
	geom_hline(mapping, ...)
}

ref_vline <- function(layers, mapping = NULL, ...) {
	geom_vline(mapping, ...)
}

ref_points <- function(layers, mapping = NULL, ...) {
	geom_points(mapping, ...)
}