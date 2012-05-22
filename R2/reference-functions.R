ref_box <- function(layers, mapping = NULL, ...) {
	# make mapping
	xy <- prototype$mapping[c("x", "y", "X", "Y")]
	map <- combine_aes(mapping, xy)

		
	layer <- geom_rect(data = prototype$data, mapping = map, ...)
	layer$plyr <- prototype$plyr
	layer$subplots <- prototype$subplots
	layer$compute_aesthetics <- prototype$compute_aesthetics
	layer$adjust_position <- prototype$adjust_position
	gglayer(layer)
}

ref_hline <- function(layers, mapping = NULL, ...) {
	
}

ref_vline <- function(layers, mapping = NULL, ...) {

}

ref_points <- function(layers, mapping = NULL, ...) {
	
}