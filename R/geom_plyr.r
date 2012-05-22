# geom_plyr performs groupwise aesthetic calculation
geom_plyr <- function(mapping = NULL, data, .vars, geom = "point", ...) {
	geom_fun <- get(paste("geom", geom, sep = "_"))
	data$.gid <- id(data[.vars], drop = TRUE)
	layer <- geom_fun(mapping, data, ...)
	layer$plyr <- TRUE
	layer$compute_aesthetics <- compute_plyr_aesthetics
	layer
}