#' geom_single representa a dataframe with a single point
geom_single <- function(mapping = NULL, data, ...) {
	data <- lapply(mapping, eval, envir = data, enclos = parent.frame())
	lengths <- lapply(data, length)
	wrong <- lengths != 1
	if (any(wrong)) {
		stop(paste("geom_single requires all aes to return one value per variable.",
			"Problems:", paste(names(wrong)[wrong], collapse = ", ")))
	}
	
	data <- data.frame(data)
	maps <- map_to_names(mapping)
	labels <- labs(mapping)
	
	list(geom_point(maps, data = data, ...), labels)
}


map_to_names <- function(uneval) {
	vars <- lapply(names(uneval), as.name)
	names(vars) <- names(uneval)
	class(vars) <- "uneval"
	vars
}


compute_single_aesthetic <- 