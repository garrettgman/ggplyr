.x_aes <- c("x", "xend", "xmax", "xmin")
.y_aes <- c("y", "yend", "ymax", "ymin")

#' add_gid intelligently adds the .gid variable to the group slot of an uneval object. If the group slot is NULL, add_gid sets group = .gid. If the group slot already contains a mapping, add_gid adds .gid to this mapping with interaction().
#'
#' @keywords internal
#' @param aes_group the group value of an uneval object
#' @export
add_gid <- function(aes_group) {
	if (is.null(aes_group)) {
		as.name(".gid")
	} else {
		as.call(list(as.name("interaction"), as.name(".gid"), aes_group))
	}
}


apply_maps <- function(data, mapping, enclos = parent.frame()) {

	map <- null_omit(mapping)
	vars <- llply(map, eval, envir = data, enclos)
	
	n <- nrow(data)
	lengths <- unlist(lapply(vars, length))
	wrong <- lengths != 1 & lengths != n
	if (any(wrong)) {
        stop(paste("Aesthetics must either be length one, or the same length as the
        	data", "Problems:", paste(names(wrong)[wrong], collapse = ", ")), 
            call. = FALSE)
    }
    
    data.frame(vars)
}

#' the difference between apply maps and apply major is that apply major will only allow one value per gid per variable
apply_major <- function(data, mapping, enclos = parent.frame()) {

	map <- null_omit(mapping)
	vars <- llply(map, eval, envir = data, enclos)
	vars  <- llply(vars, unique)
	
	lengths <- unlist(llply(vars, length))
	wrong <- lengths != 1 
	if (any(wrong)) {
        stop(paste("Major aesthetics must return one value per variable per subplot group (.gid).", "Problems:", paste(names(wrong)[wrong], collapse = ", ")), call. = FALSE)
    }
    
    data.frame(vars)
}

# combine_aes is used by ggtransform
#' combine_aes combines two uneval objects overwriting values of the second that are defined in the first.
#'
#' @keywords internal
#' @param map1 an uneval object
#' @param map2 an uneval object
#' @export
combine_aes <- function(map1, map2) {
	structure(c(map1, map2[setdiff(names(map2), names(map1))]),
		class = "uneval")
}

# combine_labels is used by ggtransform
combine_labels <- function(labs1, labs2) {
	labs(c(labs1, labs2[setdiff(names(labs2), names(labs1))]))
}



#' get_xs retrieves all mappings that need to be altered for plotting on a new x axis
#'
#' @keywords internal
#' @param data a data.frame
#' @export
get_xs <- function(data) {
	names(data)[names(data) %in% .x_aes]
}

#' get_ys retrieves all mappings that need to be altered for plotting on a new y axis
#'
#' @keywords internal
#' @param data a data.frame
#' @export
get_ys <- function(data) {
	names(data)[names(data) %in% .y_aes]
}



globalize <- function(obj){
	names(obj)[names(obj) == "x"] <- "X"
	names(obj)[names(obj) == "y"] <- "Y"
	obj
}


individual_data <- function(layers) {
	attr(layers, "split_labels") <- data.frame(.gid = seq_along(layers))
	get_data <- function(layer) {
		layer$data
	}
	ldply(layers, get_data)
}


layer_clone <- function(layer) {
	ggplot2:::plot_clone(ggplot() + layer)$layers[[1]]
}

#' null.omit removes the NULL elements from a list and returns the remaining objects as a more concise list.
#' 
#' @keywords internal
#' @param lst a list
#' @export
null_omit <- function(lst) {
	if (is.null(lst)) {
		return(NULL)
	}
	lst[!(unlist(lapply(lst, is.null)))]
}

# used by in_place
#' update_aes builds a mapping to correctly guide a layer that has combined individual subplots into a single metaplot.
#'
#' @keywords internal
#' @param imap The mapping for each individual subplot
#' @param gmap The mapping for the single, combined plot
#' @export
update_aes <- function(imap, gmap) {
	
	# combine global and individual subplot mappings
	map <- combine_aes(gmap, imap)
		
	# change group aes
	map$group <- add_gid(map$group)
	
	map
}