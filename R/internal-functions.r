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


#' Relocate subplots within main plot
#'
#' adjust_position_nest is substituted for a layer's position_adjust function when embedding graphics in a nested design
adjust_position_nest <- function (., data, plot, new) {

	data <- ddply(data, "PANEL", function(data) {
		.$position$adjust(data)
     })
	
	if ("subplots" %in% ls(.)) {
		
		# locate x and y related variables in data
		xvar <- get_xs(data)
		yvar <- get_ys(data)
		
		# scale if necessary
		if (!identical(.$subplots$x_scale, identity) || 
			!identical(.$subplots$y_scale, identity)) {
	    		data <- ddply(data, ".gid", 
	    			function(df) {
	      				df[xvar] <- lapply(df[xvar], .$subplots$x_scale)
	      				df[yvar] <- lapply(df[yvar], .$subplots$y_scale)
	      				df
	    			}
	    		)
	  	}
	  	
	  	width <- .$subplots$width
	  	height <- .$subplots$height
	  	
	  	if (is.rel(width)) {
			width <- diff(range(data$X)) / max(data$.gid) * unclass(width)
		}
    
		if (is.rel(height)) {
			height <- diff(range(data$Y)) / max(data$.gid) * unclass(height)
		}
	  	
	  	# update x and y related variables
		fun.x <- function(x) data$X + rescale11(x) * width
	  	fun.y <- function(y) data$Y + rescale11(y) * height
	  	data[xvar] <- lapply(data[xvar], fun.x)
	    data[yvar] <- lapply(data[yvar], fun.y)
		
    }
    
    fin.aes <- final_aes(.$layer_mapping(plot$mapping))
    #working here. scales_add_defaults is not changing plot$scales
    scales_add_defaults(new$scales, .$data, fin.aes, plot$plot_env)
    data[setdiff(names(data), c("X", "Y"))]
}


apply_maps <- function(data, mapping, enclos = parent.frame()) {
	map <- null_omit(mapping)
	vars <- lapply(map, eval, envir = data, enclos)
	
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

combine_labels <- function(labs1, labs2) {
	labs(c(labs1, labs2[setdiff(names(labs2), names(labs1))]))
}

compute_plyr_aesthetics <- function (., data, plot) {

    aesthetics <- .$layer_mapping(plot$mapping)
    
    if (!is.null(.$subset)) {
        include <- data.frame(eval.quoted(.$subset, data, plot$env))
        data <- data[rowSums(include, na.rm = TRUE) == ncol(include), 
            ]
    }
    if (!is.null(.$geom_params$group)) {
        aesthetics["group"] <- .$geom_params$group
    }
    
    scales_add_defaults(plot$scales, data, aesthetics, plot$plot_env)
    
    if ("plyr" %in% ls(.)){
    	
    	evaled <- compact(eval.plyr(aesthetics, data, ".gid", 
    		plot$plot_env))
    	evaled$PANEL <- data$PANEL
    	evaled
    } else {
    	evaled <- compact(eval.quoted(aesthetics, data, plot$plot_env))
    
    	lengths <- vapply(evaled, length, integer(1))
    	n <- if (length(lengths) > 0) 
        max(lengths)
    	else 0
    	wrong <- lengths != 1 & lengths != n
    	if (any(wrong)) {
    	    stop("Aesthetics must either be length one, or the same length as the 
    	    	data", "Problems:", paste(aesthetics[wrong], collapse = ", "), 
        	    call. = FALSE)
    	}
    	if (empty(data) && n > 0) {
    	    evaled$PANEL <- 1
    	} else {
    	    evaled$PANEL <- data$PANEL
    	}
    	data.frame(evaled)
	}
}

eval.plyr <- function (exprs, data = NULL, by = NULL, enclos = NULL, try = FALSE) {
    if (is.numeric(exprs)) 
        return(envir[exprs])
    qenv <- if (is.quoted(exprs)) 
        attr(exprs, "env")
    else parent.frame()
    if (is.null(data)) 
        data <- qenv
    if (is.data.frame(data) && is.null(enclos)) 
        enclos <- qenv
    if (try) {
        results <- failwith(NULL, ddply, quiet = TRUE) (data, by, apply_maps, 
        	exprs, qenv)    
    } else {
        results <- ddply(data, by, apply_maps, exprs, qenv)    
    }
    results
}

final_aes <- function(map) {
	if(!is.null(map$X)) map$x <- map$X
	if(!is.null(map$Y)) map$y <- map$Y
	map
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