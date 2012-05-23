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
    	
    	evaled <- compact(eval.plyr(aesthetics, data, c(".gid", "PANEL"), 
    		plot$plot_env))
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