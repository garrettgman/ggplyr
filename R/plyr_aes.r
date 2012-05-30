plyr_aesthetics <- function (., data, plot) {
    aesthetics <- .$layer_mapping(plot$mapping)
    if (!is.null(.$subset)) {
        include <- data.frame(eval.quoted(.$subset, data, plot$env))
        data <- data[rowSums(include, na.rm = TRUE) == ncol(include), ]
    }
    if (!is.null(.$geom_params$group)) {
        aesthetics["group"] <- .$geom_params$group
    }
    scales_add_defaults(plot$scales, data, aesthetics, plot$plot_env)
    compact(eval.plyr(aesthetics, data, c("GLYPH", "PANEL"), plot$plot_env))
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


apply_maps <- function(data, mapping, enclos = parent.frame()) {
	map <- null_omit(mapping)
	vars <- llply(map, eval, envir = data, enclos)
	n <- nrow(data)
	lengths <- unlist(lapply(vars, length))
	wrong <- lengths != 1 & lengths != n
	if (any(wrong)) {
        stop(paste("Aesthetics must either be length one, or the same length as the data", "Problems:", paste(names(wrong)[wrong], collapse = ", ")), call. = FALSE)
    }
  data.frame(vars)
}