aesply <- function(data, .var, aesthetics) {
  no.ply <- unlist(lapply(aesthetics, first_name)) == "I"
  if (!any(no.ply)) {
    return(compact(eval.plyr(aesthetics, data, .var)))
  }
  
  all.aes <- lapply(aesthetics[no.ply], remove_I)
  evaled <- compact(eval.quoted(all.aes, data))
  lengths <- vapply(evaled, length, integer(1))
  n <- if (length(lengths) > 0) max(lengths) else 0
  wrong <- lengths != 1 & lengths != n
  if (any(wrong)) {
    stop("Aesthetics must either be length one, or the same length as the data", 
         "Problems:", paste(aesthetics[wrong], collapse = ", "), 
         call. = FALSE)
  }
  data <- cbind(data, evaled)
  all.data <- data.frame(evaled) 
  aesthetics[no.ply] <- lapply(names(aesthetics)[no.ply], as.name)
  compact(eval.plyr(aesthetics, data, .var))
}


remove_I <- function(expr) {
  Identity <- function(x) {
    if (x == "I") x <- quote(identity)
    x
  }
  as.call(lapply(expr, Identity))
}

eval.plyr <- function (exprs, data = NULL, by = NULL, enclos = NULL, 
  try = FALSE) {
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
  vars <- lapply(vars, condense)
  lengths <- unlist(lapply(vars, length))
  wrong <- lengths != 1 & lengths != n
  if (any(wrong)) {
    stop(paste(
      "Aesthetics must either be length one, or the same length as the data", 
      "Problems:", paste(names(wrong)[wrong], collapse = ", ")), 
      call. = FALSE)
  }
  data.frame(vars)
}

condense <- function(var) {
  if (length(unique(var)) == 1) {
    return(unique(var))
  } 
  var
}
geom_coxcomb <- function(mapping = NULL, data = NULL, stat = "bin", 
  position = "identity", ...) { 
  
    GeomCoxcomb$new(mapping = mapping, data = data, stat = stat, 
      position = position, ...)
}

GeomCoxcomb <- proto(ggplot2:::Geom, {
  objname <- "coxcomb"
  
  default_stat <- function(.) StatBin
  default_pos <- function(.) PositionIdentity
  default_aes <- function(.) aes(colour=NA, fill="grey20", size=0.5, linetype=1, 
    weight = 1, alpha = NA)
  
  required_aes <- c("x")
  
  reparameterise <- function(., df, params) {
    df <- transform(df, 
      xmin = 2 * pi / max(x) * (x - 1), 
      xmax = 2 * pi / max(x) * x)
    
    # to create equal areas
    adjust_y <- function(df) {
      if (length(df$y) == 1) {
        df$ymin <- 0
        df$ymax <- df$y
        return(df)
      }
      span <- df$xmax[1] - df$xmin[1]
      df$y[1] <- sqrt(2 / span * df$y[1])
      for ( i in 2:length(df$y)) {
        df$y[i] <- sqrt(2 / span * df$y[i - 1])
      }
      df$ymax <- cumsum(df$y)
      df$ymin <- c(0, df$ymax[-length(df$y)])
      df
    }
    df <- ddply(df, c("x", "PANEL"), adjust_y)
    df$section <- id(df[c("x", "group")], drop = TRUE)
    
    # create polygon points
    poly_curve <- function(df, npoints) {
      non.pos <- setdiff(names(df), c(.x_aes, .y_aes, "count", "ndensity", 
        "ncount", "density"))
      theta <- seq(df$xmin, df$xmax, length = npoints)
      theta <- c(theta, theta[length(theta):1])
      r <- c(rep(df$ymin, npoints), rep(df$ymax, npoints))
      x <- r*cos(theta)
      y <- r*sin(theta)
      df <- df[1, non.pos]
      row.names(df) <- NULL
      df <- cbind(df, x, y)
      rbind(df, df[1, ])
    }
    ddply(df, c("section", "group", "PANEL"), poly_curve, params$npoints)
  }
  
  draw <- draw_groups <- function(., data, scales, coordinates, ...) {
    polys <- dlply(data, c("section", "PANEL"), function(df) {
      ggname("polygon", gTree(children=gList(
        with(coord_munch(coordinates, df, scales), 
             polygonGrob(x, y, default.units="native",
                         gp=gpar(col=colour, fill=alpha(fill, alpha), lwd=size * .pt,
                                 lty=linetype))
        )
      )))
    })
    ggname("coxcomb", do.call("grobTree", polys))               
  }
  
  guide_geom <- function(.) "polygon"
  
  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))
    with(data, grobTree(
      rectGrob(gp = gpar(col = colour, fill = alpha(fill, alpha), 
        lty = linetype)),
      linesGrob(gp = gpar(col = colour, lwd = size * .pt, 
        lineend="butt", lty = linetype))
    ))
  }
  
  new <- function(., mapping = NULL, data = NULL, stat = NULL, 
    position = NULL, npoints = 10, ...) {
    
    missing <- !(c("angle") %in% names(mapping))
    if (any(missing)) {
      stop("Missing required aesthetic: angle", call. = FALSE)
    }
    names(mapping)[names(mapping) == "angle"] <- "x"
    mapping$section <- coxcomb_sections(mapping)
    
    lyr <- do.call("layer", list(mapping = mapping, data = data, stat = stat, geom = ., 
      position = position, ...))  
    lyr$geom_params$npoints <- npoints
    lyr
  }
  
})

coxcomb_sections <- function(mapping) {
  sections <- mapping[c("alpha", "fill", "colour")]
  sections <- sections[!unlist(lapply(sections, is.null))]
  names(sections) <- NULL
  if (is.null(sections)) return(NULL)
  as.call(c(quote(interaction), sections))
}#' geom_ref_point calls a point geom but ensures that only one point is drawn per 
#' glyph
geom_ref_point <- function (mapping = NULL, data = NULL, stat = "identity", 
                            position = "identity", ...) {
  
  layer <- geom_point(mapping = mapping, data = data, stat = stat, 
                      position = position, ...)
  layer$geom$reparameterise <- function(., data, params) {
    unique(data)
  }
  layer
}#' geom_ref_rect calls a rect geom but ensures that only one box is drawn per 
#' glyph
geom_ref_rect <- function (mapping = NULL, data = NULL, stat = "identity", 
  position = "identity", ...) {
  
  layer <- geom_rect(mapping = mapping, data = data, stat = stat, 
    position = position, ...)
  layer$geom$reparameterise <- function(., data, params) {
    unique(data)
  }
  layer
}geom_scatterplots <- function(mapping = NULL, glyph.by = NULL, data = NULL,
  width = rel(0.95), height = rel(0.95), x_scale = identity, y_scale = identity,
  merge.overlaps = FALSE, reference = NULL, ...) {
  
  required <- c("x", "y", "minor.x", "minor.y")
  missing <- !(required %in% names(mapping))
  if (any(missing)) {
    stop(paste("geom_scatterplots requires the following missing aesthetics:",
     paste(required[missing], collapse = ", ")), call. = FALSE)
  }
  major <- mapping[c("x", "y")]
  mapping$x <- NULL
  mapping$y <- NULL
  names(mapping)[names(mapping) == "minor.x"] <- "x"
  names(mapping)[names(mapping) == "minor.y"] <- "y"
  
  glyph(geom_point(mapping = mapping, data = data, ...), major, glyph.by, width, 
    height, x_scale, y_scale, merge.overlaps, reference)
}
  geom_star <- function(mapping = NULL, data = NULL, stat = "identity", 
  position = "identity", ...) { 

    GeomStar$new(mapping = mapping, data = data, stat = stat, 
      position = position, ...)
}


GeomStar <- proto(ggplot2:::Geom, {
  objname <- "star"
  
  # turn cartesian coordinates polar
  reparameterise <- function(., df, params) {    
    # scale x to be between 0 and 2*pi
    df$theta <- unlist(rescale_2pi(df["angle"]))
    df$r <- unlist(rescale_01(df["r"]))
    
    include_origin <- function(data) {
      data <- data[order(data$theta, data$r), ]
      if (data$theta[1] > 0.01) {
        first <- data[1, ]
        first$theta <- 0
        first$r <- 0
        data <- rbind(first, data)
      }
      if (data$theta[length(data$theta)] < 6.27) {
        last <- data[length(data$theta), ]
        last$theta <- 6.28
        last$r <- 0
        data <- rbind(data, last)
      }
      data
    }
    df <- ddply(df, c("group", "PANEL"), include_origin)
    
    df$x <- df$r * cos(df$theta) + df$x
    df$y <- df$r * sin(df$theta) + df$y
    df
  }
  
  draw <- function(., data, scales, coordinates, ...) {
    data <- data[order(data$theta, data$r), ]
    ggname(.$my_name(), 
      gTree(children = gList(
        with(coord_munch(coordinates, data, scales), 
          polygonGrob(x, y, default.units = "native", 					
            gp = gpar(col = colour, fill = alpha(fill, alpha), 
            lwd = size * .pt, lty = linetype)
          )
        )
      ))
    )
  }	
  
  default_stat <- function(.) StatIdentity
  
  default_aes <- function(.) {
    aes(weight = 1, colour = "grey20", fill = "NA", alpha = NA, 
      linetype = "solid", size = 0.5)
  }
  
  required_aes <- c("x", "y", "r", "angle")
  
  guide_geom <- function(.) "polygon"
  
  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))
    with(data, grobTree(
      rectGrob(gp = gpar(col = colour, fill = alpha(fill, alpha), 
        lty = linetype)),
      linesGrob(gp = gpar(col = colour, lwd = size * .pt, 
        lineend="butt", lty = linetype))
    ))
  }
  
  new <- function(., mapping = NULL, data = NULL, stat = NULL, 
    position = NULL, ...) {
    
    missing <- !(c("x", "y", "r", "angle") %in% names(mapping))
    if (any(missing)) {
      stop(paste("Missing required aesthetics for geom_star:",
        paste(c("x", "y", "r", "angle")[missing], collapse = ", ")),
        call. = FALSE)
    }
    
    do.call("layer", list(mapping = mapping, data = data, stat = stat, 
      geom = ., position = position, ...))  
  }
  
})ggtransform <- function(ggobject, mapping = NULL, ...) {
	UseMethod("ggtransform")
}

ggtransform.proto <- function(ggobject, mapping = NULL, ...) {

	ggobject <- layer_clone(ggobject)
	ggobject$mapping <- combine_aes(mapping, ggobject$mapping)
	ggobject$geom_params <- c(ggobject$geom_params, list(...))
	ggobject
}

ggtransform.ggplot <- function(ggobject, mapping = NULL, ...) {

	ggobject <- ggplot2:::plot_clone(ggobject)
	ggobject$mapping <- combine_aes(mapping, ggobject$mapping)
	ggobject$layers <- lapply(ggobject$layers, ggtransform, 
		mapping = mapping, ...)
	labels <- ggobject$options$labels
	ggobject$options$labels <- combine_labels(lapply(mapping, deparse), 
		labels) 
	ggobject
	
}
	
#' @include glyphs-class.r
NULL

check_glayer <- function(object) {
	errors <- character()
	if (!is.proto(object@layer)) {
		msg <- "glayer must be a proto object."
		errors <- c(errors, msg)
	}
	if (length(errors) == 0) 
		TRUE
	else
		errors
}

setOldClass(c("proto", "environment"))

#' glayer class
#'
#' glayers are layers made with ggplyr methods. They are equivalent to the layers made by ggplot2 functions in all ways except that they contain an extra grouping variable (to denote subplot membership) and a plyr_function slot, which correctly locates subplots within the graph when plotting.
#'
#' @name glayer-class
#' @rdname glayer-class
#' @exportClass glayer
#' @aliases show,glayer-method
#' @aliases c,glayer-method
#' @aliases rep,glayer-method
#' @aliases ls,glayer-method
#' @aliases [,glayer-method
#' @aliases [<-,glayer-method
#' @aliases $,glayer-method
#' @aliases $<-,glayer-method
#' @aliases +,ggplot,glayer-method
#' @aliases +,glyphs,glayer-method
#' @aliases ggtransform,glayer-method
setClass("glayer", representation(layer = "proto"), validity = check_glayer)

#' @export
setMethod("show", signature(object = "glayer"), function(object) {
	print(object@layer) 
})

#' @export
setMethod("c", signature(x = "glayer"), function(x, ...){
	# c(get_layer(x), unlist(lapply(list(...), get_layer)))
	stop("object of type 'glayer' is not subsettable")
})

#' @export
setMethod("rep", signature(x = "glayer"), function(x, ...){
	stop("object of type 'glayer' is not subsettable")
})

#' @export
setMethod("[", signature(x = "glayer"), 
	function(x, i, j, ..., drop = TRUE) {
    	new("glayer", layer = x@layer[i])
	}
)

#' @export
setMethod("[<-", signature(x = "glayer"), function(x, i, j, ..., value) {
  	x@layer[i] <- value
	x
})


#' @export
setMethod("$", signature(x = "glayer"), function(x, name) {
	slot(x, "layer")[[name]]
})

#' @export
setMethod("$<-", signature(x = "glayer"), function(x, name, value) {
	slot(x, "layer")[[name]] <- value
	x
})

#' @export
setMethod("+", signature(e1 = "ggplot", e2 = "glayer"), 
	function(e1, e2) {
		glyph_plot(e1 + e2@layer)
	}
)

#' @export
setMethod("+", signature(e1 = "glyphs", e2 = "glayer"), 
	function(e1, e2) {
		glyph_plot(e1@.Data + e2@layer)
	}
)
	
	
#' @export
setMethod("ggtransform", signature(ggobject = "glayer"), 
	function(ggobject, mapping, ...){
		layer <- ggtransform(ggobject@layer, mapping, ...)
		new("glayer", layer = layer)
	}
)

#' @export
setGeneric("ls")

#' @export
setMethod("ls", signature(name = "glayer"), 
	function(name, pos = -1, envir = as.environment(pos), all.names = FALSE, pattern) {
		ls(slot(name, "layer"), all.names)
})
	
#' Create a glayer object
#' 
#' glayer gives a ggplot2 layer object the S4 class glayer, see \code{\link{glayer-class}}. ggplot layer objects are usually non-specific \code{\link{proto}} class objects.
#'
#' @export glayer
#' @param layer a proto object that can be used as a layer by the \code{\link{ggplot2}} package (i.e, ggplot() + layer should return a graph).
glayer <- function(layer) {
	new("glayer", layer = layer)
}#' glayer_build prepares an glyph layer (class glayer) for plotting
glayer_build <- function(layer) {
  if (!("embed" %in% ls(layer))) {
    stop("layer does not have embedded subplots")
  }
	 
  layer <- layer_clone(layer)
  layer$data <- layer$assign_glyphs(layer$data)
  minor <- ggplot_build(ggplot() + layer + facet_wrap("GLYPH")) 

  ### combine subplots (minor) into single plot
  # data
  data <- unpanel(minor$data[[1]])
  data <- layer$combine_glyphs(data)
  data$PANEL <- 1L
	
  # panel
  xspan <- range(unlist(data[names(data) %in% .x_aes]))
  yspan <- range(unlist(data[names(data) %in% .y_aes]))
  panel <- ggplot_build(qplot(xspan, yspan))$panel

  # scales
  scales <- minor$plot$scales$scales
  scales[[which_x(scales)]] <- panel$x_scales[[1]]
  scales[[which_y(scales)]] <- panel$y_scales[[1]]
	
  # axis labels
  if (!is.null(layer$embed$major.aes)) {
    labels <- labs(layer$embed$major.aes)
    minor$plot$options$labels[c("x", "y")] <- labels[c("x", "y")]
  }
		
  # make build
  minor$data <- list(data)
  minor$panel <- panel
  minor$plot$facet <- facet_null()
  minor$plot$scales$scales <- scales
	
  minor
}


unpanel <- function(df) {
  if (!is.null(df$group)) {
    df$group <- interaction(df$group, df$PANEL)
  } 
  df$GLYPH <- as.numeric(as.character(df$PANEL))
  df$PANEL <- NULL
  df
}


which_x <- function(scales) {
  vars <-  names_scales(scales)
  which(vars == "x")
}


which_y <- function(scales) {
  vars <- names_scales(scales)
  which(vars == "y")
}

#' Returns the first aes of a scale, to use as a name
names_scales <- function(scales) {
  unlist(lapply(scales, function(s) s$aesthetics[[1]]))
}	
	#' glyph turns an ordinary layer into a set of glyphs. To do this, it must:
#' 1. ensure that the x and y aesthetics are mapped to a single value
#' 2. add reference boxes
#' 3. handle merging
#' 4. split up the data
glyph <- function(layer, major.aes, glyph.by = NULL, width = rel(0.95), 
  height = rel(0.95), x_scale = identity, y_scale = identity, 
  merge.overlaps = FALSE, reference = NULL, ply.aes = TRUE, .ref = FALSE) {
  
  missing <- c(is.null(major.aes$x), is.null(major.aes$y))
  if (any(missing)) {
    stop(paste("Missing required aesthetic in major.aes:", 
      paste(c("x", "y")[missing], collapse = ", ")))
  }
  
  if (is.null(glyph.by)) {
    stop("Missing required argument in glyph: glyph.by", call. = FALSE)
  }

  if (!is.function(glyph.by)) {
    glyph.by <- group_by(glyph.by)
  }
  
  layer <- layer_clone(layer)
  layer$embed <- list(width = width, height = height, 
    x_scale = x_scale, y_scale = y_scale, merge.overlaps = merge.overlaps,
    major.aes = major.aes[c("x", "y")], glyph.by = glyph.by)
  layer$assign_glyphs <- assign_glyphs
  layer$combine_glyphs <- combine_glyphs
  if (.ref) layer$combine_glyphs <- combine_refs
  #layer$compute_aesthetics <- plyr_aesthetics
    
  if (is.null(reference)) {
    if (ply.aes) {
  	  ply_aes(glayer(layer))
    } else {
      glayer(layer)
    }
  } else {
  	ref.layer <- reference(layer, "glyph", major.aes, glyph.by, width, height, 
  	  merge.overlaps)
    if (ply.aes) {
  	  list(ref.layer, ply_aes(glayer(layer)))
    } else {
      list(ref.layer, glayer(layer))
    }
  }
}

  
assign_glyphs <- function(., data) {
  # major x and y
  data$GLYPH <- embed$glyph.by(data)
  globals <- aesply(data, "GLYPH", embed$major.aes)
  too.many <- c(length(unique(globals$x)) > length(unique(globals$GLYPH)), 
    length(unique(globals$y)) > length(unique(globals$GLYPH)))
  if (any(too.many)) {
    message(paste("Major", paste(c("x", "y")[too.many], collapse = " and "), 
      "return more than one value per glyph. Only using first."))
    globals <- unique(ddply(globals, "GLYPH", transform, x = x[1], y = y[1]))
  }
  
  # parse width, height
  width <- embed$width
  height <- embed$height
  if (is.rel(width)) {
    .$embed$width <- width <- max(resolution(vet(globals$x), zero = FALSE) * 
      unclass(width), (diff(range(vet(globals$x))) + unclass(width)) / 
      length(unique(globals$x)) * unclass(width))
  }
  if (is.rel(height)) {
    .$embed$height <- height <- max(resolution(vet(globals$y), zero = FALSE) * 
      unclass(height), (diff(range(vet(globals$y))) + unclass(height)) / 
      length(unique(globals$y)) * unclass(height))
  }

  if (embed$merge) {
    # search for overlapping glyphs, combine
    data$.gid <- factor(data$GLYPH)
    merge.key <- merge_overlaps(globals, embed$width, embed$height)
    data$GLYPH <- merge.key[data$GLYPH]
    globals <- aesply(data, "GLYPH", embed$major.aes)
    .$mapping <- add_gid(mapping)
    
    too.many <- c(length(unique(globals$x)) > length(unique(globals$GLYPH)), 
                  length(unique(globals$y)) > length(unique(globals$GLYPH)))
    if (any(too.many)) {
      message(paste("Major", paste(c("x", "y")[too.many], collapse = " and "), 
                    "return more than one value per glyph. Only using first."))
      globals <- unique(ddply(globals, "GLYPH", transform, x = x[1], y = y[1]))
    }
  }
  .$embed$globals <- globals
  data
}
  
combine_glyphs <- function(., data) {  
  data <- join(data, globalize(embed$globals), by = "GLYPH")
  
  xvar <- get_xs(data)
  yvar <- get_ys(data)
  
  # scale if necessary
  if (!identical(embed$x_scale, identity) || 
    !identical(embed$y_scale, identity)) {
    data <- ddply(data, "GLYPH", function(df) {
      df[xvar] <- embed$x_scale(df[xvar])
      df[yvar] <- embed$y_scale(df[yvar])
      df
    })
  }
  
  # update x and y related variables
  # don't scale individually or xmin and xmax's will end up on top of 
  # one another
  data[xvar] <- vet(data$X) + rescale_11(data[xvar]) * embed$width/2
  data[yvar] <- vet(data$Y) + rescale_11(data[yvar]) * embed$height/2
  
  data$X <- NULL
  data$Y <- NULL
  data
}  
  
combine_refs <- function(., data) {  
  data <- join(data, globalize(embed$globals), by = "GLYPH")
		
  xvar <- get_xs(data)
  yvar <- get_ys(data)
		
	# scale if necessary
  if (!identical(embed$x_scale, identity) || 
    !identical(embed$y_scale, identity)) {
    data <- ddply(data, "GLYPH", function(df) {
      df[xvar] <- embed$x_scale(df[xvar])
	  df[yvar] <- embed$y_scale(df[yvar])
	  df
	})
  }
	
  # update x and y related variables
  # don't scale individually or xmin and xmax's will end up on top of 
  # one another
  data[xvar] <- vet(data$X) + data[xvar] * embed$width/2
  data[yvar] <- vet(data$Y) + data[yvar] * embed$height/2
		
  data$X <- NULL
  data$Y <- NULL
  data
}



vet <- function(x) {
  if (is.character(x)) {
    x <- as.numeric(factor(x))
  }	
  if (is.factor(x)) {
  	x <- as.numeric(x)
  }
  x
}

globalize <- function(obj){
	names(obj)[names(obj) == "x"] <- "X"
	names(obj)[names(obj) == "y"] <- "Y"
	obj
}


#' add_gid intelligently adds the .gid variable to the group slot of an uneval 
#' object. If the group slot is NULL, add_gid sets group = .gid. If the group 
#' slot already contains a mapping, add_gid adds .gid to this mapping with 
#' interaction().
#'
#' @keywords internal
#' @param aes_group the group value of an uneval object
#' @export
add_gid <- function(maps) {
  if (is.null(maps$group)) {
    maps$group <- as.name(".gid")
  } else {
    maps$group <- as.call(list(quote(interaction), as.name(".gid"), 
      maps$group))
  }
  maps
}glyph_aesthetics <- function (., data, plot) {
  aesthetics <- .$layer_mapping(plot$mapping)
  if (!is.null(.$subset)) {
    include <- data.frame(eval.quoted(.$subset, data, plot$env))
    data <- data[rowSums(include, na.rm = TRUE) == ncol(include), ]
  }
  if (!is.null(.$geom_params$group)) {
    aesthetics["group"] <- .$geom_params$group
  }
  scales_add_defaults(plot$scales, data, aesthetics, plot$plot_env)
  compact(eval.glyph(aesthetics, data, c("GLYPH", "PANEL"), plot$plot_env))
}

eval.glyph <- function (exprs, data = NULL, by = NULL, enclos = NULL, try = FALSE) {
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
    results <- failwith(NULL, ddply, quiet = TRUE) (data, by, apply_glyphs, exprs, qenv)    
  } else {
    results <- ddply(data, by, apply_glyphs, exprs, qenv)    
  }
  results
}

apply_glyphs <- function(data, mapping, enclos = parent.frame()) {
  map <- null_omit(mapping)
  vars <- llply(map, eval, envir = data, enclos)
  n <- nrow(data)
  lengths <- unlist(lapply(vars, length))
  wrong <- lengths != 1 & lengths != n
  if (any(wrong)) {
    stop(paste("Aesthetics must either be length one, or the same length as the data", "Problems:", paste(names(wrong)[wrong], collapse = ", ")), call. = FALSE)
  }
  xy <- lengths[c("x", "y")]
  wrong <- xy != 1
  if (any(wrong)) {
    vars[["x"]] <- vars[["x"]][1]
    vars[["y"]] <- vars[["y"]][1]
    message(paste("glyph mapped to multiple " , paste(names(wrong)[wrong], 
      collapse = " and "), ". Using first value.", sep = ""))
  }
  data.frame(vars)
}#' Build a glyph object for rendering
#'
#' This function takes the plot object, and performs all steps necessary to produce an object that can be rendered.
glyph_build <- function(plot){
  if (length(plot$layers) == 0) stop("No layers in plot", call.=FALSE)
  if (!identical(plot$facet, facet_null())) {
  	stop("glyphs do not support facetting", call. = FALSE)
  }
	
  plot <- ggplot2:::plot_clone(plot)
  layers <- plot$layers
  layers <- propogate_data(layers, plot$data)
	
  # separate into glayers and normal layers
  gls <- unlist(lapply(layers, is.glayer))
  if (all(!gls)) return(ggplot_build(plot))
  if (all(gls) && sum(gls) == 1) return(glayer_build(layers[[gls]]))
  glayers <- layers[gls]
  plot$layers <- layers[!gls]
  gl.order <- seq_along(layers)[gls]
  nl.order <- seq_along(layers)[!gls]
	
  # build normal layers
  normal <- NULL
  if (length(plot$layers) > 0) {
	normal <- ggplot_build(plot)
  }
	
  # build glyph layers (embedded plots)
  embedded <- list()
  for (i in seq_along(glayers)) {
    embedded[[i]] <- glayer_build(glayers[[i]])
  }
	
	
  ### combine the builds
	
  # plot
  build <- embedded[[1]]
	
  # data
  # take care to order
  edata <- lapply(embedded, function(bd) bd$data[[1]])
  data <- list()
  data[gl.order] <- edata
  data[nl.order] <- normal$data
	
  # panel
  xspan <- range(unlist(lapply(data, function(df) df[names(df) %in% .x_aes])))
  yspan <- range(unlist(lapply(data, function(df) df[names(df) %in% .y_aes])))
  panel <- ggplot_build(qplot(xspan, yspan))$panel
	
  # scales 
  # collect all unique scales
  scales <- build$plot$scales$scales
  scales[[which_x(scales)]] <- panel$x_scales[[1]]
  scales[[which_y(scales)]] <- panel$y_scales[[1]]
  scale.names <- names_scales(scales)
  for (i in seq_along(embedded[-1])) {
    escales <- embedded[[i + 1]]$plot$scales$scales
	unique <- !(names_scales(escales) %in% scale.names)
	scales <- c(scales, escales[unique])
	scale.names <- names_scales(scales)
  }
  nscales <- normal$plot$scales$scales
  unique <- !(names_scales(nscales) %in% scale.names)
  scales <- c(scales, nscales[unique])
	
  # layers
  # take care to order
  gl.layers <- build$plot$layers
  for (i in seq_along(embedded[-1])) {
    gl.layers <- c(gl.layers, embedded[[i + 1]]$plot$layers)
  }
  layers[gl.order] <- gl.layers
  layers[nl.order] <- normal$plot$layers
  
  # labels 
  # collect all unique labels
  labels <- build$plot$option$labels
  for (i in seq_along(embedded[-1])) {
  	new.labels <- embedded[[i+1]]$plot$options$labels
  	unique <- !(names(new.labels) %in% names(labels))
  	labels <- c(labels, new.labels[unique])
  }
  norm.labels <- normal$plot$options$labels
  unique <- !(names(norm.labels) %in% names(labels))
  labels <- c(labels, norm.labels[unique])
	
  # make build
  build$data <- data
  build$panel <- panel
  build$plot$scales$scales <- scales
  build$plot$layers <- layers
  build$plot$options$labels <- labels
	
  build
}




is.glayer <- function(x) {
	"embed" %in% ls(x)
}

propogate_data <- function(layers, plot_data) {
	ensure_data <- function(layer){
		if (inherits(layer$data, "waiver")) {
			layer$data <- plot_data
		}
		layer
	}
	lapply(layers, ensure_data)
}#' @include ggtransform.r
NULL

check_glyphs <- function(object) {
	errors <- character()
	if (!is(object@.Data, "ggplot")) {
		msg <- "glyphs must be a ggplot object."
		errors <- c(errors, msg)
	}
	if (length(errors) == 0) 
		TRUE
	else
		errors
}

setOldClass(c("ggplot", "list"))

#' glyphs class
#'
#' a glyphs object is a ggplot object that has been extended to include methods for applying plyr when plotting. 
#'
#' @name glyphs-class
#' @rdname glyphs-class
#' @exportClass glyphs
#' @aliases show,glyphs-method
#' @aliases ggtransform,glyphs-method
setClass("glyphs", contains = c("ggplot"), validity = check_glyphs)

#' @export
setMethod("show", signature(object = "glyphs"), function(object){
	print(object)
})

#' @S3method print ggplyr
print.glyphs <- function(x, newpage = is.null(vp), vp = NULL, ...) {
    ggplot2:::set_last_plot(x)
    if (newpage) 
        grid.newpage()
    data <- glyph_build(x)
    gtable <- ggplot_gtable(data)
    if (is.null(vp)) {
        grid.draw(gtable)
    }
    else {
        if (is.character(vp)) 
            seekViewport(vp)
        else pushViewport(vp)
        grid.draw(gtable)
        upViewport()
    }
    invisible(data)
}

#' @export
setGeneric("ggtransform")

#' @export
setMethod("ggtransform", signature(ggobject = "glyphs"), 
	function(ggobject, mapping, ...){
		ggplot <- ggtransform(ggobject@.Data, mapping, ...)
		new("glyphs", ggplot)
	}
)




#' Create a glyphs object
#' 
#' glyph_plot gives a ggplot object the S4 class ggplyr, see \code{\link{glyphs-class}}. ggplyr denotes ggplot objects that contain extra information to be used to apply plyr functions when plotting.
#' 
#' @export glyph_plot
#' @param ggplot a ggplot object
glyph_plot <- function(ggplot) {
	new("glyphs", ggplot)
}

#' grid turns an ordinary layer into a gridded version of itself. 
grid <- function(layer, grid.aes = aes(), x.nbin = 10, y.nbin = 10, 
  x_scale = identity, y_scale = identity, width.adjust = 0.95, 
  height.adjust = 0.95, reference = NULL, ply.aes = TRUE, .ref = FALSE) {
  
  if (is.glayer(layer) || is.list(layer)) {
    stop("Cannot grid glayer", call. = TRUE)
  }
 # if ("plyr" %in% ls(layer)) {
  #  stop("grid and ply_aes used in wrong order.", call. = FALSE)
  #}
  
  # aes
  if(is.null(grid.aes$x)) {
    if (is.null(layer$mapping$x)) {
      stop("Grid requires an x aesthetic to be specified")
    }
    grid.aes$x <- layer$mapping$x
  }
  if(is.null(grid.aes$y)) {
    if (is.null(layer$mapping$y)) {
      stop("Grid requires a y aesthetic to be specified")
    }
    grid.aes$y <- layer$mapping$y
  }
    
  
  layer <- layer_clone(layer)
  layer$embed <- list(x_scale = x_scale, y_scale = y_scale,
    major.aes = grid.aes[c("x", "y")], grid_env = parent.frame(),
    x.nbin = x.nbin, y.nbin = y.nbin, w.adjust = width.adjust, 
    h.adjust = height.adjust, ref = .ref)
  layer$assign_glyphs <- assign_grid
  layer$combine_glyphs <- combine_glyphs
  if (.ref) layer$combine_glyphs <- combine_refs
  #layer$compute_aesthetics <- plyr_aesthetics
  
  if (is.null(reference)) {
    if (ply.aes) {
      ply_aes(glayer(layer))
    } else {
      glayer(layer)
    }
  } else {
    ref.layer <- reference(layer, "grid", grid.aes, x.nbin = x.nbin, 
      y.nbin = y.nbin)
    if (ply.aes) {
      list(ref.layer, ply_aes(glayer(layer)))
    } else {
      list(ref.layer, glayer(layer))
    }
  }
}



assign_grid <- function(., data) {
  # major x and y
  x <- eval(embed$major.aes$x, envir = data, enclos = embed$grid_env)
  x.range <- range(x)
  x.breaks <- seq(x.range[1], x.range[2], length = embed$x.nbin + 1)
  width <- diff(x.range) / embed$x.nbin * embed$w.adjust
  # positions in bottom left corner, not on ref box line
  x.val <- (x.breaks + (1 + 0.05 * !embed$ref) * width/2)[-(embed$x.nbin + 1)]
  x.glyph <- cut(x, x.breaks, labels = x.val, include.lowest = TRUE)
  
  y <- eval(embed$major.aes$y, envir = data, enclos = embed$grid_env)
  y.range <- range(y)
  y.breaks <- seq(y.range[1], y.range[2], length = embed$y.nbin + 1)
  height <- diff(y.range) / embed$y.nbin * embed$h.adjust
  y.val <- (y.breaks + (1 + 0.05 * !embed$ref) * height/2)[-(embed$y.nbin + 1)]
  y.glyph <- cut(y, y.breaks, labels = y.val, include.lowest = TRUE)
  
  data$GLYPH <- as.numeric(interaction(x.glyph, y.glyph, drop = TRUE))
  globals <- unique(data.frame(GLYPH = data$GLYPH, 
    x = as.numeric(as.character(x.glyph)),
    y = as.numeric(as.character(y.glyph))))
                        
  .$embed$width <- width 
  .$embed$height <- height 
  .$embed$globals <- globals
  
  data
}#' grid_by is a second order function: it returns a function that can be used on 
#' a data set to provide a grouping variable.
grid_by <- function(x, y, x.npoints = 10, y.npoints = 10, x.range. = NULL, 
  y.range. = NULL) {
  
  if(inherits(try(x, silent = TRUE), "try-error")) {
    x <- as.character(substitute(x))
  }
  if(inherits(try(y, silent = TRUE), "try-error")) {
    y <- as.character(substitute(y))
  }
  
  function(df) {
    missing <- !(c(x, y) %in% names(df))
    if (any(missing)) {
      stop(paste(paste(c(x, y)[missing], collapse = ", "), 
        "variables not found in data.frame"), call. = FALSE)
    }
    if (is.null(x.range.)) {
      df$.X_GRID <- grid(df[[x]], x.npoints)
    } else {
      df$.X_GRID <- grid(df[[x]], x.npoints, x.range.)
    }
    if (is.null(y.range.)) {
      df$.Y_GRID <- grid(df[[y]], y.npoints)
    } else {
      df$.Y_GRID <- grid(df[[y]], y.npoints, y.range.)
    }
    id(df[c(".X_GRID", ".Y_GRID")], drop = TRUE)
  }
}#' group_by is a second order function: it returns a function that can be used on a data set to provide a grouping variable.
group_by <- function(vars) {
  if (!is.character(vars)) {
    stop("vars should be of class 'character'", call. = FALSE)
  }
  
  function(df) {
    missing <- !(vars %in% names(df))
    if (any(missing)) {
      stop(paste(paste(vars[missing], collapse = ", "), 
        "variables not found in data.frame"), call. = FALSE)
    }
    id(df[vars], drop = TRUE)
  }
}map_glyph <- function(data, layer){
  if("map_glyphs" %in% ls(layer)) {
    data <- layer$map_glyphs(data)
  }
  data
}merge_overlaps <- function(globals, width, height) {
  x.overlaps <- abs(outer(globals$x, globals$x, "-")) < width
  y.overlaps <- abs(outer(globals$y, globals$y, "-")) < height
  overlaps <- data.frame(x.overlaps & y.overlaps)
  names(overlaps) <- as.character(globals$GLYPH)

  for (i in seq_along(overlaps)) {
    names(overlaps)[overlaps[[i]]] <- names(overlaps)[i]
  }
  vec <- as.numeric(factor(names(overlaps)))
  names(vec) <- globals$GLYPH
  vec
}
  #' ply_aes causes the aesthetics of a layer to be computed groupwise.
ply_aes <- function(layer, .vars = NULL) {
  UseMethod("ply_aes")
}

ply_aes.list <- function(layer, .vars = NULL) {
  lapply(layer, ply_aes, .vars)
}

ply_aes.glayer <- function(layer, .vars = NULL) {
  if (!is.null(.vars)) {
    layer$plyr <- list(ply.by = .vars)
  } else {
    layer$plyr <- list()
  }
  layer$compute_aesthetics <- plyr_aesthetics
  layer
}
  
ply_aes.proto <- function(layer, .vars = NULL) {
  if (!is.null(.vars)) {
    layer$plyr <- list(ply.by = .vars)
  } else {
    layer$plyr <- list()
  }
  layer$compute_aesthetics <- plyr_aesthetics
  layer
}#' what replaces compute_aesthetics
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

  if (!is.null(aesthetics$group)) {
    data$group <- unlist(eval(aesthetics$group, envir = data, 
      enclos = plot$plot_env))
    aesthetics$group <- quote(group)
  }
  if ("GLYPH" %in% names(data)) {
    aesthetics$GLYPH <- quote(GLYPH)
  }
  aesthetics$PANEL <- quote(PANEL)
  
  criteria <- c("group", "GLYPH", "PANEL", .$plyr$ply.by)
  criteria <- criteria[criteria %in% names(data)]  
  data$ply.by <- id(data[criteria], drop = TRUE)
  
  data <- aesply(data, "ply.by", aesthetics)
  data$ply.by <- NULL
  data
}
#' plyr_stats is just like ggplot2's internal method calculate_stats but it splits on combinations of PANEL and GLYPH instead of just PANEL
plyr_stats <- function (panel, data, layers) {
    lapply(seq_along(data), function(i) {
        d <- data[[i]]
        l <- layers[[i]]
        ddply(d, c("PANEL", "GLYPH"), function(panel_data) {
            scales <- ggplot2:::panel_scales(panel, panel_data$PANEL[1])
            l$calc_statistic(panel_data, scales)
        })
    })
}ref_box <- function(mapping = NULL, fill = "grey90", color = "white", ...) {	
  function(layer, type, major.aes, glyph.by = NULL, width = rel(1), 
  height = rel(1), merge.overlaps = FALSE, x.nbin = 10, y.nbin = 10) {
  	
  	def_aes <- aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1)
  	mapping <- c(mapping, def_aes[setdiff(names(def_aes), names(mapping))])
  	class(mapping) <- "uneval"
  	
    rlayer <- ply_aes(geom_ref_rect(mapping = mapping, ...))
  	if (is.null(mapping$fill)) rlayer$geom_params$fill <- fill
  	if (is.null(mapping$colour)) rlayer$geom_params$colour <- color
  	if (!inherits(layer$data, "waiver")) rlayer$data <- layer$data
  	
    switch(type,
      glyph = glyph(rlayer, major.aes = major.aes, glyph.by = glyph.by, 
        width = width, height = height, merge.overlaps = merge.overlaps, 
        ref = NULL, .ref = TRUE), 
      grid = grid(rlayer, grid.aes = major.aes, x.nbin = x.nbin, 
        y.nbin = y.nbin, width.adjust = 1, height.adjust = 1, .ref = TRUE)
    )
  }
} 



ref_hline <- function(mapping = NULL, width. = 0.2, fill = "white", ...) {	
  function(layer, type, major.aes, glyph.by = NULL, width = rel(1), 
  height = rel(1), merge.overlaps = FALSE) {

  	def_aes <- list(xmin = -1, xmax = 1, ymin = -width./2, ymax = width./2)
  	mapping <- c(mapping, def_aes[setdiff(names(def_aes), names(mapping))])
  	class(mapping) <- "uneval"
  	
  	rlayer <- ply_aes(geom_ref_rect(mapping = mapping, ...))
  	if (is.null(mapping$fill)) rlayer$geom_params$fill <- fill
  	if (!inherits(layer$data, "waiver")) rlayer$data <- layer$data
  	
  	switch(type,
  	  glyph = glyph(rlayer, major.aes = major.aes, glyph.by = glyph.by, 
  	    width = width, height = height, merge.overlaps = merge.overlaps, ref = NULL, 
  	    .ref = TRUE), 
  	  grid = grid(rlayer, grid.aes = major.aes, x.nbin = x.nbin, 
  	    y.nbin = y.nbin, width.adjust = 1, height.adjust = 1, .ref = TRUE)
  	)
  }
} 

ref_vline <- function(mapping = NULL, width. = 0.2, fill = "white", ...) {	
  function(layer, type, major.aes, glyph.by = NULL, width = rel(1), 
  height = rel(1), merge.overlaps = FALSE) {
  	
  	def_aes <- list(xmin = -width./2, xmax = width./2, ymin = -1, ymax = 1)
  	mapping <- c(mapping, def_aes[setdiff(names(def_aes), names(mapping))])
  	class(mapping) <- "uneval"
  	
  	rlayer <- ply_aes(geom_ref_rect(mapping = mapping, ...))
  	if (is.null(mapping$fill)) rlayer$geom_params$fill <- fill
  	if (!inherits(layer$data, "waiver")) rlayer$data <- layer$data
  	
  	switch(type,
  	  glyph = glyph(rlayer, major.aes = major.aes, glyph.by = glyph.by, 
  	    width = width, height = height, merge = merge.overlaps, ref = NULL, 
  	    .ref = TRUE), 
  	  grid = grid(rlayer, grid.aes = major.aes, x.nbin = x.nbin, 
  	    y.nbin = y.nbin, width.adjust = 1, height.adjust = 1, .ref = TRUE)
  	)
  }
} 

ref_points <- function(mapping = NULL, fill = "white", size = 1/2, ...) {	
  function(layer, type, major.aes, glyph.by = NULL, width = rel(1), 
  height = rel(1), merge.overlaps = FALSE) {
  	
  	corner <- function(def_aes) {
  	  mapping <- c(mapping, def_aes[setdiff(names(def_aes), names(mapping))])
  	  class(mapping) <- "uneval"
  	
  	  rlayer <- ply_aes(geom_ref_point(mapping = mapping, ...))
  	  if (is.null(mapping$fill)) rlayer$geom_params$fill <- fill
  	  if (is.null(mapping$size)) rlayer$geom_params$size <- size
  	  if (!inherits(layer$data, "waiver")) rlayer$data <- layer$data
  	
  	  switch(type,
  	    glyph = glyph(rlayer, major.aes = major.aes, glyph.by = glyph.by, 
  	      width = width, height = height, merge = merge.overlaps, ref = NULL, 
  	      .ref = TRUE), 
  	    grid = grid(rlayer, grid.aes = major.aes, x.nbin = x.nbin, 
  	      y.nbin = y.nbin, width.adjust = 1, height.adjust = 1, .ref = TRUE)
  	  )
  	}
  	list(corner(aes(x = -1, y = -1)),
  	  corner(aes(x = -1, y = 1)),
  	  corner(aes(x = 1, y = -1)),
  	  corner(aes(x = 1, y = 1)))
  }
}  
# Relative dimensions ------------------------------------------------------
rel <- function(x) {
  structure(x, class = "rel")
}

print.rel <- function(x, ...) print(noquote(paste(x, " *", sep = "")))

is.rel <- function(x) inherits(x, "rel")rescale_01 <- free <- function(xvars, xlim=NULL) {
	xnames <- names(xvars)
	numberfy <- function(x) {
		if (is.character(x)) {
			x <- as.numeric(factor(x))
		}	
		if (is.factor(x)) {
			x <- as.numeric(x)
		}
    if (inherits(x, "POSIXt")) {
      x <- as.numeric(x)
    }
		x
	}
	xvars <- lapply(xvars, numberfy)
  
	if (is.null(xlim)) {
		rng <- range(unlist(xvars), na.rm = TRUE)
	} else {
		rng <- xlim
	}
	
	scale <- function(x) {
		if ((rng[2] - rng[1]) == 0) {
			x - rng[1]
		} else {	
			(x - rng[1]) / (rng[2] - rng[1])
		}
	}
	xvars <- lapply(xvars, scale)
	names(xvars) <- xnames
	data.frame(xvars)
}


rescale_11 <- function(xvars, xlim=NULL) {
	2 * rescale_01(xvars, xlim) - 1
}

rescale_2pi <- function(xvars, xlim = NULL) {
  2 * pi * rescale_01(xvars, xlim)
}

.x_aes <- c("x", "xend", "xmax", "xmin")
.y_aes <- c("y", "yend", "ymax", "ymin")

#' retrieves the first name used in an expression. To be used with screening 
#' mappings for plyr or regular computation.
first_name <- function(expr) {
  names <- all.names(expr)
  names <- names[names != "["]
  firsts <- names[1]
  firsts[is.na(firsts)] <- ""
  firsts
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


layer_clone <- function(layer) {
  UseMethod("layer_clone")
}

layer_clone.proto <- function(layer) {
  ggplot2:::plot_clone(ggplot() + layer)$layers[[1]]
}

layer_clone.glayer <- function(layer) {
  glayer(ggplot2:::plot_clone(ggplot() + layer)$layers[[1]])
}

layer_clone.list <- function(layer) {
  lapply(layer, layer_clone)
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