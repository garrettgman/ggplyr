#' Evaluate aesthetics by group
#' 
#' aesply splits a data frame into pieces, evaluates a list of aesthetics within 
#' each piece, and combines the results into a new data frame. Each aesthetic in 
#' the list must return either a single value per piece or a single value per 
#' row in the piece.
#' 
#' @keywords internal
#' @param data a data frame
#' @param .vars a vector of variables to split data by. Each element should be 
#' the name of a variable in data saved as a character string.
#' @param aesthetics an object of class uneval, usually the output of 
#' \code{\link[ggplot2]{aes}}
#' @return a data frame
#' @export
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

#' replace I() with identity
#' 
#' remove_I searches through an expression for the \code{\link{I}} function and 
#' replaces it with \code{\link{identity}}. remove_I is used when an aesthetic 
#' has been surrounded with I() to prevent groupwise calculation.
#' @param expr an expression
remove_I <- function(expr) {
  Identity <- function(x) {
    if (x == "I") x <- quote(identity)
    x
  }
  as.call(lapply(expr, Identity))
}

#' Evaluate a list of expressions by group
#' 
#' Evaluates qoted variables by group in a data frame. Based on 
#' \code{\link[plyr]{eval.quoted}}.
#' @keywords internal
#' @param exprs a list of expressions
#' @param data a data frame
#' @param by a vector of character strings that specify variable names in data. 
#' data will be split into groups based on the unique combinations of the values 
#' of these variables within the data frame. exprs will be evaluated separately 
#' for each group.
#' @param enclos an environment
#' @return a data frame formed by combining the results of evaluating exprs in 
#' each group of data
#' @export
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

#' Calculate aesthetic values for a data frame
#' 
#' apply_maps evaluates a mapping within a data frame to calculate aesthetic 
#' values. apply_maps is intended to be used in conjunction with 
#' \code{\link[plyr]{ddply}}. If each mapping returns a single value, 
#' apply_mapping will return a single row of data. This provides a convenient 
#' way to reduce a set of geoms, similar to using \code{\link[plyr]{summarise}}
#' with ddply.
#' @keywords internal
#' @param data a data frame
#' @param mapping an object of class uneval, usually the output of 
#' \code{\link[ggplot2]{aes}} 
#' @param enclos and environment
#' @return a data frame
#' @export
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

#' reduce a single valued vector to a single element
#' @keywords internal
#' @param var a vector
#' @return a vector of length one if var only contains one unique value, var 
#' otherwise
condense <- function(var) {
  if (length(unique(var)) == 1) {
    return(unique(var))
  } 
  var
}
#' Coxcomb glyphs
#' 
#' geom_coxcomb draws the type of glyph commonly called a coxcomb plot or polar 
#' area plot, popularized by Florence Nightingale.
#' 
#' @param mapping The aesthetic mapping, usually constructed with 
#' \code{\link[ggplot2]{aes}}. Only needs to be set at the layer level if you 
#' are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override 
#' the plot defaults
#' @param stat The statistical transformation to use for this layer.
#' @param position The position adjustment to use for overlapping points in this 
#' layer
#' @param npoints the number of points to use when drawing the arcs with line 
#' segments. Defaults to 10.
#' @param na.rm If FALSE (the default), removes missing values with a warning. 
#' If TRUE, silently removes missing variables.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This 
#' can include aesthetics whose values you want to set, not map. See 
#' \code{\link[ggplot2]{layer}} for more details.
#' 
#' @section Aesthetics
#' geom_coxcomb understands the following aesthetics: x, y, colour, fill, size, 
#' linetype, weight, and alpha.
#' 
#' @export
geom_coxcomb <- function(mapping = NULL, data = NULL, stat = "bin", 
  position = "identity", npoints = 10, na.rm = FALSE, ...) { 
  
    GeomCoxcomb$new(mapping = mapping, data = data, stat = stat, 
      position = position, npoints = npoints, na.rm = na.rm, ...)
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
    position = NULL, npoints = 10, na.rm = FALSE, ...) {
    
    missing <- !(c("angle") %in% names(mapping))
    if (any(missing)) {
      stop("Missing required aesthetic: angle", call. = FALSE)
    }
    names(mapping)[names(mapping) == "angle"] <- "x"
    mapping$section <- coxcomb_sections(mapping)
    
    lyr <- do.call("layer", list(mapping = mapping, data = data, stat = stat, 
      geom = ., position = position, na.rm = na.rm, ...))  
    lyr$geom_params$npoints <- npoints
    lyr
  }
  
})

# ensures that continuous fill, alpha, and colour variables generate groups at 
# build time as if they were discrete
coxcomb_sections <- function(mapping) {
  sections <- mapping[c("alpha", "fill", "colour")]
  sections <- sections[!unlist(lapply(sections, is.null))]
  names(sections) <- NULL
  if (is.null(sections)) return(NULL)
  as.call(c(quote(interaction), sections))
}#' Star glyphs
#' 
#' geom_star draws the type of glyph commonly called a star plot, radar plot, 
#' or polar plot.
#' 
#' @param mapping The aesthetic mapping, usually constructed with 
#' \code{\link[ggplot2]{aes}}. Only needs to be set at the layer level if you 
#' are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override 
#' the plot defaults
#' @param stat The statistical transformation to use for this layer.
#' @param position The position adjustment to use for overlapping points in this 
#' layer
#' @param na.rm If FALSE (the default), removes missing values with a warning. 
#' If TRUE, silently removes missing variables.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This 
#' can include aesthetics whose values you want to set, not map. See 
#' \code{\link[ggplot2]{layer}} for more details.
#' 
#' @section Aesthetics
#' geom_coxcomb understands the following aesthetics: x, y, colour, fill, size, 
#' linetype, weight, and alpha.
#' 
#' @export
geom_star <- function(mapping = NULL, data = NULL, stat = "identity", 
  position = "identity", na.rm = FALSE, ...) { 

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
    position = NULL, na.rm = FALSE, ...) {
    
    missing <- !(c("x", "y", "r", "angle") %in% names(mapping))
    if (any(missing)) {
      stop(paste("Missing required aesthetics for geom_star:",
        paste(c("x", "y", "r", "angle")[missing], collapse = ", ")),
        call. = FALSE)
    }
    
    do.call("layer", list(mapping = mapping, data = data, stat = stat, 
      geom = ., position = position, na.rm = na.rm, ...))  
  }
  
})#' @include glyphs-class.r
NULL

check_glayer <- function(object) {
	errors <- character()
	if (!is.proto(object@layer)) {
		msg <- "glayer must be a proto object."
		errors <- c(errors, msg)
	}
  if (!("embed" %in% ls(object@layer))) {
    msg <- "glayers should contain an `embed' variable. Try building with glyph() or grid()"
    errors <- c(errors, msg)
  }
	if (length(errors) == 0) 
		TRUE
	else
		errors
}


#' @exportClass environment
#' @exportClass proto
setOldClass(c("proto", "environment"))

#' glayer class
#'
#' glayers are layers made with glyphmaps methods. They are equivalent to the 
#' layers made by ggplot2 functions in all ways except that they contain extra 
#' information that is used to divide the data into subplots and locate those 
#' subplots witihn the layer when plotting.
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
setGeneric("ls")

#' @export
setMethod("ls", signature(name = "glayer"), 
	function(name, pos = -1, envir = as.environment(pos), all.names = FALSE, pattern) {
		ls(slot(name, "layer"), all.names)
})
	
#' Create a glayer object
#' 
#' glayer gives a ggplot2 layer object the S4 class glayer, see 
#' \code{\link{glayer-class}}. ggplot layer objects are usually non-specific 
#' \code{\link[proto]{proto}} class objects. A layer should contain an embed 
#' variable before being given the class 'glayer.' See the function bodies of 
#' \code{\link{glyph}} and \code{\link{grid}} for examples.
#'
#' @export glayer
#' @param layer a proto object that can be used as a layer by the 
#' \code{\link[ggplot2]{ggplot2}} package (i.e, ggplot() + layer should return a 
#' graph).
glayer <- function(layer) {
	new("glayer", layer = layer)
}

#' Is an object (functionally) a glayer?
#' 
#' Tests whether an object is or ever was a glayer.
#' @param x an R object
#' @return logical
is.glayer <- function(x) {
  "embed" %in% ls(x)
}#' Build a glayer for rendering
#' 
#' glayer_build takes a glyph layer (class glayer), and performs all steps 
#' necessary to produce an object that can be rendered. This function outputs 
#' two pieces: a list of data frames (one for each layer), and a panel object, 
#' which contain all information about axis limits, breaks, etc.
#' 
#' If the glayer is accompanied by regular layers, glayer_build will be used in 
#' conjunction with \code{\link{glyph_build}} to build the plot for rendering.
#' 
#' @keywords internal
#' @param layer an object of class glayer
#' @seealso \code{\link{print.glyphs}} and \code{\link{glyph_build}} for 
#' functions that contain the complete set of steps for generating a glyphs plot
#' @export
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

#' Format data from a facet plot to use in a glyph plot
#' 
#' unpanel replaces the PANEL variable of a data frame with a GLYPH variable. It 
#' adjusts the data frame's group variable to retain the grouping information 
#' provided by the PANEL variable.
#' 
#' @param df A data frame. Should be the output of a facetted plot built with 
#' \code{\link[ggplot2]{ggplot_build}}
#' @return A modified data frame. See Details.
unpanel <- function(df) {
  if (!is.null(df$group)) {
    df$group <- interaction(df$group, df$PANEL)
  } 
  df$GLYPH <- as.numeric(as.character(df$PANEL))
  df$PANEL <- NULL
  df
}

#' find x scale
#' 
#' which_x picks out the scale that controls x from a list of scales
#' @param scales A list of ggplot2 scales
which_x <- function(scales) {
  vars <-  names_scales(scales)
  which(vars == "x")
}

#' find y scale
#' 
#' which_y picks out the scale that controls y from a list of scales
#' @param scales A list of ggplot2 scales
which_y <- function(scales) {
  vars <- names_scales(scales)
  which(vars == "y")
}

#' Returns the first aes of a scale, to use as an identifier for the scale
#' @param scales a list of ggplot2 scales
names_scales <- function(scales) {
  unlist(lapply(scales, function(s) s$aesthetics[[1]]))
}	
	#' Turn an ordinary layer into a layer of embedded subplots
#' 
#' glyph turns an ordinary layer into a set of glyphs. Each glyph is a plot that 
#' inherits the mappings, stat, and parameters of the initial layer. The mappings 
#' and stat for each glyph are keyed to a subset of the layer's data. The data 
#' is divided into subsets according to the glyph.by variable. Each subset is 
#' represented by one glyph. Glyphs are mapped to a pair of major x and y axes 
#' by major.aes. To allow interpretation, these major axes should correspond to 
#' the x and y aesthetics for any other layers in the plot.
#' 
#' If a layer contains no data, glyph will use the global data set for the plot. 
#' This is the data set specified in \code{\link{ggplot}}.
#' 
#' @param layer a ggplot2 layer object. See \code{\link[ggplot2]{layer}}.
#' @param major.aes An aesthetic mapping, usually constructed with 
#' \code{\link[ggplot2]{aes}}. This mapping determines where in the major x and 
#' y axes each glyph will be position. Only x and y aesthetics will be used. All 
#' other aesthetics will be ignored - consider placing them in the layer's aes 
#' mapping instead.
#' @param glyph.by variables to split the layer's data by, stored as a character 
#' vector. Similar to .variables in \code{\link[plyr]{ddply}}. Rows in the 
#' layer's data set will be assigned to subsets based on unique combinations of 
#' the variables in the glyph.by vector. Each subset will be represented by a 
#' single glyph mapped to the data within the subset.
#' @param width numeric or rel object. The width of each glyph. If width is 
#' numeric, the glyph will be drawn with a width equal to width units on the x 
#' axis. If width is of class rel (see \code{\link{rel-class}}), glyph will 
#' attempt to assign an inuitive width based on the number of total glyphs and 
#' their placement within the plot. The width can be scaled relative to this 
#' intuitive width by changing the value of the rel object.
#' @param height numeric or rel object. The height of each glyph. If height is 
#' numeric, the glyph will be drawn with a height equal to height units on the x 
#' axis. If height is of class rel (see \code{\link{rel-class}}), glyph will 
#' attempt to assign an inuitive height based on the number of total glyphs and 
#' their placement within the plot. The height can be scaled relative to this 
#' intuitive height by changing the value of the rel object.
#' @param x_scale function. The scaling to use for the x axis within each glyph. 
#' If x_scale equals \code{\link{identity}}(default), the x limits within each 
#' glyph will correspond to the range of x across all glyphs. This aids comparison 
#' because each glyph will use the same scale. If x_scale equals \code{\link{free}}, 
#' each glyph will use its own x scale. The limits of this scale will be set to 
#' the range of x values in that glyph.
#' @param y_scale function. y_scale behaves the same as x_scale but controls the 
#' scales for the y axis within each glyph.
#' @param merge.overlaps logical. If TRUE sets of glyphs that are connected by 
#' overlapping boundaries will be merged and plotted as a single glyph. This new 
#' glyph combines the data of the overlapping glyphs and is plotted in the 
#' centered location of the glyphs (maen x and y values).
#' @param reference function. Function used to create reference objects for 
#' glyphs. If NULL, no reference objects are used. Reference objects are plotted 
#' on a layer beneath the glyphs. They provide a consistent frame of reference to 
#' aid comparisons between the glyphs. Functions that create reference objects 
#' include \code{\link{ref_box}}, \code{\link{ref_hline}}, \code{\link{ref_vline}}, 
#' and \code{\link{ref_points}}.
#' @param ply.aes logical. If TRUE (default) aesthetics are calculated separately 
#' for each group, as with \code{\link{ply_aes}}. If FALSE aesthetics are 
#' calculated based on entire data set for the layer.
#' @parma .ref internal argument used for plotting reference objects.
#' @return an object of class glayer
#' @export
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

#' Assigns glyph membership to rows
#' 
#' assign_glyphs assigns each row in a layer's data set to a glyph during 
#' \code{\link{glayer_build}}. assign_glyphs sets final width and heights when 
#' width and heights are passed as rel objects. It computes and the position 
#' aesthetics for each glyph and stores them in the layer's embed variable to be 
#' used by combine_glyphs. assign_glyphs also handles merging when 
#' merge.overlaps = TRUE in a \code{\link{glyph}} call.
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

#' Calculate final positions in a glayer
#' 
#' combine_glyphs calculates the final positions for every location in a glayer.
#' glayer_build first builds each glyph separately as if it were a facet. If 
#' plotted, these glyphs would overlap with each other. combine_glyph relocates 
#' each glyph based on the global positions stored in the layer's embed variable.
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
  

#' combine_refs relocates reference objects within a layer. It works exactly like 
#' combine_glyphs but does not rescale the x and y variables for the reference 
#' object.
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


#' Ensure that an object is numeric
#' 
#' vet tests whether an object is a factor or character string. If so it 
#' attempts to coerce the variable to numeric.
#' 
#' @keywords internal
#' @param x an R object
#' @return a numeric object
#' @export
vet <- function(x) {
  if (is.character(x)) {
    x <- as.numeric(factor(x))
  }	
  if (is.factor(x)) {
  	x <- as.numeric(x)
  }
  x
}

#' rename global x and y variables in capitals
#' @keywords internal
#' @param obj a data.frame
#' @export
globalize <- function(obj){
	names(obj)[names(obj) == "x"] <- "X"
	names(obj)[names(obj) == "y"] <- "Y"
	obj
}

#' Include .gid in groupings
#' 
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
}#' Build a glyphs object for rendering
#' 
#' glyph_build takes a glyph plot object (class glyphs), and performs all steps 
#' necessary to produce an object that can be rendered. This function outputs 
#' two pieces: a list of data frames (one for each layer), and a panel object, 
#' which contain all information about axis limits, breaks, etc.
#' 
#' @keywords internal
#' @param layer an object of class glayer
#' @seealso \code{\link{print.glyphs}} for functions that contain the complete 
#' set of steps for generating a glyphs plot
#' @export
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

#' Ensure each layer contains a data set
#' 
#' propogate_data checks each layer for a data set. If none is found it assigns 
#' a copy of the plot level data set to the layer. propogate_data avoids the 
#' side effects of ggplot2:::map_layout, which performs a similar function.
#' @param layers ggplot2 layer objects
#' @param plot_data the global data set for a ggplot2 plot
propogate_data <- function(layers, plot_data) {
	ensure_data <- function(layer){
		if (inherits(layer$data, "waiver")) {
			layer$data <- plot_data
		}
		layer
	}
	lapply(layers, ensure_data)
}check_glyphs <- function(object) {
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

#' @exportClass list
#' @exportClass ggplot
setOldClass(c("ggplot", "list"))

#' glyphs class
#'
#' a glyphs object is a ggplot object that has been extended to include methods 
#' for embedding subplots when plotting. 
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


#' Create a glyphs object
#' 
#' glyph_plot gives a ggplot object the S4 class `glyphs', see 
#' \code{\link{glyphs-class}}. glyphs denotes ggplot objects that contain extra 
#' information to be used to embed subplots when plotting. glyphs objects have 
#' similar, but different print and build methods than ggplot2 objects.
#' 
#' @param ggplot a ggplot object
#' #' @export glyph_plot
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
}#' Specify how to group data during build.
#' 
#' group_by is a second order function: it returns a function that can be used 
#' on a data set to provide a grouping variable. This allows functions like 
#' \code{\link{glyph}} and \code{\link{grid}} to specify how to group data when 
#' a glayer inherits a plot level data set at build time.
#' 
#' @keywords internal
#' @param vars variable names to group a data set by, stored as a character 
#' vector
#' @export
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
}#' Identify and merge overlapping glyph assignments
#' 
#' merge_overlaps checks glyph positions against glyph width and heights to 
#' identify groups of overlapping glyphs. It then computes an alternative GLYPH 
#' variable that assigns all glyphs in an overlapping group to the same name.
#' 
#' @keywords internal
#' @param globals a data frame of glyph names and positions
#' @param width glyph width in the same units as the global x positions
#' @param height glyph height in the same units as global y positions
#' @return A named vector The names of the vector correspond to old glyph 
#' assignments, the values correspond to new assignments that merge overlapping 
#' glyphs.
#' @export
merge_overlaps <- function(globals, width, height) {
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
  #' Compute aesthetics groupwise
#' 
#' ply_aes causes the aesthetics of a layer to be computed groupwise. ply_aes 
#' implements the split-apply-combine strategy of data analysis in a graphical 
#' framework. It first splits a layer's data frame into subgroups, then evaluates 
#' the layers mappings separately within each group, and finally combines the 
#' results into a single data frame which is used to build the plot for 
#' rendering. 
#' 
#' Users may specify which groupings to use through the .vars 
#' argument. If this argument is left NULL, ply_aes will search for and use a 
#' group aes, a glyphing or gridding criteria (in a glayer), a facetting 
#' criteria, or any combination of these that it finds. 
#' 
#' @param layer a ggplot2 layer or glayer object. This layer's aesthetics will 
#' be computed groupwise, but the layer will remain the same in every other 
#' respect.
#' @param .vars variable names to group by (optional), stored as a character 
#' string
#' @export
ply_aes <- function(layer, .vars = NULL) {
  UseMethod("ply_aes")
}

#' @S3method ply_aes list
ply_aes.list <- function(layer, .vars = NULL) {
  lapply(layer, ply_aes, .vars)
}

#' @S3method ply_aes glayer
ply_aes.glayer <- function(layer, .vars = NULL) {
  if (!is.null(.vars)) {
    layer$plyr <- list(ply.by = .vars)
  } else {
    layer$plyr <- list()
  }
  layer$compute_aesthetics <- plyr_aesthetics
  layer
}

#' @S3method ply_aes proto
ply_aes.proto <- function(layer, .vars = NULL) {
  if (!is.null(.vars)) {
    layer$plyr <- list(ply.by = .vars)
  } else {
    layer$plyr <- list()
  }
  layer$compute_aesthetics <- plyr_aesthetics
  layer
}


#' compute_aesthetics groupwise at build
#' 
#' plyr_aesthetics replaces a layer's compute_aesthetics method when 
#' \code{\link{ply_aes}} is called. This results in aesthetics being computed 
#' groupwise during \code{\link[ggplot2]{ggplot_build}}
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
}#' Reference box glyph
#' 
#' ref_box creates a layer of reference boxes to be plotted behind a layer of 
#' glyphs. Each box spans the full width and height of the glyph. Reference 
#' boxes make it easier to determine the location of an object within a glyph 
#' and to compare  objects across glyphs. Reference boxes can also convey 
#' information on their own through fill, colour, alpha, linetype, and (line) 
#' size mappings. By default the fill and colour parameters of a reference box
#' match the grey and white color scheme of ggplot2 panels in 
#' \code{\link[ggplot2]{theme_grey}}.
#' 
#' ref_box is a second order function. It returns a function that can be used to 
#' create a layer of reference boxes with the specified mapping and parameters. 
#' The output of ref_box is intended to be passed as the reference argument for 
#' \code{\link{grid}} or \code{\link{glyph}}.
#' 
#' @param mapping An aesthetic mapping, usually constructed with 
#' \code{\link[ggplot2]{aes}}.
#' @param fill The color, as a character string, to be used as the fill if fill 
#' is not specified in the mapping
#' @param color The color, as a character string, to be used as the color if 
#' color is not specified in the mapping
#' @param ... other arguments to be used as parameters in the reference box 
#' layer
#' @seealso \code{\link{ref_hline}}, \code{\link{ref_vline}} and 
#' \code{\link{ref_points}}
#' @export
ref_box <- function(mapping = NULL, fill = "grey90", color = "white", ...) {	
  function(layer, type, major.aes, glyph.by = NULL, width = rel(1), 
  height = rel(1), merge.overlaps = FALSE, x.nbin = 10, y.nbin = 10) {
  	
  	def_aes <- aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1)
  	mapping <- c(mapping, def_aes[setdiff(names(def_aes), names(mapping))])
  	class(mapping) <- "uneval"
  	
    rlayer <- ply_aes(geom_rect(mapping = mapping, ...))
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


#' Horizontal reference line glyph
#' 
#' ref_hline creates a layer of horizontal reference lines to be plotted behind 
#' a layer of glyphs. Each line spans the full width of the glyph. The thickness 
#' of the line can be adjusted with the thickness argument. Reference lines make it 
#' easier to determine the location of an object within a glyph and to compare 
#' objects across glyphs. Reference lines can also convey information on their 
#' own through fill, colour, alpha, linetype, and (line) size mappings. By 
#' default the fill parameter of a reference line is set to white.
#' 
#' ref_hline is a second order function. It returns a function that can be used 
#' to create a layer of reference lines with the specified mapping and 
#' parameters. The output of ref_hline is intended to be passed as the reference 
#' argument for \code{\link{grid}} or \code{\link{glyph}}.
#' 
#' @param mapping An aesthetic mapping, usually constructed with 
#' \code{\link[ggplot2]{aes}}.
#' @param thickness the thickness of the line as a proportion of the overall 
#' glyph height. Defaults to 0.2.
#' @param fill The color, as a character string, to be used as the fill if fill 
#' is not specified in the mapping
#' @param ... other arguments to be used as parameters in the reference box 
#' layer
#' @seealso \code{\link{ref_box}}, \code{\link{ref_vline}} and \code{\link{ref_points}}
#' @export
ref_hline <- function(mapping = NULL, thickness = 0.2, fill = "white", ...) {	
  function(layer, type, major.aes, glyph.by = NULL, width = rel(1), 
  height = rel(1), merge.overlaps = FALSE) {

  	def_aes <- list(xmin = -1, xmax = 1, ymin = -thickness/2, ymax = thickness/2)
  	mapping <- c(mapping, def_aes[setdiff(names(def_aes), names(mapping))])
  	class(mapping) <- "uneval"
  	
  	rlayer <- ply_aes(geom_rect(mapping = mapping, ...))
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

#' Vertical reference line glyph
#' 
#' ref_vline creates a layer of vertical reference lines to be plotted behind a 
#' layer of glyphs. Each line spans the full height of the glyph. The thickness 
#' of the line can be adjusted with the thickness argument. Reference lines make it 
#' easier to determine the location of an object within a glyph and to compare 
#' objects across glyphs. Reference lines can also convey information on their 
#' own through fill, colour, alpha, linetype, and (line) size mappings. By 
#' default the fill parameter of a reference line is set to white.
#' 
#' ref_vline is a second order function. It returns a function that can be used 
#' to create a layer of reference lines with the specified mapping and 
#' parameters. The output of ref_vline is intended to be passed as the reference 
#' argument for \code{\link{grid}} or \code{\link{glyph}}.
#' 
#' @param mapping An aesthetic mapping, usually constructed with 
#' \code{\link[ggplot2]{aes}}.
#' @param thickness the thickness of the line as a proportion of the overall 
#' glyph width. Defaults to 0.2.
#' @param fill The color, as a character string, to be used as the fill if fill 
#' is not specified in the mapping
#' @param ... other arguments to be used as parameters in the reference box 
#' layer
#' @seealso \code{\link{ref_box}}, \code{\link{ref_hline}} and 
#' \code{\link{ref_points}}
#' @export
ref_vline <- function(mapping = NULL, thickness = 0.2, fill = "white", ...) {	
  function(layer, type, major.aes, glyph.by = NULL, width = rel(1), 
  height = rel(1), merge.overlaps = FALSE) {
  	
  	def_aes <- list(xmin = -thickness/2, xmax = thickness/2, ymin = -1, ymax = 1)
  	mapping <- c(mapping, def_aes[setdiff(names(def_aes), names(mapping))])
  	class(mapping) <- "uneval"
  	
  	rlayer <- ply_aes(geom_rect(mapping = mapping, ...))
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

#' Corner points reference glyph
#' 
#' ref_points creates a layer of reference points to be plotted behind a layer of 
#' glyphs. Each glyph is given four reference points, which are plotted one in 
#' each corner of the glyph's two dimensional range. Reference points make it 
#' easier to determine the location of an object within a glyph and to compare 
#' objects across glyphs. Reference lines can also convey information on their 
#' own through colour, alpha, and size mappings. By default the colour parameter 
#' of a reference line is set to white.
#' 
#' ref_points is a second order function. It returns a function that can be used 
#' to create a layer of reference points with the specified mapping and 
#' parameters. The output of ref_points is intended to be passed as the reference 
#' argument for \code{\link{grid}} or \code{\link{glyph}}.
#' 
#' @param mapping An aesthetic mapping, usually constructed with 
#' \code{\link[ggplot2]{aes}}.
#' @param colour The color, as a character string, to be used as the color if 
#' color is not specified in the mapping
#' @param ... other arguments to be used as parameters in the reference box 
#' layer
#' @seealso \code{\link{ref_box}}, \code{\link{ref_hline}} and 
#' \code{\link{ref_vline}}
#' @export
ref_points <- function(mapping = NULL, colour = "white", size = 1/2, ...) {	
  function(layer, type, major.aes, glyph.by = NULL, width = rel(1), 
  height = rel(1), merge.overlaps = FALSE) {
  	
  	corner <- function(def_aes) {
  	  mapping <- c(mapping, def_aes[setdiff(names(def_aes), names(mapping))])
  	  class(mapping) <- "uneval"
  	
  	  rlayer <- ply_aes(geom_point(mapping = mapping, ...))
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
#' Make a rel class object
#' 
#' rel class objects are used to specify the width and height of glyphs in 
#' \code{\link{glyph}} calls. The numeric component of the rel object specifies 
#' the proportion of the relative width or height to use for the final width or 
#' height. The relative width or height of a glyph is calculated at the time a 
#' plot is built for rendering. It depends on the number of glyphs in a plot and 
#' their placement within the plot.
#' 
#' @param x numeric the proportion of the relative width or height to use as 
#' the final width or height
#' @return an object of class rel
#' @export
rel <- function(x) {
  structure(x, class = "rel")
}

#' @S3method print rel
print.rel <- function(x, ...) print(noquote(paste(x, " *", sep = "")))

#' is x a rel object?
#' @param x an R object
#' @return logical
#' @export
is.rel <- function(x) inherits(x, "rel")#' rescale vectors to [0,1]
#' 
#' rescale_01 rescales every vector in a list of vectors to the range [0, 1]. 
#' rescale_01 rescales the vectors as a group (instead of rescaling each vector 
#' independently). This is a helpful feature for rescaling related variables (such 
#' as xmin and xmax) without nullifying the difference between the two.
#' 
#' If a vector is a character or factor vector, rescale_01 attempts to coerce it 
#' to numeric before scaling. The scale is determined by finding the range of 
#' values contained in the list of vectors and mapping it ot [0, 1]. 
#' 
#' If the full range of values to be scaled is not present in the vectors, users 
#' can specify the range to be scaled to [0,1] with the xlim argument. Values in 
#' the vectors will be rescaled as if they according to this range.
#' 
#' @param xvars a list of vectors
#' @param xlim NULL (default) or a numeric vector of length two that specifies 
#' the range of values to scale to [0, 1]
#' @return a list of vectors
#' @seealso \code{\link{rescale_11}}, \code{\link{rescale_2pi}}
#' @export
rescale_01 <- free <- function(xvars, xlim=NULL) {
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

#' rescale vectors to [-1,1]
#' 
#' rescale_11 rescales every vector in a list of vectors to the range [-1, 1]. 
#' rescale_11 rescales the vectors as a group (instead of rescaling each vector 
#' independently). This is a helpful feature for rescaling related variables (such 
#' as xmin and xmax) without nullifying the difference between the two.
#' 
#' If a vector is a character or factor vector, rescale_11 attempts to coerce it 
#' to numeric before scaling. The scale is determined by finding the range of 
#' values contained in the list of vectors and mapping it ot [-1, 1]. 
#' 
#' If the full range of values to be scaled is not present in the vectors, users 
#' can specify the range to be scaled to [-1,1] with the xlim argument. Values in 
#' the vectors will be rescaled as if they according to this range.
#' 
#' @param xvars a list of vectors
#' @param xlim NULL (default) or a numeric vector of length two that specifies 
#' the range of values to scale to [-1, 1]
#' @return a list of vectors
#' @seealso \code{\link{rescale_01}}, \code{\link{rescale_2pi}}
#' @export
rescale_11 <- function(xvars, xlim=NULL) {
	2 * rescale_01(xvars, xlim) - 1
}

#' rescale vectors to [0, 2 * pi]
#' 
#' rescale_2pi rescales every vector in a list of vectors to the range 
#' [0, 2 * pi] (e.g, for working with radians). rescale_2pi rescales the vectors 
#' as a group (instead of rescaling each vector independently). This is a 
#' helpful feature for rescaling related variables (such as xmin and xmax) 
#' without nullifying the difference between the two.
#' 
#' If a vector is a character or factor vector, rescale_11 attempts to coerce it 
#' to numeric before scaling. The scale is determined by finding the range of 
#' values contained in the list of vectors and mapping it to [0, 2 * pi]. 
#' 
#' If the full range of values to be scaled is not present in the vectors, users 
#' can specify the range to be scaled to [0, 2 * pi] with the xlim argument. Values in 
#' the vectors will be rescaled as if they according to this range.
#' 
#' @param xvars a list of vectors
#' @param xlim NULL (default) or a numeric vector of length two that specifies 
#' the range of values to scale to [0, 2 * pi]
#' @return a list of vectors
#' @seealso \code{\link{rescale_01}}, \code{\link{rescale_11}}
#' @export
rescale_2pi <- function(xvars, xlim = NULL) {
  2 * pi * rescale_01(xvars, xlim)
}

.x_aes <- c("x", "xend", "xmax", "xmin")
.y_aes <- c("y", "yend", "ymax", "ymin")

#' Get the first name in an expression
#' 
#' first_name retrieves the first name used in an expression. To be used with 
#' screening mappings for plyr or regular computation.
#' @keywords internal
#' @param expr an \code{\link{expression}} or \code{\link{call}} from which 
#' names are to be extracted
#' @return character
#' @export
first_name <- function(expr) {
  names <- all.names(expr)
  names <- names[names != "["]
  firsts <- names[1]
  firsts[is.na(firsts)] <- ""
  firsts
}

#' Get all mappings related to plotting on the x axis
#' 
#' get_xs retrieves all mappings that need to be altered for plotting on a new x axis
#'
#' @keywords internal
#' @param data a data frame
#' @export
get_xs <- function(data) {
  names(data)[names(data) %in% .x_aes]
}

#' Get all mappings related to plotting on the y axis
#'
#' get_ys retrieves all mappings that need to be altered for plotting on a new y axis
#'
#' @keywords internal
#' @param data a data frame
#' @export
get_ys <- function(data) {
  names(data)[names(data) %in% .y_aes]
}

#' Clone a ggplot2 layer object
#' 
#' layer_clone returns an identical object to the input. This object can be 
#' manipulated without unintentionally affecting other instances of the layer 
#' through proto based referencing behaviour.
#' 
#' @keywords internal
#' @param layer a ggplot2 layer object, a glayer object, or a list of such 
#' objects
#' @return a ggplot2 layer object, a glayer object, or a list of such objects
#' @export
layer_clone <- function(layer) {
  UseMethod("layer_clone")
}

#' @S3method layer_clone proto
layer_clone.proto <- function(layer) {
  ggplot2:::plot_clone(ggplot() + layer)$layers[[1]]
}

#' @S3method layer_clone glayer
layer_clone.glayer <- function(layer) {
  glayer(ggplot2:::plot_clone(ggplot() + layer)$layers[[1]])
}

#' @S3method layer_clone list
layer_clone.list <- function(layer) {
  lapply(layer, layer_clone)
}

#' Remove NULL objects from a list
#' 
#' null.omit removes the NULL elements from a list and returns the remaining 
#' objects as a more concise list.
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