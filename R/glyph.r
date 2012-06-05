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
#' @param .ref internal argument used for plotting reference objects.
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

# Assigns glyph membership to rows
# 
# assign_glyphs assigns each row in a layer's data set to a glyph during 
# \code{\link{glayer_build}}. assign_glyphs sets final width and heights when 
# width and heights are passed as rel objects. It computes and the position 
# aesthetics for each glyph and stores them in the layer's embed variable to be 
# used by combine_glyphs. assign_glyphs also handles merging when 
# merge.overlaps = TRUE in a \code{\link{glyph}} call.
assign_glyphs <- function(., data) {
  # major x and y
  data$GLYPH <- embed$glyph.by(data)
  globals <- aesply(data, "GLYPH", embed$major.aes)
  too.many <- c(length(unique(globals$x)) > length(unique(globals$GLYPH)), 
    length(unique(globals$y)) > length(unique(globals$GLYPH)))
  if (any(too.many)) {
    message(paste("Major", paste(c("x", "y")[too.many], collapse = " and "), 
      "return more than one value per glyph. Only using first."))
    globals <- unique(plyr::ddply(globals, "GLYPH", transform, x = x[1], 
      y = y[1]))
  }

  # parse width, height
  width <- embed$width
  height <- embed$height
  if (is.rel(width)) {
    .$embed$width <- width <- max(ggplot2::resolution(vet(globals$x), 
      zero = FALSE) * unclass(width), (diff(range(vet(globals$x))) + 
      unclass(width)) / length(unique(globals$x)) * unclass(width))
  }
  if (is.rel(height)) {
    .$embed$height <- height <- max(ggplot2::resolution(vet(globals$y), 
      zero = FALSE) * unclass(height), (diff(range(vet(globals$y))) + 
      unclass(height)) / length(unique(globals$y)) * unclass(height))
  }

  if (embed$merge) {
    # search for overlapping glyphs, combine
    data$.gid <- factor(data$GLYPH)
    merge.key <- merge_overlaps(globals, embed$width, embed$height)
    data$GLYPH <- merge.key[data$GLYPH]
    globals <- aesply(data, "GLYPH", embed$major.aes)
    .$mapping <- add_gid(.$mapping)
    
    too.many <- c(length(unique(globals$x)) > length(unique(globals$GLYPH)), 
                  length(unique(globals$y)) > length(unique(globals$GLYPH)))
    if (any(too.many)) {
      message(paste("Major", paste(c("x", "y")[too.many], collapse = " and "), 
                    "return more than one value per glyph. Only using first."))
      globals <- unique(plyr::ddply(globals, "GLYPH", transform, x = x[1], 
        y = y[1]))
    }
  }
  .$embed$globals <- globals
  data
}

# Calculate final positions in a glayer
# 
# combine_glyphs calculates the final positions for every location in a glayer.
# glayer_build first builds each glyph separately as if it were a facet. If 
# plotted, these glyphs would overlap with each other. combine_glyph relocates 
# each glyph based on the global positions stored in the layer's embed variable.
combine_glyphs <- function(., data) {  
  data <- plyr::join(data, globalize(embed$globals), by = "GLYPH")
  
  xvar <- get_xs(data)
  yvar <- get_ys(data)
  
  # scale if necessary
  if (!identical(embed$x_scale, identity) || 
    !identical(embed$y_scale, identity)) {
    data <- plyr::ddply(data, "GLYPH", function(df) {
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
  

# combine_refs relocates reference objects within a layer. It works exactly like 
# combine_glyphs but does not rescale the x and y variables for the reference 
# object.
combine_refs <- function(., data) {  
  data <- plyr::join(data, globalize(embed$globals), by = "GLYPH")
		
  xvar <- get_xs(data)
  yvar <- get_ys(data)
		
	# scale if necessary
  if (!identical(embed$x_scale, identity) || 
    !identical(embed$y_scale, identity)) {
    data <- plyr::ddply(data, "GLYPH", function(df) {
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
}