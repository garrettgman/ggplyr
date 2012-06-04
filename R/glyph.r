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
}