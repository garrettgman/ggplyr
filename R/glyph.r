#' glyph turns an ordinary layer into a set of glyphs. To do this, it must:
#' 1. ensure that the x and y aesthetics are mapped to a single value
#' 2. add reference boxes
#' 3. handle merging
#' 4. split up the data
glyph <- function(layer, major.aes, glyph.by = NULL, width = rel(0.95), 
  height = rel(0.95), x_scale = identity, y_scale = identity, 
  merge.overlaps = FALSE, reference = NULL, .ref = FALSE) {
  
  missing <- c(is.null(major.aes$x), is.null(major.aes$y))
  if (any(missing)) {
    stop(paste("Missing required aesthetic in major.aes:", 
      paste(c("x", "y")[missing], collapse = ", ")))
  }
  
  layer <- layer_clone(layer)
  layer$embed <- list(width = width, height = height, 
    x_scale = x_scale, y_scale = y_scale, merge.overlaps = merge.overlaps,
    major.aes = major.aes[c("x", "y")], glyph.by = glyph.by)
  layer$assign_glyphs <- assign_glyphs
  layer$combine_glyphs <- combine_glyphs
  if (.ref) layer$combine_glyphs <- combine_refs
  layer$compute_aesthetics <- plyr_aesthetics
    
  if (is.null(reference)) {
  	glayer(layer)
  } else {
  	ref.layer <- reference(layer, major.aes, glyph.by, width, height, 
  	  merge.overlaps)
  	list(ref.layer, glayer(layer))
  }
} 
  
assign_glyphs <- function(., data) {
  # major x and y
  data$GLYPH <- id(data[embed$glyph.by], drop = TRUE)
  globals <- ddply(data, "GLYPH", apply_glyphs, embed$major.aes)
    
  # parse width, height
  width <- embed$width
  height <- embed$height

  if (is.rel(width)) {
    .$embed$width <- (diff(range(vet(globals$x))) + unclass(width)) / 
      length(unique(globals$x)) * unclass(width)
  }
  if (is.rel(height)) {
    .$embed$height <- (diff(range(vet(globals$y))) + unclass(height)) / 
      length(unique(globals$y)) * unclass(height)
  }

  if (embed$merge) {
    # search for overlapping glyphs, combine
    merges <- merge_overlaps(globals, embed$width, embed$height)
    data <- update_GLYPH(data, merges)
    embed$globals <- ddply(data, "GLYPH", apply_glyphs, embed$major.aes)
  }
  .$embed$globals <- globals
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

update_GLYPH <- function(df, updates) {
	if (!("GLYPH" %in% names(df))) {
		stop("Cannot update glyphs: data.frame does not have GLYPH variable", 
			call. = FALSE)
	}
	
	obsolete <- df$GLYPH %in% names(updates)
	df$GLYPH[obsolete] <- updates[as.character(df$GLYPH[obsolete])]
	df
}