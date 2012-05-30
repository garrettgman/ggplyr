#' glyph turns an ordinary layer into a set of glyphs. To do this, it must:
#' 1. ensure that the x and y aesthetics are mapped to a single value
#' 2. add reference boxes
#' 3. handle merging
#' 4. split up the data
glyph <- function(layer, major.aes, glyph.by = NULL, width = rel(1), height = rel(1), 
  x_scale = identity, y_scale = identity, merge.overlaps = FALSE, reference = NULL) {
  
  missing <- c(is.null(major.aes$x), is.null(major.aes$y))
  if (any(missing)) {
    stop(paste("Missing required aesthetic in major.aes:", 
      paste(c("x", "y")[missing], collapse = ", ")))
  }
  
  layer <- layer_clone(layer)
  
  # update aesthetics, save glyph params
  mnames <- names(layer$mapping)
  mnames[mnames == "x"] <- "minor.x"
  mnames[mnames == "y"] <- "minor.y"
  names(layer$mapping) <- mnames
  layer$mapping$x <- major.aes$x
  layer$mapping$y <- major.aes$y
  layer$width <- width
  layer$height <- height
  layer$glyph.by <- glyph.by
  layer$x_scale <- x_scale
  layer$y_scale <- y_scale
  layer$merge.overlaps <- merge.overlaps
  
  layer$assign_glyphs <- function(., data){
    data$GLYPH <- id(data[glyph.by], drop = TRUE)
    # parse width, height
    if (is.rel(width) || is.rel(height) || merge.overlaps) {
      global_aes <- list(x = mapping$x, y = mapping$y)
      globals <- ddply(data, "GLYPH", apply_glyphs, global_aes)
    }
    if (is.rel(width)) {
      .$width <- (max(diff(range(globals$x)), 1) + unclass(width)) / 
        length(unique(globals$x)) * unclass(width)
    }
    if (is.rel(height)) {
      .$height <- (max(diff(range(globals$y)), 1) + unclass(height)) / 
        length(unique(globals$y)) * unclass(height)
    }
  
    # split data
    if (merge.overlaps) {
      merges <- merge_overlaps(globals, width, height)
      data$GLYPH <- update_GLYPH(data$GLYPH, merges)
    }
    data
  }
  
  
  # update build time functions
  layer$compute_aesthetics <- glyph_aesthetics
    
  # scales x and y,
  # trains to width and height
  # catches and handles overlaps
  # adds scaled minor axes to major axes
  # returns properly named data set
  layer$map_glyphs <- function(., df) {
    df <- switch_names(df)
    
    xvar <- get_xs(df)
    yvar <- get_ys(df)
      
    # scale if necessary
    if (!identical(body(x_scale), body(identity)) || 
      !identical(body(y_scale), body(identity))) {
      data <- ddply(data, "GLYPH", function(df) {
        df[xvar] <- x_scale(df[xvar])
        df[yvar] <- y_scale(df[yvar])
        df
      })
    }
      
    # update x and y related variables
    # don't scale individually or xmin and xmax's will end up on top of one another
    df[xvar] <- vet(df$X) + rescale_11(df[xvar]) * width
    df[yvar] <- vet(df$Y) + rescale_11(df[yvar]) * height
      
    df$X <- NULL
    df$Y <- NULL
    df
  }
  
  # changes layer draw to draw reference boxes first and then draw layer 
  # if (!is.null(reference)) layer$draw <- 
  
  # assigns glayer S4 class to ensure glyph_build
  glayer(layer)
}

switch_names <- function(data) {
  dnames <- names(data)
  prefixes <- stringr::str_sub(dnames, 1, 6)
  dnames[dnames == "x"] <- "X"
  dnames[dnames == "y"] <- "Y"
  dnames[prefixes == "minor."] <- stringr::str_sub(dnames[prefixes == "minor."], 7)
  names(data) <- dnames
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

update_GLYPH <- function(glyph.vec, updates) {
  obsolete <- glyph.vec %in% names(updates)
  glyph.vec[obsolete] <- updates[as.character(glyph.vec[obsolete])]
  glyph.vec
}