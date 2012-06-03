#' grid turns an ordinary layer into a gridded version of itself. 
grid <- function(layer, grid.aes = aes(), x.nbin = 10, y.nbin = 10, 
  x_scale = identity, y_scale = identity, width.adjust = 0.95, 
  height.adjust = 0.95, reference = NULL, .ref = FALSE) {
  
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
    glayer(layer)
  } else {
    ref.layer <- reference(layer, "grid", grid.aes, x.nbin = x.nbin, 
      y.nbin = y.nbin)
    list(ref.layer, glayer(layer))
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
}