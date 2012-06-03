ref_box <- function(mapping = NULL, fill = "grey90", color = "white", ...) {	
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
        width = width, height = height, merge = merge.overlaps, ref = NULL, 
        .ref = TRUE), 
      grid = grid(rlayer, grid.aes = major.aes, x.nbin = x.nbin, 
        y.nbin = y.nbin, width.adjust = 1, height.adjust = 1, .ref = TRUE)
    )
  }
} 



ref_hline <- function(mapping = NULL, width. = 0.2, fill = "white", ...) {	
  function(layer, major.aes, glyph.by = NULL, width = rel(1), 
  height = rel(1), merge.overlaps = FALSE) {

  	def_aes <- list(xmin = -1, xmax = 1, ymin = -width./2, ymax = width./2)
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

ref_vline <- function(mapping = NULL, width. = 0.2, fill = "white", ...) {	
  function(layer, major.aes, glyph.by = NULL, width = rel(1), 
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
  function(layer, major.aes, glyph.by = NULL, width = rel(1), 
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
