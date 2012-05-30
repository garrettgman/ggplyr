merge_overlaps <- function(globals, width, height) {
	globals <- arrange(globals, x)
	g1 <- globals[1:(nrow(globals) - 1), ]
	g2 <- globals[-1, ]

	x.overlaps <- abs(g1$x - g2$x) < width 
	y.overlaps <- abs(g1$y - g2$y) < height
	overlaps <- x.overlaps & y.overlaps
	
	# fix multiple overlaps
	new = g1$GLYPH[overlaps]
	old = g2$GLYPH[overlaps]
	still.old <- which(new %in% old)
	while(length(still.old)) {
		new[still.old[1]] <- new[which(old == new[still.old[1]])]
		still.old <- which(new %in% old)
	}
	names(new) <- old
	new
}