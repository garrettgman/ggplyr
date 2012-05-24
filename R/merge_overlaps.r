merge_overlaps <- function(globals, width, height) {
	globals <- globals[c(".gid", "x", "y")]
	globals <- arrange(globals, x)
	g1 <- globals[1:(nrow(globals) - 1), ]
	g2 <- globals[-1, ]

	x.overlaps <- abs(g1$x - g2$x) < width 
	y.overlaps <- abs(g1$y - g2$y) < height
	overlaps <- x.overlaps & y.overlaps
	
	# but what about multiple overlaps?
	new = g1$.gid[overlaps]
	old = g2$.gid[overlaps]
	still.old <- which(new %in% old)
	while(length(still.old)) {
		new[still.old[1]] <- new[which(old == new[still.old[1]])]
		still.old <- which(new %in% old)
	}
	names(new) <- old
	new
}