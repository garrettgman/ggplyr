rescale_01 <- free <- function(xvars, xlim=NULL) {
	xnames <- names(xvars)
	numberfy <- function(x) {
		if (is.character(x)) {
			x <- as.numeric(factor(x))
		}	
		if (is.factor(x)) {
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

