rescale01 <- function(x, xlim=NULL) {
  if (is.character(x)) {
  	x <- as.numeric(factor(x))
  }	
  if (is.factor(x)) {
  	x <- as.numeric(x)
  }
  if (is.null(xlim)) {
	  rng <- range(x, na.rm = TRUE)
  } else {
  	  rng <- xlim
  }
  
  if ((rng[2] - rng[1]) == 0) {
  	  x - rng[1]
  } else {	
      (x - rng[1]) / (rng[2] - rng[1])
  }
}
rescale11 <- function(x, xlim=NULL) 2 * rescale01(x, xlim) - 1