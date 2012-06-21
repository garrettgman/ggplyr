#' grid takes a vector of numbers, divides it into n bins, and rounds each point to the nearest center of a bin.
grid <- function(x, binwidth = NULL, nbins = 10, range. = range(x)) {
  if (diff(range.) == 0) return(x)
  if (is.null(binwidth)) {
    binwidth <- diff(range.) / npoints
  }
  x <- x - range.[1] + binwidth / 2
  a <- x %/% binwidth + c(0,1)[round(x %% binwidth / binwidth) + 1]
  a[a == 0] <- 1 # include leftmost point
  x <- range.[1] + binwidth * a
  x - binwidth/2
}
