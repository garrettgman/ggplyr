#' grid_by is a second order function: it returns a function that can be used on 
#' a data set to provide a grouping variable.
grid_by <- function(x, x.npoints = 10, y, y.npoints = 10, x.range. = NULL, 
  y.range. = NULL) {
  
  if(inherits(try(x, silent = TRUE), "try-error")) {
    x <- as.character(substitute(x))
  }
  if(inherits(try(y, silent = TRUE), "try-error")) {
    y <- as.character(substitute(y))
  }
  
  function(df) {
    missing <- !(c(x, y) %in% names(df))
    if (any(missing)) {
      stop(paste(paste(c(x, y)[missing], collapse = ", "), 
        "variables not found in data.frame"), call. = FALSE)
    }
    if (is.null(x.range.)) {
      df$.X_GRID <- grid(df[[x]], x.npoints)
    } else {
      df$.X_GRID <- grid(df[[x]], x.npoints, x.range.)
    }
    if (is.null(y.range.)) {
      df$.Y_GRID <- grid(df[[y]], y.npoints)
    } else {
      df$.Y_GRID <- grid(df[[y]], y.npoints, y.range.)
    }
    id(df[c(".X_GRID", ".Y_GRID")], drop = TRUE)
  }
}