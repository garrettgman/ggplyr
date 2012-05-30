#' group_by is a second order function: it returns a function that can be used on a data set to provide a grouping variable.
group_by <- function(vars) {
  if (!is.character(vars)) {
    stop("vars should be of class 'character'", call. = FALSE)
  }
  
  function(df) {
    missing <- !(vars %in% names(df))
    if (any(missing)) {
      stop(paste(paste(vars[missing], collapse = ", "), 
        "variables not found in data.frame"), call. = FALSE)
    }
    id(df[vars], drop = TRUE)
  }
}