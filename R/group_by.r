#' Specify how to group data during build.
#' 
#' group_by is a second order function: it returns a function that can be used 
#' on a data set to provide a grouping variable. This allows functions like 
#' \code{\link{glyph}} and \code{\link{grid}} to specify how to group data when 
#' a glayer inherits a plot level data set at build time.
#' 
#' @keywords internal
#' @param vars variable names to group a data set by, stored as a character 
#' vector
#' @export
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