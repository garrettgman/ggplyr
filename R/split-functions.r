#' group_by groups a data frame into a list of smaller data frames. Each smaller data frame contains all of the rows of the original data frame that contain one unique combination of the sorting variables. 
#' 
#' @param data a data frame to group
#' @param the name of the variable(s) to group by, as character strings
#' @export
group_by <- function(data, vars) {
	if (empty(data)) 
        return(data)
    vars <- as.quoted(vars)
    plyr:::splitter_d(data, vars)
}
	

# splitter_d above is from from plyr 1.7.1 package. Saved here in case it changes.
# splitter_d <- function (data, .variables = NULL, drop = TRUE) {
#    stopifnot(is.quoted(.variables))
#    if (length(.variables) == 0) {
#        splitv <- rep(1, nrow(data))
#        split_labels <- NULL
#        attr(splitv, "n") <- max(splitv)
#        vars <- character(0)
#    }
#    else {
#        splits <- eval.quoted(.variables, data)
#        splitv <- id(splits, drop = drop)
#        split_labels <- split_labels(splits, drop = drop, id = splitv)
#        vars <- unlist(lapply(.variables, all.vars))
#    }
#    index <- split_indices(seq_along(splitv), as.integer(splitv), 
#        attr(splitv, "n"))
#    il <- indexed_df(data, index, vars)
#    structure(il, class = c(class(il), "split", "list"), split_type = "data.frame", 
#        split_labels = split_labels)
# }