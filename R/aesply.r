aesply <- function(data, .var, aesthetics) {
  no.ply <- unlist(lapply(aesthetics, first_name)) == "I"
  if (!any(no.ply)) {
    return(compact(eval.plyr(aesthetics, data, .var)))
  }
  
  all.aes <- lapply(aesthetics[no.ply], remove_I)
  evaled <- compact(eval.quoted(all.aes, data))
  lengths <- vapply(evaled, length, integer(1))
  n <- if (length(lengths) > 0) max(lengths) else 0
  wrong <- lengths != 1 & lengths != n
  if (any(wrong)) {
    stop("Aesthetics must either be length one, or the same length as the data", 
         "Problems:", paste(aesthetics[wrong], collapse = ", "), 
         call. = FALSE)
  }
  data <- cbind(data, evaled)
  all.data <- data.frame(evaled) 
  aesthetics[no.ply] <- lapply(names(aesthetics)[no.ply], as.name)
  compact(eval.plyr(aesthetics, data, .var))
}


remove_I <- function(expr) {
  Identity <- function(x) {
    if (x == "I") x <- quote(identity)
    x
  }
  as.call(lapply(expr, Identity))
}

