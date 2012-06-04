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

eval.plyr <- function (exprs, data = NULL, by = NULL, enclos = NULL, 
  try = FALSE) {
  if (is.numeric(exprs)) 
    return(envir[exprs])
  qenv <- if (is.quoted(exprs)) 
    attr(exprs, "env")
  else parent.frame()
  if (is.null(data)) 
    data <- qenv
  if (is.data.frame(data) && is.null(enclos)) 
    enclos <- qenv
  if (try) {
    results <- failwith(NULL, ddply, quiet = TRUE) (data, by, apply_maps, 
                                                    exprs, qenv)    
  } else {
    results <- ddply(data, by, apply_maps, exprs, qenv)    
  }
  results
}


apply_maps <- function(data, mapping, enclos = parent.frame()) {
  map <- null_omit(mapping)
  vars <- llply(map, eval, envir = data, enclos)
  n <- nrow(data)
  lengths <- unlist(lapply(vars, length))
  wrong <- lengths != 1 & lengths != n
  if (any(wrong)) {
    stop(paste(
      "Aesthetics must either be length one, or the same length as the data", 
      "Problems:", paste(names(wrong)[wrong], collapse = ", ")), 
      call. = FALSE)
  }
  data.frame(vars)
}

