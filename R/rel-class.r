# Relative dimensions ------------------------------------------------------
rel <- function(x) {
  structure(x, class = "rel")
}

print.rel <- function(x, ...) print(noquote(paste(x, " *", sep = "")))

is.rel <- function(x) inherits(x, "rel")