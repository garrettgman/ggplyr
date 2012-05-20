\name{fun}
\alias{fun}
\title{Reorganize a function to take a single argument}
\usage{
  fun(name, .input, ...)
}
\arguments{
  \item{name}{name of the function to reorganize as an
  object or character string}

  \item{.input}{name of the argument that new function will
  take}

  \item{...}{values to set other arguments of function to.
  If an argument is not named it will be set to its default
  value.}
}
\description{
  fun reorganizes a function to take a single argument. It
  sets other specified arguments to known values. fun can
  be used to change the order of arguments within a
  function. This is helpful for passing functions to
  \code{\link{plyr}} style functions which automatically
  use the first argument of a function's definition.
}

