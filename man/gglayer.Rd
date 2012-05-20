\name{gglayer}
\alias{gglayer}
\title{Create a gglayer object}
\usage{
  gglayer(layer)
}
\arguments{
  \item{layer}{a proto object that can be used as a layer
  by the \code{\link{ggplot2}} package (i.e, ggplot() +
  layer should return a graph).}
}
\description{
  gglayer gives a ggplot2 layer object the S4 class
  gglayer, see \code{\link{gglayer-class}}. ggplot layer
  objects are usually non-specific \code{\link{proto}}
  class objects.
}

