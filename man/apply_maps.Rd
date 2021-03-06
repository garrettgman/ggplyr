\name{apply_maps}
\alias{apply_maps}
\title{Calculate aesthetic values for a data frame}
\usage{
  apply_maps(data, mapping, enclos = parent.frame())
}
\arguments{
  \item{data}{a data frame}

  \item{mapping}{an object of class uneval, usually the
  output of \code{\link[ggplot2]{aes}}}

  \item{enclos}{and environment}
}
\value{
  a data frame
}
\description{
  apply_maps evaluates a mapping within a data frame to
  calculate aesthetic values. apply_maps is intended to be
  used in conjunction with \code{\link[plyr]{ddply}}. If
  each mapping returns a single value, apply_mapping will
  return a single row of data. This provides a convenient
  way to reduce a set of geoms, similar to using
  \code{\link[plyr]{summarise}} with ddply.
}
\keyword{internal}

