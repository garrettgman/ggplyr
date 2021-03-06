\name{rel}
\alias{rel}
\title{Make a rel class object}
\usage{
  rel(x)
}
\arguments{
  \item{x}{numeric the proportion of the relative width or
  height to use as the final width or height}
}
\value{
  an object of class rel
}
\description{
  rel class objects are used to specify the width and
  height of glyphs in \code{\link{glyph}} calls. The
  numeric component of the rel object specifies the
  proportion of the relative width or height to use for the
  final width or height. The relative width or height of a
  glyph is calculated at the time a plot is built for
  rendering. It depends on the number of glyphs in a plot
  and their placement within the plot.
}

