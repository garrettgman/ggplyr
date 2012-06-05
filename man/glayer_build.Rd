\name{glayer_build}
\alias{glayer_build}
\title{Build a glayer for rendering}
\usage{
  glayer_build(layer)
}
\arguments{
  \item{layer}{an object of class glayer}
}
\description{
  glayer_build takes a glyph layer (class glayer), and
  performs all steps necessary to produce an object that
  can be rendered. This function outputs two pieces: a list
  of data frames (one for each layer), and a panel object,
  which contain all information about axis limits, breaks,
  etc.
}
\details{
  If the glayer is accompanied by regular layers,
  glayer_build will be used in conjunction with
  \code{\link{glyph_build}} to build the plot for
  rendering.
}
\seealso{
  \code{\link{print.glyphs}} and \code{\link{glyph_build}}
  for functions that contain the complete set of steps for
  generating a glyphs plot
}
\keyword{internal}

