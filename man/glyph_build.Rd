\name{glyph_build}
\alias{glyph_build}
\title{Build a glyphs object for rendering}
\usage{
  glyph_build(plot)
}
\arguments{
  \item{layer}{an object of class glayer}
}
\description{
  glyph_build takes a glyph plot object (class glyphs), and
  performs all steps necessary to produce an object that
  can be rendered. This function outputs two pieces: a list
  of data frames (one for each layer), and a panel object,
  which contain all information about axis limits, breaks,
  etc.
}
\seealso{
  \code{\link{print.glyphs}} for functions that contain the
  complete set of steps for generating a glyphs plot
}
\keyword{internal}

