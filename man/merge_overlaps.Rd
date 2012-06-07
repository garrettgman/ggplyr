\name{merge_overlaps}
\alias{merge_overlaps}
\title{Identify and merge overlapping glyph assignments}
\usage{
  merge_overlaps(globals, width, height)
}
\arguments{
  \item{globals}{a data frame of glyph names and positions}

  \item{width}{glyph width in the same units as the global
  x positions}

  \item{height}{glyph height in the same units as global y
  positions}
}
\value{
  A named vector The names of the vector correspond to old
  glyph assignments, the values correspond to new
  assignments that merge overlapping glyphs.
}
\description{
  merge_overlaps checks glyph positions against glyph width
  and heights to identify groups of overlapping glyphs. It
  then computes an alternative GLYPH variable that assigns
  all glyphs in an overlapping group to the same name.
}
\keyword{internal}

