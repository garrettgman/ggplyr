\name{ref_points}
\alias{ref_points}
\title{Corner points reference glyph}
\usage{
  ref_points(mapping = NULL, colour = "white", size = 1/2,
  ...)
}
\arguments{
  \item{mapping}{An aesthetic mapping, usually constructed
  with \code{\link[ggplot2]{aes}}.}

  \item{colour}{The color, as a character string, to be
  used as the color if color is not specified in the
  mapping}

  \item{...}{other arguments to be used as parameters in
  the reference box layer}
}
\description{
  ref_points creates a layer of reference points to be
  plotted behind a layer of glyphs. Each glyph is given
  four reference points, which are plotted one in each
  corner of the glyph's two dimensional range. Reference
  points make it easier to determine the location of an
  object within a glyph and to compare objects across
  glyphs. Reference lines can also convey information on
  their own through colour, alpha, and size mappings. By
  default the colour parameter of a reference line is set
  to white.
}
\details{
  ref_points is a second order function. It returns a
  function that can be used to create a layer of reference
  points with the specified mapping and parameters. The
  output of ref_points is intended to be passed as the
  reference argument for \code{\link{grid}} or
  \code{\link{glyph}}.
}
\seealso{
  \code{\link{ref_box}}, \code{\link{ref_hline}} and
  \code{\link{ref_vline}}
}

