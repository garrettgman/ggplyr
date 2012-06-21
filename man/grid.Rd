\name{grid}
\alias{grid}
\title{Turn a layer into a grid of embedded subplots.}
\usage{
  grid(layer, grid.aes = ggplot2::aes(), x.nbin = 10,
    y.nbin = 10, x_scale = identity, y_scale = identity,
    width.adjust = 0.95, height.adjust = 0.95,
    reference = ref_box(), ply.aes = TRUE, .ref = FALSE)
}
\arguments{
  \item{layer}{a ggplot2 layer object. See
  \code{\link[ggplot2]{layer}}.}

  \item{grid.aes}{An aesthetic mapping, usually constructed
  with \code{\link[ggplot2]{aes}}. This mapping determines
  which variables will be used to create the grid. grid
  will use the x grid aesthetic as the major x axis and the
  y grid aesthetic as the major y axis. All other
  aesthetics will be ignored - consider placing them in the
  layer's aes mapping instead.}

  \item{x.nbin}{numeric. The number of bins to divide the
  major x axis into when creating the grid. Defaults to
  10.}

  \item{y.nbin}{numeric. The number of bins to divide the
  major y axis into when creating the grid. Defaults to
  10.}

  \item{x_scale}{function. The scaling to use for the x
  axis within each glyph. If x_scale equals
  \code{\link{identity}}(default), the x limits within each
  glyph will correspond to the range of x across all
  glyphs. This aids comparison because each glyph will use
  the same scale. If x_scale equals \code{\link{free}},
  each glyph will use its own x scale. The limits of this
  scale will be set to the range of x values in that
  glyph.}

  \item{y_scale}{function. y_scale behaves the same as
  x_scale but controls the scales for the y axis within
  each glyph.}

  \item{width.adjust}{numeric. The proportion of horizontal
  space within a grid box that each subplot should occupy.
  Used to control overlapping and appearance. Each subplot
  is anchored to the bottom left corner of the grid box and
  then spans the proportion of the box specified by width
  adjust.}

  \item{height.adjust}{numeric. The proportion of vertical
  space within a grid box that each subplot should occupy.
  Behaves the same as width.adjust.}

  \item{reference}{function. Function used to create
  reference objects for the embedded plots. If NULL, no
  reference objects are used. Reference objects are plotted
  on a layer beneath the subplots. They provide a
  consistent frame of reference to aid comparisons across
  subplots. Functions that create reference objects include
  \code{\link{ref_box}}, \code{\link{ref_hline}},
  \code{\link{ref_vline}}, and \code{\link{ref_points}}. By
  default, reference is set to ref_box, which creates the
  familiar mesh pattern associated with grids.}

  \item{ply.aes}{logical. If TRUE (default) aesthetics are
  calculated separately for each subplot, as with
  \code{\link{ply_aes}}. If FALSE aesthetics are calculated
  based on entire data set for the layer.}

  \item{.ref}{internal argument used for plotting reference
  objects.}
}
\value{
  an object of class glayer
}
\description{
  grid turns an ordinary layer into an array of subplots
  laid out in a grid. Each subplot inherits the mappings,
  stat, and parameters of the initial layer. The mappings
  and stat for each subplot are mapped just to the data
  that occurs in the subplot's region on the grid. Any
  variables in the layer's data set may be used as x and y
  axes to create a grid on. These 'major' x and y axes need
  not correspond to the 'minor' x and y axes within each
  subplot. Major x and y axes are set with the grid.aes
  argument. Minor x and y axes are inherited from the
  layer's original mapping. To allow interpretation, the
  major axes of a gridded layer should correspond to the x
  and y aesthetics for any other (non - gridded) layers in
  the plot.
}
\details{
  The layout of the grid is specified by separately
  choosing the number of bins to use along the x and y axes
  with the x.nbin and y.nbin arguments.

  Gridded layers follow usual data inheritence rules for
  ggplot2 layer objects. If a layer contains no data, grid
  will use the global data set for the plot. This is the
  data set specified in \code{\link{ggplot}}.
}

