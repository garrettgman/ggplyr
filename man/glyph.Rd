\name{glyph}
\alias{glyph}
\title{Turn a layer into a layer of embedded subplots}
\usage{
  glyph(layer, major.aes, glyph.by = NULL,
    width = rel(0.95), height = rel(0.95),
    x_scale = identity, y_scale = identity,
    merge.overlaps = FALSE, reference = NULL,
    ply.aes = TRUE, .ref = FALSE)
}
\arguments{
  \item{layer}{a ggplot2 layer object. See
  \code{\link[ggplot2]{layer}}.}

  \item{major.aes}{An aesthetic mapping, usually
  constructed with \code{\link[ggplot2]{aes}}. This mapping
  determines where in the major x and y axes each glyph
  will be position. Only x and y aesthetics will be used.
  All other aesthetics will be ignored - consider placing
  them in the layer's aes mapping instead.}

  \item{glyph.by}{variables to split the layer's data by,
  stored as a character vector. Similar to .variables in
  \code{\link[plyr]{ddply}}. Rows in the layer's data set
  will be assigned to subsets based on unique combinations
  of the variables in the glyph.by vector. Each subset will
  be represented by a single glyph mapped to the data
  within the subset.}

  \item{width}{numeric or rel object. The width of each
  glyph. If width is numeric, the glyph will be drawn with
  a width equal to width units on the x axis. If width is
  of class \code{\link{rel}}, glyph will attempt to assign
  an inuitive width based on the number of total glyphs and
  their placement within the plot. The width can be scaled
  relative to this intuitive width by changing the value of
  the rel object.}

  \item{height}{numeric or rel object. The height of each
  glyph. Height behaves the same way as width, but applies
  to the y dimension.}

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

  \item{merge.overlaps}{logical. If TRUE sets of glyphs
  that are connected by overlapping boundaries will be
  merged and plotted as a single glyph. This new glyph
  combines the data of the overlapping glyphs and is
  plotted in the centered location of the glyphs (maen x
  and y values).}

  \item{reference}{function. Function used to create
  reference objects for glyphs. If NULL, no reference
  objects are used. Reference objects are plotted on a
  layer beneath the glyphs. They provide a consistent frame
  of reference to aid comparisons between the glyphs.
  Functions that create reference objects include
  \code{\link{ref_box}}, \code{\link{ref_hline}},
  \code{\link{ref_vline}}, and \code{\link{ref_points}}.}

  \item{ply.aes}{logical. If TRUE (default) aesthetics are
  calculated separately for each group, as with
  \code{\link{ply_aes}}. If FALSE aesthetics are calculated
  based on entire data set for the layer.}

  \item{.ref}{internal argument used for plotting reference
  objects.}
}
\value{
  an object of class glayer
}
\description{
  glyph turns an ordinary layer into a set of glyphs. Each
  glyph is a plot that inherits the mappings, stat, and
  parameters of the initial layer. The mappings and stat
  for each glyph are keyed to a subset of the layer's data.
  The data is divided into subsets according to the
  glyph.by variable. Each subset is represented by one
  glyph. Glyphs are mapped to a pair of major x and y axes
  by major.aes. To allow interpretation, these major axes
  should correspond to the x and y aesthetics for any other
  layers in the plot.
}
\details{
  Glyphed layers follow usual data inheritence rules for
  ggplot2 layer objects. If a layer contains no data, glyph
  will use the global data set for the plot. This is the
  data set specified in \code{\link{ggplot}}.
}

