\name{assign_glyphs}
\alias{assign_glyphs}
\title{Assigns glyph membership to rows}
\usage{
  assign_glyphs(., data)
}
\description{
  assign_glyphs assigns each row in a layer's data set to a
  glyph during \code{\link{glayer_build}}. assign_glyphs
  sets final width and heights when width and heights are
  passed as rel objects. It computes and the position
  aesthetics for each glyph and stores them in the layer's
  embed variable to be used by combine_glyphs.
  assign_glyphs also handles merging when merge.overlaps =
  TRUE in a \code{\link{glyph}} call.
}

