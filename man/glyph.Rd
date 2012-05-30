\name{glyph}
\alias{glyph}
\title{glyph turns an ordinary layer into a set of glyphs. To do this, it must:
1. ensure that the x and y aesthetics are mapped to a single value
2. add reference boxes
3. handle merging
4. split up the data}
\usage{
  glyph(layer, major.aes, glyph.by = NULL, width =
  rel(0.95), height = rel(0.95), x_scale = identity,
  y_scale = identity, merge.overlaps = FALSE, reference =
  NULL, .ref = FALSE)
}
\description{
  glyph turns an ordinary layer into a set of glyphs. To do
  this, it must: 1. ensure that the x and y aesthetics are
  mapped to a single value 2. add reference boxes 3. handle
  merging 4. split up the data
}

