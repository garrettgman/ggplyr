\name{combine_glyphs}
\alias{combine_glyphs}
\title{Calculate final positions in a glayer}
\usage{
  combine_glyphs(., data)
}
\description{
  combine_glyphs calculates the final positions for every
  location in a glayer. glayer_build first builds each
  glyph separately as if it were a facet. If plotted, these
  glyphs would overlap with each other. combine_glyph
  relocates each glyph based on the global positions stored
  in the layer's embed variable.
}

