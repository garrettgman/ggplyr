\name{nest}
\alias{nest}
\title{nest combines subplot x and y data with plot level x and y coordinates to embed the subplots into a set of major axiis. nest calculates the plot level coordinates and returns a list object whcih is used by embed_layers to perform the actual comining. This list contains a function to transform the subplot data and a mapping to be used to correctly label the x and y axes.}
\usage{
  nest(data, major_aes = NULL, x_scale = identity, y_scale
  = identity, width = 1, height = 1, reference = NULL)
}
\description{
  nest combines subplot x and y data with plot level x and
  y coordinates to embed the subplots into a set of major
  axiis. nest calculates the plot level coordinates and
  returns a list object whcih is used by embed_layers to
  perform the actual comining. This list contains a
  function to transform the subplot data and a mapping to
  be used to correctly label the x and y axes.
}

