\name{embed_layers}
\alias{embed_layers}
\title{embed_layers takes a list of layers and arranges them into an embedded layer that will be plotted as an embedded plot. Each layer must have the same parameters, geom, stat, and aesthetics. Only the data differs.}
\usage{
  embed_layers(layers, .fun)
}
\description{
  note: .fun should return a list(fun = combine function,
  major_aes = aes(x = , y = )
}

