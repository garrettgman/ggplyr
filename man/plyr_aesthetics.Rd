\name{plyr_aesthetics}
\alias{plyr_aesthetics}
\title{compute_aesthetics groupwise at build}
\usage{
  plyr_aesthetics(., data, plot)
}
\description{
  plyr_aesthetics replaces a layer's compute_aesthetics
  method when \code{\link{ply_aes}} is called. This results
  in aesthetics being computed groupwise during
  \code{\link[ggplot2]{ggplot_build}}
}

