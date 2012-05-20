\name{ggplyr_build}
\alias{ggplyr_build}
\title{Build a ggplyr object for rendering}
\usage{
  ggplyr_build(plot)
}
\arguments{
  \item{plot}{ggplyr object}
}
\description{
  This function takes the plot object, and performs all
  steps necessary to produce an object that can be
  rendered. This function outputs two pieces: a list of
  data.frames (one for each layer), and a panel object,
  which contain all information about axis limits, breaks,
  etc.
}
\details{
  ggplyr_build follows \code{\link{ggplot_build}} but
  implements extra steps to calcuate groupwise aesthetics
  and positioning.
}

