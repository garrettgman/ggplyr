\name{ygply}
\alias{ygply}
\title{split ggplot2 layer, apply function, and return results as a ggplot2 graph}
\usage{
  ygply(.data, .split.fun = NULL, .apply.fun = NULL,
  .combine.fun = fun(as_is, "layers"), .progress = "none",
  .parallel = FALSE)
}
\arguments{
  \item{.data}{ggplot2 layer to be processed}

  \item{.split.fun}{function used to split the data set
  that underlies the ggplot2 layer, or the names of the
  variables to split on as character strings.}

  \item{.apply.fun}{function to apply to each subset of the
  ggplot2 layer}

  \item{.combine.fun}{function to combine output of
  .apply.fun into a graph. .combine.fun should return a
  ggplot2 layer object, or a list of ggplot2 layer
  objects.}

  \item{.progress}{name of the progress bar to use, see
  \code{\link{create_progress_bar}}}

  \item{.parallel}{if TRUE, apply function in parallel
  using the parallel backend provided by foreach}
}
\value{
  A ggplot2 object of class ggplot
}
\description{
  For each subset of data in a ggplot2 layer, apply a
  function and then combine results into a ggplot2 graph.
  Results will be combined with the function provided as
  the .combine.fun argument. This function should be
  compatible with the output of the function provided as
  the .apply.fun argument. Note, ygply can only manipulate
  data that is stored in the ggplot2 layer. If the layer
  inherits data from a global \code{\layer{ggplot}} call,
  yyplot will not have access to the data.
}

