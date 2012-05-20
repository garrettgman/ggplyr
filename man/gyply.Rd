\name{gyply}
\alias{gyply}
\title{split ggplot2 graph, apply function, and return results as a ggplot2 layer object}
\usage{
  gyply(.data, .split.fun = NULL, .apply.fun = NULL,
  .combine.fun = fun(as_is, "layers"), .progress = "none",
  .parallel = FALSE)
}
\arguments{
  \item{.data}{ggplot2 graph to be processed}

  \item{.split.fun}{function used to split the data sets
  that underlie the layers of the ggplot2 graph, or the
  names of the variables to split on as character strings.}

  \item{.apply.fun}{function to apply to each subset of the
  above data sets.}

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
  A list of ggplot2 layer objects
}
\description{
  For each subset of data in a ggplot2 graph, apply a
  function and then combine results into a list of ggplot2
  layers. This list can be used to build a ggplot2 plot.
  Results will be combined with the function provided as
  the .combine.fun argument. This function should be
  compatible with the output of the function provided as
  the .apply.fun argument. Note, ggply will attempt to
  modify each layer in a graph, which means ggply is
  unlikely to work if multiple data sets exist between
  different layers. ggply is compatible with multiple data
  sets in a graph, so long as each data set can be
  manipulated with the same \code{\link{dlply}} call.
}

