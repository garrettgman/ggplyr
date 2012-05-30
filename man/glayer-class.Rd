\name{glayer-class}
\alias{"$,glayer-method"}
\alias{"$<-,glayer-method"}
\alias{"+,ggplot,glayer-method"}
\alias{"+,glyphs,glayer-method"}
\alias{"[,glayer-method"}
\alias{"[<-,glayer-method"}
\alias{"c,glayer-method"}
\alias{"ggtransform,glayer-method"}
\alias{"ls,glayer-method"}
\alias{"rep,glayer-method"}
\alias{"show,glayer-method"}
\alias{glayer-class}
\title{glayer class}
\description{
  glayers are layers made with ggplyr methods. They are
  equivalent to the layers made by ggplot2 functions in all
  ways except that they contain an extra grouping variable
  (to denote subplot membership) and a plyr_function slot,
  which correctly locates subplots within the graph when
  plotting.
}

