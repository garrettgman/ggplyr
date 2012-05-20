\name{gglayer-class}
\alias{"[,gglayer-method"}
\alias{"[<-,gglayer-method"}
\alias{"+,ggplot,gglayer-method"}
\alias{"+,ggplyr,gglayer-method"}
\alias{"$,gglayer-method"}
\alias{"$<-,gglayer-method"}
\alias{"c,gglayer-method"}
\alias{"ggtransform,gglayer-method"}
\alias{"rep,gglayer-method"}
\alias{"show,gglayer-method"}
\alias{gglayer-class}
\title{gglayer class}
\description{
  gglayers are layers made with ggplyr methods. They are
  equivalent to the layers made by ggplot2 functions in all
  ways except that they contain an extra grouping variable
  (to denote subplot membership) and a plyr_function slot,
  which correctly locates subplots within the graph when
  plotting.
}

