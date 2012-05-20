\name{group_by}
\alias{group_by}
\title{group_by groups a data frame into a list of smaller data frames. Each smaller data frame contains all of the rows of the original data frame that contain one unique combination of the sorting variables.}
\usage{
  group_by(data, vars)
}
\arguments{
  \item{data}{a data frame to group}

  \item{the}{name of the variable(s) to group by, as
  character strings}
}
\description{
  group_by groups a data frame into a list of smaller data
  frames. Each smaller data frame contains all of the rows
  of the original data frame that contain one unique
  combination of the sorting variables.
}

