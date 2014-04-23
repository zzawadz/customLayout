\name{repRow}
\alias{repRow}
\title{Repeats rows in matrix.}
\usage{
repRow(x, fr)
}
\arguments{
  \item{x}{matrix}

  \item{fr}{numeric value or vector with number of
  repetition of given row.}
}
\description{
Repeats rows in matrix.
}
\examples{
repRow(matrix(1:4,ncol=2),c(4,2))
repRow(matrix(1:4,ncol=2),2)
}

