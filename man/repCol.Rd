\name{repCol}
\alias{repCol}
\title{Repeats columns in matrix.}
\usage{
repCol(x, fr)
}
\arguments{
  \item{x}{matrix}

  \item{fr}{numeric value or vector with number of
  repetition of given column.}
}
\description{
Repeats columns in matrix.
}
\examples{
repCol(matrix(1:4,ncol=2),c(4,2))
repCol(matrix(1:4,ncol=2),2)
}

