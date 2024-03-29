% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/splitField.R
\name{lay_split_field}
\alias{lay_split_field}
\alias{laySplitField}
\title{Split a selected field from layout using a schema from another layout.}
\usage{
lay_split_field(lay, newlay, field)

laySplitField(lay, newlay, field)
}
\arguments{
\item{lay}{a Layout object.}

\item{newlay}{a Layout object used to split a field from \code{lay}.}

\item{field}{id of a field from \code{lay}.}
}
\value{
a new Layout object.
}
\description{
Split a selected field from layout using a schema from another layout.
}
\examples{

l1 <- lay_new(matrix(c(1:4), ncol = 2), widths = c(4, 1))
l2 <- lay_new(matrix(c(1:4), ncol = 2), widths = c(1, 1))
l3 <- lay_split_field(l1, l2, 2)
lay_show(l3)

}
