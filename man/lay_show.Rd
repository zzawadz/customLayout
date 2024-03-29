% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{lay_show}
\alias{lay_show}
\alias{layShow}
\title{Print the layout structure to the graphical device.}
\usage{
lay_show(layout)

layShow(layout)
}
\arguments{
\item{layout}{an object of class Layout.}
}
\value{
No return value, called for side effects.
}
\description{
Print the layout structure to the graphical device.
}
\examples{

l1 <- lay_new(matrix(c(1:2), ncol = 2), widths = c(4, 1))
l2 <- lay_new(matrix(c(1:3), ncol = 3), widths = c(2, 1, 3))
l3 <- lay_bind_row(l1, l2, heights = c(2, 1))
lay_show(l3)

l4 <- lay_new(matrix(c(1:2), ncol = 2), widths = c(4, 1))
l5 <- lay_new(matrix(c(1:3), ncol = 1), heights = c(2, 1, 1))
l6 <- lay_bind_col(l4, l5, widths = c(1, 1))
lay_show(l6)

}
