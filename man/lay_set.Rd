% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Layout.R
\name{lay_set}
\alias{lay_set}
\alias{laySet}
\title{Set custom layout.}
\usage{
lay_set(layout)

laySet(layout)
}
\arguments{
\item{layout}{object of class Layout.}
}
\value{
Returns the number of figures in layout.
}
\description{
Set custom layout.
}
\examples{

lplots = lay_new(matrix(1:2))
lpie   = lay_new(1)
lay = lay_bind_col(lplots,lpie)
lay_set(lay)
plot(1:10)
plot(1:10)
plot(1:20)

}
