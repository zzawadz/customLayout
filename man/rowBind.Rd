\name{rowBind}
\alias{rowBind}
\title{Take two Layout objects and combine by rows.}
\usage{
rowBind(x, y, heights = c(1, 1))
}
\arguments{
  \item{x}{object of class Layout}

  \item{y}{object of class Layout}
}
\description{
Take two Layout objects and combine by rows.
}
\examples{
l1 = createLayout(matrix(c(1:2),ncol = 2),widths=c(4,1))
l2 = createLayout(matrix(c(1:4),ncol = 2),widths=c(1,1))
lb = rowBind(l1,l2)
layoutShow(lb)
}

