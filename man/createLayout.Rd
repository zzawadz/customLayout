\name{createLayout}
\alias{createLayout}
\title{Create custom layout.}
\usage{
createLayout(mat, widths = NULL, heights = NULL, plots = list())
}
\arguments{
  \item{layout}{object of class Layout.}
}
\description{
Create custom layout.
}
\examples{
require(customLayout)
par(mar = c(3,2,2,1))
lay = createLayout(matrix(1:4,nc=2),widths=c(3,2),heights=c(2,1))
lay2 = createLayout(matrix(1:3))
cl = colBind(lay,lay2, widths=c(3,1))
setLayout(cl) # initialize drawing area
plot(1:100+rnorm(100))
plot(rnorm(100), type = "l")
hist(rnorm(500))
acf(rnorm(100))
pie(c(3,4,6),col = 2:4)
pie(c(3,2,7),col = 2:4+3)
pie(c(5,4,2),col = 2:4+6)
}

