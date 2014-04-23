\name{setLayout}
\alias{setLayout}
\title{Set custom layout.}
\usage{
setLayout(layout)
}
\arguments{
  \item{layout}{object of class Layout.}
}
\description{
Set custom layout.
}
\examples{
lplots = createLayout(matrix(1:2))
lpie   = createLayout(1)
lay = colBind(lplots,lpie)
setLayout(lay)
plot(1:10)
plot(1:10)
plot(1:20)
}

