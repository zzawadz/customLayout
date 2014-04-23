\name{listPlot}
\alias{listPlot}
\title{Create list of plots that can be stored in Layout object.}
\usage{
listPlot(...)
}
\arguments{
  \item{...}{expressions that produces plots}
}
\description{
Create list of plots that can be stored in Layout object.
}
\examples{
# Easy way to draw plots stored in list of plots
list_plot = listPlot(plot(1:1000), plot(1:100))
par(mfrow = c(2,1))
sapply(list_plot, eval)

#' list_plot = listPlot(plot(1:1000), plot(1:100))

lplots = createLayout(matrix(1:2),plots=list_plot)
renderPlots(lplots)
}

