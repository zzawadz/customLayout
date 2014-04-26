\name{renderPlots}
\alias{renderPlots}
\title{Set custom layout and render all plots stored in Layout object.}
\usage{
renderPlots(layout)
}
\arguments{
  \item{layout}{object of class Layout.}
}
\description{
Set custom layout and render all plots stored in Layout
object.
}
\examples{
list_plot = listPlot(plot(1:1000), plot(1:100))
list_pie  = listPlot(pie(1:3,col = 2:4))

lplots = createLayout(matrix(1:2),plots=list_plot)
lpie   = createLayout(1,plots=list_pie)
lay = colBind(lplots,lpie)
renderPlots(lay)
}

