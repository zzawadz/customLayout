% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/officer.R
\name{phl_layout}
\alias{phl_layout}
\title{Create layout for the officer PowerPoint slide.}
\usage{
phl_layout(
  cl,
  slideWidth = 10,
  slideHeight = 7.5,
  margins = c(bottom = 0.25, left = 0.25, top = 0.25, right = 0.25),
  innerMargins = c(bottom = 0.025, left = 0.025, top = 0.025, right = 0.025)
)
}
\arguments{
\item{cl}{layout object}

\item{slideWidth}{width of the slide in inches (default 10)}

\item{slideHeight}{height of the slide in inches (default 7.5)}

\item{margins}{A numerical vector of the form c(bottom, left, top, right)f
which gives the size of margins on the four sides of the layout.
The default is c(0.25, 0.25, 0.25, 0.25).}

\item{innerMargins}{A numerical vector of the form c(bottom, left, top, right)
which gives the size of margins on the four sides of the each placeholder in the layout.
The default is c(0.025, 0.025, 0.025, 0.025).}
}
\value{
A list containing the coordinates of the slide segments created from layout scheme.
}
\description{
Create layout for the officer PowerPoint slide.
}
\examples{

library(officer)
library(customLayout)
library(magrittr)
library(ggplot2)

lay = lay_new(matrix(1:4,nc = 2),widths=c(3, 2),heights=c(2, 1))
lay2 = lay_new(matrix(1:3))
cl = lay_bind_col(lay,lay2, widths = c(3,1))

allPositions <- phl_layout(cl, innerMargins = rep(0.1,4))

my_pres <- read_pptx() \%>\% 
  add_slide(master = "Office Theme", layout = "Two Content")

p <- qplot(mpg, wt, data = mtcars)

for(pos in allPositions) {
  my_pres <- my_pres \%>\% officer::ph_with(
    p, location = ph_location(
    width = pos["width"],
    height = pos["height"], 
    left = pos["left"],
    top = pos["top"]) )
}

if(interactive()) {
  if(!dir.exists("tmp")) dir.create("tmp")
  print(my_pres, target = "tmp/test-officer-layout.pptx")
}

}
