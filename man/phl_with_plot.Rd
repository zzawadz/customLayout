% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/officer.R
\name{phl_with_plot}
\alias{phl_with_plot}
\title{Add plot into layout placeholder}
\usage{
phl_with_plot(x, olay, id, plotFnc, res = 300, ...)
}
\arguments{
\item{x}{rpptx object}

\item{olay}{an OfficerLayout object created using \code{\link{phl_layout}}}

\item{id}{an single integer with an id of the placeholder from \code{olay} object.}

\item{plotFnc}{a function which creates a plot when called.}

\item{res}{The nominal resolution in ppi which will be recorded in the bitmap file. Default 300. See \code{res} parameter in \code{\link{png}}.}

\item{...}{other arguments passed to \code{\link{png}} function.}
}
\value{
\code{rpptx} object which represents PowerPoint presentation in \code{officer}. The returned object contains a new element on the slide.
}
\description{
Add plot into layout placeholder
}
