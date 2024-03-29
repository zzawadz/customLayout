% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/officer.R
\name{phl_with_text}
\alias{phl_with_text}
\title{add text into layout placeholder}
\usage{
phl_with_text(x, olay, id, str, type = "title", ...)
}
\arguments{
\item{x}{rpptx object}

\item{olay}{an OfficerLayout object created using \code{\link{phl_layout}}}

\item{id}{an single integer with an id of the placeholder from \code{olay} object.}

\item{str}{text to add.}

\item{type}{type of the text placeholder. See \code{\link{ph_add_text}} for more details.}

\item{...}{other arguments passed to \code{\link{ph_add_text}}.}
}
\value{
\code{rpptx} object which represents PowerPoint presentation in \code{officer}. The returned object contains a new element on the slide.
}
\description{
add text into layout placeholder
}
