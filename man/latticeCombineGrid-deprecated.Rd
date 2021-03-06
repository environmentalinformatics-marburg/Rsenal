% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/latticeCombineGrid.R
\name{latticeCombineGrid-deprecated}
\alias{latticeCombineGrid-deprecated}
\title{combine multiple lattice plots in a facetted grid (panels)}
\usage{
latticeCombineGrid(trellis.list, between = list(y = 0.3, x = 0.3)
, as.table = TRUE, ...)
}
\arguments{
\item{trellis.list}{a list containing lattice plot objects}

\item{between}{space between panels}

\item{as.table}{if TRUE (default) drawing is top left to bottom right}

\item{...}{additional arguments passed to \code{\link{c.trellis}}}
}
\value{
a single lattice plot object
}
\description{
this function combines multiple lattice plot objects in a facetted
grid. Note that the global plot settings (e.g. xlim, ylim, ...) 
are taken from the first object though the user can specify whether
\code{scales} should be identical or not. 
This is particularly useful when looping over large amounts of data
using \code{\link{lapply}} (see examples).
}
\seealso{
\code{\link{Rsenal-deprecated}}
}
\author{
Tim Appelhans
}
\keyword{internal}
