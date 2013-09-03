\name{ctab}
\alias{ctab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calculate contingency table
}
\description{
  Calculation of contingency table from two rasters with categorical data
}
\usage{
  ctab(a, b, mask= "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
    An object of type "raster" containing categorical data
  }
  \item{b}{
    An object of type "raster" containing categorical data. Must be of the same dimensions as a.
  }
  \item{mask}{
    A string vector of category numbers which are are to be ignored in the calculations. The numbers must be identical to the values of the input rasters.
  }
}

\value{
  An object of class "matrix"
}

\author{
  Hanna Meyer
}
\seealso{
\code{\link{kstat}}
}
\examples{
  ## Example calculating contingency table from 2 rasters.
  library(raster)
  r <- raster(nc=5, nr=5)
  s <- raster(nc=5, nr=5)
  r[]=sample(0:4, 25,replace=T)
  s[]=sample(0:4, 25,replace=T)
  ctab(r,s,mask="0")
}