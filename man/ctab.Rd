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
For calculation of kappa indices see \code{\link{kstat}}
}
\examples{
#### Calculate the contingency table from two Land cover maps
library(raster)
LUC1990=raster(system.file("LUC1990.rst", package="Rsenal"))
LUC2006=raster(system.file("LUC2006.rst", package="Rsenal"))
ctab(LUC1990,LUC2006)
#### Calculate the contingency table from a land cover map and a map of ground truth sites
LUC1990=raster(system.file("LUC1990.rst", package="Rsenal"))
trainingSites=raster(system.file("training.rst", package="Rsenal"))
#consider to mask the background since ground truth sites don't cover the whole area
ctab(LUC1990,trainingSites,mask="0")
}