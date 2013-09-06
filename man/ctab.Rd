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
#### Example 1: Calculate the contingency table from two Land cover maps
#load data
library(raster)
LUC1990=raster(system.file("LUC1990.rst", package="Rsenal"))
LUC2006=raster(system.file("LUC2006.rst", package="Rsenal"))

#plot to get an impression:
par(mfrow=c(1,2))
plot(LUC1990,main="LUC Marburg 1990")
plot(LUC2006,main="LUC Marburg 2006")

#calculate contingency table:
ctab(LUC1990,LUC2006)


#### Example 2: Calculate the contingency table from a land cover map and a map of ground truth sites
#load data
LUC1990=raster(system.file("LUC1990.rst", package="Rsenal"))
trainingSites=raster(system.file("training.rst", package="Rsenal"))

#plot to get an impression:
par(mfrow=c(1,2))
plot(LUC1990,main="LUC Marburg 1990")
plot(trainingSites,main="Training sites (Ground truth)")

#calculate contingency table:
#(consider to mask the background since ground truth sites don't cover the whole area)
ctab(LUC1990,trainingSites,mask="0")
}