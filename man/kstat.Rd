\name{kstat}
\alias{kstat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Similarity of categorical maps
}
\description{
Calculation of Kappa indices as well as quantity and location (dis)agreements to assess similarity of two categorical maps.
}
\usage{
kstat(a, b, mask= "", perCategory = TRUE)
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
  \item{perCategory}{
If TRUE indices are calculated per category in addition to overall indices.
}
}
\details{
Calculation of kappa index, kappa of quantity, Kappa of location (see Cohen 1960, Pontius 2000, Hagen 2002) for the overall map and for each category (Monserud & Leemans 1992) as well as quantity and location (dis)agreements according to Pontius 2011.
}
\value{
A data frame containing the values for overall kappa (K), Kappa of location (Kloc), Kappa of quantity (Khisto), chance agreement (CA), quantity agreement (QA), allocation agreement (AA), allocation disagreement (AD), quantity disagreement (QD).
}
\references{
Cohen, J., 1960. A coefficient of agreement for nominal scales, Educational and Psychological Measurements, 20: 37–46.

Hagen,  A. (2002):  Multi-method  assessement  of  map  similarity.  Proceeding  of  5 th   AGILE  Conference  on  Geographic Information Science, April. 25 – 27 Spain, 171- 182. 

Monserud, R. A., and Leemans, R.(1992): Comparing global vegetation maps with the kappa statistic. Ecological Modeling, 62: 275-293.

Pontius, R.G. (2000): Quantifcation Error Versus Allocation Error in Comparison of Categorical Maps. Photogrammatric Engineering & Remote Sensing, 66: 1011–1016.

Pontius, R.G., Jr., and Millones, M. (2011): Death to Kappa: Birth of Quantity 
Disagreement and Allocation Disagreement for Accuracy Assessment. International Journal of Remote Sensing, 32: 4407–4429.
}
\author{
Hanna Meyer
}
\examples{
## Example calculating the indices from two rasters with 4 categories of interest
library(raster)
r <- raster(nc=5, nr=5)
s <- raster(nc=5, nr=5)
r[]=sample(0:4, 25,replace=T)
s[]=sample(0:4, 25,replace=T)
kstat(r,s,mask="0")
}