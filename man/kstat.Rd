\name{kstat}
\alias{kstat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Similarity of categorical maps
}
\description{
Calculation of Kappa indices as well as quantity and location (dis)agreements to assess similarity of two categorical maps.
Might be used mainly to compare a raster of ground truth sites with a classified map or to compare a model output with a reference map. 
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
Calculation of kappa index, kappa of quantity, Kappa of location (see Cohen 1960, Pontius 2000, Hagen 2002) for the overall map and for each category (Monserud & Leemans 1992) as well as quantity and location (dis)agreements according to Pontius (2011).
}
\value{
A data frame containing the values for overall kappa (K), Kappa of location (Kloc), Kappa of histogram (Khisto), chance agreement (CA), quantity agreement (QA), allocation agreement (AA), allocation disagreement (AD), quantity disagreement (QD).
}
\references{
Cohen, J., (1960): A coefficient of agreement for nominal scales, Educational and Psychological Measurements, 20: 37–46.

Hagen,  A. (2002):  Multi-method  assessement  of  map  similarity.  Proceeding  of  5 th   AGILE  Conference  on  Geographic Information Science, April. 25 – 27 Spain, 171- 182. 

Monserud, R. A., and Leemans, R.(1992): Comparing global vegetation maps with the kappa statistic. Ecological Modeling, 62: 275-293.

Pontius, R.G. (2000): Quantifcation Error Versus Allocation Error in Comparison of Categorical Maps. Photogrammatric Engineering & Remote Sensing, 66: 1011–1016.

Pontius, R.G., Jr., and Millones, M. (2011): Death to Kappa: Birth of Quantity Disagreement and Allocation Disagreement for Accuracy Assessment. International Journal of Remote Sensing, 32: 4407–4429.
}
\author{
Hanna Meyer
}
\seealso{
For returning only the contingency table see \code{\link{ctab}}.
}
\examples{
#### Calculate the indices to compare two Land cover maps
library(raster)
LUC1990=raster(system.file("LUC1990.rst", package="Rsenal"))
LUC2006=raster(system.file("LUC2006.rst", package="Rsenal"))
kstat(LUC1990,LUC2006)
#### Calculate the indices to compare a land cover map with a map of ground truth sites
LUC1990=raster(system.file("LUC1990.rst", package="Rsenal"))
trainingSites=raster(system.file("training.rst", package="Rsenal"))
#consider to mask the background since ground truth sites don't cover the whole area
kstat(LUC1990,trainingSites,mask="0")

#For an example of an other field of application of these indices see 
#http://giswerk.org/doku.php?id=people:c:creudenbach:alpenwerk:achtunglawine
}