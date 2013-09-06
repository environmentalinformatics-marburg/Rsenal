\name{roc}
\alias{roc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calculation of ROC and AUC
}
\description{
  Calculation of receiver operating characteristics (ROC) and the corresponding area under the ROC-curve (AUC) from a probability map and a classified map.
}
\usage{
  roc(pred, obs, mask=NA, th=50, plot=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pred}{
    An object of type "RasterLayer" containing the probability of each pixel to belong to the class of investigation.
  }
  \item{obs}{
    An object of type "RasterLayer" containing 1 and 0 values with 1 indicating that a pixel belongs to the class of investigation.
  }
  \item{mask}{
    An object of type "RasterLayer" containing 1 and 0 values with 0 indicating that the pixel cannot be classified into the class of investigation.
  }
    \item{th}{
    The number of thresholds to calculate the ROC-curve
  }
      \item{plot}{
    Logical. Indicates whether a plot of the ROC curve should be drawn or not.
  }
}
\details{
The probability map is ranked decendingly. For each threshold, the corresponding percentage of pixels in the ranked probability map is then classified as 1, the pixels with lower probability are classified as 0. This classification is then compared to the observed classification and the false positives and true positives are calculated.
The AUC is the resulting sum of true positives at each threshold divided by the number of thresholds. An AUC of 0.5 indicates a ROC-curve asssociated with a random classification.

This function might be used to compare the performance of different models, assess the threshold-independent performance of a model or to find the best threshold for your model.
}

\value{
 A list with the first element is the calculated AUC and the second element is a matrix containing for each threshold the percentage of pixels classified as 1, the true positive rate and the false positive rate.
}

\author{
  Hanna Meyer
}
\note{
The number of pixels is divided by the number of thresholds. In case that the number of pixels cannot be equally dived by the number of thresholds, the number of thresholds is adjusted. So don't get confused if the result has one row more or less than the specified number of thresholds.
}
\seealso{
For further functions related to model validation see \code{\link{ctab}} and \code{\link{kstat}}
}
\examples{
#### Example 1: Calculate the ROC curve from a model of the growth of the town "Marbug". 
library(raster,rgdal)

#load data
#Use a probability map assuming high potential for city expansion is just 
#resulting from proximity to current urban area:
pred=raster(system.file("probability.rst", package="Rsenal"))
#observed city growth between 1990 and 2006
obs=raster(system.file("citygrowth.tif", package="Rsenal"))
#masking current urban area since these pixels have no potential for change
mask=raster(system.file("citymask.tif", package="Rsenal"))

#plot to get an impression:
par(mfrow=c(1,3))
plot(pred,main="Probability for urban expansion")
plot(obs,main="Urban expansion from 1990 to 2006")
plot(mask,main="Mask: Urban area 1990")

#calculate ROC
roc(pred,obs,mask,th=25)
}