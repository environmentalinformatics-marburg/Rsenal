#' Calculation of ROC and AUC
#' 
#' @param pred An object of type "RasterLayer" containing the probability of each pixel to belong to the class of investigation.
#' @param obs An object of type "RasterLayer" containing 1 and 0 values with 1 indicating that a pixel belongs to the class of investigation.
#' @param mask An object of type "RasterLayer" containing 1 and 0 values with 0 indicating that the pixel cannot be classified into the class of investigation.
#' @param plot Logical. Indicates whether a plot of the ROC curve should be drawn or not
#' @param th The number of thresholds to calculate the ROC-curve
#' 
#' @details
#' The probability map is ranked decendingly. For each threshold, the corresponding percentage of pixels in the ranked probability map is then classified as 1, the pixels with lower probability are classified as 0. This classification is then compared to the observed classification and the false positives and true positives are calculated.
#' The AUC is the integral of a natural spline interpolation. An AUC of 0.5 indicates a ROC-curve asssociated with a random classification.
#' This function might be used to compare the performance of different models, assess the threshold-independent performance of a model or to find the best threshold for your model.
#' 
#' @return
#' A list with the first element is the calculated AUC and the second element is a matrix containing for each threshold the percentage of pixels classified as 1, the true positive rate and the false positive rate.
#' 
#' @author
#' Hanna Meyer
#' 
#' @note
#' The number of pixels is divided by the number of thresholds. In case that the number of pixels cannot be equally dived by the number of thresholds, the number of thresholds is adjusted. So don't get confused if the result has one row more or less than the specified number of thresholds.
#' 
#' @seealso
#' For further functions related to model validation see \code{\link{ctab}} and \code{\link{kstat}}
#' 
#' @examples
#' #### Example 1: Calculate the ROC curve from a model of the growth of the town "Marbug". 
#' library(raster)
#' library(rgdal)
#' 
#' #load data
#' #Use a probability map assuming high potential for city expansion is just 
#' #resulting from proximity to current urban area:
#' pred <- raster(system.file("probability.rst", package="Rsenal"))
#' #observed city growth between 1990 and 2006
#' obs <- raster(system.file("citygrowth.tif", package="Rsenal"))
#' #masking current urban area since these pixels have no potential for change
#' mask <- raster(system.file("citymask.tif", package="Rsenal"))
#' 
#' #plot to get an impression:
#' par(mfrow=c(1,3))
#' plot(pred,main="Probability for urban expansion")
#' plot(obs,main="Urban expansion from 1990 to 2006")
#' plot(mask,main="Mask: Urban area 1990")
#' 
#' #calculate ROC
#' roc(pred,obs,mask,th=25)
#' 
#' @aliases
#' roc
#' 

roc=function(pred,obs,mask=NA,plot=TRUE,th=100) {
  require(raster)
  if (class(pred)!="RasterLayer"||class(obs)!="RasterLayer"){
    stop ("Input data must be of type 'RasterLayer'")
  }
  if (class(mask)=="RasterLayer"){
    pred=pred*mask
  }
  va=values(pred)
  vb=values(obs)
  pixel=length(va)
  vasort=sort(va,decreasing=TRUE,index.return=TRUE)
  vbsort=vb[vasort$ix]
  vasort=vasort$x
  th=th-2 #adjust the number of threshold since 0 and the number of pixels (=max. threshold) are added
  result=matrix(nrow=length(c(0,seq(round(pixel/th,0),pixel,by=round(pixel/th,0)),pixel)),ncol=3)
  for (i in c(0,seq(round(pixel/th,0),pixel,by=round(pixel/th,0)),pixel)){
    vasort[]=0
    vasort[1:i]=1 #classify the probability map according to current threshold
    A=sum(vasort==1&vbsort==1) #calculate contingency table
    B=sum(vasort==1&vbsort==0)
    C=sum(vasort==0&vbsort==1)
    D=sum(vasort==0&vbsort==0)
    #calculate false positive and versus true positive rate
    result[which(c(0,seq(round(pixel/th,0),pixel,by=round(pixel/th,0)),pixel)==i),1]=i/pixel
    result[which(c(0,seq(round(pixel/th,0),pixel,by=round(pixel/th,0)),pixel)==i),3]= A/(A+C)
    result[which(c(0,seq(round(pixel/th,0),pixel,by=round(pixel/th,0)),pixel)==i),2]=B/(B+D)
  }
  AUC=integrate(splinefun(result[,2],result[,3],method="natural"),0,1)$value
  if (plot==TRUE){
    plot(result[,2],result[,3],type="l",xlab="False positive rate",ylab="True positive rate",xlim=c(0,1),ylim=c(0,1))
    lines(c(0,1),c(0,1),col="grey50")
    legend("topleft",legend=paste("AUC = ",round(AUC,3)),bty="n")
  }
  colnames(result)=c("threshold","falsePositives","truePositives")
  result2=list()
  result2[[1]]=AUC
  result2[[2]]=result
  return (result2)
}