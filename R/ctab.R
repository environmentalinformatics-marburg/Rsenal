#' Calculate contingency table
#' 
#' @param a An object of type "raster" containing categorical data
#' @param b An object of type "raster" containing categorical data. 
#' Must be of the same dimensions as a.
#' @param mask A string vector of category numbers which are are to be 
#' ignored in the calculations. The numbers must be identical to the 
#' values of the input rasters.
#' 
#' @description
#' Calculation of contingency table from two rasters with categorical data
#' 
#' @return
#' An object of class "matrix"
#' 
#' @author
#' Hanna Meyer
#' 
#' @seealso
#' For calculation of kappa indices see \code{\link{kstat}}
#' 
#' @examples
#' #### Example 1: Calculate the contingency table from two Land cover maps
#' #load data
#' library(raster)
#' LUC1990 <- raster(system.file("LUC1990.rst", package="Rsenal"))
#' LUC2006 <- raster(system.file("LUC2006.rst", package="Rsenal"))
#' 
#' #plot to get an impression:
#' par(mfrow=c(1,2))
#' plot(LUC1990,main="LUC Marburg 1990")
#' plot(LUC2006,main="LUC Marburg 2006")
#' 
#' #calculate contingency table:
#' ctab(LUC1990,LUC2006)
#' 
#' 
#' #### Example 2: Calculate the contingency table from a land cover map 
#' #and a map of ground truth sites
#' #load data
#' LUC1990 <- raster(system.file("LUC1990.rst", package="Rsenal"))
#' trainingSites <- raster(system.file("training.rst", package="Rsenal"))
#' 
#' #plot to get an impression:
#' par(mfrow=c(1,2))
#' plot(LUC1990,main="LUC Marburg 1990")
#' plot(trainingSites,main="Training sites (Ground truth)")
#' 
#' #calculate contingency table:
#' #(consider to mask the background since ground truth sites don't cover the whole area)
#' ctab(LUC1990,trainingSites,mask="0")
#' 
#' @export ctab
#' @aliases ctab
#' @name ctab
ctab<-function(a,b,mask=""){
  va<-values(a)
  vb<-values(b)
  name<-sort(union(unique(a),unique(b)))
  result<-matrix(0,ncol=length(name),nrow=length(name))
  colnames(result)<-name
  row.names(result)<-name
  for (i in 1:nrow(result)){
    for (k in 1:ncol(result)){
      result[i,k]<-sum(va==row.names(result)[i]&vb==colnames(result)[k])
    }
  }
  for (i in 1:length(mask)){
    result<-result[row.names(result)!=mask[i],]
    if (ncol(as.matrix(result))==1||nrow(as.matrix(result))==1){
      stop ("Calculation of contingency table requires at least two categories")
    }
    result<-result[,colnames(result)!=mask[i]] 
  }
  if (ncol(as.matrix(result))==1||nrow(as.matrix(result))==1){
    stop ("Calculation of contingency table requires at least two categories")
  }	
  return(result)
}