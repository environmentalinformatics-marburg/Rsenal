#' Similarity of categorical maps
#' 
#' @param a An object of type "raster" containing categorical data
#' @param b An object of type "raster" containing categorical data. 
#' Must be of the same dimensions as a.
#' @param mask A string vector of category numbers which are are to be 
#' ignored in the calculations. The numbers must be identical to the 
#' values of the input rasters.
#' @param perCategory If TRUE indices are calculated per category in 
#' addition to overall indices.
#'                                                                                                                                        
#' @description
#' Calculation of Kappa indices as well as quantity and location (dis)agreements to assess similarity of two categorical maps.
#' Might be used mainly to compare a raster of ground truth sites with a classified map or to compare a model output with a reference map. 
#'  
#' @details
#' Calculation of kappa index, kappa of quantity, Kappa of location 
#' (see Cohen 1960, Pontius 2000, Hagen 2002) for the overall map and 
#' for each category (Monserud and Leemans 1992) as well as quantity and 
#' location (dis)agreements according to Pontius (2011).
#' The first step to calculate the indices is to calculate a \strong{contingency table}:
#' \tabular{llllllll}{
#' \tab\tab\cr
#' {} \tab{} \tab \strong{} \tab\strong{M} \tab\strong{A} \tab \strong{P} \tab\strong{} \tab\strong{A} \cr
#' \tab\tab\cr
#' {} \tab{} \tab \strong{}\tab \strong{1}\tab \strong{2} \tab \strong{  ..}\tab\strong{c} \tab \strong{Total}\cr
#' \strong{M} \tab \tab \strong{1} \tab {p11} \tab {p12} \tab {..} \tab {p1c} \tab {p1T} \cr
#' \strong{A} \tab \tab \strong{2} \tab {p21} \tab {p22} \tab {..} \tab {p2c} \tab {p2T}\cr
#' \strong{P} \tab \tab \strong{..} \tab {..} \tab {..}  \tab {..}\tab {..} \tab {..}\cr
#' \strong{} \tab \tab \strong{c} \tab {pc1} \tab {pc2} \tab {..} \tab {pcc} \tab {pcT}\cr
#' \strong{B} \tab \tab \strong{Total} \tab {pT1} \tab {pT2} \tab {..} \tab {pTc} \tab {1} \cr
#' }
#' From the contingency table the following statistics are calculated:
#' \itemize{
#' \item Fraction of Agreement: \eqn{P(A)=\sum_{i=1}^{c}p_{ii}}
#' \item Fraction of Agreement subject to the observed distribution: \eqn{P(E)=\sum_{i=1}^{c}p_{iT}*p_{Ti}}
#' \item Maximum Fraction of Agreement subject to the observed distribution: \eqn{P(max) = \sum_{i=1}^{c}min(p_{iT}, p_{Ti})}
#' }
#' From theses statistics, the \strong{kappa indices} are calculated as:
#' \itemize{
#' \item Kappa index: \eqn{K=\frac{P(A)-P(E)}{1-P(E)}}
#' \item Kappa of location: \eqn{K_{loc}=\frac{P(A)-P(E)}{P(max)-P(E)}}
#' \item Kappa of histogram: \eqn{K_{histo}=\frac{P(max)-P(E)}{1-P(E)}}
#' }
#' \strong{Disagreements} are calculated as:
#' \itemize{
#' \item Chance agreement: \eqn{CA=100*min((1/cmax),P(A),P(E))}
#' \item Quantity agreement: \eqn{QA=ifelse(min((1/cmax),P(E),P(A)==(1/cmax),100*min((P(E)-1/cmax),P(A)-1/cmax),0)}
#' \item Allocation disagreement: \eqn{AD=100*(P(Max)-P(A))}
#' \item Quantity disagreement: \eqn{QD=100*(1-P(Max))}
#' }
#' with \eqn{cmax} is the number of categories.
#' The calculations of \strong{Kappa per category} require a reclassification for each category:
#' \tabular{ccccc}{
#' \tab\tab\cr
#' {} \tab{} \tab\tab\strong{A} \tab\strong{}  \cr
#' \tab\tab\cr
#' \tab \tab \strong{i}\tab \strong{other}\tab \strong{Total} \cr
#' \tab  \strong{i} \tab {pii} \tab {pi.-pii}\tab {pi.} \cr
#' \strong{B} \tab \strong{other} \tab {p.i-pii} \tab {d}\tab {1-pi.}\cr
#' \tab \strong{Total} \tab {p.i} \tab {1-p.i}  \tab {1} \cr
#' }
#' The Kappa per category i is calculate in the same way as the normal Kappa 
#' but from the reclassified contingency table. Thus, the formula for the 
#' Kappa per category is 
#' \deqn{K_i=\frac{Pii-Pi.P.i}{\frac{pi.+p.i}{2}-pi.p.i}} 
#' as introduced by Fleiss (1981) and applied in e. g. Monserud 1992 and 
#' Hagen (2002). The Disagreements per category are as well calculated 
#' as explained above but from the reclassified contingency table for 
#' each category.
#' 
#' @return
#' A data frame containing the values for overall kappa (K), 
#' Kappa of location (Kloc), Kappa of histogram (Khisto), 
#' chance agreement (CA), quantity agreement (QA), 
#' allocation agreement (AA), allocation disagreement (AD), 
#' quantity disagreement (QD).
#' 
#' @references
#' Cohen, J., (1960): A coefficient of agreement for nominal scales, Educational and Psychological Measurements, 20: 37-46.
#' Fleiss, J.L., (1981): Statistical Methods for Rates and Proportions (2nd edition). John Wiley and Sons, New York, NY, 321 pp.
#' Hagen,  A. (2002):  Multi-method  assessement  of  map  similarity.  Proceeding  of  5 th   AGILE  Conference  on  Geographic Information Science, April. 25-27 Spain, 171-182. 
#' Light RJ (1971) Measure of response agreement for qualitative data: some generalizations and alternatives. Psychol Bull 76:365-377.
#' Monserud, R. A., and Leemans, R.(1992): Comparing global vegetation maps with the kappa statistic. Ecological Modeling, 62: 275-293.
#' Pontius, R.G. (2000): Quantifcation Error Versus Allocation Error in Comparison of Categorical Maps. Photogrammatric Engineering and Remote Sensing, 66: 1011-1016.
#' Pontius, R.G., Jr., and Millones, M. (2011): Death to Kappa: Birth of Quantity Disagreement and Allocation Disagreement for Accuracy Assessment. International Journal of Remote Sensing, 32: 4407-4429.
#' 
#' @note
#' The calculation of kappa per category bases on the approach of Fleiss (1981). Dont get confused with the Kappa per category calculated in e.g. Idrisi which follows the approach of Light (1971) and is an asymmetric Kappa index depending on which map is treated as reference map.
#' 
#' @author
#' Hanna Meyer
#' 
#' @seealso
#' For returning only the contingency table see \code{\link{ctab}}.
#' 
#' @examples
#' #### Example 1: Calculate the indices to compare two Land cover maps
#' #load data
#' library(raster)
#' LUC1990=raster(system.file("LUC1990.rst", package="Rsenal"))
#' LUC2006=raster(system.file("LUC2006.rst", package="Rsenal"))
#' 
#' #plot to get an impression:
#' par(mfrow=c(1,2))
#' plot(LUC1990,main="LUC Marburg 1990")
#' plot(LUC2006,main="LUC Marburg 2006")
#' 
#' #calculate indices:
#' kstat(LUC1990,LUC2006)
#' 
#' 
#' #### Example 2: Calculate the indices to compare a land cover map with a map of ground truth sites
#' #load data:
#' LUC1990=raster(system.file("LUC1990.rst", package="Rsenal"))
#' trainingSites=raster(system.file("training.rst", package="Rsenal"))
#' 
#' #plot to get an impression:
#' par(mfrow=c(1,2))
#' plot(LUC1990,main="LUC Marburg 1990")
#' plot(trainingSites,main="Training sites (Ground truth)")
#' 
#' #calculate indices
#' #(consider to mask the background since ground truth sites don't cover the whole area)
#' kstat(LUC1990,trainingSites,mask="0")
#' 
#' #For an example of an other field of application of these indices see 
#' #http://giswerk.org/doku.php?id=people:c:creudenbach:alpenwerk:achtunglawine
#' 
#' @aliases
#' kstat

kstat<-function(a,b,mask="",perCategory=TRUE) {
  require(raster)
  ##################################################################################################
  #Function: Calculate kappa indices and disagreements
	calculateKappa<-function(ct){
		KappaResult<-matrix()
		ct<-ct/sum(ct)#percent of pixels 
		cmax<-nrow(ct)#number of categories
		#Fraction of Agreement:
		PA<-0
		for (i in 1:cmax) {
			PA<-PA+ct[i,i]
		}
		#Expected Fraction of Agreement subject to the observed distribution:
		PE<-0
		for (i in 1:cmax) {
			PE<-PE+sum(ct[i,])*sum(ct[,i])
		}
		#Maximum  Fraction  of  Agreement  subject  to  the  observed  distribution:
		PMax<-0
		for (i in 1:cmax) {
			PMax<-PMax+min(sum(ct[i,]),sum(ct[,i]))
		}	
		#Kappa Index:
		K<-(PA-PE)/(1-PE)
		#Kappa of location:
		Kloc<-(PA-PE)/(PMax-PE)
		#Kappa of histogram:
		Khisto<-(PMax-PE)/(1-PE)
		#chance agreement:
		CA<-100*min((1/cmax),PA,PE)
		#quantity agreement:
		QA<-ifelse(min((1/cmax),PE,PA)==(1/cmax),100*min((PE-1/cmax),PA-1/cmax),0)
		#allocation agreement:
		AA<-100*max(PA-PE,0)
		#allocation disagreement:
		AD<-100*(PMax-PA)
		#quantity disagreement:
		QD<-100*(1-PMax)
	  KappaResult<-cbind(K,Kloc,Khisto,CA,QA,AA,AD,QD)	
		return (KappaResult)
	}
  ##################################################################################################
  #Initialise
  if (any(dim(a)!=dim(b))){
    stop ("Dimensions of a and b do not match!")
  }
  ct<-ctab(a,b,mask=mask)
	result<-list()
  #reclass to calculate kappa per category
	cttmp<-ct
	if (ncol(cttmp)<=2||perCategory==FALSE){
		result[[1]]<-calculateKappa(ct)
		if (perCategory==TRUE){
			print ("Warning: Kappa per category requires at least 3 categories")
		}
	}
	if (perCategory==TRUE&ncol(cttmp)>2) {
		for (ca in 1:(ncol(cttmp)+1)){
			if (ca==ncol(cttmp)+1) {
				ct<-cttmp
			} 
			else {
				ct<-cttmp
				ctNew<-ct[1:2,1:2]
				ctNew[1,1]<-ct[ca,ca]
				ctNew[1,2]<-sum(ct[ca,])-ct[ca,ca]
				ctNew[2,1]<-sum(ct[,ca])-ct[ca,ca]
				ctNew[2,2]<-sum(ct)-sum(ct[ca,])-sum(ct[,ca])+ctNew[1,1]
				ct<-ctNew
			}
			result[[ca]]<-calculateKappa(ct)
		}
	}
  ##################################################################################################
  #arrange results	
	resultTab<-result[[1]]
	if (length(result)>1){
		for (i in 2:length(result)){
			resultTab<-rbind(resultTab,result[[i]])
		}
	}
	resultTab<-as.data.frame(resultTab)
	colnames(resultTab)<-c("K","Kloc","Khisto","CA","QA","AA","AD","QD")
	if (nrow(resultTab)>1){
		row.names(resultTab)<-c(colnames(cttmp),"overall")	
	}
	else row.names(resultTab)<-"overall"
	return (resultTab)
}