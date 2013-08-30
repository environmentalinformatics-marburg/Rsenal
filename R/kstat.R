kstat=function(a,b,mask="",perCategory=TRUE) {
  library(raster)
  ##################################################################################################
  #Function: Calculate contingency table
  ctab=function(a,b){
    va=values(a)
    vb=values(b)
    name=sort(union(unique(a),unique(b)))
    result=matrix(0,ncol=length(name),nrow=length(name))
    colnames(result)=name
    row.names(result)=name
    for (i in 1:nrow(result)){
      for (k in 1:ncol(result)){
        result[i,k]=sum(va==row.names(result)[i]&vb==colnames(result)[k])
      }
    }
    return(result)
  }
  ##################################################################################################
  #Function: Calculate kappa indices and disagreements
	calculateKappa=function(ct){
		KappaResult=matrix()
		ct=ct/sum(ct)#percent of pixels 
		cmax=nrow(ct)#number of categories
		#Fraction of Agreement:
		PA=0
		for (i in 1:cmax) {
			PA=PA+ct[i,i]
		}
		#Expected Fraction of Agreement subject to the observed distribution:
		PE=0
		for (i in 1:cmax) {
			PE=PE+sum(ct[i,])*sum(ct[,i])
		}
		#Maximum  Fraction  of  Agreement  subject  to  the  observed  distribution:
		PMax=0
		for (i in 1:cmax) {
			PMax=PMax+min(sum(ct[i,]),sum(ct[,i]))
		}	
		#Kappa Index:
		K=(PA-PE)/(1-PE)
		#Kappa of location:
		Kloc=(PA-PE)/(PMax-PE)
		#Kappa of quantity:
		Khisto=(PMax-PE)/(1-PE)
		#chance agreement:
		CA=100*min((1/cmax),PA,PE)
		#quantity agreement:
		QA=ifelse(min((1/cmax),PE,PA)==(1/cmax),100*min((PE-1/cmax),PA-1/cmax),0)
		#allocation agreement:
		AA=100*max(PA-PE,0)
		#allocation disagreement:
		AD=100*(PMax-PA)
		#quantity disagreement:
		QD=100*(1-PMax)
	  KappaResult=cbind(K,Kloc,Khisto,CA,QA,AA,AD,QD)	
		return (KappaResult)
	}
  ##################################################################################################
  #Initialise
  if (any(dim(a)!=dim(b))){
    stop ("Dimensions of a and b does not match!")
  }
  ct=ctab(a,b)
	result=list()
  #mask and make sure that at least two classes are still available
	for (i in 1:length(mask)){
		ct=ct[row.names(ct)!=mask[i],]
		if (ncol(as.matrix(ct))==1||nrow(as.matrix(ct))==1){
			stop ("Calculation of Kappa requires at least two categories")
		}
		ct=ct[,colnames(ct)!=mask[i]] 
	}
	if (ncol(as.matrix(ct))==1||nrow(as.matrix(ct))==1){
		stop ("Calculation of Kappa requires at least two categories")
	}	
  #reclass to calculate kappa per category
	cttmp=ct
	if (ncol(cttmp)<=2||perCategory==FALSE){
		result[[1]]=calculateKappa(ct)
		if (perCategory==TRUE){
			print ("Warning: Kappa per category requires at least 3 categories")
		}
	}
	if (perCategory==TRUE&ncol(cttmp)>2) {
		for (ca in 1:(ncol(cttmp)+1)){
			if (ca==ncol(cttmp)+1) {
				ct=cttmp
			} 
			else {
				ct=cttmp
				ctNew=ct[1:2,1:2]
				ctNew[1,1]=ct[ca,ca]
				ctNew[1,2]=sum(ct[ca,])-ct[ca,ca]
				ctNew[2,1]=sum(ct[,ca])-ct[ca,ca]
				ctNew[2,2]=sum(ct)-sum(ct[ca,])-sum(ct[,ca])+ctNew[1,1]
				ct=ctNew
			}
			result[[ca]]=calculateKappa(ct)
		}
	}
  ##################################################################################################
  #arrange results	
	resultTab=result[[1]]
	if (length(result)>1){
		for (i in 2:length(result)){
			resultTab=rbind(resultTab,result[[i]])
		}
	}
	resultTab=as.data.frame(resultTab)
	colnames(resultTab)=c("K","Kloc","Khisto","CA","QA","AA","AD","QD")
	if (nrow(resultTab)>1){
		row.names(resultTab)=c(colnames(cttmp),"overall")	
	}
	else row.names(resultTab)="overall"
	return (resultTab)
}