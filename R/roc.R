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
  AUC=sum(result[,3])/nrow(result)
  if (plot==TRUE){
    plot(result[,2],result[,3],type="l",xlab="False positive rate",ylab="True positive rate",xlim=c(0,1),ylim=c(0,1))
    lines(c(0,1),c(0,1),col="grey50")
    legend("topleft",legend=paste("AUC = ",round(AUC,3)),bty="n")
  }
  colnames(result)=c("TH","FP","TP")
  result2=list()
  result2[[1]]=AUC
  result2[[2]]=result
  return (result2)
}