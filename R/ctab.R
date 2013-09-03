ctab=function(a,b,mask=""){
  require(raster)
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
  for (i in 1:length(mask)){
    result=result[row.names(result)!=mask[i],]
    if (ncol(as.matrix(result))==1||nrow(as.matrix(result))==1){
      stop ("Calculation of contingency table requires at least two categories")
    }
    result=result[,colnames(result)!=mask[i]] 
  }
  if (ncol(as.matrix(result))==1||nrow(as.matrix(result))==1){
    stop ("Calculation of contingency table requires at least two categories")
  }	
  return(result)
}