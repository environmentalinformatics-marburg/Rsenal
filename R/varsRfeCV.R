#' extract cross-validated important variables
#' 
#' @description 
#' this function approaches the identification of important variables from
#' \code{\link{rfe}} more conservatively than \code{\link{caret}}. It uses
#' the standard deviation (or standard error) of the cross-validated error 
#' metric to identify important variables.
#' 
#' @param rfe.model a rfe model. See \code{\link{rfe}}
#' @param metric the metric to be used. Note this needs to be the metric used 
#' to calculate the \code{\link{rfe}} model
#' @param maximize logical: Is a higher value of the metric favourable
#'  (e.g metric = Rsquared) or not (e.g metric = RMSE). maximize=TRUE is 
#'  determined automatically as long as metric is either Rsquared, ROC, Accuracy.
#'  maximize =FALSE is used for all other metrics. Set this manually if you
#'  use an other metric where higher values are favourable.
#' @param sderror If TRUE then standard error is calculated. If FALSE then
#' standard deviations are used
#' @return
#' a character vector of the variable names
#' 
#' @author
#' Hanna Meyer, Tim Appelhans
#' 
#' @export varsRfeCV
#' @aliases varsRfeCV

varsRfeCV <- function (rfe.model,
                       metric = rfe.model$metric,
                       maximize=FALSE,
                       sderror=TRUE) {
  
  
  if (metric=="Rsquared"||metric=="ROC"||metric=="Accuracy"){
    maximize=TRUE
  } else {
    maximize =FALSE
  }
  
  data <- as.data.frame(rfe.model$resample)
  
  sdv <- c()
  means <- c()
  
  for (i in unique(data$Variables)) {
    if(!sderror){
      sdv <- c(sdv,
               sd(eval(parse(text = paste("data$",
                                          metric)))[data$Variables == i]))
    }
    if(sderror){
      sdv <- c(sdv,se(eval(parse(text = paste("data$",
                                              metric)))[data$Variables == i]))
    }
    means <- c(means,
               mean(eval(parse(text=paste("data$",
                                          metric)))[data$Variables==i]))
  }
  if (maximize)  {
    upr <- means - sdv
  } else {
    upr <- means + sdv
  }
  #start_var <- min(unique(data$Variables))
  #start_offset <- abs(start_var - 1)
  
  #   print(means)
  #   print(upr)
  #   print(upr[rfe.model$bestSubset - start_offset])
  #   print(means < upr[rfe.model$bestSubset - start_offset])
  if (maximize){
    n_vars <- unique(data$Variables)[which(means >
                                             upr[which(unique(data$Variables)==
                                                         rfe.model$bestSubset)])[1]]
  } else{
    n_vars <- unique(data$Variables)[which(means <
                                             upr[which(unique(data$Variables)==
                                                         rfe.model$bestSubset)])[1]]
  }
  
  subset <- rfe.model$variables[rfe.model$variables$Variables==n_vars,]
  uniqueVars <- unique(subset$var)
  bestVar<-c()
  for (i in 1:length(uniqueVars)){
    bestVar[i]=sum(subset$Overall[subset$var==uniqueVars[i]])/10
  }
  names(bestVar) <- uniqueVars
  bestVar<-sort(bestVar,decreasing=TRUE)[1:n_vars]
  
  #bestVar <- rfe.model$control$functions$selectVar(rfe.model$variables, n_vars)
  #return(rfe.model$optVariables[1:n_vars])
  return(bestVar)
  
}
