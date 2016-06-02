#' Best subset feature selection
#' @description Evaluate all combinations of predictors during model training
#' @param predictors see \code{\link{train}}
#' @param response see \code{\link{train}}
#' @param method see \code{\link{train}}
#' @param metric see \code{\link{train}}
#' @param maximize see \code{\link{train}}
#' @param trControl see \code{\link{train}}
#' @param tuneLength see \code{\link{train}}
#' @param tuneGrid see \code{\link{train}}
#' @param seed A random number
#' @param runParallel Logical
#' @return A list of class train
#' @details Models are iteratively fitted using all different combinations
#' of predictor variables. Thus 2^X models are calculated.
#'
#' The internal cross validation can be run in parallel. See information
#' on parallel processing of carets train functions for details.
#' 
#' @note This validation is particulary suitable for 
#' leave-one-station-out cross validations where variable selection
#' MUST be based on the performance of the model on the hold out station.
#' A more time efficient alternative is the forward feature selection
#'  (\code{\link{ffs}}).
#' @author Hanna Meyer
#' @seealso \code{\link{train}},  \code{\link{ffs}}, 
#' \code{\link{trainControl}},\code{\link{rfe}}
#' @examples
#' \dontrun{
#' data(iris)
#' bssmodel <- bss(iris[,1:4],iris$Species)
#' bssmodel$finalModel$xNames
#' }
#' @export bss
#' @aliases bss

bss <- function (predictors, 
                 response, 
                 method = "rf",
                 metric = ifelse(is.factor(response), "Accuracy", "RMSE"),
                 maximize = ifelse(metric == "RMSE", FALSE, TRUE),
                 trControl = trainControl(),
                 tuneLength = 3,
                 tuneGrid = NULL,
                 seed = 100,
                 runParallel = FALSE){
  require(caret)
  if(runParallel){
    require(doParallel)
    cl <- makeCluster(detectCores())
    registerDoParallel(cl)
  }
  n <- length(names(predictors))
  if(maximize) evalfunc <- function(x){max(x,na.rm=T)}
  if(!maximize) evalfunc <- function(x){min(x,na.rm=T)}
  isBetter <- function (actmodelperf,bestmodelperf,maximize=maximize){
    ifelse (!maximize, return(actmodelperf < bestmodelperf),
            return(actmodelperf > bestmodelperf))
  }  
  testgrid <- expand.grid(lapply(seq_along(names(predictors)), c, 0))
  testgrid <- testgrid[-which(rowSums(testgrid==0)>=(length(names(predictors))-1)),]
  acc <- 0
   for (i in 1:nrow(testgrid)){
    set.seed(seed)
    model <- train(predictors[,unlist(testgrid[i,])],
                   response,method=method,trControl=trControl,
                   tuneLength=tuneLength,
                   tuneGrid=tuneGrid)
    actmodelperf <- evalfunc(model$results[,names(model$results)==metric])
    if (i == 1){
      bestmodelperf <- actmodelperf
      bestmodel <- model
    } else{
      if (isBetter(actmodelperf,bestmodelperf,maximize=maximize)){
        bestmodelperf <- actmodelperf 
        bestmodel <- model
      }
    }
    acc <- acc+1
    print(paste0("models that still need to be trained: ",
                 2^n-(n+1) - acc))
  }
  if(runParallel){
    stopCluster(cl)
  }
  return(bestmodel)
}