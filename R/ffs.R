#' Forward feature selection
#' @description A simple forward feature selection algorithm
#' @param predictors see \code{\link{train}}
#' @param response see \code{\link{train}}
#' @param method see \code{\link{train}}
#' @param metric see \code{\link{train}}
#' @param maximize see \code{\link{train}}
#' @param trControl see \code{\link{train}}
#' @param tuneLength see \code{\link{train}}
#' @param tuneGrid see \code{\link{train}}
#' @param seed A random number
#' @return A list of class train
#' @details Models with two predictors are first trained using all possible 
#' pairs of predictor variables. The best model of these initial models is kept.
#' On the basis of this best model the predictor variables are iteratively
#' increased and each of the remaining variables is tested for its improvement
#' of the currently best model. The process stops if none of the remaining 
#' variables increases the model performance when added to the current best model.
#' 
#' The internal cross validation can be run in parallel. See information
#' on parallel processing of carets train functions for details.
#' 
#' @note This validation is particulary suitable for 
#' leave-one-station-out cross validations where variable selection
#' MUST be based on the performance of the model on the hold out station.
#' A computational time expesnsive alternative is the best subset
#' selection (\code{\link{bss}}).
#' @author Hanna Meyer
#' @seealso \code{\link{train}},  \code{\link{bss}}, 
#' \code{\link{trainControl}},\code{\link{rfe}}
#' @examples
#' #' \dontrun{
#' data(iris)
#' ffsmodel <- ffs(iris[,1:4],iris$Species)
#' ffsmodel$finalModel$xNames
#' }
#' @export ffs
#' @aliases ffs


ffs <- function (predictors, 
                 response, 
                 method = "rf",
                 metric = ifelse(is.factor(response), "Accuracy", "RMSE"),
                 maximize = ifelse(metric == "RMSE", FALSE, TRUE),
                 trControl = trainControl(),
                 tuneLength = 3,
                 tuneGrid = NULL,
                 seed = 100){
  require(caret)
  if(maximize) evalfunc <- function(x){max(x,na.rm=T)}
  if(!maximize) evalfunc <- function(x){min(x,na.rm=T)}
  isBetter <- function (actmodelperf,bestmodelperf,maximize=maximize){
    ifelse (!maximize, return(actmodelperf < bestmodelperf),
            return(actmodelperf > bestmodelperf))
  }
  #### chose initial best model from all combinations of two variables
  twogrid <- t(data.frame(combn(names(predictors),2)))
  for (i in 1:nrow(twogrid)){
    set.seed(seed)
    model <- train(predictors[,twogrid[i,]],
                   response,
                   method=method,
                   trControl=trControl,
                   tuneLength = tuneLength,
                   tuneGrid = tuneGrid)
    ### compare the model with the currently best model
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
  }
  #### increase the number of predictors by one (try all combinations) 
  #and test if model performance increases
  for (k in 1:(length(names(predictors))-2)){
    startvars <- names(bestmodel$trainingData)[-which(
      names(bestmodel$trainingData)==".outcome")]
    nextvars <- names(predictors)[-which(
      names(predictors)%in%startvars)]
    if (length(startvars)<(k+1)){
      message(paste0("Note: No increase in performance found using more than ",
                     length(startvars), " variables"))
      return(bestmodel)
      
      break()
    }
    for (i in 1:length(nextvars)){
      set.seed(seed)
      model <- train(predictors[,c(startvars,nextvars[i])],
                     response,
                     method = method,
                     trControl = trControl,
                     tuneLength = tuneLength,
                     tuneGrid = tuneGrid)
      actmodelperf <- evalfunc(model$results[,names(model$results)==metric])
      if(isBetter(actmodelperf,bestmodelperf,maximize=maximize)){
        bestmodelperf <- actmodelperf 
        bestmodel <- model
      }
    }
  }
  
  return(bestmodel)
  
}
