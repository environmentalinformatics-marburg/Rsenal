#' Forward feature selection
#' @description A simple forward feature selection algorithm
#' @param predictors see \code{\link{train}}
#' @param response see \code{\link{train}}
#' @param method see \code{\link{train}}
#' @param metric see \code{\link{train}}
#' @param maximize see \code{\link{train}}
#' @param withinSD Logical Models are only selected if they are better than the 
#' currently best models Standard error
#' @param trControl see \code{\link{train}}
#' @param tuneLength see \code{\link{train}}
#' @param tuneGrid see \code{\link{train}}
#' @param seed A random number
#' @param runParallel Logical
#' @param ... arguments passed to the classification or regression routine 
#' (such as randomForest). Errors will occur if values for tuning parameters are 
#' passed here.
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
#' Using withinSE will favour models with less variables and
#' probably shorten the calculation time 
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
#'  \dontrun{
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
                 withinSD = TRUE,
                 trControl = trainControl(),
                 tuneLength = 3,
                 tuneGrid = NULL,
                 seed = 100,
                 runParallel = FALSE,
                 ...){
  require(caret)
  if(runParallel){
    require(doParallel)
    cl <- makeCluster(detectCores())
    registerDoParallel(cl)
  }
  n <- length(names(predictors))
  acc <- 0
  if(maximize) evalfunc <- function(x){max(x,na.rm=T)}
  if(!maximize) evalfunc <- function(x){min(x,na.rm=T)}
  isBetter <- function (actmodelperf,bestmodelperf,
                        bestmodelperfSD=NULL,
                        maximization=maximize,
                        withinSE=withinSD){
    if(withinSE){
      result <- ifelse (!maximization, actmodelperf < bestmodelperf-bestmodelperfSD,
              actmodelperf > bestmodelperf+bestmodelperfSD)
    }else{
      result <- ifelse (!maximization, actmodelperf < bestmodelperf,
              actmodelperf > bestmodelperf)
    }
    return(result)
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
    if(withinSD){
      #sd:
      #actmodelperfSD <- model$results[,names(model$results)==paste0(metric,"SD")][
      #  which(model$results[,names(model$results)==metric]==actmodelperf)]
      #se:
      actmodelperfSD <- Rsenal::se(
        sapply(unique(model$resample$Resample),
               FUN=function(x){mean(model$resample[model$resample$Resample==x,
                                                   metric])}))
      
    }
    if (i == 1){
      bestmodelperf <- actmodelperf
      if(withinSD){
        bestmodelperfSD <- actmodelperfSD
      }
      bestmodel <- model
    } else{
      if (isBetter(actmodelperf,bestmodelperf,maximize=maximize,withinSD=FALSE)){
        bestmodelperf <- actmodelperf 
        if(withinSD){
          bestmodelperfSD <- actmodelperfSD
        }
        bestmodel <- model
      }
    }
    acc <- acc+1
    print(paste0("maxmimum number of models that still need to be trained: ",
                 (((n-1)^2)+n-1)/2 + (((n-2)^2)+n-2)/2 - acc))
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
      if(withinSD){
        #actmodelperfSD <- model$results[,names(model$results)==paste0(metric,"SD")][
        #  which(model$results[,names(model$results)==metric]==actmodelperf)]
      
        actmodelperfSD <- Rsenal::se(
          sapply(unique(model$resample$Resample),
                 FUN=function(x){mean(model$resample[model$resample$Resample==x,
                                                     metric])}))
        
        }
      if(isBetter(actmodelperf,bestmodelperf,bestmodelperfSD,
                  maximize=maximize,withinSD=withinSD)){
        bestmodelperf <- actmodelperf 
        if(withinSD){
          bestmodelperfSD <- actmodelperfSD
        }
        bestmodel <- model
      }
      acc <- acc+1
      print(paste0("maxmimum number of models that still need to be trained: ",
                   (((n-1)^2)+n-1)/2 + (((n-2)^2)+n-2)/2 - acc))
    }
  }
  if(runParallel){
    stopCluster(cl)
  }
  return(bestmodel)
}
