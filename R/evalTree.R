evalTree <- function(independ = NULL, 
                     depend, 
                     data, 
                     seed = 10, 
                     size = 1000,
                     minbucket = 100,
                     n.cores = 1,
                     ...) {

  
  ##############################################################################
  ##  
  ##  This function evaluates the performance of a so-called conditional
  ##  inference tree (see ?ctree for details) by calculating various scores, 
  ##  e.g. accuracy, probability of detection, false alarm ratio. See
  ##  http://cawcr.gov.au/projects/verification/ for further information.
  ##  
  ##  Parameters are as follows:
  ##
  ##  independ (numeric):   Column number(s) of independent variables.
  ##  depend (numeric):     Column number of dependent variable. 
  ##  data (data.frame):    Data frame containing independent and dependent
  ##                        variables.
  ##  seed (numeric):       Seed for random number generation.
  ##  size (numeric):       Size of the training sample.
  ##  minbucket (numeric):  Numeric vector specifying the minimum sum of weights
  ##                        in a terminal node.
  ##  n.cores (numeric):    Number of cores for parallel execution.
  ##  ...                   Further arguments passed on to ctree_control().
  ##
  ##############################################################################
  
  # Load required packages
  lib <- c("doParallel", "party")
  sapply(lib, function(...) stopifnot(require(..., character.only = T)))
  
  # Parallelization
  registerDoParallel(cl <- makeCluster(n.cores))
  
  # Draw random sample
  set.seed(seed)
  index <- sample(nrow(data), size)
  
  # Training and validation data
  train <- data[index, ]
  valid <- data[-index, ]
  
  # Setup ctree() formula
  if (is.null(independ)) {
    frml <- as.formula(paste(names(data)[depend], ".", sep = " ~ "))
  } else {
    frml <- as.formula(paste(names(data)[depend], 
                             paste(names(data)[independ], collapse = " + "), 
                             sep = " ~ "))
  }
  
  # Loop through different bucket sizes
  out.tree <- foreach(i = minbucket, .packages = "party", 
                      .combine = function(...) {
                        as.data.frame(rbind(...))
                      }) %dopar% {
                                                
    # Conditional inference tree
    tree <- ctree(frml, data = train, 
                  controls = ctree_control(minbucket = i, ...))
    
    # Prediction
    pred <- predict(tree, valid, type = "response")
    
    # Calculate and return scores
    table.result <- table(valid$fire, pred)
    
    C <- table.result[1,1]  # correct negatives
    F <- table.result[1,2]  # false alarm
    M <- table.result[2,1]  # misses
    H <- table.result[2,2]  # hits
    T <- C+F+M+H            # total      
    
    Acc = (H+C)/T           # accuracy
    BIAS = (H+F)/(H+M)      # bias score
    POD = H/(H+M)           # probability of detection
    PFD = F/(F+C)           # probability of false detection
    FAR = F/(H+F)           # false alarm ratio
    CSI = H/(H+F+M)         # critical success index
    H_random1 = ( ((H+F)*(H+M)) + ((C+F)*(C+M)) ) /T
    HSS = ((H+C)-H_random1)/(T-H_random1) # Heidke skill score
    HKD = (H/(H+M))-(F/(F+C))             # Hanssen-Kuipers discriminant
    H_random2 = ((H+M)*(H+F))/(H+F+M+C)
    ETS=(H-H_random2)/((H+F+M)-H_random2) # equitable threat score
    AreaR = ((M+H)/T) * 100               # rain area radar
    AreaS = ((F+H)/T) * 100               # rain area satellite
    
    score <- c(Acc, BIAS, POD, PFD, FAR, CSI, HSS, HKD, ETS,
               T, AreaR, AreaS, C, F, M, H)
    
    return(score)
  }
  
  # Set column and row names
  names(out.tree) <- c("ACC", "BIAS", "POD", "PFD", "FAR", "CSI", "HSS", "HKD", 
                       "ETS", "T", "AreaR", "AreaS", "C", "F", "M", "H")
  #   names(out.tree) <- c("accuracy", "bias_score", "probability_of_detection", 
  #                        "probability_of_false_detection", "false_alarm_ratio", 
  #                        "critical_success_index", "heidke_skill_score", 
  #                        "hanssen_kuipers_discriminant", "equitable_threat_score", 
  #                        "total_pixels", "rain_area_radar", "rain_area_satellite", 
  #                        "correct_negatives", "false_alarm", "misses", "hits")
  
  # Deregister cluster and return output
  stopCluster(cl)
  return(data.frame(minbucket = minbucket, out.tree))
}