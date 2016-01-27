#' create a list of seeds for trainControl()
#' 
#' @description 
#' this function creates a list of seeds to be used with \code{\link[caret]{trainControl}}
#' 
#' @param size the number of models to be evaluated
#' @param number the number of boot-starpping/cv repeats
#' @param seed the global seed to be set
#' 
#' @return
#' a list of integer seeds
#' 
#' @author
#' Tim Appelhans (largely taken from the examples of \code{\link[caret]{trainControl}})
#' 
#' @examples
#' trainSeeds(9, 3)
#' 
#' @export trainSeeds
#' @aliases trainSeeds

trainSeeds <- function(size, number, seed = 357) {
  
  set.seed(seed)
  seeds <- vector(mode = "list", length = number + 1)
  
  for(i in 1:(number + 1)) {
    seeds[[i]] <- sample.int(1000, size + 1)
    seeds[[(number + 1)]] <- sample.int(1000, 1)
  }
  
  return(seeds)
  
}
