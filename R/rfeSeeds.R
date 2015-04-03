#' create a list of seeds for rfeControl()
#' 
#' @description 
#' this function creates a list of seeds to be used with \code{\link{rfeControl}}
#' 
#' @param sizes the sizes of the feature selection iterations
#' @param number the number of boot-starpping/cv repeats
#' @param seed the global seed to be set
#' 
#' @return
#' a list of integer seeds
#' 
#' @author
#' Tim Appelhans (largely taken from the examples of \code{\link{rfeControl}})
#' 
#' @examples
#' rfeSeeds(1:9, 3)
#' 
#' @export rfeSeeds
#' @aliases rfeSeeds

rfeSeeds <- function(sizes, number, seed = 357) {
  
  set.seed(seed)
  seeds <- vector(mode = "list", length = number + 1)
  
  for(i in 1:(number + 1)) {
    seeds[[i]] <- sample.int(1000, length(sizes) + 1)
    seeds[[(number + 1)]] <- sample.int(1000, 1)
  }
  
  return(seeds)
  
}
