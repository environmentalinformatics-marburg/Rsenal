#' Index of Association
#' 
#' @description
#' This is a very basic index of association (IOA). It simply counts how often 
#' two vectors increase or decrease in parallel.
#' 
#' @param x a numeric vector 
#' @param y a numeric vector 
#' 
#' @return
#' numeric between 0 and 1. 0 means the two vectors are always 
#' changing in opposite directions, 1 means they are always changing
#' in identical directions
#' 
#' @author
#' Norman Matloff
#' 
#' @references
#' Matloff N. (2011), The Art of R Programming (and page 49 therein).
#' 
#' @examples
#' a <- 1:10
#' b <- 11:20
#' c <- 110:101
#' set.seed(123)
#' d <- rnorm(100, 7, 1)
#' set.seed(234)
#' e <- rnorm(100, 200, 10)
#' 
#' ioa(a, b)
#' ioa(a, c)
#' ioa(d, e)
#' 
#' @export ioa

ioa <- function(x, y) {

  ## DEFINE FUNCTION TO CONVERT VECTOR TO BINARY: 
  ## 1 FOR POSITIVE STEP, -1 FOR NEGATIVE
  findud <- function(v) {
    vud <- v[-1] - v[-length(v)]
    return(ifelse(vud > 0, 1, -1))
  }
  
  ## APPLY FINDUD TO BOTH VECTORS
  ## RETURN THE MEAN OF HOW OFTEN THEY ARE EQUAL
  ud <- lapply(list(x, y), findud)
  return(mean(na.exclude(ud[[1]] == ud[[2]])))
}
