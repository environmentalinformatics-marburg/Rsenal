#' Calculate modal value from integer vector
#'
#' @description
#' This function computes the modal value, i.e. the value that appears most 
#' often, from an integer vector.
#' 
#' @param v Integer. The input vector.
#' 
#' @return
#' The \code{integer} modal value.
#' 
#' @author
#' Tim Appelhans
#' 
#' @examples
#' set.seed(10)
#' x <- sample(1:10, 100, replace = TRUE)
#' 
#' modeInteger(x)
#' 
#' @export modeInteger
#' @aliases modeInteger
modeInteger <- function(v) {
  a <- sort(table(v), decreasing = TRUE)
  r <- c()
  for(i in 1:length(a)) {
    if ( a[[1]] == a[[i]] ) {
      r <- c(r, as.integer(names(a)[i]))
    } else break; # since it's sorted, once we find
                  # a different value, we can stop
  }
  r
}
