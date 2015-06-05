#' Calculate modal value from factor vector
#'
#' @description
#' This function computes the modal value, i.e. the value that appears most 
#' often, from a factor vector.
#' 
#' @param v Factor. The input vector.
#' 
#' @return
#' The modal value as \code{character}.
#' 
#' @author
#' Tim Appelhans
#' 
#' @examples
#' ## sample data (flights departing from Houston airports)
#' library(hflights)
#' data(hflights)
#' 
#' ## most common flight number
#' modeFactor(hflights$FlightNum)
#'  
#' @export modeFactor
#' @aliases modeFactor
modeFactor <- function(v) {
  a <- sort(table(v), decreasing = TRUE)
  r <- c()
  for(i in 1:length(a)) {
    if ( a[[1]] == a[[i]] ) {
      r <- c(r, names(a)[i])
    } else break; # since it's sorted, once we find
                  # a different value, we can stop
  }
  r
}
