#' calculate the standard error of the mean
#' 
#' @description
#' this function calculates the standard error of the mean.
#' 
#' @param x a numeric vector
#' 
#' @author
#' Tim Appelhans
#' 
#' @examples
#' a <- rnorm(1000, 20, 5)
#' mean(a)
#' se(a)
#' 
#' @export se
#' @aliases se

se <- function(x) sd(x, na.rm = TRUE)/sqrt(length(na.exclude(x)))
