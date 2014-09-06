#' create a function conditional to a set number of observations
#' 
#' @description
#' This function is a so-called closure. It will set up a function so that 
#' it will only be evaluated if the vector on which it is called has at least 
#' \code{n} observations. If it doesn't, it will return NA.
#' 
#' @param n the number of minimum observations for evaluation
#' @param fun the function to be evaluated
#' @param ... additional arguments passed to the function (e.g. na.rm = )
#' 
#' @return
#' a function that will calculate \code{fun} on a given vector if at least
#' \code{n} valid observations are present
#' 
#' @author
#' Tim Appelhans
#' 
#' @examples
#' ## create some dummy data
#' vec <- rnorm(100, 5, 1)
#' vec[sample(length(vec), 20)] <- NA #replace 20 values of vec with NA
#' 
#' f <- funN(70, mean, na.rm = TRUE)
#' f(vec)
#' 
#' f1 <- funN(90, mean, na.rm = TRUE)
#' f1(vec)
#' 
#' f3 <- funN(80, quantile, na.rm = TRUE)
#' f3(vec)
#' 
#' f4 <- funN(81, quantile, na.rm = TRUE)
#' f4(vec)

funN <- function(n, fun, ...) {
  function(x) {
    nvalid <- sum(!is.na(x))
    out <- fun(x, ...)
    if (nvalid >= n) out else NA
  }
}