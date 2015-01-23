#' Man-Kendall test for significant trends
#' 
#' @description
#' In contrast to the ordinary \code{\link{MannKendall}} function included in
#' the \bold{Kendall} package, this function returns significant trends 
#' (according to user-defined p value) only.   
#' 
#' @param x Numeric. A vector of data.
#' @param p Numeric. Significance level. 
#' 
#' @return
#' A numeric or, if the return value is not sufficiently significant, \code{NA}.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{MannKendall}}
#'  
#' @export significantMannKendall
#' @aliases significantMannKendall
significantMannKendall <- function(x, p = .001) {
  
  stopifnot(require(Kendall))
  
  mk <- MannKendall(x)
  
  sl <- mk$sl
  tau <- mk$tau
  tau[abs(sl) >= p] <- NA
  
  return(tau)
}