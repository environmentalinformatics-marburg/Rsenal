#' Fit harmonic trend model to time-series values 
#' 
#' @description This is a wrapper function around \code{\link{harmonic}} from 
#' the \code{TSA} package that constructs harmonic functions for fitting a
#' harmonic trend model. 
#' 
#' @param x Numeric. A vector containing time-series values.
#' @param frq Integer, defaults to 12 (months). The number of observations per 
#' unit of time. See \code{\link{ts}}. 
#' @param st Numeric. A vector containing start year and month. 
#' @param nd Numeric. A vector containing end year and month.
#' @param m Integer, defaults to 2. The number of pairs of harmonic functions to
#' be created. See \code{\link{harmonic}}.
#' @param fun Aggregating function to be applied to the fitted values.
#' 
#' @return
#' A numeric vector. 
#' 
#' @author
#' Tim Appelhans
#' 
#' @seealso
#' \code{\link{harmonic}}
#' 
#' @export vectorHarmonics
#' @aliases vectorHarmonics
vectorHarmonics <- function(x,
                            frq = 12L,
                            st = c(1982, 1),
                            nd = c(1986, 12),
                            m = 2L,
                            fun = median) {
  
  xts <- ts(x, start = st, end = nd, frequency = frq)
  har <- TSA::harmonic(xts, m = m)
  mod <- lm(xts ~ har)
  fit <- ts(fitted(mod), start = st, end = nd, frequency = frq)
  fit.med <- apply(matrix(fit, ncol = frq, byrow = TRUE), 2, FUN = fun)
  
  return(fit.med)
  
}