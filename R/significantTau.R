#' Compute Kendall's tau 
#' 
#' @description 
#' Apply the Mann-Kendall trend test (Mann, 1945) to a series of observations 
#' and return Kendall's tau (Kendall, 1937) based on a predefined significance 
#' level. In contrast to other readily available implementations, it is left to 
#' the user to decide whether or not to apply pre-whitening.
#' 
#' @param x A 'numeric' vector.
#' @param p Significance level to be tested.
#' @param prewhitening 'logical'. If \code{TRUE}, pre-whitening (see description 
#' in package \strong{zyp}) is applied prior to the Mann-Kendall trend test.
#' @param df 'logical'. If \code{TRUE}, a 'data.frame' holding the value of 
#' Kendall's tau and the referring significance level. 
#' @param ... Further arguments passed on to \code{\link{zyp.trend.vector}}.
#' 
#' @return Kendall's tau depending on the predefined significance level. If 
#' \code{p} is exceeded, this automatically defaults to \code{NA}.
#' 
#' @author 
#' Florian Detsch
#' 
#' @seealso 
#' \code{\link{MannKendall}}, \code{\link{zyp.trend.vector}}.
#' 
#' @examples 
#' ## Example taken from ?Kendall::MannKendall
#' library(Kendall)
#' data(PrecipGL)
#' plot(PrecipGL)
#'
#' ## Mann-Kendall trend test without pre-whitening
#' significantTau(PrecipGL, p = 0.001)
#' 
#' ## Mann-Kendall trend test with pre-whitening
#' significantTau(PrecipGL, p = 0.001, prewhitening = TRUE)
#' 
#' @export significantTau
#' @name significantTau
significantTau <- function(x, p = 0.001, prewhitening = FALSE, df = FALSE, ...) {
  
  # if only one unique value exists in 'x', return NA
  if (length(unique(x)) == 1)
    return(NA)
  
  # with prewhitening
  if (prewhitening) {
    
    # try to compute pre-whitened mann-kendall trend test
    try(mk <- zyp::zyp.trend.vector(x, ...), silent = TRUE)
    
    # if previous computation fails, return NA
    if (class(mk) == "try-error") {
      sig <- tau <- NA
    # else return kendall's tau and referring p value  
    } else {    
      
      id_sig <- grep("sig", names(mk))
      sig <- mk[id_sig]
      
      id_tau <- grep("tau", names(mk))
      tau <- mk[id_tau]
    }

    # without prewhitening
  } else {
    mk <- Kendall::MannKendall(x)
    
    sig <- mk$sl
    tau <- mk$tau
  }
  
  # return data.frame
  if (df) {
    return(data.frame(tau = tau, p = sig))

  # reject value of tau if p >= 0.001
  } else {
    
    if (is.logical(sig) | is.logical(p)) {
      return(NA)
    } else {
      if (sig >= p) {
        return(NA)
        # keep value of tau if p < 0.001
      } else {
        return(tau)
      }
    }
  }
}
  