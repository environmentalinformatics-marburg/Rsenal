#' Calculate FDR for a given number of test runs
#' 
#' @description
#' this function calculates the False Discovery Rate for a statistical test
#' based on its sensitivity, the assumed prevalence of the effect and the
#' specificity of the test.
#' 
#' @param n the number of tests (e.g. the number of pixels for pixel-wise calculations)
#' @param prevalence the fraction of real effects present (this is a guess!!)
#' @param sensitivity the sensitivity of the test, i.e. how likely it is 
#' that the test will discover an effect if there is one
#' @param p.val the p-value to be acceptable
#' 
#' @author
#' Tim Appelhans
#' 
#' @examples
#' ## reproducing Figure 1 from \url{http://rsos.royalsocietypublishing.org/content/1/3/140216}
#' n <- 10000
#' prev <- 0.01
#' sens <- 0.8
#' p_val <- 0.05
#' 
#' statFDR(n, prev, sens, p_val)
#' 
#' 
#' @export statFDR
#' @aliases statFDR
#' 
statFDR <- function(n,
                    prevalence = 0.1,
                    sensitivity = 0.8,
                    p.val = 0.05) {
  
  spec <- 1 - p.val
  n_eff <- n * prevalence
  n_noeff <- n * (1 - prevalence)
  true_pos <- n_eff * sensitivity
  false_neg <- n_eff * (1 - sensitivity)
  true_neg <- n_noeff * spec
  false_pos <- n_noeff * p.val
  
  fdr <- false_pos / (false_pos + true_pos)
  
  return(fdr)
  
}
