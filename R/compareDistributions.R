#' compare two density distributions side by side
#' 
#' @description 
#' this function will produce a plot of two density functions displayed 
#' side by side
#' 
#' @param left numeric vector
#' @param right numeric vector
#' @param add.spread logical, whether to plot the spread (q25 to q75 and the median)
#' @param print.stats logical, whether to print summary statistics for each 
#' distribution
#' @param xlim,ylim numeric axis limits, see \code{\link{xyplot}}
#' @param clrs a character vector of length 2 specifying the colors 
#' for the filled density regions
#' @param xlab,ylab character axis labels, see \code{\link{plot}}
#' @param ... additional arguments passed to \code{\link{density}}
#' 
#' @return
#' A trellis object
#' 
#' @author
#' Tim Appelhans
#' 
#' @examples
#' \dontrun{
#' compareDistributions(rnorm(1000, 2, 3), rnorm(1000, -5, 1))
#' compareDistributions(rnorm(1000, 2, 3), rnorm(1000, -5, 1), 
#'                      add.spread = FALSE)
#' compareDistributions(rnorm(1000, 2, 3), rnorm(1000, -5, 1), 
#'                      add.spread = TRUE, clrs = c("red", "brown"))
#' compareDistributions(rnorm(1000, 2, 5), rnorm(1000, -5, 4), 
#'                      print.stats = FALSE, add.spread = FALSE)
#'                      
#' ## pass additional parameters to density() 
#' compareDistributions(rnorm(1000, 2, 5), rnorm(1000, -5, 4), 
#'                      print.stats = FALSE, add.spread = FALSE, bw = 5)
#' compareDistributions(rnorm(1000, 2, 5), rnorm(1000, -5, 4), 
#'                      print.stats = FALSE, add.spread = FALSE, bw = 8,
#'                      kernel = "rectangular")
#' compareDistributions(rnorm(1000, 2, 5), rnorm(1000, -5, 4), 
#'                      print.stats = FALSE, add.spread = TRUE, bw = 8,
#'                      n = 3)
#' compareDistributions(rnorm(1000, 2, 5), rnorm(1000, -5, 4), 
#'                      print.stats = TRUE, add.spread = TRUE, bw = 0.1)
#' compareDistributions(rnorm(1000, 2, 5), rnorm(1000, -5, 4), 
#'                      print.stats = TRUE, add.spread = TRUE, bw = 0.5)  
#' }                                        
#' 
#' @name compareDistributions-deprecated
#' @usage compareDistributions(left, right, add.spread = TRUE
#' , print.stats = TRUE, xlim = NULL, ylim = NULL
#' , clrs = c("purple", "lightblue"), xlab = "density", ylab = "value", ...)     
#' @seealso \code{\link{Rsenal-deprecated}}  
#' @keywords internal
NULL

#' @rdname Rsenal-deprecated
#' @section \code{compareDistributions}:
#' For \code{compareDistributions}, use \code{\link[Orcs]{compareDistributions}} 
#' instead.
#' 
#' @export 
compareDistributions <- function(left, 
                                 right,
                                 add.spread = TRUE,
                                 print.stats = TRUE,
                                 xlim = NULL,
                                 ylim = NULL,
                                 clrs = c("purple", "lightblue"),
                                 xlab = "density",
                                 ylab = "value",
                                 ...) {
  
  .Deprecated("Orcs::compareDistributions", "Rsenal")
  return(Orcs::compareDistributions(left, right, add.spread, print.stats, xlim
                                    , ylim, clrs, xlab, ylab, ...))
}
  