#' create squared matrix for use with raster::focal
#' 
#' @description 
#' this function produces a square matrix for use with \code{\link{focal}}.
#' User can set the size (must be odd) and center value (i.e. 0 or 1). All
#' non-center values are set to 1. 
#' 
#' @param n size (i.e. ncol and nrow)
#' @param center the value (weight) for the center of the window. Defaults to 0
#' 
#' @return
#' A \code{\link{matrix}} of 1s with \code{n} columns and \code{n} rows and 
#' a center value of \code{center}
#' 
#' @author
#' Tim Appelhans
#' 
#' @examples
#' standardFocalWindow() # 3 by 3 matrix
#' standardFocalWindow(n = 5, center = 1)
#' 
#' @export standardFocalWindow
#' @aliases standardFocalWindow

standardFocalWindow <- function(n = 3, center = 0L) {
  matrix(c(rep(1L, as.integer(n * n / 2)), center, 
           rep(1L, as.integer(n * n / 2))), 
         ncol = n)
}
