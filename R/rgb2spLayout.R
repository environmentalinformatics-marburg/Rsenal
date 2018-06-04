#' Convert an RGB RasterBrick/Stack to use with spplot
#'
#' @description
#' This function takes a red-green-blue 'RasterStack' (or 'RasterBrick') object
#' and produces a list with color information that can be passed on to the
#' \code{sp.layout} argument from \code{\link[sp]{spplot}}.
#'
#' @param x a RasterBrick or RasterStack
#' @param quantiles Upper and lower quantiles used for color stretching.
#' @param alpha Level of transparency.
#'
#' @author
#' Tim Appelhans, Florian Detsch
#'
#' @name rgb2spLayout-deprecated
#' @usage rgb2spLayout(x, quantiles = c(0.02,0.98), alpha = 1)     
#' @seealso \code{\link{Rsenal-deprecated}}  
#' @keywords internal
NULL

#' @rdname Rsenal-deprecated
#' @section \code{rgb2spLayout}:
#' For \code{rgb2spLayout}, use \code{\link[Orcs]{rgb2spLayout}} instead.
#' 
#' @export 
rgb2spLayout <- function(x,
                         quantiles = c(0.02, 0.98),
                         alpha = 1) {
  
  .Deprecated("Orcs::rgb2spLayout", "Rsenal")
  return(Orcs::rgb2spLayout(x, quantiles, alpha))
}
