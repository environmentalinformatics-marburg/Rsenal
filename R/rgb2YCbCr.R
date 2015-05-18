#' Convert from RGB to YCbCr color space
#' 
#' @description
#' This function transforms a RGB \code{Raster*} object to 'YCbCr' color space.  
#' 
#' @param rgb A \code{RasterStack} or \code{RasterBrick} object. A total of 3
#' bands is expected, namely red, green and blue. 
#' 
#' @seealso \code{\link{plotRGB}}
#' 
#' @author
#' Tim Appelhans
#' 
#' @examples
#' library(raster)
#' library(sp)
#' 
#' b <- brick(system.file("external/rlogo.grd", package="raster"))
#' 
#' ## using plotRGB
#' plotRGB(b)
#' 
#' ## convert brick to list
#' lout <- rgb2spLayout(b)
#' lout_alph <- rgb2spLayout(b, alpha = 0.5)
#' 
#' ## create random spatial points for plotting
#' df <- data.frame(dat = rnorm(100, 2, 1),
#'                  x = rnorm(100, 50, 20),
#'                  y = rnorm(100, 50, 25))
#' coordinates(df) <- ~x+y
#' 
#' ## plot spatial points with rgb background
#' spplot(df, sp.layout = lout)
#' spplot(df, sp.layout = lout_alph)
#' 
#' @export rgb2YCbCr
#' @aliases rgb2YCbCr
rgb2YCbCr <- function(r, g, b) {
  stopifnot(require(raster))
  y <- 16 + (65.481 * r + 128.553 * g + 24.966 * b)
  cb <- 128 + (-37.797 * r - 74.203 * g + 112 * b)
  cr <- 128 + (112 * r - 93.786 * g - 18.214 * b)
  stack(y, cb, cr)
}