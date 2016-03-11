#' Convert from RGB to YCbCr color space
#' 
#' @description
#' This function transforms a RGB \code{Raster*} object to YCbCr color space.  
#' 
#' @param x A \code{RasterStack} or \code{RasterBrick} object. 3
#' bands are mandatory (usually red, green and blue).
#' @param r Integer, defaults to '1'. Index of the red channel. 
#' @param g Integer, defaults to '2'. Index of the green channel. 
#' @param b Integer, defaults to '3'. Index of the blue channel. 
#' 
#' @return 
#' A true-color \code{RasterStack} object converted to YCbCr color space.
#' 
#' @author
#' Florian Detsch, Tim Appelhans
#' 
#' @references
#' Deb, K. and Suny, A.H. (2014): Shadow Detection and Removal Based on YCbCr Color Space. Smart Computing Review 4, 23-33, doi:10.6029/smartcr.2014.01.003.
#' 
#' @examples
#' b_rgb <- brick(system.file("external/rlogo.grd", package="raster"))
#' plotRGB(b_rgb)
#' 
#' b_ycbcr <- rgb2YCbCr(b_rgb)
#' plot(b_ycbcr, 
#'      main = c("luminance", "blue-yellow chrominance", "green-red chrominance"))
#' 
#' @export rgb2YCbCr
#' @aliases rgb2YCbCr
rgb2YCbCr <- function(x, r = 1, g = 2, b = 3) {
  
  ### prerequisites
  
  ## compatibility check
  if (nlayers(x) < 3)
    stop("Argument 'x' needs to be a Raster* object with at least 3 layers (usually red, green and blue).")
  
  
  ### processing
  
  ## red, green and blue bands
  r <- x[[r]]
  g <- x[[g]]
  b <- x[[b]]
  
  ## luminance
  y <- 16 + (65.481 * r + 128.553 * g + 24.966 * b)
  
  ## blue-yellow chrominance
  cb <- 128 + (-37.797 * r - 74.203 * g + 112 * b)
  
  ## green-red chrominance
  cr <- 128 + (112 * r - 93.786 * g - 18.214 * b)
  
  ## return ycbcr raster stack
  stack(y, cb, cr)
}