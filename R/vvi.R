#' Visible vegetation index
#' 
#' @description
#' This function calculates a visible vegetation index (VVI) from a RGB 
#' \code{Raster*} object.  
#' 
#' @param rgb A \code{RasterStack} or \code{RasterBrick} object. 3
#' bands are mandatory (usually red, green and blue).
#' @param r Integer, defaults to '1'. Index of the red channel. 
#' @param g Integer, defaults to '2'. Index of the green channel. 
#' @param b Integer, defaults to '3'. Index of the blue channel. 
#' 
#' @return 
#' A VVI \code{RasterLayer}.
#' 
#' @author
#' Florian Detsch, Tim Appelhans
#' 
#' @references
#' Planetary Habitability Laboratory (2015): Visible Vegetation Index (VVI). Available online via \url{http://phl.upr.edu/projects/visible-vegetation-index-vvi}.
#' 
#' @examples
#' library(RColorBrewer)
#' 
#' data(gmap_hel)
#' plotRGB(gmap_hel)
#'
#' gmap_hel_veg <- vvi(gmap_hel)
#' plot(gmap_hel_veg, col = brewer.pal(5, "BrBG"), alpha = .5, add = TRUE)
#' 
#' @export vvi
#' @aliases vvi
vvi <- function(rgb, r = 1, g = 2, b = 3) {
  
  ### prerequisites
  
  ## compatibility check
  if (nlayers(rgb) < 3)
    stop("Argument 'rgb' needs to be a Raster* object with at least 3 layers (usually red, green and blue).")
  
  
  ### processing
  
  ## separate visible bands
  red <- rgb[[r]]
  green <- rgb[[g]]
  blue <- rgb[[b]]
  
  ## calculate vvi
  rst_vvi <- (1 - abs((red - 30) / (red + 30))) * 
    (1 - abs((green - 50) / (green + 50))) * 
    (1 - abs((blue - 1) / (blue + 1)))
  
  ## return vvi
  return(rst_vvi)
}