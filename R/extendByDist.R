#' Extend an object's extent by distance
#'
#' @description
#' Similar to \code{\link{extend}}, this function extends the extent of an 
#' Extent, Raster* or Spatial* object. Rather than requiring the user 
#' to supply either a target extent or a desired number of columns and rows to 
#' add to the initial Raster*, extension is based on native map units, e.g. 
#' decimal degrees when the projection is EPSG:4326. 
#' 
#' @param x Extent, Raster* or Spatial* object.
#' @param width Numeric. Distance to add to original geometry (in native map 
#' units). Can be of length 1 (applied equally in each direction), 2 (first 
#' entry applied in vertical direction, second entry in horizontal direction) or 
#' 4 (applied separately in bottom/left/top/right direction).
#' 
#' @return
#' An extended extent.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{extend}}, \code{\link{extent}}
#' 
#' @examples
#' ## initial extent (see ?gmap_hel)
#' data(gmap_hel)
#' extent(gmap_hel)
#' 
#' ## extend by 0.1 degrees
#' extendByDist(gmap_hel, width = .1)
#' 
#' @export extendByDist
#' @aliases extendByDist
extendByDist <- function(x, width) {
 
  ## extract extent
  if (class(x) != "Extent")
    x <- extent(x)
    
  ## add distance
  int_len <- length(width)
  if (int_len == 1) {
    width <- rep(width, 4)
  } else if (int_len == 2) {
    width <- c(width[1], width[2], width[1], width[2])
  } else if (int_len == 3 | int_len > 4) {
    stop("Please supply a valid width (see ?extendBywidth).")
  }
  
  ymin(x) <- ymin(x) - width[1]
  xmin(x) <- xmin(x) - width[2]
  ymax(x) <- ymax(x) + width[3]
  xmax(x) <- xmax(x) + width[4]
  
  return(x)
}