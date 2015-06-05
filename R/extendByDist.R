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
#' @param dist Numeric. Distance (in native map units) from original geometry.
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
#' rst <- kiliAerial(rasterize = TRUE, minNumTiles = 4L)
#' 
#' extent(rst)
#' extendByDist(rst, dist = 1000)
#' 
#' @export extendByDist
#' @aliases extendByDist
extendByDist <- function(x, dist) {
 
  ## required package
  stopifnot(require(raster))
  
  ## extract extent
  if (class(x) != "Extent")
    x <- extent(x)
    
  ## add distance
  xmin(x) <- xmin(x) - dist
  xmax(x) <- xmax(x) + dist
  ymin(x) <- ymin(x) - dist
  ymax(x) <- ymax(x) + dist
  
  return(x)
}