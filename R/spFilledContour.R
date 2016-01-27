#' Plot rasters as filled contours
#' 
#' @description
#' this function plots Raster* objects as filled contours
#' 
#' @param x a Raster* object
#' @param xlab,ylab character axis labels, see \code{\link{xyplot}}
#' @param ... Further arguments passed on to \code{\link[lattice]{levelplot}}
#' 
#' @seealso \code{\link{panel.filledcontour}}
#' 
#' @author
#' Tim Appelhans
#' 
#' @examples
#' library(raster)
#' library(sp)
#' 
#' data(meuse.grid)
#' coordinates(meuse.grid) = ~x+y
#' proj4string(meuse.grid) <- CRS("+init=epsg:28992")
#' gridded(meuse.grid) = TRUE
#' 
#' ## convert to raster
#' meuse_rst <- raster(meuse.grid, layer = "dist")
#' 
#' ## standard spplot
#' spplot(meuse_rst)
#' 
#' ## spFilledContour plot
#' plot.new() # needed when using package 'gridBase' (used in panel.filledcontour)
#' spFilledContour(x = meuse_rst)
#' 
#' 
#' @export spFilledContour
#' @aliases spFilledContour

spFilledContour <- function(x, xlab = "", ylab = "", ...) {
  
  mat <- raster::as.matrix(flip(x, 2))

  #plot.new()
  lattice::levelplot(t(mat), panel = panel.filledcontour,
                     row.values = as.numeric(unique(coordinates(x)[, 1])),
                     column.values = as.numeric(unique(coordinates(x)[, 2])),
                     xlab = xlab, ylab = ylab, ...)
  
}