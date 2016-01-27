#' panel function for raster extent objects
#' 
#' @description 
#' this function produces a \code{\link{panel.polygon}} from a 
#' \code{\link{extent}} object. This can then be used to visualise 
#' the extent on top of an \code{\link{spplot}} object.
#' 
#' @param ext extent object
#' @param ... additional arguments passed on to \code{\link{panel.polygon}} 
#' 
#' @return
#' A \code{\link{panel.polygon}} object
#' 
#' @author
#' Tim Appelhans
#' 
#' @examples
#' library(latticeExtra)
#' rst <- raster(ncols = 360, nrows = 180)
#' rst[] <- rnorm(360 * 180)
#' 
#' ex <- extent(c(27.5, 62.5, -17.5, 17.5))
#' 
#' spplot(rst) + layer(panel.extent(ex))
#' 
#' @export panel.extent
#' @aliases panel.extent

panel.extent <- function(ext, ...) {
  
  x <- c(ext@xmin, ext@xmax, ext@xmax, ext@xmin)
  y <- c(ext@ymin, ext@ymin, ext@ymax, ext@ymax)
  
  pp <- panel.polygon(x = x, y = y, ...)
  
  return(pp)
  
}

