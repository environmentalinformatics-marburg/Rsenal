#' panel function for maps
#' 
#' @description 
#' this function produces a \code{\link{panel.polygon}} from a 
#' \code{\link{map}} object. This can then be used to visualise 
#' the map on top of an \code{\link{spplot}} object.
#' NOTE: currently only tested for unprojected latlon data.
#' 
#' @param map.data the map type - see \code{\link{map}} database argument
#' @param col the colour of the fill
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
#' spplot(rst) + layer(panel.map("world"))
#' 
#' @export panel.map
#' @aliases panel.map

panel.map <- function(map.data, col = "transparent", ...) {
  
  library(maps)
  
  mm <- map(map.data, plot = FALSE, fill = TRUE, 
            col = col)
  
  pp <- panel.polygon(mm$x, mm$y, col = col, ...)
  
  return(pp)
  
}