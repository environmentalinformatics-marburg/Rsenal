#' Environmental Informatics Marburg colour palette 
#' 
#' @description
#' this function updates existing \code{\link{spplot}} objects so that
#' i) the axes are drawn at all four sites (as these usually represent
#' coordinates), ii) the default colorpalette from envinmr.theme() is used
#' and ii) the colorkey is plotted on top of the graph so
#' that the main = ... argument can be used to describe the colorkey.
#' 
#' @param n the number of colours to be created
#' 
#' @author
#' Tim Appelhans
#' 
#' @seealso
#' \code{\link{colorRampPalette}}
#' 
#' @examples
#' rst <- raster::raster(volcano)
#' 
#' p1 <- spplot(rst)
#' p1
#' 
#' p2 <- envinmrRasterPlot(p1, col.regions = envinmrPalette(100))
#' p2
#' 
#' @export envinmrPalette
#' @aliases envinmrPalette

envinmrPalette <- envinmr.theme()$regions.fun
