#' modify spplot objects
#' 
#' @description
#' this function updates existing \code{\link{spplot}} objects so that
#' i) the axes are drawn at all four sites (as these usually represent
#' coordinates), ii) the default colorpalette from envinmr.theme() is used
#' and ii) the colorkey is plotted on top of the graph so
#' that the main = ... argument can be used to describe the colorkey.
#' 
#' @param spplot.obj the \code{\link{spplot}} object to be modified
#' @param ... additional arguments passed to \code{\link{update.trellis}}
#' 
#' @return
#' a single lattice plot object
#' 
#' @author
#' Tim Appelhans
#' 
#' @seealso
#' \code{\link{update.trellis}}, \code{\link{spplot}}
#' 
#' @examples
#' rst <- raster::raster(volcano)
#' 
#' p1 <- spplot(rst)
#' p1
#' 
#' p2 <- envinmrRasterPlot(p)
#' p2
#' 
#' ## change the colors as you like
#' p3 <- envinmrRasterPlot(p2, 
#'                         col.regions = 
#'                          colorRampPalette(brewer.pal(9, "YlGnBu"))(100))
#' p3
#' 
#' latticeCombineGrid(list(p2, p1, p3))
#' 
#' @export envinmrRasterPlot
#' @aliases envinmrRasterPlot


envinmrRasterPlot <- function(spplot.obj, ...) {
  library(latticeExtra)
  th <- envinmr.theme()
  tmp <- update(spplot.obj,
                col.regions = th$regions$col,
                scales = list(draw = TRUE, y = list(rot = c(90)), 
                              alternating = 3))
  tmp <- update(tmp, ...)
  
  tmp$legend <- 
    list(top = 
           list(fun = "draw.colorkey",
                args = 
                  list(key = list(space = "top",
                                  width = 1,
                                  height = 0.75,
                                  if(!is.null(tmp$panel.args.common$col.regions)) {
                                    col = tmp$panel.args.common$col.regions
                                  } else { 
                                    col = envinmr.theme()$regions.fun
                                  },
                                  at = tmp$panel.args.common$at),
                       draw = FALSE)))
  
  return(tmp)
}
