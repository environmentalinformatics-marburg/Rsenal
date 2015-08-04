#' modify spplot objects
#' 
#' @description
#' this function updates existing \code{\link{spplot}} objects so that
#' i) the axes are drawn at all four sites (as these usually represent
#' coordinates) and ii) the colorkey is plotted on top of the graph so
#' that the main = ... argument can be used to describe the colorkey.
#' 
#' @param spplot.obj the \code{\link{spplot}} object to be modified
#' @param rot Rotation angle of the y-axis tick labels. Defaults to 90 degrees.
#' @param col Color vector. If not specified, color information will 
#' automatically be extracted from \code{spplot.obj}.
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
#' p2 <- envinmrRasterPlot(p1, col.regions = envinmrPalette(100))
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


envinmrRasterPlot <- function(spplot.obj, rot = 90, col, ...) {
  library(latticeExtra)
  tmp <- update(spplot.obj,
                scales = list(draw = TRUE, y = list(rot = rot), 
                              alternating = 3), 
                ...)
  
  tmp$legend <- 
    list(top = 
           list(fun = "draw.colorkey",
                args = 
                  list(key = list(space = "top",
                                  width = 1,
                                  height = 0.75,
                                  if(!is.null(tmp$panel.args.common$col.regions) &
                                     missing(col)) {
                                    col = tmp$panel.args.common$col.regions
                                  } else if (is.null(tmp$panel.args.common$col.regions) &
                                             missing(col)) {
                                    col = envinmr.theme()$regions.fun
                                  } else {
                                    col = col
                                  },
                                  at = tmp$panel.args.common$at),
                       draw = FALSE)))
  
  return(tmp)
}
