#' Calculate offset coordinates for grid-based text annotations
#' 
#' @description
#' Calculate offset coordinates for (\strong{grid}-based) text drawing 
#' functions, e.g. \code{\link{grid.text}}.
#' 
#' @param x Numeric. A vector containing x coordinates, or a 2-column
#' matrix containing x and y coordinates.
#' @param y Numeric. A vector containing y coordinates, or \code{NULL} 
#' if x is a two-column matrix.
#' @param offset Numeric. The desired offset in normalized parent coordinates
#' ("npc", see \code{\link{unit}}).
#' @param pos Integer. Text position specifier(s) as used by \code{\link{text}}.
#' If not supplied, optimal text positions will be determined with respect to 
#' neighboring locations using \code{\link{thigmophobe}}. 
#' @param xlim Numeric. X-axis limits (xmin, xmax) of the current plot. If not
#' supplied, limits are automatically calculated from supplied x and y
#' coordinates.
#' @param ylim Numeric. Y-axis limits (ymin, ymax) of the current plot. If not
#' supplied, limits are automatically calculated from supplied x and y
#' coordinates.
#' 
#' @return
#' A numeric matrix containing offset coordinates.
#' 
#' @author
#' Florian Detsch
#' 
#' @examples
#' \dontrun{
#' # required packages
#' library(grid)
#' library(plotrix)
#' library(lattice)
#'
#' # sample point data
#' x_cntr <- 330000
#' y_cntr <- 9650000
#' 
#' ls_xy <- lapply(c(x_cntr, y_cntr), function(i) {
#'   set.seed(i * 100)
#'   rnorm(10, i, 10000)
#' })
#' 
#' df <- data.frame(id = 1:10,
#'                  x = ls_xy[[1]],
#'                  y = ls_xy[[2]])
#' coordinates(df) <- ~ x + y
#' projection(df) <- "+init=epsg:21037"
#' 
#' # offset point locations
#' loc_lbl <- calcOffsetGridText(x = coordinates(df), offset = .015)
#' 
#' # vis
#' spplot(df, col.regions = "black", auto.key = FALSE)
#' 
#' downViewport(trellis.vpname(name = "figure"))
#' for (i in 1:nrow(df))
#'   grid.text(label = df$id[i], x = loc_lbl[i, 1], y = loc_lbl[i, 2],
#'             just = pos2just(thigmophobe(coordinates(df)))[i])
#' }
#'             
#' @name calcOffsetGridText-deprecated
#' @usage calcOffsetGridText(x, y = NULL, offset = 0.02, pos = NULL
#' , xlim = NULL, ylim = NULL)     
#' @seealso \code{\link{Rsenal-deprecated}}  
#' @keywords internal
NULL

#' @rdname Rsenal-deprecated
#' @section \code{calcOffsetGridText}:
#' For \code{calcOffsetGridText}, use \code{\link[Orcs]{offsetGridText}} and 
#' non-exported \code{Orcs:::calcOffsetGridText} instead.
#' 
#' @export
calcOffsetGridText <- function(x, y = NULL, offset = 0.02, pos = NULL
                               , xlim = NULL, ylim = NULL) {
  
  stopifnot(
    requireNamespace("Orcs")
  )
  
  .Deprecated("Orcs::offsetGridText", "Rsenal")
  Orcs:::calcOffsetGridText(x, y, offset, pos, xlim, ylim)
}