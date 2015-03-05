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
#' @param ... Further arguments. Currently not in use. 
#' 
#' @return
#' A numeric matrix containing offset coordinates.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{grid.text}}, \code{\link{text}}, \code{\link{thigmophobe}}
#' 
#' @examples
#' # required packages
#' library(grid)
#' library(plotrix)
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
#'               
#' @export calcOffsetGridText
#' @aliases calcOffsetGridText
calcOffsetGridText <- function(x, y = NULL, offset = 0.02, pos = NULL, 
                               xlim = NULL, ylim = NULL, ...) {
  
  stopifnot(require(plotrix))
  stopifnot(require(grid))
  
  if (is.matrix(x)) {
    y <- x[, 2]
    x <- x[, 1]
  }
  
  # relative ("npc") pointcoordinates
  num_xmin <- if (is.null(xlim)) min(x) - .04 * (max(x) - min(x)) else xlim[1]
  num_xmax <- if (is.null(xlim)) max(x) + .04 * (max(x) - min(x)) else xlim[2]
  num_xrng <- num_xmax - num_xmin
  num_x_rel <- (x-num_xmin) / num_xrng
  
  num_ymin <- if (is.null(ylim)) min(y) - .04 * (max(y) - min(y)) else ylim[1]
  num_ymax <- if (is.null(ylim)) max(y) + .04 * (max(y) - min(y)) else ylim[2]
  num_yrng <- num_ymax - num_ymin
  num_y_rel <- (y-num_ymin) / num_yrng
    
  # best label locations (if 'pos' is not supplied)
  int_loc_lbl <- if (is.null(pos)) thigmophobe(num_x_rel, num_y_rel) else pos
  ch_loc_lbl <- pos2just(int_loc_lbl)
  
  # apply offset to point coordinates
  ls_off <- lapply(1:length(num_x_rel), function(tmp_cnt) {

    tmp_x <- num_x_rel[tmp_cnt]
    tmp_y <- num_y_rel[tmp_cnt]
    
    ch_jst <- ch_loc_lbl[tmp_cnt]
    
    if (ch_jst %in% c("left", "right")) {
      if (ch_jst == "left") {tmp_x <- tmp_x+offset} else {tmp_x <- tmp_x-offset}
    } else {
      if (ch_jst == "top") {tmp_y <- tmp_y-offset} else {tmp_y <- tmp_y+offset}
    }
    
    tmp_mat <- matrix(c(tmp_x, tmp_y), byrow = TRUE, ncol = 2)

    return(tmp_mat)
  })

  mat_off <- do.call("rbind", ls_off)
  return(mat_off)
}