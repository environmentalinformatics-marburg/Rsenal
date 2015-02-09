#' histogram matching for RasterLayers
#'
#' @description
#' this function is a wrapper around \code{\link{histmatch}} for
#' RasterLayers.
#'
#' @param x RasterLayer to be adjusted
#' @param y target RasterLayer
#' @param ttab logical, whether to return the transformation table
#' @param minval lower bound of the possible range for transformation
#' (if \code{NULL}, the minimum of both layers)
#' @param maxval upper bound of the possible range for transformation
#' (if \code{NULL}, the maximum of both layers)
#' @param by step size used to build the new historgram
#' (if \code{NULL}, 1 for integer master layer, 0.01 for double master layer)
#' @param ... additional arguments (mask) passed to \code{\link{histmatch}}
#'
#' @return
#' if \code{ttab = FALSE} a RasterLayer, if \code{ttab = TRUE} a list
#' with components
#' \code{recode} the trasnsformation table used to match the histograms
#' \code{newraster} the transformed RasterLayer
#'
#' @author
#' Tim Appelhans
#'
#' @seealso
#' \code{\link{histmatch}}
#'
#' @examples
#' library(latticeExtra)
#' tofix <- raster(matrix(rnorm(10000, 100, 3), nrow = 100, ncol = 100))
#' master <- raster(matrix(rnorm(10000, 130, 7), nrow = 100, ncol = 100))
#'
#' raw_p <- densityplot(tofix[], xlim = c(80, 160), plot.points = FALSE) +
#'  as.layer(densityplot(master[], xlim = c(80, 160),
#'           plot.points = FALSE, col = "red"))
#'
#' raw_p
#'
#' fixed <- histmatchRaster(tofix, master)
#' raw_p + as.layer(densityplot(fixed[], xlim = c(80, 160),
#'                  plot.points = FALSE, col = "black", lty = 2))
#'
#' @export histmatchRaster
#' @aliases histmatchRaster

histmatchRaster <- function(x, y,
                            ttab = FALSE,
                            minval = NULL,
                            maxval = NULL,
                            by = NULL,
                            ...) {

  library(landsat)
  library(raster)
  library(plyr)

  tbfxd <- as(x, "SpatialGridDataFrame")
  mstr <- as(y, "SpatialGridDataFrame")

  if (is.null(minval)) {
    minv <- floor(min(pmin(x[], y[]), na.rm = TRUE))
    } else minv <- minval  
  
  if (is.null(maxval)) {
    maxv <- ceiling(max(pmax(x[], y[]), na.rm = TRUE))
    } else maxv <- maxval

  if (is.null(by)) {
    step <- switch(typeof(x[]),
                   double = 0.01,
                   integer = 1L)
  } else step <- by

  minv <- round_any(minv, step, f = floor)
  maxv <- round_any(maxv, step, f = ceiling)
  print(paste("min: ", minv))
  print(paste("max: ", maxv))
  
  fix <- histmatch(mstr, tbfxd, minval = minv,
                   maxval = maxv, by = step, ...)
  fix_rst <- raster(fix$newimage)

  if (ttab) {
    return(list(recode = fix$recode,
                newraster = fix_rst))
  } else {
    return(fix_rst)
  }
}
