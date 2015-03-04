#' Convert an RGB RasterBrick/Stack to use with spplot
#' 
#' @description
#' this function takes a RGB image stack/brick and produces a list with
#' the colorinformation which can be used in the sp.layout argument of spplot 
#' 
#' @param x a RasterBrick or RasterStack
#' 
#' @seealso \code{\link{plotRGB}}
#' 
#' @author
#' Tim Appelhans
#' 
#' @examples
#' library(raster)
#' library(sp)
#' 
#' b <- brick(system.file("external/rlogo.grd", package="raster"))
#' 
#' ## using plotRGB
#' plotRGB(b)
#' 
#' ## convert brick to list
#' lout <- rgb2spLayout(b)
#' lout_alph <- rgb2spLayout(b, alpha = 0.5)
#' 
#' ## create random spatial points for plotting
#' df <- data.frame(dat = rnorm(100, 2, 1),
#'                  x = rnorm(100, 50, 20),
#'                  y = rnorm(100, 50, 25))
#' coordinates(df) <- ~x+y
#' 
#' ## plot spatial points with rgb background
#' spplot(df, sp.layout = lout)
#' spplot(df, sp.layout = lout_alph)
#' 
#' @export rgb2spLayout
#' @aliases rgb2spLayout

rgb2spLayout <- function(x, alpha) {
  
  library(raster)
  
  if (!isTRUE(class(x) %in% c("RasterBrick", "RasterStack"))) {
    stop("x needs to be of class 'RasterBrick' or 'RasterStack'")
  }
  
  colim.recl <- reclassify(x, cbind(NA, 1))
  colim.recl[colim.recl < 0] <- 1
  
  ### use downloaded map for sp raster layout definition
  cols <- rgb(colim.recl[[1]][] / 255, 
              colim.recl[[2]][] / 255, 
              colim.recl[[3]][] / 255,
              alpha = alpha)
  
  map.cols <- matrix(cols,
                     nrow = colim.recl@nrows,
                     ncol = colim.recl@ncols)
  
  attr(map.cols, "class") <- c("ggmap", "raster")
  attr(map.cols, "bb") <- data.frame(ll.y = colim.recl@extent@ymin,
                                     ll.x = colim.recl@extent@xmin,
                                     ur.y = colim.recl@extent@ymax,
                                     ur.x = colim.recl@extent@xmax)
  
  bbMap <- attr(map.cols, 'bb')
  latCenter <- with(bbMap, ll.y + ur.y)/2
  lonCenter <- with(bbMap, ll.x + ur.x)/2
  height <- with(bbMap, ur.y - ll.y)
  width <- with(bbMap, ur.x - ll.x)
  
  ## Use sp.layout of spplot: a list with the function and its
  ## arguments
  sp.raster <- list('grid.raster', map.cols,
                    x = lonCenter, y = latCenter,
                    width = width, height = height,
                    default.units = 'native')
  
  return(sp.raster)
}
