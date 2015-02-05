#' convert u and v components to wind direction and wind speed
#' 
#' @description
#' this function converts u and v components to wind direction and wind speed
#' 
#' @param u a vector of u components
#' @param v a vector of v components
#' 
#' @return
#' a matrix
#' 
#' @seealso \code{\link{wdws2uv}}
#' 
#' @author
#' Tim Appelhans
#' 
#' @examples
#' set.seed(123)
#' wd <- as.integer(rnorm(10, 180, 90))
#' 
#' set.seed(123)
#' ws <- rnorm(10, 4, 1)
#' 
#' ## convert to u and v
#' wdws2uv(wd, ws)
#' 
#' ## convert back
#' uv <- wdws2uv(wd, ws)
#' uv2wdws(uv[, 1], uv[, 2]) 
#' 
#' @export uv2wdws
#' @aliases uv2wdws

uv2wdws <- function(u,v) {

  degrees <- function(radians) 180 * radians / pi
    
  mathdegs <- degrees(atan2(v, u))
  wdcalc <- ifelse(mathdegs > 0, mathdegs, mathdegs + 360)
  wd <- ifelse(wdcalc < 270, 270 - wdcalc, 270 - wdcalc + 360)
  ws <- sqrt(u^2 + v^2)

  return(cbind(wd, ws))

}