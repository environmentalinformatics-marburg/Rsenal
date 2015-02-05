#' convert wind direction and wind speed to u and v components
#' 
#' @description
#' this function converts wind direction and wind speed to u and v components
#' 
#' @param wd a vector of wind directions
#' @param ws a vector of wind speeds
#' 
#' @return
#' a matrix
#' 
#' @seealso \code{\link{uv2wdws}}
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
#' @export wdws2uv
#' @aliases wdws2uv

wdws2uv <- function(wd, ws) {

  radians <- function(degrees) degrees * pi / 180
  u <- -ws * sin(radians(wd))
  v <- -ws * cos(radians(wd))
  return(cbind(u, v))

}
