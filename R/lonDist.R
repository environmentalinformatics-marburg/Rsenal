#' Calculate distance between two longitudes
#'
#' @description
#' This function is intended to calculate the distance between two longitudes 
#' at a given latitude. If only one longitude location is supplied, the length 
#' of 1° of longitude is calculated at the given latitude.
#' 
#' @param lon_min \code{numeric}. The starting longitude.
#' @param lon_max \code{numeric}. The (optional) end longitude. If not supplied, 
#' the argument automatically equals \code{lon_min + 1}.
#' @param lat \code{numeric}. The latitude for which to calculate longitude
#' distance.
#' @param to_km \code{logical}. Determines whether the calculated distance is 
#' returned in kilometers (default) or miles.
#' @param ... Currently not in use.
#' 
#' @return
#' The linear distance between the starting and end longitude at the given
#' latitude or, if argument \code{lon_max} is not supplied, the length of 1° of 
#' longitude at the given latitude.
#' 
#' @author
#' Florian Detsch
#' 
#' @examples
#' lonDist(lon_min = 0, lat = 0)
#' 
#' @export lonDist
#' @aliases lonDist
lonDist <- function(lon_min,
                    lon_max = NULL,
                    lat,
                    to_km = TRUE,
                    ...) {
  
  # Length of degree at equator (in miles)
  lodl_eq <- 69.172
  
  # Conversion to kilometers (optional)
  if (to_km)
    lodl_eq <- lodl_eq / 0.62137
  
  # Conversion of degrees to radians for cos() application
  lat_rad <- lat * pi / 180
  
  # Length of degree longitude at given latitude
  lodl <- cos(lat_rad) * lodl_eq
  
  # Fractional length of degree longitude based on supplied minimum and maximum
  # longitude (optional)
  if (!is.null(lon_max)) {
    flodl <- lodl * (lon_max - lon_min)
    return(flodl)
  } else {
    return(lodl)
  }
  
}