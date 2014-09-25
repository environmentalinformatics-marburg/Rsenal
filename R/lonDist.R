# This function is intended to calculate the distance between two degrees of 
# longitude at a given latitude. If only one degree of longitude is supplied, 
# the length of one degree of longitude is calculated at the given latitude.

lonDist <- function(lon_min,
                    lon_max = NULL,
                    lat,
                    toKm = TRUE,
                    ...) {
  
  # Length of degree at equator (in miles)
  lodl_eq <- 69.172
  
  # Conversion to kilometers (optional)
  if (toKm)
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