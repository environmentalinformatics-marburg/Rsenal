# This function is intended to calculate the distance between two degrees of 
# latitude. If only one degree of latitude is supplied, the length of one degree 
# of latitude is calculated.

latDist <- function(lat_min,
                    lat_max = NULL,
                    toKm = TRUE,
                    ...) {
  
  # Length of degree at equator (in miles)
  lodl_eq <- 69.172
  
  # Conversion to kilometers (optional)
  if (toKm)
    lodl_eq <- lodl_eq / 0.62137
  
  # Conversion of degrees to radians for cos() application
  lat_min_rad <- lat_min * pi / 180
  
  # Length of degree longitude at given latitude
  lodl <- cos(lat_min_rad) * lodl_eq
  
  # Fractional length of degree latitude based on supplied minimum and maximum
  # latitude
  flodl <- lodl * abs(lat_max - lat_min)
  
  return(flodl)
}