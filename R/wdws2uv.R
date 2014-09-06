wdws2uv <- function(wd, ws) {

  radians <- function(degrees) degrees * pi / 180
  u <- -ws * sin(radians(wd))
  v <- -ws * cos(radians(wd))
  return(cbind(u, v))

}