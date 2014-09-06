wdws2uv <- function(wd,ws) {

  radians <- function(degrees) degrees * pi / 180
  v <- -ws * cos(radians(wd))
  return(v)

}