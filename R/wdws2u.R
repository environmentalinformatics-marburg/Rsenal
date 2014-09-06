wdws2u <- function(wd,ws) {
  
  radians <- function(degrees) degrees * pi / 180
  u <- -ws * sin(radians(wd))
  return(u)
  
}