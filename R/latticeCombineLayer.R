latticeCombineLayer <- function(trellis.list, ...) {
  
  outLayer <- function(x, y, ...) {
    x + as.layer(y, ...)
  }
  
  out <- Reduce(outLayer, trellis.list, ...)
  return(out)
}