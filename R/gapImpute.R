gapImpute <- function(x, fun, ...) {
  
  tmp <- fun(x, ...)
  if (length(tmp) == length(x)) x[is.na(x)] <- tmp[is.na(x)] else
    if (length(tmp) == 1) 
      x[is.na(x)] <- tmp else 
        stop("cannot use this function. fun must either produce a single value or a vector of the same length as x!")
  
  return(x)
  
}