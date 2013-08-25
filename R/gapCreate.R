gapCreate <- function(x, n, size, seed = 123) {
  
  set.seed(seed)
  gap.start <- sample(seq(length(x)), size = n, replace = FALSE)
  gap.end <- gap.start + size - 1
  
  gaps <- unlist(lapply(seq(length(gap.start)), function(i) {
    seq(gap.start[i], gap.end[i], 1)
  }))
  
  x[gaps] <- NA
  
  return(x)
  
}
  