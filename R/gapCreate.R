gapCreate <- function(x, n, size, seed = 123, fixed.size = TRUE) {
  
  set.seed(seed)
  gap.start <- sample(seq(length(x)), size = n, replace = FALSE)
  
  set.seed(seed)
  if(!fixed.size) size <- sample(seq(size), size = n, replace = TRUE)
  gap.end <- gap.start + size - 1

  gaps <- unlist(lapply(seq(length(gap.start)), function(i) {
    seq(gap.start[i], gap.end[i], 1)
  }))

  y <- x
  y[gaps] <- NA
  y <- y[1:length(x)]

  if (is.na(y[1])) y[1] <- x[1]
  if (is.na(y[length(y)])) y[length(y)] <- x[length(x)]
  
  return(y)
  
}
  