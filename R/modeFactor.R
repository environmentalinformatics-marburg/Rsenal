modeFactor <- function(v) {
  a <- sort(table(v), decreasing = TRUE)
  r <- c()
  for(i in 1:length(a)) {
    if ( a[[1]] == a[[i]] ) {
      r <- c(r, names(a)[i])
    } else break; # since it's sorted, once we find
                  # a different value, we can stop
  }
  r
}
