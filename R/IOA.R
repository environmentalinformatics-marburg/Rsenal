IOA <- function(x, y) {

  ## DEFINE FUNCTION TO CONVERT VECTOR TO BINARY: 
  ## 1 FOR POSITIVE STEP, -1 FOR NEGATIVE
  findud <- function(v) {
    vud <- v[-1] - v[-length(v)]
    return(ifelse(vud > 0, 1, -1))
  }
  
  ## APPLY FINDUD TO BOTH VECTORS
  ## RETURN THE MEAN OF HOW OFTEN THEY ARE EQUAL
  ud <- lapply(list(x, y), findud)
  return(mean(na.exclude(ud[[1]] == ud[[2]])))
}
