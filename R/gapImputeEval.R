gapImputeEval <- function(x, n, size, seed = 123, funs, ...) {

  gappy <- gapCreate(x, n, size, seed)
  
  imptd <- lapply(seq(funs), function(i) {
    gapImpute(gappy, funs[[i]])
  })
  
  eval <- sapply(seq(imptd), function(i) {
    rsq <- summary(lm(x ~ imptd[[i]]))$r.squared
    ioa <- IOA(x, imptd[[i]])
    return(c(rsq, ioa))
  })
  
  out <- eval
  rownames(out) <- c("rsq", "IOA")
  colnames(out) <- paste("fun", sprintf("%02.f", seq(funs)), sep = ".")
  
  return(out)
}