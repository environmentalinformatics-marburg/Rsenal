gapImputeEval <- function(x, n, size, seed = 123, 
                          fixed.size = TRUE, funs, ...) {

  gappy <- gapCreate(x, n, size, seed, fixed.size)
  
  imptd <- lapply(seq(funs), function(i) {
    gapImpute(gappy, funs[[i]])
  })
  
  eval <- sapply(seq(imptd), function(i) {
    rsq <- summary(lm(x ~ imptd[[i]]))$r.squared
    ioa <- IOA(x, imptd[[i]])
    return(c(rsq, ioa))
  })
  
  out <- as.data.frame(eval)
  colnames(out) <- paste("fun", sprintf("%02.f", seq(funs)), sep = ".")
  out$stat <- c("rsq", "IOA")
  
  return(out)
}