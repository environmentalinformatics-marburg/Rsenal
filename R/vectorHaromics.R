vectorHarmonics <- function(x,
                            frq = 12,
                            st = c(1982, 1),
                            nd = c(1986, 12),
                            m = 2,
                            fun = median) {
  
  stopifnot(require(TSA))
  
  xts <- ts(x, start = st, end = nd, frequency = frq)
  har <- harmonic(xts, m = m)
  mod <- lm(xts ~ har)
  fit <- ts(fitted(mod), start = st, end = nd, frequency = frq)
  fit.med <- apply(matrix(fit, ncol = frq, byrow = T), 2, FUN = fun)
  
  return(fit.med)
  
}