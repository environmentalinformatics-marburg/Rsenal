hclClrs <- function(n, c = 60, l = 50) {
  
  library(colorspace)
  
  hcl(h = seq(230, 0, length.out = n), 
      c = c, l = l, fixup = TRUE)
  
}
