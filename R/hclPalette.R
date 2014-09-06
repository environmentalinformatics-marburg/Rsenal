#' create a color palette using hcl color space
#' 
#' @param n length of the palette (i.e. how many colors)
#' @param c chroma value for the palette
#' @param l luminance value for the palette
#' @param ... additional arguments to be passed to \code{\link{hcl}}
#' 
#' @author
#' Tim Appelhans
#' 
#' 

hclPalette <- function(n, c = 60, l = 50, ...) {
  
  library(colorspace)
  
  hcl(h = seq(270, 0, length.out = n), 
      c = c, l = l, ...)
  
}
