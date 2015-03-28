#' fixed width numeric/integer to character conversion
#' 
#' @description 
#' this function is a wrapper around a \code{\link{sprintf}} call for 
#' numeric and integer values (I seem to be unable to memorise the sprintf
#' call, hence the wrapper). 
#' 
#' @param x a numeric or integer vector
#' @param n the width (nchar) of the resulting character 
#' 
#' @return
#' A character vector of length x
#' 
#' @author
#' Tim Appelhans
#' 
#' @examples
#' fixWidth(1, 2)
#' fixWidth(c(1, 20, 300, 4000, 50000), 5)
#' 
#' @export fixWidth
#' @aliases fixWidth

fixWidth <- function(x, n = 1L) {
  
  expr <- paste0("%0", n, ".0f")
  sprintf(expr, x)
  
}
