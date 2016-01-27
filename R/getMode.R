#' compute the mode of column within a data frame
#' 
#' @description 
#' this function computes the modal value of a specified column of a data frame
#' 
#' @param df a data frame
#' @param col a column name or column number
#' @param side (requires documentation)
#' 
#' @return
#' a vector
#' 
#' @author
#' Tim Appelhans
#' 
#' @examples
#' df <- data.frame(a = c(1, 2, 3, 3, 3, 3, 4, 5, 6, 7), 
#'                  b = c("a", "b", "c", "d", "e", "e", "e", "e", "f", "g"))
#' 
#' getMode(df, "a")
#' getMode(df, 2)
#' 
#' @export getMode
#' @aliases getMode

getMode <- function(df, col, side = c("lower", "upper")) {
  
  library(dplyr)
  
  side <- side[1]
  var_count <- dplyr::count_(df, col, sort = TRUE)
  
  mod_vals <- which(var_count$n == max(var_count$n))
  
  out <- switch(side,
                "lower" = var_count[[col]][mod_vals][1],
                "upper" = var_count[[col]][mod_vals][length(mod_vals)])
  
  return(out)
}
