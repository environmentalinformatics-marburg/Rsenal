#' remove rows with NA values in selcted columns of a data frame
#' 
#' @description 
#' this function will remove all NA entries from selected columns of a data frame
#' 
#' @param df a data frame
#' @param cols a vector of column names or column numbers 
#' 
#' @return
#' A data frame 
#' 
#' @author
#' Tim Appelhans
#' 
#' @examples
#' df <- data.frame(a = 1:10, b = letters[1:10], c = rnorm(10, 2, 1))
#' df$a[sample(nrow(df), 3)] <- NA
#' df$b[sample(nrow(df), 2)] <- NA
#' df$c[sample(nrow(df), 1)] <- NA
#' 
#' df
#' 
#' removeNA(df, 1)
#' removeNA(df, 2)
#' removeNA(df, 3)
#' removeNA(df, c(1, 2))
#' removeNA(df, c(1, 2, 3))
#' 
#' @export removeNA
#' @aliases removeNA

removeNA <- function(df, cols) {
  
  identifyNA <- function(x) which(x != is.na(x))
  
  for (i in cols) {
    df <- df[identifyNA(df[i]), ]
  }
  return(df)
}