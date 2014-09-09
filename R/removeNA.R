#' remove rows with NA values in selcted columns of a data frame
#' 
#' @export removeNA

removeNA <- function(df, cols) {
  
  identifyNA <- function(x) which(x != is.na(x))
  
  for (i in cols) {
    df <- df[identifyNA(df[i]), ]
  }
  return(df)
}