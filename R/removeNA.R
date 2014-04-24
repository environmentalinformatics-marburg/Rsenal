removeNA <- function(df, cols) {
  
  identifyNA <- function(x) which(x != is.na(x))
  
  for (i in cols) {
    df <- df[identifyNA(df[i]), ]
  }
  return(df)
}