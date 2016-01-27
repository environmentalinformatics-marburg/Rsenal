#' generate passwords
#' 
#' generates a password based on a desired number of digits and a random seed.
#' 
#' @param ndigits desired number of digits
#' @param seed see \code{\link{set.seed}}
#' 
#' @return a password.
#' 
#' @author 
#' Tim Appelhans
#' 
#' @examples 
#' pwGen(4, seed = as.numeric(Sys.Date()))
#' 
#' @export pwGen
#' @aliases pwGen

pwGen <- function(ndigits, seed = 123) {
  
  set.seed(seed)
  
  specials <- c("!", "_", "?", "+", "-", "=", ">", "<", "#")
  pw <- sample(c(letters, specials, 0:9, LETTERS), ndigits)
  cat(pw, "\n", sep = "")
  
}
