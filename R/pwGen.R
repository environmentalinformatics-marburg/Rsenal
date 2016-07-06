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

pwGen <- function(ndigits, seed = 123, master = NULL) {
  
  specials <- c("!", "_", "?", "+", "-", "=", "@", "&", "$", "%", "#")
  
  if (is.null(master)) {
    set.seed(seed)
    pw <- sample(c(letters, specials, 0:9, LETTERS), ndigits)
    cat(pw, "\n", sep = "")
  } else {
    master_seed <- readLines(master)[1]
    ms <- strsplit(master_seed, " ")[[1]]
    
    int <- lapply(seq(ms), function(i) {
      if (nchar(ms[i]) > 6) ms[i] <- substr(ms[i], 1, 6)
      sqrt(strtoi(ms[i], 36))
    })
    if (anyNA(int)) stop("bad master key")
    
    mi <- as.integer(round(Reduce("+", int)))
    set.seed(seed + mi)
    pw <- sample(c(letters, specials, 0:9, LETTERS), ndigits)
    cat(pw, "\n", sep = "")
  }
  
}
  