pw <- function(ndigits, seed = 123) {
  
  set.seed(seed)
  
  specials <- c("!", "_", "?", "+", "-", "=", ">", "<", "#")
  pw <- sample(c(letters, specials, 0:9, LETTERS), ndigits)
  cat(pw, "\n", sep = "")
  
}
