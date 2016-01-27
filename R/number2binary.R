#' Convert an integer to fixed-length binary format
#' 
#' @description
#' This is a wrapper function around \code{\link{intToBits}} that returns a bit
#' code in fixed-length integer format. The code is based on an SO post by
#' Paul Hiemstra, see \url{https://stackoverflow.com/questions/12088080/how-to-convert-number-into-binary-vector}.
#' 
#' @param number An \code{integer} to be converted.
#' @param no_bits \code{integer}. The desired length of the returned bit code.
#' @param to_char \code{logical}. If \code{TRUE}, the resulting bit code is 
#' returned as merged string.
#' 
#' @return
#' A bit code in fixed-length \code{integer} format or, if \code{to_char}, a
#' single \code{character}.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{intToBits}}.
#' 
#' @examples
#' number2binary(10)
#' number2binary(10, no_bits = 4)
#' number2binary(10, no_bits = 4, to_char = TRUE)
#' 
#' @export number2binary
#' @aliases number2binary
number2binary <- function(number, no_bits, to_char = FALSE) {
  
  binary_vector <- rev(as.numeric(intToBits(number)))
  
  if(!missing(no_bits)) {
    binary_vector <- binary_vector[-(1:(length(binary_vector) - no_bits))]
  }
  
  if (to_char)
    binary_vector <- paste(binary_vector, collapse = "")
  
  return(binary_vector)
}