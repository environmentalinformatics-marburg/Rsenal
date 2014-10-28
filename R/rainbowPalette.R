#' Perceptually optimised rainbow colors
#' 
#' @description
#' This function returns a perceptually optimised rainbow color palette.
#' 
#' @param n the number of colours to be returned
#' 
#' @return
#' a vector of colours
#' 
#' @examples
#' pal <- function(col, border = "transparent", ...) {
#'   n <- length(col)
#'   plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
#'        axes = FALSE, xlab = "", ylab = "", ...)
#'   rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
#' } # courtesy Paul Murrell
#' 
#' pal(rainbowPalette(1000))
#' 
#' @export rainbowPalette
#' 
rainbowPalette <- function(n) {

  rnbw <- colorRampPalette(c("#413b93", "#524ab7", "#4065B1", "#488BC2", "#55A1B1", 
                             "#63AD99", "#7FB972", "#B5BD4C", "#D9AD3C", 
                             "#E68E34", "#E6642C", "#d94020", "#d92120"))
  
  rnbw(n)

}