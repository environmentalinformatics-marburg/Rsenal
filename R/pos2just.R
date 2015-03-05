#' Convert integer text position specifier to string
#' 
#' @description
#' Convert integer position specifiers as supported by 
#' \code{\link{text}} to character position specifiers as supported by 
#' \strong{grid}-based text drawing functions (e.g. \code{\link{grid.text}}).
#' 
#' @param pos Integer. A position specifier for text annotations as used by 
#' \code{\link{text}}.
#' @param ... Further arguments. Currently not in use. 
#' 
#' @return
#' A character vector used as input for text justification in \strong{grid}-based text
#' drawing functions.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{text}}, \code{\link{grid.text}}
#' 
#' @examples  
#' set.seed(100)
#' pos <- sample(1:4, 5, replace = TRUE)
#' 
#' pos2just(pos)
#'               
#' @export pos2just
#' @aliases pos2just
pos2just <- function(pos, ...) {
  
  sapply(pos, function(x) {
    if (x == 1) {
      return("top")
    } else if (x == 2) {
      return("right")
    } else if (x == 3) {
      return("bottom")
    } else if (x == 4) {
      return("left")
    } else {
      stop("Invalid position specifier supplied: ", x)
    }
  })
  
}