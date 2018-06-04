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
#' @name pos2just-deprecated
#' @usage pos2just(pos, ...)     
#' @seealso \code{\link{Rsenal-deprecated}}  
#' @keywords internal
NULL

#' @rdname Rsenal-deprecated
#' @section \code{pos2just}:
#' For \code{pos2just}, use \code{Orcs:::pos2just} instead.
#' 
#' @export 
pos2just <- function(pos, ...) {
  
  .Deprecated("Orcs::offsetGridText", "Rsenal")
  Orcs:::pos2just(pos, ...)
}