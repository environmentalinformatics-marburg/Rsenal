#' Draw Shadow Text
#'
#' @description 
#' Create and draw shadow text by wrapping a textual expression into a colored 
#' framing.
#' 
#' @param label A \code{character} or \code{\link{expression}} vector, see 
#' \code{\link{textGrob}}.
#' @param x,y Horizontal and vertical text position as \code{\link{unit}} 
#' objects passed to \code{\link{grid.text}}.
#' @param r Blur radius of colored framing as \code{numeric}.
#' @param col,fill Framing and fill color passed to \code{\link{gpar}}.
#' @param name,gp,vp Graphical parameters passed to \code{\link{gTree}}.
#' @param ... Additional arguments passed to \code{\link{grid.text}}.
#' 
#' @return A text grob created by \code{\link{gTree}}.
#' 
#' @references \url{https://rdrr.io/github/baptiste/gridplot/src/R/tmatrix.r} 
#' (accessed on 04 June 2018).
#' @author Baptiste Auguie, Florian Detsch
#' @family grob userlevel
#' 
#' @name stextGrob-deprecated
#' @aliases stextGrob grid.stext
#' @usage stextGrob(label, x = unit(0.5, "npc"), y = unit(0.5, "npc")
#' , col = "white", fill = "black", r = 0.1, gp = gpar(), vp = NULL
#' , name = NULL, ...)     
#' @seealso \code{\link{Rsenal-deprecated}}  
#' @keywords internal
NULL

#' @rdname Rsenal-deprecated
#' @section \code{stextGrob}:
#' For \code{stextGrob}, use \code{\link[Orcs]{stextGrob}} instead.
#' 
#' @export 
stextGrob <- function(label, x = unit(0.5, "npc"), y = unit(0.5, "npc")
                      , col = "white", fill = "black", r = 0.1 
                      , gp = gpar(), vp = NULL, name = NULL, ...) {
  
  .Deprecated("Orcs::stextGrob", "Rsenal")
  return(Orcs::stextGrob(label, x, y, col, fill, r, gp, vp, name, ...))
}