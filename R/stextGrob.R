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
#' @seealso \code{\link{grid.text}}.
#' 
#' @references \url{https://github.com/baptiste/gridextra/blob/2c935bf9ec6d6d2a77cb0de2180b5e085d26ef39/R/stextGrob.r} 
#' (accessed on January 28, 2016).
#' @author Baptiste Auguie, Florian Detsch
#' @family grob userlevel
#' 
#' @examples
#' library(grid)
#' grid.newpage()
#' grid.rect(gp = gpar(fill = "grey"))
#' grid.stext("test")
#' 
#' @export
#' @aliases stextGrob grid.stext 
stextGrob <- function(label, x = unit(0.5, "npc"), y = unit(0.5, "npc")
                      , col = "white", fill = "black", r = 0.1 
                      , gp = gpar(), vp = NULL, name = NULL, ...) {
  
  let <- textGrob("a")
  
  tg <- textGrob(label, x, y, gp = gpar(col = fill), ...)
  
  tgl <- c(lapply(seq(0, 2 * pi, length = 18), function(theta) {
    textGrob(label, x + cos(theta) * r * grobWidth(let),
             y + sin(theta) * r * grobHeight(let), gp = gpar(col = col), ...)
  }), list(tg))
  
  
  gTree(children = do.call(gList, tgl), vp = vp, name = name, gp = gp)
}

#' @export
grid.stext <- function(...){
  g <- stextGrob(...)
  grid.draw(g)
  invisible(g)
}
