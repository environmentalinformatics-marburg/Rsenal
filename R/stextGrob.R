##' shadow text
##'
##' adds a blurred white version of a label below the text
##' @title stextGrob
##' @aliases stextGrob grid.stext
##' @param label see textGrob
##' @param r blur radius
##' @param x see textGrob
##' @param y see textGrob
##' @param just see textGrob
##' @param hjust see textGrob
##' @param vjust see textGrob
##' @param rot see textGrob
##' @param check.overlap see textGrob
##' @param default.units see textGrob
##' @param name see textGrob
##' @param gp see textGrob
##' @param vp see textGrob
##' @return gTree
##' @seealso \url{https://github.com/baptiste/gridextra/blob/2c935bf9ec6d6d2a77cb0de2180b5e085d26ef39/R/stextGrob.r} 
##' (accessed on January 28, 2016).
##' @author Baptiste Auguie
##' @family grob userlevel
##' @export
##' @examples
##' library(grid)
##' grid.newpage()
##' grid.rect(gp=gpar(fill="grey"))
##' grid.stext("test")
stextGrob <- function (label, r=0.1, x = unit(0.5, "npc"), y = unit(0.5, "npc"), 
                       just = "centre", hjust = NULL, vjust = NULL, rot = 0, check.overlap = FALSE, 
                       default.units = "npc", name = NULL, gp = gpar(), vp = NULL){
  
  let <- textGrob("a")
  
  tg <- textGrob(label=label, x=x, y=y, gp=gpar(col="black"),
                 just = just, hjust = hjust, vjust = vjust, rot = rot,
                 check.overlap = check.overlap, 
                 default.units = default.units)
  
  tgl <- c(lapply(seq(0, 2*pi, length=18), function(theta){
    
    textGrob(label=label,x=x+cos(theta)*r*grobWidth(let),
             y=y+sin(theta)*r*grobHeight(let), gp=gpar(col="white"),
             just = just, hjust = hjust, vjust = vjust, rot = rot,
             check.overlap = check.overlap, 
             default.units = default.units)
    
  }), list(tg))
  
  
  g <- gTree(children=do.call(gList, tgl), vp=vp, name=name, gp=gp)
  
}

##' @export
grid.stext <- function(...){
  g <- stextGrob(...)
  grid.draw(g)
  invisible(g)
}
