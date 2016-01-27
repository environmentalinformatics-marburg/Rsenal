#' Extract legend from ggplot
#' 
#' @description
#' This is a magnificent piece of code to extract a legend from a ggplot, e.g. 
#' for further use with \code{\link{grid.draw}}. The code was originally taken
#' from \url{https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots}.
#' 
#' @param ... An object of class 'ggplot'.
#' 
#' @return
#' An object of class 'TableGrob'.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{ggplot}}, \code{\link{ggplot_gtable}}
#' 
#' @references
#' \url{https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots}
#' 
#' @examples  
#' library(ggplot2)
#' library(grid)
#' 
#' p <- qplot(mpg, wt, data = mtcars, colour = factor(cyl))
#' 
#' p_legend <- ggExtractLegend(p)
#' grid.draw(p_legend)
#'               
#' @export ggExtractLegend
#' @aliases ggExtractLegend
ggExtractLegend <- function(...) {
  
  tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(...)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  
  return(legend)
} 
