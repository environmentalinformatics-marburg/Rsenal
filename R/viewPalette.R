#' view color ramps in a palette preview
#'
#' @description
#' This will create a plot of a color ramp
#'
#' @param col the color palette to be visualised
#' @param border color of the segment borders. Defaults to "transparent"
#' @param name an optional name to be put into the plot
#'
#' @author
#' Tim Appelhans
#'
#' @examples
#' library(colorspace)
#' library(viridisLite)
#' 
#' clrs <- list(rainbow = rainbow(100),
#'              rainbow_grey = desaturate(rainbow(100)),
#'              viridis = viridis(100),
#'              viridis_grey = desaturate(viridis(100)),
#'              inferno = inferno(100),
#'              inferno_grey = desaturate(inferno(100)),
#'              magma = magma(100),
#'              magma_grey = desaturate(magma(100)))
#' 
#' out <- lapply(seq(clrs), function(i) {
#'   viewPalette(clrs[[i]], name = names(clrs)[i])
#' })
#' 
#' Orcs::latticeCombineGrid(out)
#'
#' @export viewPalette
#' @aliases viewPalette

viewPalette <- function(col, border = "transparent", name = "") {
  
  n <- length(col)
  mat <- matrix(1:n, nrow = n)
  lattice::levelplot(mat, col.regions = col, scales = list(draw = FALSE),
                     ylab = "", xlab = "", colorkey = FALSE,
                     at = lattice::do.breaks(c(1, n), nint = n), asp = 0.2,
                     panel = function(...) {
                       lattice::panel.levelplot(...)
                       grid::grid.text(label = name, x = 0.9, y = 0.9,
                                       just = c("right", "top"))
                     })
}
