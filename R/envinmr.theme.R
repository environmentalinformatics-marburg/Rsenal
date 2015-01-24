#' Environmental Informatics Marburg lattice plotting theme
#' 
#' @description
#' This theme is a modified version of \code{\link{theEconomist.theme}}. 
#' Especially the regions colour setting has been modified to a 
#' conceptually pleasant rainbow colour palette (optimised for temperature).
#' 
#' @author
#' Tim Appelhans
#' 
#' @seealso
#' \code{\link{theEconomist.theme}}
#' 
#' @examples
#' library(lattice)
#' levelplot(matrix(rnorm(10000), 100, 100), 
#'           par.settings = envinmr.theme(), at = seq(-5, 5, 0.1))
#' 
#' @export envinmr.theme
#' @aliases envinmr.theme

envinmr.theme <- function(win.fontfamily = NULL, 
                          with.bg = FALSE, 
                          box = "black", 
                          ...) {
  library(latticeExtra)
  theme <- list(background = list(col = if (with.bg) "#D5E2E9" else "transparent"), 
                plot.line = list(col = "#00526D", lwd = 2.5), 
                superpose.line = list(col = c("#00526D", "#00A3DB", 
                                              "#7A2713", "#939598", 
                                              "#6CCFF6"), lwd = 2.5), 
                plot.symbol = list(col = "#00526D", pch = 16), 
                superpose.symbol = list(col = c("#00526D", "#00A3DB", 
                                                "#7A2713", "#939598", 
                                                "#6CCFF6"), pch = 16), 
                plot.polygon = list(col = "#00526D"), 
                superpose.polygon = list(col = c("#5F92A8", "#00526D", 
                                                 "#6CCFF6", "#00A3DB", 
                                                 "#A7A9AC")), 
                regions = list(col = colorRampPalette(c("#ebeaf7", "#83b0d6", 
                                                        "#55A1B1", "#63AD99", 
                                                        "#7FB972", "#B5BD4C", 
                                                        "#D9AD3C", "#E68E34", 
                                                        "#E6642C", "#D92120",
                                                        "#460000"))(100)),
                regions.fun = function(n) {
                  colorRampPalette(c("#ebeaf7", "#83b0d6", 
                                     "#55A1B1", "#63AD99", 
                                     "#7FB972", "#B5BD4C", 
                                     "#D9AD3C", "#E68E34", 
                                     "#E6642C", "#D92120",
                                     "#460000"))(n)
                },
                par.main.text = list(font = 1),
                reference.line = list(col = if (with.bg) "white" else "#aaaaaa", 
                                      lwd = 1.75), 
                dot.line = list(col = if (with.bg) "white" else "#aaaaaa", 
                                lwd = 1.75), 
                add.line = list(col = "#ED1C24", lwd = 1.5), 
                axis.line = list(col = box), box.3d = list(col = box), 
                strip.border = list(col = box), 
                strip.background = list(col = if (with.bg) "white" else "#CBDDE6"), 
                strip.shingle = list(col = if (with.bg) "#CBDDE6" else "#00A3DB", 
                                     alpha = 0.5), 
                axis.text = list(cex = 0.8), 
                box.dot = list(col = "#00526D", pch = "|", lwd = 1.75), 
                box.rectangle = list(fill = "#00526D", alpha = 0.5, 
                                     col = "#00526D", lwd = 1.75), 
                box.umbrella = list(col = "#00526D", lty = 1, lwd = 1.75))
  if (.Platform$OS.type == "windows" && !is.null(win.fontfamily)) {
    windowsFonts(TheEconomistLike = win.fontfamily)
    theme$grid.pars$fontfamily <- "TheEconomistLike"
  }
  else {
  }
  modifyList(modifyList(standard.theme("pdf"), theme), simpleTheme(...))
}
