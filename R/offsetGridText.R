#' Insert offset text annotation into 'trellis' plot
#' 
#' @description
#' This is a wrapper function around \code{\link{calcOffsetGridText}} and 
#' \strong{grid}-based text drawing functions (currently including 
#' \code{\link{grid.text}} and \code{\link{grid.stext}}) that automatically adds
#' offset text annotations to a 'trellis' plot.
#' 
#' @param x Numeric. A vector containing x coordinates, or a 2-column
#' matrix containing x and y coordinates.
#' @param y Numeric. A vector containing y coordinates, or \code{NULL} 
#' if x is a two-column matrix.
#' @param labels Character. The text to be written.
#' @param xlim Numeric. X-axis limits (xmin, xmax) of the current plot. If not
#' supplied, limits are automatically calculated from supplied x and y
#' coordinates.
#' @param ylim Numeric. Y-axis limits (ymin, ymax) of the current plot. If not
#' supplied, limits are automatically calculated from supplied x and y
#' coordinates.
#' @param pos Integer. Text position specifier(s) as used by \code{\link{text}}.
#' If not supplied, optimal text positions will be determined with respect to 
#' neighboring locations using \code{\link{thigmophobe}}. 
#' @param stext Logical. If \code{TRUE}, shadow text will be drawn (see 
#' \code{\link{grid.stext}}) instead of ordinary \code{\link{grid.text}}.
#' @param offset Numeric. The desired offset in normalized parent coordinates
#' ("npc", see \code{\link{unit}}).
#' @param ... Further arguments passed on the chosen \strong{grid} text 
#' drawing function. 
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{grid.text}}, \code{\link{grid.stext}}, \code{\link{thigmophobe}}, 
#' \code{\link{calcOffsetGridText}}.
#' 
#' @examples
#' # bing satellite image of mt. kilimanjaro
#' rst_kili <- kiliAerial(minNumTiles = 12,
#'                        projection = "+init=epsg:4326")
#' spl_kili <- rgb2spLayout(rst_kili, alpha = .8)
#' 
#' # kilimanjaro peaks
#' kibo <- c(-3.065053, 37.359031)
#' mawenzi <- c(-3.095436, 37.455061)
#' shira <- c(-3.038222, 37.210408)
#' 
#' mat_peaks <- rbind(kibo, mawenzi, shira)
#' df_peaks <- data.frame(peak = c("Kibo", "Mawenzi", "Shira"), 
#'                        x = mat_peaks[, 2], 
#'                        y = mat_peaks[, 1])
#' 
#' coordinates(df_peaks) <- ~ x + y
#' projection(df_peaks) <- "+init=epsg:4326"
#' 
#' # visualization
#' library(latticeExtra)
#' 
#' xlim_kili <- c(37.15, 37.55)
#' ylim_kili <- c(-3.25, -2.9)
#' 
#' p_kili <- spplot(df_peaks, auto.key = FALSE, col.regions = "white", 
#'                  xlim = xlim_kili, ylim = ylim_kili, cex = 2, pch = 20,
#'                  scales = list(draw = TRUE), sp.layout = spl_kili) + 
#'   layer(sp.points(df_peaks, cex = 1.5, pch = 20, col = "black"))
#' 
#' print(p_kili)
#' 
#' library(grid)
#' downViewport(trellis.vpname(name = "figure"))
#' offsetGridText(x = coordinates(df_peaks), labels = c("Kibo", "Mawenzi", "Shira"),  
#'                xlim = xlim_kili, ylim = ylim_kili, stext = TRUE, offset = .02,
#'                gp = gpar(fontsize = 20, fontfamily = "Bookman Old Style"))
#'                               
#' @export offsetGridText
#' @aliases offsetGridText
offsetGridText <- function(x, y = NULL, labels, xlim = NULL, ylim = NULL, 
                           pos = NULL, stext = FALSE, offset = .02, ...) {
  
  # install old version of 'gridExtra' package
  stopifnot(require(gridExtra))
  if (packageVersion("gridExtra") != "0.9.1") {
    cat("Installing 'gridExtra' version 0.9.1 ...\n")
    reinstall <- TRUE
    detach("package:gridExtra")
    install.packages("https://cran.r-project.org/src/contrib/Archive/gridExtra/gridExtra_0.9.1.tar.gz", 
                     repos = NULL)
    library(gridExtra)
  } else {
    reinstall <- FALSE
  }
  
  stopifnot(require(plotrix))
  
  if (is.matrix(x)) {
    y <- x[, 2]
    x <- x[, 1]
  }
  
  # best label locations (if 'pos' is not supplied)
  int_loc_lbl <- if (is.null(pos)) thigmophobe(x, y) else pos
  ch_loc_lbl <- pos2just(int_loc_lbl)
  
  # calculate offset point coordinates
  mat_crd_rel_off <- calcOffsetGridText(x = x, y = y, xlim = xlim, ylim = ylim, 
                                        pos = pos, offset = offset)
  
  for (tmp_cnt in 1:nrow(mat_crd_rel_off)) {
    if (stext) {
      grid.stext(labels[tmp_cnt], 
                 x = unit(mat_crd_rel_off[tmp_cnt, 1], "npc"), 
                 y = unit(mat_crd_rel_off[tmp_cnt, 2], "npc"), 
                 just = ch_loc_lbl[tmp_cnt], ...)
    } else {
      grid.text(labels[tmp_cnt], 
                x = unit(mat_crd_rel_off[tmp_cnt, 1], "npc"), 
                y = unit(mat_crd_rel_off[tmp_cnt, 2], "npc"),
                just = ch_loc_lbl[tmp_cnt], ...)
    }
  }
  
  if (reinstall) {
    detach("package:gridExtra", unload = TRUE)
    install.packages("gridExtra")
  }
  
  return(invisible())
      
}