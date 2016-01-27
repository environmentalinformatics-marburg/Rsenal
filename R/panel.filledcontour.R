#' create smooth contour fills for lattice levelplots
#' 
#' @description 
#' this panel function will create a smooth fill for a 
#' \code{\link{levelplot}} similar to \code{\link{filled.contour}}
#' 
#' @param x vector
#' @param y vector
#' @param z vector
#' @param subscripts optional 
#' @param fill.contours should regions between contours be (colour) filled?
#' @param at sequence for the fill contours
#' @param contours should contour lines be drawn?
#' @param at.contours sequence for the contour lines
#' @param col.regions colour palette for filling
#' @param col set of colours passed on to \code{\link{filled.contour}}
#' @param col.contours colour of contour lines
#' @param lty.contours linetype of contour lines
#' @param lwd.contours size of contours
#' @param draw.labels should labels be drawn along the contour lines?
#' @param ... currently not used
#' 
#' @author
#' Tim Appelhans 
#' (modified from: http://grokbase.com/t/r/r-help/089fhk4km2/r-creating-smooth-color-regions-with-panel-contourplot)
#' 
#' @examples
#' library(latticeExtra)
#' 
#' data(volcano)
#' 
#' plot.new()
#' levelplot(volcano, panel = Rsenal:::panel.filledcontour,
#'           at.contour = seq(100, 200, 20))

panel.filledcontour <- function(x, y, z, 
                                subscripts,
                                fill.contours = TRUE,
                                at,
                                contours = TRUE, 
                                at.contours = NULL, 
                                col.regions = cm.colors,
                                col = col.regions(length(at) - 1),
                                col.contours = "black",
                                lty.contours = 1,
                                lwd.contours = 1,
                                draw.labels = TRUE,
                                ...) {
  
  library("gridBase")
  library("plyr")
  
  z <- matrix(z[subscripts],
              nrow = length(unique(x[subscripts])),
              ncol = length(unique(y[subscripts])))
  if (!is.double(z)) storage.mode(z) <- "double"
  
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  if (panel.number() > 1) par(new = TRUE)
  par(fig = gridFIG(), omi = c(0, 0, 0, 0), mai = c(0, 0, 0, 0))
  cpl <- current.panel.limits()
  plot.window(xlim = cpl$xlim, ylim = cpl$ylim,
              log = "", xaxs = "i", yaxs = "i")
  
  # paint the color contour regions
  if (fill.contours) {
  
    fill_seq <- at
    
    .filled.contour(as.double(do.breaks(cpl$xlim, 
                                        nrow(z) - 1)),
                    as.double(do.breaks(cpl$ylim, 
                                        ncol(z) - 1)),
                    z, 
                    levels = as.double(fill_seq), 
                    col = col)
    
  } else NULL
    
  #add contour lines
  if (contours) {
    
    if (is.null(at.contours)) {
      cont_seq <- at
    } else {
      cont_seq <- at.contours
    }
    
    contour(as.double(do.breaks(cpl$xlim, nrow(z) - 1)),
            as.double(do.breaks(cpl$ylim, ncol(z) - 1)),
            z, 
            levels = as.double(cont_seq), 
            add = TRUE,
            col = col.contours, # color of the lines
            lty = lty.contours,
            lwd = lwd.contours,
            drawlabels = draw.labels)
    
  } else NULL
  
}
