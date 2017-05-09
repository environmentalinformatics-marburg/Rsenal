#' Display Contour Lines
#' 
#' @description 
#' Display contour lines deduced from a digital elevation model (DEM) in a way 
#' compatible with all kinds of \code{"trellis"} graphics. 
#' 
#' @param dem DEM as \code{RasterLayer} or \code{character} file path.
#' @param zlevs.conts Elevation levels for which to include contour lines.
#' @param labels Labels associated with 'zlevs.conts'.
#' @param cex,col,labcex Additional graphical parameters, see 
#' \code{\link[graphics]{contour}} and \code{\link{par}}.
#' @param ... Additional arguments passed to \code{Rsenal:::panel.smoothconts}.
#' 
#' @return 
#' An object of class \code{"trellis"}.
#' 
#' @seealso 
#' \code{\link[graphics]{contour}}, \code{\link{Lattice}}. 
#' 
#' @author 
#' Tim Appelhans, Florian Detsch
#' 
#' @export visDEM
#' @name visDEM
visDEM <- function(dem, zlevs.conts = seq(1000, 5500, 500), 
                   labels = c(1000, "", 2000, "", 3000, "", 4000, "", 5000, ""), 
                   cex = 1.8, col = "grey50", labcex = 1, ...) {

  ## import dem (if necessary)
  if (!inherits(dem, "Raster")) {
    if (is.character(dem)) {
      dem <- raster::raster(dem)
    } else {
      stop("Please supply a 'RasterLayer' or a valid file path.")
    }
  }
  
  ## extract coordinates and flip dem  
  dem_flipped <- raster::flip(dem, "y")
  x <- sp::coordinates(dem_flipped)[, 1]
  y <- sp::coordinates(dem_flipped)[, 2]
  z <- dem_flipped[]
  
  ## create figure
  lattice::levelplot(z ~ x * y, colorkey = FALSE,  
                     panel = function(...) {
                       panel.smoothconts(zlevs.conts = zlevs.conts, labels = labels, 
                                         col = col, cex = cex, labcex = labcex, ...)
                     })
  
}

panel.smoothconts <- function(x, y, z, col = "grey30", 
                              contours = TRUE, cex = 1.8, labcex = 1,
                              zlevs.conts = seq(500, 6000, 500),
                              ...) {
  
  z <- matrix(z,
              nrow = length(unique(x)),
              ncol = length(unique(y)))
  rownames(z) <- unique(x)
  colnames(z) <- unique(y)
  
  if (!is.double(z)) storage.mode(z) <- "double"
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  if (lattice::panel.number() > 1) par(new = TRUE)
  par(fig = gridBase::gridFIG(), omi = c(0, 0, 0, 0), mai = c(0, 0, 0, 0))
  cpl <- lattice::current.panel.limits(unit = "native")
  graphics::plot.window(xlim = cpl$xlim, ylim = cpl$ylim,
                        log = "", xaxs = "i", yaxs = "i")
  # paint the color contour regions
  
  if (isTRUE(contours)) 
    graphics::contour(as.double(do.breaks(range(as.numeric(rownames(z))), nrow(z) - 1)),
                      as.double(do.breaks(range(as.numeric(colnames(z))), ncol(z) - 1)),
                      z, levels = as.double(zlevs.conts), 
                      add = TRUE, cex = cex,
                      axes = FALSE, lwd = 0.7,
                      col = col, # color of the lines
                      drawlabels = TRUE, # add labels or not
                      labcex = labcex, 
                      ...
    )
}
