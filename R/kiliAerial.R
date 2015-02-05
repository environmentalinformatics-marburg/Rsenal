#' Retrieve an (aerial) image of Mt. Kilimanjaro
#' 
#' @description
#' Based on the functionalities offered by the R \bold{OpenStreetMap} package, 
#' this function per default retrieves a Bing aerial image centered on Mt. 
#' Kilimanjaro, Tanzania, that is aligned with the 8-km GIMMS grid covering the
#' region.
#' 
#' @param projection Character, defaults to "+init=epsg:21037". Output projection
#' of the retrieved OSM data, passed on to \code{\link{openproj}}.
#' @param type Character, defaults to "bing". Tile server from which to get the
#' map, see \code{\link{openmap}}. 
#' @param template \code{Extent} object, or any object from which an \code{Extent}
#' object can be extracted. Defaults to the extent of the 8-km GIMMS grid used 
#' in \strong{Detsch et al. (in preparation)}.
#' @param rasterize Logical, defaults to FALSE. Determines whether a multi-layer 
#' \code{Raster*} object is returned.
#' @param ... Further arguments passed on to \code{\link{openmap}}. 
#' 
#' @return
#' An object of class \code{OpenStreetMap} or, if \code{rasterize = TRUE}, 
#' a \code{RasterStack} object.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{openmap}}, \code{\link{openproj}}
#' 
#' @examples  
#' library(ggplot2)
#' 
#' img <- kiliAerial(minNumTiles = 20)
#' autoplot(img)
#'               
#' @export kiliAerial
#' @aliases kiliAerial
kiliAerial <- function(projection = "+init=epsg:21037", 
                       type = "bing",
                       template = NULL, 
                       rasterize = FALSE,
                       ...) {
 
  # packages
  lib <- c("raster", "rgdal", "OpenStreetMap")
  jnk <- sapply(lib, function(x) stopifnot(require(x, character.only = TRUE)))
  
  # gimms extent
  if (is.null(template)) {
    template <- extent(c(36.99033, 37.74099, -3.415092, -2.83096))
  } else {
    template <- extent(template)
  }
  
  # data retrieval
  kili.map <- openproj(openmap(upperLeft = c(ymax(template), xmin(template)), 
                               lowerRight = c(ymin(template), xmax(template)), 
                               type = type, ...), 
                       projection = projection)
  
  # rasterization (optional)
  if (rasterize)
    kili.map <- raster(kili.map)
  
  return(kili.map)
}