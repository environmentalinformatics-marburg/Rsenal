#' Retrieve an (aerial) image of Mt. Kilimanjaro
#' 
#' @description
#' Based on functionalities offered by the R \bold{OpenStreetMap} package, 
#' this function per default retrieves a Bing aerial image centered on Mt. 
#' Kilimanjaro, Tanzania, that is aligned with the 8-km GIMMS NDVI grid covering 
#' the region.
#' 
#' @param upperLeft,lowerRight 'numeric'. Upper-left (lower-right) bounding box 
#' coordinates passed on to \code{\link{openmap}}. If not supplied and 
#' 'template' is missing, the returned image defaults to the extent of the study 
#' area covered in \strong{Detsch et al. (under review)}.
#' @param template 'Extent' object, or any object that is compatible with 
#' \code{\link{extent}}. 
#' @param projection 'character', defaults to "+init=epsg:21037". Output projection
#' of the retrieved image, see \code{\link{openproj}}.
#' @param type 'character', defaults to "bing". Tile server from which to get the
#' map, see \code{\link{openmap}}. 
#' @param rasterize 'logical'. If \code{TRUE} (default), the function returns a 
#' RGB 'RasterStack' object.
#' @param ... Further arguments passed on to \code{\link{openmap}}. 
#' 
#' @return
#' An object of class \code{OpenStreetMap} or, if \code{rasterize = TRUE}, 
#' a multi-layer 'RasterStack' object.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{openmap}}, \code{\link{openproj}}
#' 
#' @references 
#' Detsch, F., Otte, I., Appelhans, T., and T. Nauss (2015): Seasonal and 
#' long-term vegetation dynamics from NDVI time series at Mt. Kilimanjaro, 
#' Tanzania. Under review at \emph{Remote Sensing of Environment}.
#' 
#' @examples  
#' ## download high-resolution image
#' img <- kiliAerial(minNumTiles = 12L, rasterize = FALSE)
#' 
#' ## create figure
#' library(ggplot2)
#' autoplot(img)
#'               
#' @export kiliAerial
#' @aliases kiliAerial
kiliAerial <- function(upperLeft, lowerRight, template = NULL, 
                       projection = "+init=epsg:21037", 
                       type = "bing",
                       rasterize = TRUE,
                       ...) {
 
  # packages
  lib <- c("raster", "rgdal", "OpenStreetMap")
  jnk <- sapply(lib, function(x) stopifnot(require(x, character.only = TRUE)))
  
  # gimms extent
  if (is.null(template) & (missing("upperLeft") | missing("lowerRight")))
    template <- raster::extent(readRDS(system.file("extdata/gimms_grid.rds", 
                                                   package = "Rsenal")))
  
  # data retrieval
  if (missing("upperLeft")) upperLeft <- c(ymax(template), xmin(template))
  if (missing("lowerRight")) lowerRight <- c(ymin(template), xmax(template))
  
  kili.map <- openproj(openmap(upperLeft = upperLeft, lowerRight = lowerRight, 
                               type = type, ...), 
                       projection = projection)
  
  # rasterization (optional)
  if (rasterize)
    kili.map <- raster(kili.map)
  
  return(kili.map)
}