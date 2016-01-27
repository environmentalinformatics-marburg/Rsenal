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
#' map. 
#' @param ... Further arguments passed on to \code{\link{openmap}}. 
#' 
#' @return
#' An RGB 'RasterStack' object.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{openmap}}, \code{\link{openproj}}, \code{\link{gmap}}.
#' 
#' @references 
#' Detsch, F., Otte, I., Appelhans, T., and T. Nauss (2015): Seasonal and 
#' long-term vegetation dynamics from NDVI time series at Mt. Kilimanjaro, 
#' Tanzania. Under review at \emph{Remote Sensing of Environment}.
#' 
#' @examples  
#' \dontrun{
#' ## download high-resolution image
#' img <- kiliAerial(minNumTiles = 12L)
#' 
#' ## create figure
#' plotRGB(img)
#' }          
#'                    
#' @export kiliAerial
#' @aliases kiliAerial
kiliAerial <- function(upperLeft, lowerRight, template = NULL, 
                       projection = "+init=epsg:21037", 
                       type = c("bing", "google"),
                       ...) {
 
  ## reference extent based on regular 8-km gimms grid
  if (is.null(template) & (missing("upperLeft") | missing("lowerRight")))
    template <- raster::extent(readRDS(system.file("extdata/gimms_grid.rds", 
                                                   package = "Rsenal")))
  
  ## data retrieval via openstreetmap
  if (type[1] == "bing") {
    if (missing("upperLeft")) upperLeft <- c(ymax(template), xmin(template))
    if (missing("lowerRight")) lowerRight <- c(ymin(template), xmax(template))
    
    kili.map <- 
      OpenStreetMap::openproj(
        OpenStreetMap::openmap(upperLeft = upperLeft, 
                               lowerRight = lowerRight, 
                               type = "bing", ...), 
        projection = projection)
  
    # rasterization
    kili.map <- raster::raster(kili.map)
    
  ## data retrieval via google
  } else {
    
    if (!(missing("upperLeft") & missing("lowerRight")))
      template <- raster::extent(upperLeft[2], lowerRight[2], 
                                 lowerRight[1], upperLeft[1])
    
    kili.map <- dismo::gmap(x = template, type = "satellite", scale = 2, 
                            rgb = TRUE)
    
    kili.map <- raster::projectRaster(kili.map, crs = projection, 
                                      method = "bilinear")
  }  

  return(kili.map)
}