#' Retrieve a Bing (or Google) Maps Aerial Image
#' 
#' @description
#' Retrieve a Bing Maps aerial image centered on the Kilimanjaro region, 
#' Tanzania, by default. This image is aligned with the 1/12-degree AVHRR GIMMS 
#' NDVI grid covering the region. As an alternative, image retrieval is also 
#' available through Google Maps which is switched on automatically if the 
#' \strong{OpenStreetMap} package is not available. 
#' 
#' @param upperLeft,lowerRight \code{numeric}. Bounding box coordinates in the 
#' form \code{c(lat, long)}. If not specified and 'template' is missing, the 
#' resulting image defaults to the spatial extent of the study area covered in 
#' Detsch \emph{et al.} (2016; see References).
#' @param template \code{Extent}, or any object from which an \code{Extent} can 
#' be extracted, see \code{\link[raster]{crop}}. 
#' @param projection \code{character}, defaults to \code{"+init=epsg:4326"} 
#' (\url{http://spatialreference.org/ref/epsg/wgs-84/}). Desired coordinate 
#' reference system of the retrieved image.
#' @param type \code{character}. Tile server from which to download data, 
#' defaults to "bing" (only valid if package \strong{OpenStreetMap} is 
#' available). See \code{openmap} and \code{\link{gmap}} for currently 
#' available options.
#' @param ... Additional arguments passed to the underlying download functions, 
#' except for 'rgb' in \code{\link[dismo]{gmap}} which is set \code{TRUE}. 
#' 
#' @return
#' A 3-layered RGB \code{RasterStack}.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{openmap} and \code{openproj} in \strong{OpenStreetMap}, 
#' \code{\link{gmap}}.
#' 
#' @references 
#' Detsch F, Otte I, Appelhans T, Hemp A, Nauss T (2016) Seasonal and long-term 
#' vegetation dynamics from 1-km GIMMS-based NDVI time series at Mt. 
#' Kilimanjaro, Tanzania. Remote Sensing of Environment 178: 70-83, 
#' \doi{10.1016/j.rse.2016.03.007}.
#' 
#' @examples  
#' \dontrun{
#' img <- kiliAerial(minNumTiles = 12L, projection = "+init=epsg:21037")
#' plotRGB(img)
#' }          
#'                    
#' @export kiliAerial
#' @name kiliAerial
kiliAerial <- function(upperLeft, lowerRight, template = NULL, 
                       projection = "+init=epsg:4326", 
                       type = "bing",
                       ...) {
 
  ## reference extent based on regular 8-km gimms grid
  if (is.null(template) & (missing("upperLeft") | missing("lowerRight")))
    template <- raster::extent(readRDS(system.file("extdata/gimms_grid.rds", 
                                                   package = "Rsenal")))
  
  ## data retrieval via openstreetmap (if available)
  tps = try(formals(OpenStreetMap::openmap)$type, silent = TRUE)
  
  if (!inherits(tps, "try-error") & type %in% eval(tps)) {
    if (missing("upperLeft")) upperLeft <- c(ymax(template), xmin(template))
    if (missing("lowerRight")) lowerRight <- c(ymin(template), xmax(template))
    
    kili.map <- 
      OpenStreetMap::openproj(
        OpenStreetMap::openmap(upperLeft = upperLeft, 
                               lowerRight = lowerRight, 
                               type = type, ...), 
        projection = projection, method = "ngb")
  
    # rasterization
    kili.map <- raster::raster(kili.map)
    
  ## data retrieval via google
  } else {
    
    if (inherits(tps, "try-error")) {
      warning("Package 'OpenStreetMap' is not available. Retrieving image from Google Maps...\n")
      type = ifelse(type == "bing", "satellite", "terrain")
    }
    
    if (!(missing("upperLeft") & missing("lowerRight")))
      template <- raster::extent(upperLeft[2], lowerRight[2], 
                                 lowerRight[1], upperLeft[1])
    
    kili.map <- dismo::gmap(x = template, type = type, rgb = TRUE, ...)
    
    kili.map <- raster::projectRaster(kili.map, crs = projection, 
                                      method = "ngb")
  }  

  return(kili.map)
}