if ( !isGeneric('gisView') ) {
  setGeneric('gisView', function(x, ...)
    standardGeneric('gisView'))
}

#' view raster layers interactively
#' 
#' @description 
#' this function produces an interactive GIS-like view of the specified 
#' raster layers on top of the specified base maps.
#' 
#' @param x a \code{\link{raster}}* object
#' @param cols color palette for the layers
#' @param na.color color for missing values
#' @param values a vector of values for the visualisation of the layers.
#' Per default these are calculated based on the supplied raster* object.
#' @param map.types character spcifications for the base maps.
#' see \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
#' for available options.
#' @param layer.opacity opacity of the layers
#' @param legend should a legend be plotted
#' @param legend.opacity opacity of the legend
#' @param trim should the raster be trimmed in case there are NAs on the egdes
#' @param ... additional arguments passed on to \code{\link{addLegend}} 
#' 
#' @author
#' Tim Appelhans
#' 
#' @examples
#' LUC1990 <- raster(system.file("LUC1990.rst", package = "Rsenal"))
#' LUC2006 <- raster(system.file("LUC2006.rst", package = "Rsenal"))
#' 
#' LUC1990_f <- as.factor(LUC1990)
#' LUC2006_f <- as.factor(LUC2006)
#' 
#' gisView(LUC1990_f)
#' gisView(LUC2006_f)
#' 
#' stck <- stack(LUC1990_f, LUC2006_f)
#' gisView(stck)
#' 
#' @export gisView
#' @name gisView
#' @rdname gisView
#' @aliases gisView,RasterStack-method

setMethod('gisView', signature(x = 'RasterLayer'), 
          function(x,
                   map = NULL,
                   cols = envinmrPalette(7), 
                   na.color = "transparent",
                   values = NULL,
                   map.types = c("OpenStreetMap",
                                 "Esri.WorldImagery"),
                   layer.opacity = 0.8,
                   legend = TRUE,
                   legend.opacity = 1,
                   trim = TRUE,
                   ...) {
  
            stopifnot(require(leaflet))
            
            is.fact <- raster::is.factor(x)
            
            llcrs <- CRS("+init=epsg:3857")@projargs
            
            if (!identical(projection(x), llcrs)) {
              cat("\n", "reprojecting to web mercator", "\n\n")
              if (is.fact) {
                x <- raster::projectRaster(x, crs = llcrs, method = "ngb")
              } else {
                x <- projectRaster(x, crs = llcrs)
              }
            }
            
            if (trim) x <- trim(x)
            
            if (is.fact) x <- raster::as.factor(x)
            
            if (is.null(values)) {
              if (is.fact) {
                values <- x@data@attributes[[1]]$ID
              } else {
                values <- seq(round(min(x[], na.rm = TRUE), 5),
                              round(max(x[], na.rm = TRUE), 5),
                              length.out = 10)
              }
            } else {
              values <- round(values, 5)
            }
            
            if (is.fact) {
              pal <- colorFactor(cols, as.factor(values), 
                                 levels = values, na.color = na.color)
            } else {
              pal <- colorNumeric(cols, values, na.color = na.color)
            }
            
            ## create base map using specified map types
            if (is.null(map)) {
              m <- leaflet() %>%
                addTiles(group = map.types[1]) %>%
                addProviderTiles(provider = map.types[2],
                                 group = map.types[2])
            } else {
              m <- map
            }
            
            ## add layers to base map
            m <- addRasterImage(map = m,
                                x = x,
                                colors = pal,
                                project = FALSE,
                                opacity = layer.opacity,
                                group = names(x))

            if (legend) {
              ## add legend
              m <- addLegend(map = m,
                             pal = pal,
                             opacity = legend.opacity, 
                             values = values, ...)
            }
            
            ## add layer control buttons
            if (is.null(map)) {
              m <- addLayersControl(map = m,
                                  position = "topleft",
                                  baseGroups = map.types,
                                  overlayGroups = names(x))
            } else {
              m <- addLayersControl(map = m,
                                    position = "topleft",
                                    baseGroups = map.types,
                                    overlayGroups = c(m$x$calls[[5]]$args[[2]], 
                                                      names(x)))
            }
            
            return(m)
            
          }
          
)

#' @describeIn gisView

setMethod('gisView', signature(x = 'RasterStack'), 
          function(x, 
                   ...) {
            
            m <- gisView(x[[1]], ...)
            
            for (i in 2:nlayers(x)) {
              m <- gisView(x[[i]], m, ...)
            }
            
            return(m)
            
          }
          
)

